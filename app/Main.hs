module Main where

import Codec.Archive.Zip.Conduit.UnZip
import Codec.Binary.UTF8.String
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Resource
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BLC
import qualified Data.Conduit as C
import qualified Data.Conduit.Combinators as CC
import Data.Maybe
import Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Text.ICU.Convert
import Data.Void
import Path
import System.Directory
import System.Environment
import System.Exit
import System.IO

import Lib

type ConduitResource i o = C.Conduit i (ResourceT IO) o
type ZipStream = Either ZipEntry BS.ByteString
type FileNameBlob = BS.ByteString
type FileDataBlob = BS.ByteString
data FileFragmentData = FileFragmentData
    { nameFragmentData :: FileNameBlob
    , binaryFragmentData :: FileDataBlob
    } deriving (Eq, Show)
data FileFragment = FileFragment
    { nameFragment :: T.Text
    , binaryFragment :: FileDataBlob
    } deriving (Eq, Show)


separator = T.pack "__"

extractFiles :: ConduitResource FileFragment Void
extractFiles = do
    maybeFragment <- C.await
    case maybeFragment of
        Nothing -> return ()
        Just fragment -> do
            let path = nameFragment fragment
            liftIO $ T.putStrLn path
            liftIO $ BS.appendFile (T.unpack path) (binaryFragment fragment)
            extractFiles

modifyPath :: T.Text -> Path Abs Dir -> ConduitResource FileFragmentData FileFragment
modifyPath prefix dir = do
    maybeFragment <- C.await
    case maybeFragment of
        Nothing -> return ()
        Just (FileFragmentData fileName fileData) -> do
            conv <- liftIO $ open "Shift_JIS" Nothing
            let uniFileName = toUnicode conv fileName
            let basename = T.reverse $ T.takeWhile (\c -> c /= '/') $ T.reverse uniFileName
            if basename == T.pack "Thumbs.db"
                then modifyPath prefix dir
                else do
                    let targetDir = T.pack $ toFilePath dir
                    let filePath = targetDir <> prefix <> (T.replace (T.pack "/") separator uniFileName)
                    --liftIO $ T.putStrLn filePath
                    C.yield $ FileFragment filePath fileData
                    modifyPath prefix dir

zipToFiles' :: Maybe BS.ByteString -> ConduitResource ZipStream FileFragmentData
zipToFiles' entryName = do
    --liftIO $ print entryName
    buf <- C.await
    --liftIO $ print buf
    case buf of
        Nothing -> return ()
        Just eith -> case eith of
            Left ze -> do
                let zen = zipEntryName ze
                let siz = fromJust $ zipEntrySize ze
                if '/' == (last $ BSC.unpack zen)
                    then zipToFiles' Nothing
                    else
                        if siz == 0
                            then do
                                C.yield $ FileFragmentData zen $ BSC.pack []
                                zipToFiles' $ Just zen
                            else zipToFiles' $ Just zen
            Right bs -> case entryName of
                Nothing -> error "Actual Data with not FilePath"
                Just zen -> do
                    C.yield $ FileFragmentData zen bs
                    zipToFiles' $ Just zen

zipToFiles :: ConduitResource ZipStream FileFragmentData
zipToFiles = zipToFiles' Nothing

extractTo :: T.Text -> Path Abs Dir -> ConduitResource ZipStream Void
extractTo prefix dir = zipToFiles C..| modifyPath prefix dir C..| extractFiles

runStream :: FilePath -> FilePath -> IO ()
runStream dir path = do
    pwd <- getCurrentDirectory
    targetPath <- parseAnyDir pwd $ dir
    zipPath <- parseAnyFile pwd path
    let prefix = flip T.append separator $ T.pack $ toFilePath $ filename zipPath
    info <- runResourceT $ C.runConduit $
        CC.sourceFileBS (toFilePath zipPath) C..| C.fuseUpstream unZipStream (extractTo prefix targetPath)
    putStr "zip comment: "
    BSC.putStrLn $ zipComment info

main :: IO ()
main = do
    hSetEncoding stdout utf8
    args <- getArgs
    if length args < 2 then do
        hPutStrLn stderr "Usage: unzip_join_prefix zipfile [zipfile2 [...]] target_dir"
        exitFailure
    else do
        let dir = last args
        let paths = reverse . tail . reverse $ args
        mapM_ (runStream dir) paths

