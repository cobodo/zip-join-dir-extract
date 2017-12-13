{-# LANGUAGE ScopedTypeVariables #-}

module Lib
    ( parseAnyDir
    , parseAnyFile
    ) where

import Control.Monad.Catch
import Path

parseAnyFile :: MonadCatch m => FilePath -> FilePath -> m (Path Abs File)
parseAnyFile pwd file = parseAbsFile file `catch` \(e::SomeException) -> do
    pwdPath <- parseAbsDir pwd
    relPath <- parseRelFile file
    return $ pwdPath </> relPath

parseAnyDir :: MonadCatch m => FilePath -> FilePath -> m (Path Abs Dir)
parseAnyDir pwd dir = parseAbsDir dir `catch` \(e::SomeException) -> do
    pwdPath <- parseAbsDir pwd
    relPath <- parseRelDir dir
    return $ pwdPath </> relPath

