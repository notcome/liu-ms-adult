module LiuMS.CacheManager where

import Prelude hiding (readFile, writeFile)

import Control.Monad
import Control.Monad.Trans.Except
import Control.Monad.IO.Class
import Data.ByteString
import System.Directory
import System.FilePath

import           Servant
import qualified Data.ByteString.Lazy.Char8 as BL

import LiuMS.Compiler

data CacheManager = CacheManager FilePath FilePath [(String, Compiler)]

loadResource :: String -> CacheManager -> FilePath
             -> ExceptT ServantErr IO ByteString
loadResource suffix (CacheManager contents cache compilers) path = do
  compiler       <- getCompiler suffix compilers
  liftIO $ Prelude.putStrLn resourceFullPath
  resourceExists <- liftIO $ doesFileExist resourceFullPath
  unless resourceExists $ throwE errResourceNotFound

  if cachable compiler
    then do
      maybeCache <- tryLoadCache compiler
      case maybeCache of
        Just result -> return result
        Nothing     -> loadResource' compiler
    else loadResource' compiler

  where
    filename, resourceFullPath, cacheFullPath :: FilePath
    filename = path ++ "." ++ suffix
    --filename = "cmn-hans-cn." ++ suffix
    resourceFullPath = joinPath [contents, filename]
    cacheFullPath    = joinPath [cache,    filename]

    errResourceNotFound :: ServantErr
    errResourceNotFound = err500 {
      errBody = BL.pack $ "Resource for " ++ path ++ " not found." }

    errCompilerNotFound :: ServantErr
    errCompilerNotFound = err500 {
      errBody = BL.pack $ "Compiler for ." ++ suffix ++ " not found." }

    getCompiler :: String -> [(String, Compiler)]
                -> ExceptT ServantErr IO Compiler
    getCompiler suffix compilers =
      case lookup suffix compilers of
        Nothing -> throwE errCompilerNotFound
        Just x  -> return x

    tryLoadCache :: Compiler -> ExceptT ServantErr IO (Maybe ByteString)
    tryLoadCache compiler = do
      cacheExists <- liftIO $ doesFileExist cacheFullPath
      if cacheExists
        then liftIO $ do
          cacheTime    <- getModificationTime cacheFullPath
          resourceTime <- getModificationTime resourceFullPath
          if cacheTime < resourceTime
            then return Nothing
            else Just <$> readFile cacheFullPath
        else return Nothing

    loadResource' :: Compiler -> ExceptT ServantErr IO ByteString
    loadResource' compiler = liftIO $ do
      result <- compile compiler resourceFullPath
      createDirectoryIfMissing True $ joinPath [cache, path]
      writeFile cacheFullPath result
      return result
