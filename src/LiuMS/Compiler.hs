module LiuMS.Compiler where

import qualified Data.ByteString as BS

data Compiler = Compiler
  { cachable :: Bool
  , compile  :: FilePath -> IO BS.ByteString
  }

unpureCompile :: (BS.ByteString -> BS.ByteString)
              -> (FilePath -> IO BS.ByteString)
unpureCompile f path = f <$> BS.readFile path
