module Main where

import System.Environment

import Network.Wai
import Network.Wai.Handler.Warp
import Servant

import qualified LiuMS as LMS

app :: FilePath -> Application
app = serve apiType . LMS.server
  where
    apiType :: Proxy LMS.SiteAPI
    apiType = Proxy

main :: IO ()
main = do
  args <- getArgs
  if length args == 0
  then putStrLn "usage: liums [PORT] [CONTENTS_DIR]"
  else let port = read (args !! 0) :: Int
           path = args !! 1
       in do
         putStrLn $ "listen on port " ++ show port
         run port (app path)
