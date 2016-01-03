module Main where

import Network.Wai
import Network.Wai.Handler.Warp
import Servant

import qualified LiuMS as LMS

siteAPI :: Proxy LMS.SiteAPI
siteAPI = Proxy

app :: Application
app = serve siteAPI LMS.server

main :: IO ()
main = run 8081 app
