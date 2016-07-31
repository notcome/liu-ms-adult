{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeOperators     #-}

module LiuMS where

import Control.Monad
import Control.Monad.Reader
import Control.Monad.Trans.Except
import Control.Monad.IO.Class
import System.Directory

import Data.ByteString.Lazy.Char8 (pack)

import Text.Blaze.Html     (Html)
import Text.Blaze.Internal (unsafeByteString)

import Text.Pandoc.Options
import Text.Pandoc.Readers.Markdown
import Text.Pandoc.Writers.HTML

import Servant hiding (Handler)
import Servant.Utils.StaticFiles

import System.FilePath (addTrailingPathSeparator)
import Network.Wai
import Network.Wai.Application.Static (staticApp, defaultFileServerSettings)

import Servant.ContentType.PlainHtml
import Servant.ContentType.Processable
import LiuMS.Template.Basic

import LiuMS.Config
import LiuMS.CacheManager

type Gets = Get '[PlainHtml :<- Basic] Html

type SiteAPI = ConfiglessAPI :<|> ConfigfulAPI

type ConfiglessAPI = "static" :> Raw
type ConfigfulAPI  = Gets
     :<|> "about" :> Gets

type PostsAPI = Capture "year"  Integer
             :> Capture "month" Integer
             :> (Gets :<|> Raw)

type ProjectsAPI = Capture "project" String
                :> (Gets :<|> Raw)

hostPage :: FilePath -> Handler Html
hostPage page = do
  root <- askContentPath
  let pageDir  = root ++ "/contents/" ++ page ++ "/"
  let textPath = pageDir ++ "index.md"
  exists   <- liftIO $ doesFileExist textPath
  unless exists $ lift $ throwE fileNotFoundErr
  textFile <- liftIO $ readFile textPath
  let (Right markdown) = readMarkdown def textFile
  return $ writeHtml def markdown
  where
    fileNotFoundErr = err404 {
      errBody = pack $ page ++ " not found." }

configfulServer :: ServerT ConfigfulAPI Handler
configfulServer = load "index"
             :<|> load "about"
  where
    load :: FilePath -> Handler Html
    load path = do
      manager <- askCacheManager
      result  <- lift $ loadResource "md" manager path
      return $ unsafeByteString result

server :: Config -> Server SiteAPI
server config = serveDirectory (contentPath config ++ "/static")
           :<|> enter (runHandlerNat config) configfulServer

type Handler = ReaderT Config (ExceptT ServantErr IO)

runHandlerNat :: Config -> (Handler :~> ExceptT ServantErr IO)
runHandlerNat config = Nat (flip runReaderT config)
