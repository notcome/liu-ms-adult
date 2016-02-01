{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeOperators     #-}

module LiuMS where

import Control.Monad
import Control.Monad.Reader
import Control.Monad.Trans.Either
import Control.Monad.IO.Class
import System.Directory

import Data.ByteString.Lazy.Char8 (pack)

import Text.Blaze.Html     (Html)
import Text.Blaze.Internal (unsafeByteString)

import Text.Pandoc.Options
import Text.Pandoc.Readers.Markdown
import Text.Pandoc.Writers.HTML

import Servant
import Servant.API.WaiApp

import System.FilePath (addTrailingPathSeparator)
import Network.Wai
import Network.Wai.Application.Static (staticApp, defaultFileServerSettings)

import Servant.ContentType.PlainHtml
import Servant.ContentType.Processable
import LiuMS.Template.Basic

import LiuMS.Config
import LiuMS.CacheManager

type Gets = Get '[PlainHtml :<- Basic] Html

type SiteAPI    = Gets
  :<|> "about" :> Gets
  -- :<|> "posts"    :> PostsAPI
  -- :<|> "projects" :> ProjectsAPI
  :<|> "static" :> WaiApp

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
  unless exists $ lift (left fileNotFoundErr)
  textFile <- liftIO $ readFile textPath
  let (Right markdown) = readMarkdown def textFile
  return $ writeHtml def markdown
  where
    fileNotFoundErr = err404 {
      errBody = pack $ page ++ " not found." }

hostStatic :: FilePath -> Handler Application
hostStatic path = do
  root <- askContentPath
  return $ Servant.API.WaiApp.serveDirectory $ root ++ "/" ++ path

liuMSServer :: ServerT SiteAPI Handler
liuMSServer = load "index"
         :<|> load "about"
         :<|> hostStatic "static"
  where
    load :: FilePath -> Handler Html
    load path = do
      manager <- askCacheManager
      result  <- lift $ loadResource "md" manager path
      return $ unsafeByteString result

server :: Config -> Server SiteAPI
server config = enter (runHandlerNat config) liuMSServer
