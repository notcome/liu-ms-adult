{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module LiuMS where

import Control.Monad
import Control.Monad.Trans.Either
import Control.Monad.IO.Class
import System.Directory

import Data.ByteString.Lazy.Char8 (pack)

import Text.Blaze.Html (Html)
import Clay            (Css)

import Text.Pandoc.Options
import Text.Pandoc.Readers.Markdown
import Text.Pandoc.Writers.HTML

import Servant
import Servant.Server
import Servant.Utils.StaticFiles

import Servant.ContentType.PlainHtml
import Servant.ContentType.Processable
import Servant.ContentType.ClayCss
import LiuMS.Template.Basic
import LiuMS.Css.Basic

type Gets = Get '[PlainHtml :<- Basic] Html

type SiteAPI    = Gets
  :<|> "about" :> Gets
  -- :<|> "posts"    :> PostsAPI
  -- :<|> "projects" :> ProjectsAPI
  :<|> "css"    :> CssAPI
  :<|> "static" :> Raw

type PostsAPI = Capture "year"  Integer
             :> Capture "month" Integer
             :> (Gets :<|> Raw)

type ProjectsAPI = Capture "project" String
                :> (Gets :<|> Raw)

type CssAPI = "basic" :> Get '[ClayCss] Css

rootPath :: FilePath
rootPath = "./site/"

page :: FilePath -> Server Gets
page filename = do
  let path = rootPath ++ filename
  exists   <- liftIO $ doesFileExist path
  unless exists $ left fileNotFoundErr
  textFile <- liftIO $ readFile path
  let (Right markdown) = readMarkdown def textFile
  return $ writeHtml def markdown
  where
    fileNotFoundErr = err404 { errBody = pack $ filename ++ " not found." }

css :: Server CssAPI
css = return cssBasic

static :: Server Raw
static = serveDirectory $ rootPath ++ "static/"

server :: Server SiteAPI
server = page "index.md"
    :<|> page "about.md"
    :<|> css
    :<|> static
