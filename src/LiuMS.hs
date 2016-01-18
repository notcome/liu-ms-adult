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

import Text.Pandoc.Options
import Text.Pandoc.Readers.Markdown
import Text.Pandoc.Writers.HTML

import Servant

import Servant.ContentType.PlainHtml
import Servant.ContentType.Processable
import LiuMS.Template.Basic

type Gets = Get '[PlainHtml :<- Basic] Html

type SiteAPI    = Gets
  :<|> "about" :> Gets
  -- :<|> "posts"    :> PostsAPI
  -- :<|> "projects" :> ProjectsAPI
  :<|> "static" :> Raw

type PostsAPI = Capture "year"  Integer
             :> Capture "month" Integer
             :> (Gets :<|> Raw)

type ProjectsAPI = Capture "project" String
                :> (Gets :<|> Raw)

hostPage :: FilePath -> FilePath -> Server Gets
hostPage root page = do
  let pageDir  = root ++ "/contents/" ++ page ++ "/"
  let textPath = pageDir ++ "index.md"
  exists   <- liftIO $ doesFileExist textPath
  unless exists $ left fileNotFoundErr
  textFile <- liftIO $ readFile textPath
  let (Right markdown) = readMarkdown def textFile
  return $ writeHtml def markdown
  where
    fileNotFoundErr = err404 {
      errBody = pack $ page ++ " not found." }

hostStatic :: FilePath -> Server Raw
hostStatic = serveDirectory . (++ "/static/")

server :: FilePath -> Server SiteAPI
server path = let
  page = hostPage path
  in      page "index"
     :<|> page "about"
     :<|> hostStatic path
