{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeOperators     #-}





{-# LANGUAGE TypeFamilies     #-}

module LiuMS where

import Control.Monad
import Control.Monad.Reader
import Control.Monad.Trans.Either
import Control.Monad.IO.Class
import System.Directory

import Data.ByteString.Lazy.Char8 (pack)

import Text.Blaze.Html (Html)

import Text.Pandoc.Options
import Text.Pandoc.Readers.Markdown
import Text.Pandoc.Writers.HTML

import Servant
import System.FilePath (addTrailingPathSeparator)
import Network.Wai
import Network.Wai.Application.Static (staticApp, defaultFileServerSettings)

import Servant.ContentType.PlainHtml
import Servant.ContentType.Processable
import LiuMS.Template.Basic





import Data.Typeable (Typeable)
import Servant.Server.Internal
data MyRaw deriving Typeable

instance HasServer MyRaw where

  type ServerT MyRaw m = m Application

  route Proxy wrappedApp request respond = do
    (Right rawApp) <- runEitherT wrappedApp
    rawApp request (respond . succeedWith)

type Gets = Get '[PlainHtml :<- Basic] Html

type SiteAPI    = Gets
  :<|> "about" :> Gets
  -- :<|> "posts"    :> PostsAPI
  -- :<|> "projects" :> ProjectsAPI
  :<|> "static" :> MyRaw

type PostsAPI = Capture "year"  Integer
             :> Capture "month" Integer
             :> (Gets :<|> Raw)

type ProjectsAPI = Capture "project" String
                :> (Gets :<|> Raw)

type LiuMSHandler = ReaderT FilePath (EitherT ServantErr IO)

hostPage :: FilePath -> LiuMSHandler Html
hostPage page = do
  root <- ask
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

hostStatic :: FilePath -> LiuMSHandler Application
hostStatic path = do
  root <- ask
  return $ serveDirectory $ root ++ path
  where
    serveDirectory = staticApp . defaultFileServerSettings . addTrailingPathSeparator

liuMSServer :: ServerT SiteAPI LiuMSHandler
liuMSServer = hostPage "index"
         :<|> hostPage "about"
         :<|> hostStatic "static"

server :: FilePath -> Server SiteAPI
server root = enter (runLiuMSHandlerNat root) liuMSServer

runLiuMSHandlerNat' :: FilePath
                    -> (forall a. LiuMSHandler a -> EitherT ServantErr IO a)
runLiuMSHandlerNat' path handler = runReaderT handler path

runLiuMSHandlerNat :: FilePath
                   -> (LiuMSHandler :~> EitherT ServantErr IO)
runLiuMSHandlerNat path = Nat $ runLiuMSHandlerNat' path
