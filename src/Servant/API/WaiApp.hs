{-# LANGUAGE TypeFamilies #-}

module Servant.API.WaiApp where

import Control.Monad.Trans.Either
import Data.Typeable (Proxy, Typeable)

import Network.Wai
import Servant.Server.Internal

import System.FilePath                (addTrailingPathSeparator)
import Network.Wai.Application.Static (staticApp, defaultFileServerSettings)

--data WaiApp deriving Typeable

--instance HasServer WaiApp config where
--  type ServerT WaiApp m = m Application
--  route :: Proxy WaiApp -> Config config -> Delayed (Server WaiApp) -> Router
--  route _ wrappedApp request respond = do
--    (Right rawApp) <- runEitherT wrappedApp
--    rawApp request (respond . succeedWith)

serveDirectory :: FilePath -> Application
serveDirectory = staticApp
               . defaultFileServerSettings
               . addTrailingPathSeparator
