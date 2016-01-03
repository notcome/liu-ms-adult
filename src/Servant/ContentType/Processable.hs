{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Servant.ContentType.Processable where

import Data.ByteString.Lazy (ByteString)
import Data.Proxy
import Data.Typeable        (Typeable)

import Network.HTTP.Media   (MediaType)

import Servant.API          (Accept (..), MimeRender (..))

data to :<- from deriving Typeable

class Processable method from where
  type Processed method :: *
  process               :: Proxy method -> from -> (Processed method)

instance Accept to => Accept (to :<- from) where
  contentType _ = contentType (Proxy :: Proxy to)

instance {-# OVERLAPPABLE #-}
  ( MimeRender to (Processed from)
  , Processable from a
  ) => MimeRender (to :<- from) a where
    mimeRender _ = mimeRender' Proxy . process' Proxy where
      mimeRender' :: Proxy to -> Processed from -> ByteString
      mimeRender' = mimeRender
      process' :: Proxy from -> a -> Processed from
      process' = process
