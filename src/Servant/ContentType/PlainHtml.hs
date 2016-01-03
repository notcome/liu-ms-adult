{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Servant.ContentType.PlainHtml where

import Data.Typeable        (Typeable)

import Data.ByteString.Lazy (fromStrict)
import Data.Text            (Text)
import Data.Text.Encoding   (encodeUtf8)

import Network.HTTP.Media   ((//), (/:))
import Servant.API          (Accept (..), MimeRender (..))

data PlainHtml deriving Typeable

class ToPlainHtml a where
  toPlainHtml :: a -> Text

instance Accept PlainHtml where
  contentType _ = "text" // "html" /: ("charset", "utf-8")

instance {-# OVERLAPPABLE #-}
  ToPlainHtml a => MimeRender PlainHtml a where
    mimeRender _ = fromStrict . encodeUtf8 . toPlainHtml
