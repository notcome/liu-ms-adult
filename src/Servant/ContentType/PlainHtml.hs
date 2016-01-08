{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Servant.ContentType.PlainHtml where

import Data.Typeable        (Typeable)

import Network.HTTP.Media   ((//), (/:))
import Servant.API          (Accept (..), MimeRender (..))

import Text.Blaze.Html               (ToMarkup, toMarkup)
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)

data PlainHtml deriving Typeable

instance Accept PlainHtml where
  contentType _ = "text" // "html" /: ("charset", "utf-8")

instance {-# OVERLAPPABLE #-}
  ToMarkup a => MimeRender PlainHtml a where
    mimeRender _ = renderHtml . toMarkup
