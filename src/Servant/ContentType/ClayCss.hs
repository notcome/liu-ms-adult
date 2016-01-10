{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Servant.ContentType.ClayCss where

import Data.Typeable           (Typeable)

import Network.HTTP.Media      ((//), (/:))
import Servant.API             (Accept (..), MimeRender (..))

import Clay                    (Css, render)
import Data.Text.Lazy.Encoding (encodeUtf8)

data ClayCss deriving Typeable

instance Accept ClayCss where
  contentType _ = "text" // "css" /: ("charset", "utf-8")

instance MimeRender ClayCss Css where
    mimeRender _ = encodeUtf8 . render
