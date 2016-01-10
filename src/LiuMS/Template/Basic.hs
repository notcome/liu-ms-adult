{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}

module LiuMS.Template.Basic where

import Data.String (fromString)

import Text.Blaze.Html5 (Html, ToMarkup, (!), toMarkup)
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A

import LiuMS.SiteInfo
import Servant.ContentType.Processable

data Basic

instance {-# OVERLAPPABLE #-}
  ToMarkup doc => Processable Basic doc where
    type Processed Basic = Html
    process _ = render siteInfo

render :: ToMarkup doc => SiteInfo -> doc -> Html
render info doc = H.html ! A.lang siteLang $ do
  H.docType
  H.head $ do
    H.meta ! A.charset "utf-8"
    H.title siteTitle
    H.meta ! A.name    "description"
           ! A.content siteDesc
    H.meta ! A.name    "author"
           ! A.content siteAuthor
    H.meta ! A.name    "viewport"
           ! A.content "width=device-width, initial-scale=1"
    H.link ! A.rel     "stylesheet"
           ! A.href    "/css/basic"
  H.body $ do
    H.header $ H.nav $ H.ul headerItems
    toMarkup doc
    H.footer $ H.p $ "保留所有权利"

  where
    siteLang   = fromString $ language    info
    siteTitle  = fromString $ title       info
    siteDesc   = fromString $ description info
    siteAuthor = fromString $ author      info
    headerItems :: Html
    headerItems = let
      item (ref, text) = H.li $ H.a ! A.href ref $ text
      in mapM_ item [ ("/", "首页")
                    , ("/projects", "项目")
                    , ("/posts", "文章")
                    , ("/about", "关于")]
