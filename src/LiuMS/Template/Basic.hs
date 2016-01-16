{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}

module LiuMS.Template.Basic where

import Data.Monoid
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
  htmlHead
  H.body $ do
    pageHeader
    H.main $ toMarkup doc
    H.script ! A.src "static/basic.js" $ ""

  where
    siteLang   = fromString $ language    info
    siteTitle  = fromString $ title       info
    siteDesc   = fromString $ description info
    siteAuthor = fromString $ author      info

    htmlHead :: Html
    htmlHead = H.head $ do
      H.meta ! A.charset "utf-8"
      H.title siteTitle
      H.meta ! A.name    "description"
             ! A.content siteDesc
      H.meta ! A.name    "author"
             ! A.content siteAuthor
      H.meta ! A.name    "viewport"
             ! A.content "width=device-width, initial-scale=1"
      H.link ! A.rel     "stylesheet"
             ! A.href    "static/basic.css"

    topnav :: Html
    topnav = H.nav ! A.id "topnav" $ do
      H.ul ! class_ "list-mobile" $ do
        iconItem "menu" "#" ! A.onclick "toggle('topnav-list')"
        iconItem "lms"  "/"
        -- H.li $ iconLink "menu" ! A.onclick "toggle('topnav-list')"
        -- H.li $ iconLink "lms"  ! A.href "/"
      H.ul ! class_ "list" $ do
        -- H.li $ iconLink "lms"  ! A.href "/"
        mapM_ textItem $ [ ("文章", "/articles")
                         , ("项目", "/projects")
                         , ("照片", "/photos")
                         , ("关于", "/about")
                         ]
      where
        class_   className   = A.class_ ("topnav-" <> className)
        iconItem class' href = H.li $ do
          H.a ! class_ ("item-" <> class')
              ! A.href href
              $ ""
        textItem (txt, ref)  = H.li $ H.a ! A.href ref $ txt

    pageHeader :: Html
    pageHeader = H.header $ topnav
