{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module LiuMS.Data.Index where

import qualified Text.Blaze.Html5 as H
import           Text.Hskribo

document :: Document
document = RawSectionNode $ do
  H.docType
  H.head $ do
    H.title "hello"
  H.body $ do
    H.h1 "fuck u"
