{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module LiuMS where

import Servant
import Servant.ContentType.Processable
import Servant.ContentType.PlainHtml

import           Text.Hskribo (text, emph, def, heading, par, section, aside)
import qualified Text.Hskribo as Hbo

import LiuMS.Template

import qualified Text.Blaze.Html5 as H
import qualified LiuMS.Data.Index as Index

type SiteAPI = Get '[PlainHtml] Hbo.Document
  :<|> "document" :> Capture "title" String :> Get '[PlainHtml :<- Template] Hbo.Document

document :: String -> Hbo.Document
document _ = section
  [ heading [text "LLVM 项目到底是要解决哪些基础设施问题？"]
  , par [def [text "LLVM"], text "，曾经叫", emph [text "Low Level Virtual Machine"], text "是一套编译器后端基础设施。"]
  ]

server :: Server SiteAPI
server = return Index.document
  :<|>   return . document
