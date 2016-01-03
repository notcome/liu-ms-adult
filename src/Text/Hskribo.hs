{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Text.Hskribo where

import           Data.Monoid ((<>))
import qualified Data.Text   as T

import Servant.ContentType.PlainHtml

data HskriboDocIx = IxSection | IxInline

type IxSection = 'IxSection
type IxInline  = 'IxInline

data HskriboDoc (ix :: HskriboDocIx) where
  SectionNode   :: TagType -> [HskriboDoc IxSection] -> HskriboDoc IxSection
  ParagraphNode :: TagType -> [HskriboDoc IxInline]  -> HskriboDoc IxSection
  InlineNode    :: TagType -> [HskriboDoc IxInline]  -> HskriboDoc IxInline
  TextNode      :: T.Text -> HskriboDoc IxInline

newtype TagType = TagType { unTagType :: T.Text }

type SectionNode = HskriboDoc IxSection
type InlineNode  = HskriboDoc IxInline
type Document    = HskriboDoc IxSection

text :: T.Text -> InlineNode
text = TextNode

emph :: [InlineNode] -> InlineNode
emph = InlineNode $ TagType "em"

def :: [InlineNode] -> InlineNode
def = InlineNode $ TagType "def"

heading :: [InlineNode] -> SectionNode
heading = ParagraphNode $ TagType "h1"

par :: [InlineNode] -> SectionNode
par = ParagraphNode $ TagType "p"

section :: [SectionNode] -> SectionNode
section = SectionNode $ TagType "section"

aside :: [SectionNode] -> SectionNode
aside = SectionNode $ TagType "aside"

instance ToPlainHtml SectionNode where
  toPlainHtml (SectionNode tag children) = T.concat $
    [start] <> map toPlainHtml children <> [end]
    where start = "<"  <> unTagType tag <> ">\n"
          end   = "</" <> unTagType tag <> ">\n"
  toPlainHtml (ParagraphNode tag children) = T.concat $
    [start] <> map toPlainHtml children <> [end]
    where start = "<"  <> unTagType tag <> ">"
          end   = "</" <> unTagType tag <> ">\n"

instance ToPlainHtml InlineNode where
  toPlainHtml (InlineNode tag children) = T.concat $
    [start] <> map toPlainHtml children <> [end]
    where start = "<"  <> unTagType tag <> ">"
          end   = "</" <> unTagType tag <> ">"
  toPlainHtml (TextNode txt) = txt
