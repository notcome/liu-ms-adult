{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE KindSignatures       #-}

module Text.Hskribo.Type
  ( HskriboDoc (..)
  , TagType
  , SectionNode, InlineNode, Document
  ) where

import qualified Data.Text        as T
import qualified Text.Blaze.Html  as H

data HskriboDocIx = IxSection | IxInline

type IxSection = 'IxSection
type IxInline  = 'IxInline

data HskriboDoc (ix :: HskriboDocIx) where
  SectionNode    :: TagType -> [HskriboDoc IxSection] -> HskriboDoc IxSection
  ParagraphNode  :: TagType -> [HskriboDoc IxInline]  -> HskriboDoc IxSection
  InlineNode     :: TagType -> [HskriboDoc IxInline]  -> HskriboDoc IxInline
  TextNode       :: T.Text -> HskriboDoc IxInline
  RawSectionNode :: H.Html -> HskriboDoc IxSection
  RawInlineNode  :: H.Html -> HskriboDoc IxInline

type TagType = H.Html -> H.Html

type SectionNode = HskriboDoc IxSection
type InlineNode  = HskriboDoc IxInline
type Document    = HskriboDoc IxSection

instance H.ToMarkup (HskriboDoc ix) where
  toMarkup (RawSectionNode raw) = raw
  toMarkup (RawInlineNode  raw) = raw
  toMarkup (TextNode text)      = H.toMarkup text
  toMarkup node = case node of
    (SectionNode   tag children) -> convert tag children
    (ParagraphNode tag children) -> convert tag children
    (InlineNode    tag children) -> convert tag children
    where convert tag = tag . sequence_ . map H.toMarkup
