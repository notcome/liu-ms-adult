{-# LANGUAGE OverloadedStrings    #-}

module Text.Hskribo
  ( module Text.Hskribo.Type
  , text, emph, def, heading
  , par, section, aside
  ) where

import Data.Text (Text)
import Text.Blaze.Internal (customParent)
import Text.Hskribo.Type

import qualified Text.Blaze.Html5 as H

text :: Text -> InlineNode
text = TextNode

emph :: [InlineNode] -> InlineNode
emph = InlineNode H.em

def :: [InlineNode] -> InlineNode
def = InlineNode $ customParent "def"

heading :: [InlineNode] -> SectionNode
heading = ParagraphNode H.h1

par :: [InlineNode] -> SectionNode
par = ParagraphNode H.p

section :: [SectionNode] -> SectionNode
section = SectionNode H.section

aside :: [SectionNode] -> SectionNode
aside = SectionNode H.aside
