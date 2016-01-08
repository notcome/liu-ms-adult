module Text.Hskribo.InlineTags
  ( quote, cite, define, abbr
  -- , data, time
  -- , code, samp, input
  -- , var
  -- , emph, strong, mark, keyword, voice
  -- , sub, sup
  -- , link
  -- , text
  ) where

import Data.Text (Text)
import Text.Hskribo.Type

import qualified Text.Blaze.Internal         as H
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A

quote :: [InlineNode] -> InlineNode
quote = InlineNode H.q

cite :: [InlineNode] -> InlineNode
cite = InlineNode H.cite

define :: [InlineNode] -> InlineNode
define = InlineNode H.dfn

abbr :: Text -> [InlineNode] -> InlineNode
abbr title = InlineNode $ H.abbr H.! A.title (H.textValue title)
