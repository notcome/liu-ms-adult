module LiuMS.Compiler.Markdown where

import Data.ByteString       (ByteString)
import Data.Text.Encoding    (decodeUtf8)
import Data.Text             (unpack)
import Data.ByteString.Lazy  (toStrict)

import Text.Blaze.Renderer.Utf8
import Text.Pandoc.Options
import Text.Pandoc.Readers.Markdown
import Text.Pandoc.Writers.HTML

import LiuMS.Compiler

markdown :: Compiler
markdown = Compiler { cachable = True
                    , compile  = unpureCompile compile
                    }
  where
    compile :: ByteString -> ByteString
    compile = toStrict
            . renderHtml
            . writeHtml def
            . fromRight
            . readMarkdown def
            . unpack
            . decodeUtf8

    fromRight :: Either a b -> b
    fromRight (Right x) = x
    fromRight _         = undefined
