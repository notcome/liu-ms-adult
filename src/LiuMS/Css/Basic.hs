{-# LANGUAGE OverloadedStrings #-}

module LiuMS.Css.Basic (cssBasic) where

import Clay

cssBasic :: Css
cssBasic = do
  html ? fontFamily ["Charter", "PingFang SC"] [sansSerif]
