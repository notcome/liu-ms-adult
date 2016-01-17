{-# LANGUAGE OverloadedStrings #-}

module LiuMS.Css.Basic (cssBasic) where

import Prelude hiding (rem)

import Clay
import Data.Monoid

cssBasic :: Css
cssBasic = do
  resetCss
  baseCss

resetCss :: Css
resetCss = do
  -- Prevent text size adjust after device orientation change on iOS and IE
  html ? do
    "-ms-text-size-adjust"     -: "100%"
    "-webkit-text-size-adjust" -: "100%"

  let lists  = ul <> ol <> li <> dl <> dt <> dd
  let blocks = section <> article <> header <> footer <> main_ <> aside
  body <> lists <> blocks ? do
    "margin"  -: "0"
    "padding" -: "0"

  img ? do
    width  inherit
    height inherit

  a <> (a # active) ? do
    color          $ rgb 44 62 80
    textDecoration none

  a # hover ? do
    textDecoration underline

  -- Support for Edge, Firefox: details, summary
  -- Support for IE 11:         main
  -- Note that IE 11 is not a supported platform. It's added solely for the convenience under certain "strange" situations.
  main_ <> details <> summary ? display block

  -- Normalize vertical alignment of [progress] on Chrome, Firefox
  progress ? verticalAlign baseline

baseCss :: Css
baseCss = do
  html ? do
    fontSize   $ px 16
    color      $ rgb 52 73 94
    lineHeight $ em 1.6
    fontFamily [] [sansSerif]

  body ? do
    display flex
    "flex-direction" -: "row"

  main_ ? do
    maxWidth    $ rem 40
    minWidth    $ rem 36
    marginLeft  $ auto
    marginRight $ auto

  header ? do
    width $ rem 10
    figure ? do
      "margin" -: "2rem"
      width  $ rem 6
      height $ rem 6
    nav |> ul |> li ? do
      textAlign    $ alignSide sideCenter
      marginBottom $ em 0.5

topnav :: Css
topnav = do
  ".topnav-wrapper" ? height (px 48)
  "#topnav" ? do
    height $ px  48
    width  $ pct 100
    position absolute
    backgroundColor "#333333"
    transition "all" (ms 400) ease (ms 0)
    ".topnav-list" |> a ? do
      color white
      ":hover" ? textDecoration none
