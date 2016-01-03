{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE TypeSynonymInstances  #-}


module LiuMS.Template where

import Servant.ContentType.Processable

import Text.Hskribo

data Template

instance Processable Template Document where
  type Processed Template = Document
  process _ x = section [heading [text "Liu.MS"], x]
