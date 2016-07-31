{-# LANGUAGE TypeOperators #-}

module LiuMS.Config
  ( Config (..), Language, SiteInfo (..)
  , liuMSInfo, mkConfig
  , askContentPath, askCacheManager, askSiteInfo
  ) where

import Control.Monad.Reader
import Control.Monad.Trans.Except
import Servant

import LiuMS.CacheManager
import LiuMS.Compiler
import LiuMS.Compiler.Markdown

data Config = Config
  { contentPath  :: FilePath
  , cacheManager :: CacheManager
  , siteInfo     :: [(Language, SiteInfo)]
  }

type Language = String

data SiteInfo = SiteInfo
  { title       :: String
  , description :: String
  , author      :: String
  , language    :: String
  } deriving (Eq, Show)

liuMSInfo :: [(Language, SiteInfo)]
liuMSInfo =
  [ ( "cmn-Hans-CN"
    , SiteInfo
      { title       = "Liu.ms"
      , description = "我，刘闽晟，是一位对计算机科学、语言学、政治科学感兴趣的本科生。点击网站来了解我的生活、兴趣、观点。"
      , author      = "刘闽晟"
      , language    = "cmn-Hans-CN"
      }
    )
  , ( "eng-Latn-US"
    , SiteInfo
      { title       = "Liu.ms"
      , description = "I am Minsheng Liu, an undergraduate interested in CS, Linguistics, and Political Science. Click this site to learn more about my life, interests, and viewpoints."
      , author      = "Minsheng Liu"
      , language    = "eng-Latn-US"
      }
    )
  ]

mkConfig :: FilePath -> FilePath -> Config
mkConfig contentPath cachePath = Config contentPath manager liuMSInfo where
  manager = CacheManager (contentPath ++ "/contents")
                         cachePath
                         [("md", markdown)]

askContentPath  :: (MonadReader Config m) => m FilePath
askContentPath  = contentPath  <$> ask

askCacheManager :: (MonadReader Config m) => m CacheManager
askCacheManager = cacheManager <$> ask

askSiteInfo     :: (MonadReader Config m) => m SiteInfo
askSiteInfo     = do infos <- siteInfo <$> ask
                     return $ snd $ head infos
