module LiuMS.SiteInfo
  ( SiteInfo (..)
  , siteInfo
  ) where

data SiteInfo = SiteInfo
  { title       :: String
  , description :: String
  , author      :: String
  , language    :: String
  }

siteInfo :: SiteInfo
siteInfo = cnSiteInfo

cnSiteInfo :: SiteInfo
cnSiteInfo = SiteInfo
  { title       = "Liu.ms"
  , description = "我，刘闽晟，是一位对计算机科学、语言学、政治科学感兴趣的本科生。点击网站来了解我的生活、兴趣、观点。"
  , author      = "刘闽晟"
  , language    = "cmn-Hans-CN"
  }

enSiteInfo :: SiteInfo
enSiteInfo = SiteInfo
  { title       = "Liu.ms"
  , description = "I am Minsheng Liu, an undergraduate interested in CS, Linguistics, and Political Science. Click this site to learn more about my life, interests, and viewpoints."
  , author      = "Minsheng Liu"
  , language    = "eng-Latn-US"
  }
