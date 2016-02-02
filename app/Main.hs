{-# LANGUAGE DeriveFunctor   #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TupleSections   #-}

{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Main where

import Control.Applicative
import Control.Monad
import Data.Maybe
import Debug.Trace
import System.Directory
import System.Environment
import System.FilePath

import Network.Wai
import Network.Wai.Handler.Warp
import Servant

import qualified LiuMS        as LMS
import qualified LiuMS.Config as LMS

newtype Fix f = In { out :: f (Fix f) }

type Algebra f a = f a -> a

cata :: Functor f => Algebra f a -> Fix f -> a
cata alg = alg . fmap (cata alg) . out

data LabelTreeF a f = LeafF a
                    | NodeF [(String, f)]
                    deriving (Functor)

type LabelTree a = Fix (LabelTreeF a)

pattern Leaf x  = (In (LeafF x))
pattern Node xs = (In (NodeF xs))

select :: String -> LabelTree a -> Maybe (LabelTree a)
select q (Node xs) = lookup q xs
select _ _         = Nothing

match :: [String] -> LabelTree a -> Maybe (LabelTree a)
match qs z = foldl helper (Just z) qs where
  helper s q = s >>= select q

nodes :: LabelTree a -> [a]
nodes = cata helper where
  helper (LeafF x)  = [x]
  helper (NodeF xs) = concatMap snd xs

type Resource = FilePath
type Library = LabelTree Resource

instance Show Library where
  show = unlines . cata helper where
    helper (LeafF _)  = []
    helper (NodeF xs) = concatMap printNode xs

    printNode (name, children) = name : map ("  " ++) children

buildLibrary :: [String] -> String -> FilePath -> IO (Maybe (FilePath, Library))
buildLibrary exts lang path = do
  isFile <- doesFileExist      path
  isDir  <- doesDirectoryExist path
  resultAsFile <- handleFile isFile
  resultAsDir  <- handleDir  isDir
  return $ resultAsFile <|> resultAsDir
  where
    wantedFileName :: Maybe FilePath
    wantedFileName = let
      baseName         = takeBaseName  path
      extension        = takeExtension path
      (partL', partN') = splitAt 3 $ reverse baseName
      (partL,  partN)  = (reverse partL', reverse partN')
      in do unless (extension `elem` exts) $ Nothing
            if head partL /= '_'
            then return baseName
            else if tail partL == lang
                 then return partN
                 else Nothing

    handleFile :: Bool -> IO (Maybe (FilePath, Library))
    handleFile False = return Nothing
    handleFile True  = return $ wantedFileName >>= return . (, Leaf path)

    handleDir  :: Bool -> IO (Maybe (FilePath, Library))
    handleDir  False = return Nothing
    handleDir  True  = do
      dirContents <- getDirectoryContents path
      let dirContents' = [ joinPath [path, item]
                         | item <- dirContents
                         , head item /= '.'
                         ]
      leaves <- mapM (buildLibrary exts lang) dirContents'
      let leaves' = catMaybes leaves
      return $ Just (takeFileName path, Node leaves')

app :: LMS.Config -> Application
app = serve apiType . LMS.server
  where
    apiType :: Proxy LMS.SiteAPI
    apiType = Proxy

main :: IO ()
main = do
  args <- getArgs
  if length args == 0
  then putStrLn "usage: liums [PORT] [CONTENTS_DIR]"
  else let port   = read (args !! 0) :: Int
           path   = args !! 1
           config = LMS.mkConfig path (path ++ "/cache")
       in do
         (Just (_, lib)) <- buildLibrary [".md"] "cn" path
         print lib
         return ()
         --putStrLn $ "listen on port " ++ show port
         --run port (app config)
