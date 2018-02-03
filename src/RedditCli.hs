{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module RedditCli
   ( redditCli
   ) where

import Network.Wreq
import Control.Lens
import Data.Aeson
import Data.Aeson.Lens
import Data.List
import Data.Maybe
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as BS
import Data.Map as Map
import SubRedditData
import Text.Show

getSubRedditChildren :: String -> IO [PostListing]
getSubRedditChildren subreddit = do
 r <- asJSON =<< get subreddit
 return . subRedditChildren . subRedditData  $ r ^. responseBody

resToString :: [PostListing] -> String
resToString xs = let ys = zip [0..] xs
  in Prelude.foldl articleToStr "" ys

articleToStr :: String -> (Int, PostListing) -> String
articleToStr x y =
    let index = show . (+) 1 $ fst y
        z = snd y
        w = postListingData z
  in x ++ index ++ ". " ++ (title w) ++ "\n\n"

createUrl subreddit = ("https://reddit.com/r/" ++ subreddit ++ ".json")

redditCli :: IO ()
redditCli = do
  -- putStrLn $ getListNumber [1, 2] 1
  putStrLn "Enter a subreddit" 
  do 
    line <- getLine
    do
      res <- getSubRedditChildren (createUrl line)
      do
        putStrLn (resToString res)
