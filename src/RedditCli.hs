{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module RedditCli
   ( redditCli
   ) where

import Network.Wreq
import Control.Lens
import Data.Aeson
import Data.Aeson.Lens
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as BS
import Data.Map as Map
import SubRedditData

getSubRedditChildren :: String -> IO [PostListing]
getSubRedditChildren subreddit = do
 r <- asJSON =<< get subreddit
 return . subRedditChildren . subRedditData  $ r ^. responseBody

resToString :: [PostListing] -> String
resToString xs = let ys = Prelude.fmap (\x -> postListingData x ) xs
 in Prelude.foldl (\x y -> x ++ (title y) ++ "\n") "" ys

createUrl subreddit = ("https://reddit.com/r/" ++ subreddit ++ ".json")

redditCli :: IO ()
redditCli = do
  putStrLn "Enter a subreddit" 
  do 
    line <- getLine
    do
      res <- getSubRedditChildren (createUrl line)
      do
        putStrLn (resToString res)
