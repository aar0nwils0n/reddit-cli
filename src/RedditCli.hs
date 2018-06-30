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
import Data.Char (digitToInt)
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as BS
import Data.Map as Map
import SubRedditData
import Text.Show
import CommentsData
import Data.Either

getSubRedditChildren :: String -> IO [PostListing]
getSubRedditChildren sub = do
 r <- asJSON =<< get ("https://reddit.com/r/" ++ sub ++ ".json")
 return . subRedditChildren . subRedditData  $ r ^. responseBody

getComments sub num ps = do
  r <- asJSON =<< get ("https://reddit.com/r/" ++ sub ++ "/comments/" ++ (SubRedditData.id . postListingData $ ps !! (num - 1)) ++ ".json")
  return $ r ^. responseBody

commentsToString :: [Comment] -> String ->  String
commentsToString xs str = Prelude.foldl commentToString str xs

commentToString :: String -> Comment -> String
commentToString x y = let
  cData = commentData $ y
  indent = getIndent . kind $ y
  newStr = x ++ "\n" ++ indent ++ (author cData) ++ " - ↑" ++ (show $ ups cData) ++ " ↓" ++ (show $ downs cData) ++ ": " ++ (body cData) ++ "\n"
  in  (commentsToString (CommentsData.children cData) newStr) ++ (replyStr $ replies cData)


replyStr :: Either String Comment -> String
replyStr x
  | isLeft x = ""
  | isRight x = commentToString "" (fromRight emptyComment x)


getIndent :: String -> String
getIndent x
  | x == "Listing" = ""
  | otherwise = Prelude.map (\x -> ' ') [0..(digitToInt $ (!!) x 1)]

resToString :: [PostListing] -> String
resToString xs = let ys = zip [0..] xs
  in Prelude.foldl articleToStr "" ys

articleToStr :: String -> (Int, PostListing) -> String
articleToStr x y =
    let index = show . (+) 1 $ fst y
        z = snd y
        w = postListingData z
  in x ++ index ++ ". " ++ (title w) ++ "\n\n"


redditCli :: IO ()
redditCli = do
  putStrLn "Enter a subreddit" 
  line <- getLine
  res <- getSubRedditChildren line
  putStrLn . (++) (resToString res) $ (++) "\n\n" "Enter Number To View Comments"
  num <- getLine
  commentsRes <- getComments line (read num) res
  putStrLn (commentsToString commentsRes "")
  redditCli
