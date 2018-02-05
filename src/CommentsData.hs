{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module CommentsData where

import GHC.Generics
import Data.Aeson
import Control.Applicative

data Comment = Comment {
    commentData :: CommentData
    , kind :: String
  } deriving (Generic, Show)

instance FromJSON Comment where 
  parseJSON (Object x) = Comment
    <$> x .: "data"
    <*> x .: "kind"
  parseJSON _ = fail "Expected an Object"


strToArr :: String -> [Comment]
strToArr x = []

data CommentData = CommentData {
  children :: [Comment]
  , replies :: Either String [Comment]
  , body :: String
  , author :: String
  , ups :: Int
  , downs :: Int
} deriving (Generic, Show)

instance FromJSON CommentData where
  parseJSON (Object x) = CommentData
    <$> x .:? "children" .!= []
    <*> ((Left . strToArr <$> x .: "replies") <|> (Right <$> x .: "replies"))
    <*> x .:? "body" .!= ""
    <*> x .:? "author" .!= ""
    <*> x .:? "ups" .!= 0
    <*> x .:? "downs" .!= 0