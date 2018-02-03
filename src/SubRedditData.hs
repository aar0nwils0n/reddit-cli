{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module SubRedditData where

import GHC.Generics
import Data.Aeson

data SubReddit = SubReddit {
    subRedditData :: SubRedditData
  } deriving (Generic, Show)
  
instance FromJSON SubReddit where
  parseJSON (Object x) = SubReddit <$> x .: "data"
  parseJSON _ = fail "Expected an Object"

data SubRedditData = SubRedditData {
    subRedditChildren :: [PostListing]
  } deriving (Generic, Show)

instance FromJSON SubRedditData where
  parseJSON (Object x) = SubRedditData <$> x .: "children"
  parseJSON _ = fail "Expected an Object"

data PostListing = PostListing {
    postListingData :: PostListingData
  } deriving (Generic, Show)

instance FromJSON PostListing where
  parseJSON (Object x) = PostListing <$> x .: "data"
  parseJSON _ = fail "Expected an Object"

data PostListingData = PostListingData {
    title :: String
    , selftext :: String
  } deriving (Generic, Show)

instance FromJSON PostListingData

