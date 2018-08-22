{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Harvest.Project where

import Data.Aeson

data Project = Project
  { id :: Int
  , name :: String
  } deriving (Show)

instance FromJSON Project where
  parseJSON = withObject "project" $ \o ->
    Project
      <$> o .: "id"
      <*> o .: "name"
