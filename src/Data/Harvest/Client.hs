{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Harvest.Client where

import Data.Aeson

data Client = Client
  { id :: Int
  , name :: String
  } deriving (Show)

instance FromJSON Client where
  parseJSON = withObject "client" $ \o ->
    Client
      <$> o .: "id"
      <*> o .: "name"
