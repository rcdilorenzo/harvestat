{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Harvest.User where

import Data.Aeson

data User = User
  { id :: Int
  , name :: String
  } deriving (Show)

instance FromJSON User where
  parseJSON = withObject "user" $ \o ->
    User
      <$> o .: "id"
      <*> o .: "name"
