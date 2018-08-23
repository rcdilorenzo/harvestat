{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Harvest.TimeEntry where

import Data.Aeson
import Data.Time

import Data.Harvest.Project (Project)
import Data.Harvest.Client (Client)
import Data.Harvest.User (User)

import ApiClient

data ExternalReference = ExternalReference
  { referenceId :: String
  , groupId :: String
  , permalink :: String
  , service :: String
  , serviceIconUrl :: String
  } deriving (Show)

data TimeEntry = TimeEntry
  { id :: Int
  , date :: Day
  , hours :: Double
  , billable :: Bool
  , notes :: Maybe String
  , reference :: Maybe ExternalReference
  , user :: User
  , project :: Project
  , client :: Client
  , createdAt :: Maybe UTCTime
  , updatedAt :: Maybe UTCTime
  } deriving (Show)

data ResponseTimeEntries = ResponseTimeEntries
  { entries :: [TimeEntry] } deriving Show

instance FromJSON ExternalReference where
  parseJSON = withObject "external_reference" $ \o ->
    ExternalReference
      <$> o .: "id"
      <*> o .: "group_id"
      <*> o .: "permalink"
      <*> o .: "service"
      <*> o .: "service_icon_url"

instance FromJSON TimeEntry where
  parseJSON = withObject "time_entry" $ \o ->
    TimeEntry
      <$> o .: "id"
      <*> o .: "spent_date"
      <*> o .: "hours"
      <*> o .: "billable"
      <*> o .: "notes"
      <*> o .: "external_reference"
      <*> o .: "user"
      <*> o .: "project"
      <*> o .: "client"
      <*> o .: "created_at"
      <*> o .: "updated_at"

instance FromJSON ResponseTimeEntries where
  parseJSON = withObject "time_entries" $ \o ->
    ResponseTimeEntries <$> o .: "time_entries"

instance Semigroup ResponseTimeEntries where
  (<>) a b = ResponseTimeEntries $ (entries a) <> (entries b)

instance Monoid ResponseTimeEntries where
  mempty = ResponseTimeEntries []

instance CountableMonoid ResponseTimeEntries where
  count = length . entries
