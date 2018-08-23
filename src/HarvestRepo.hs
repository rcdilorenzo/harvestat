{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module HarvestRepo where

import Control.Lens
import Data.Text as DT
import Data.Time

import ApiClient

import Network.Wreq (Options, param, defaults, header,
                     getWith, responseBody, asJSON)

import Data.ByteString.Char8 as Char8

import Data.Harvest.TimeEntry (ResponseTimeEntries)

defaultOptions :: IO Options
defaultOptions = do
  token <- env "HARVEST_TOKEN"
  accountId <- env "HARVEST_ID"
  return $ defaults
    & header "Harvest-Account-ID" .~ [Char8.pack accountId]
    & header "Authorization" .~ [Char8.pack $ "Bearer " ++ token]

path :: String -> String
path path' = "https://api.harvestapp.com/api/v2" ++ path'

timeEntriesP :: Day -> Day -> Int -> IO ResponseTimeEntries
timeEntriesP fromDay toDay page = do
  baseOptions <- defaultOptions
  response <- getWith (baseOptions
                       & dayParam "from" fromDay
                       & dayParam "to" toDay
                       & param "page" .~ [DT.pack $ show page])
              (path "/time_entries") >>= asJSON
  return $ response ^. responseBody


