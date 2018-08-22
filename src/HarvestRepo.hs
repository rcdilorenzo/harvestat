{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module HarvestRepo where

import System.Environment
import Data.Maybe
import Control.Lens
import Data.Time
import Data.Time.Format.ISO8601 (formatShow, iso8601Format, ISO8601)
import Data.Text (Text)
import Data.Text as DT

import Network.Wreq (Response, Options, param, defaults, header,
                     getWith, responseBody, asJSON)

import Data.ByteString.Lazy.Internal (ByteString)
import Data.ByteString.Char8 as Char8

import Data.Aeson (Value, decode)
import Data.Aeson.Lens

import Data.Harvest.TimeEntry (ResponseTimeEntries)

env :: String -> IO String
env key = lookupEnv key >>= return . fromMaybe ("<ENV: " ++ key ++ ">")

defaultOptions :: IO Options
defaultOptions = do
  token <- env "HARVEST_TOKEN"
  id <- env "HARVEST_ID"
  return $ defaults
    & header "Harvest-Account-ID" .~ [Char8.pack id]
    & header "Authorization" .~ [Char8.pack $ "Bearer " ++ token]

path :: String -> String
path path' = "https://api.harvestapp.com/api/v2" ++ path'

dayParam :: (ISO8601 t) => Text -> t -> Options -> Options
dayParam key value = param key .~ [(DT.pack $ formatShow iso8601Format value)]

timeEntriesP :: Day -> Day -> IO ResponseTimeEntries
timeEntriesP from to = do
  baseOptions <- defaultOptions
  response <- getWith (baseOptions
                       & (dayParam "from" from)
                       & (dayParam "to" to))
              (path "/time_entries?from=2018-08-19") >>= asJSON
  return $ response ^. responseBody


