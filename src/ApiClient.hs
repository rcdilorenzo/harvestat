{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module ApiClient where

import System.Environment
import Data.Maybe
import Control.Lens
import Data.Time.Format.ISO8601 (formatShow, iso8601Format, ISO8601)
import Data.Text (Text)
import Data.Text as DT

import Network.Wreq (Response, Options, param, defaults, header,
                     getWith, responseBody, asJSON)

import Data.ByteString.Lazy.Internal (ByteString)
import Data.ByteString.Char8 as Char8

import Data.Aeson (Value, decode)
import Data.Aeson.Lens

class (Monoid t) => CountableMonoid t where
  count :: t -> Int

env :: String -> IO String
env key = lookupEnv key >>= return . fromMaybe ("<ENV: " ++ key ++ ">")

dayParam :: (ISO8601 t) => Text -> t -> Options -> Options
dayParam key value = param key .~ [DT.pack $ formatShow iso8601Format value]

requestAll :: (CountableMonoid t) => (Int -> IO t) -> IO t
requestAll request = requestNext request mempty 1

requestNext :: (CountableMonoid t) => (Int -> IO t) -> t -> Int -> IO t
requestNext request state page = request page >>= returnMergeOrNext
  where
    returnMergeOrNext responseState
      | (ApiClient.count responseState) == 100
      = requestNext request newState (page + 1)

      | otherwise = return newState
      where
        newState = mconcat [state, responseState]
