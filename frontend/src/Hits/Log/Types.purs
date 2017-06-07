module Hits.Log.Types(
  Commit(..),
  DateTime(..),
  decodeLog) where

import Prelude (bind, pure, ($), (>>=))
import Data.Either (Either)
import Data.Argonaut (class DecodeJson, Json, decodeJson, (.?))
import Data.Traversable (traverse)


data Commit = Commit {
  sha :: String,
  author :: String,
  date :: DateTime,
  message :: String
  }

instance decodeCommit :: DecodeJson Commit where
  decodeJson json = do
    obj <- decodeJson json
    sha <- obj .? "sha"
    author <- obj .? "author"
    date <- obj .? "date"
    message <- obj .? "message"
    pure $ Commit { sha, author, date, message }

data DateTime = DateTime {
  dayOfWeek :: String,
  month :: String,
  day :: Int,
  time :: String,
  year :: Int
  }

instance decodeDateTime :: DecodeJson DateTime where
  decodeJson json = do
    obj <- decodeJson json
    dayOfWeek <- obj .? "dayOfWeek"
    month <- obj .? "month"
    day <- obj .? "day"
    time <- obj .? "time"
    year <- obj .? "year"
    pure $ DateTime { dayOfWeek, month, day, time, year }


decodeLog :: Json -> Either String (Array Commit)
decodeLog json = decodeJson json >>= traverse decodeJson
