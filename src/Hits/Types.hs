{-# LANGUAGE DeriveGeneric, OverloadedStrings, DeriveAnyClass #-}

module Hits.Types(
  Change (Added, Deleted, Modified, Renamed),
  FileChanges (..),
  Commit (..),
  DateTime (..)
  ) where

import Data.Aeson (ToJSON)
import Data.Text as T
import GHC.Generics

data Change = Added
            | Deleted
            | Modified
            | Renamed
            deriving (Eq, Show, Generic, ToJSON)

data FileChanges = FileChanges {
  changeType :: Change,
  fileName :: Text
  } deriving (Show, Eq, Generic, ToJSON)


data Commit = Commit {
  sha :: Text,
  author :: Text,
  date :: DateTime,
  message :: Text
  } deriving (Show, Eq, Generic, ToJSON)


data DateTime = DateTime {
  dayOfWeek :: Text,
  month :: Text,
  day :: Integer,
  time :: Text,
  year :: Integer
  } deriving (Eq, Show, Generic, ToJSON)

