{-# LANGUAGE DeriveGeneric, OverloadedStrings, DeriveAnyClass #-}

module Hits.Types(
  FileChanges,
  fileChange,
  Commit
  ) where

import Data.Aeson (ToJSON)
import Data.Text as T
import Data.Attoparsec.Text as AP
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


change :: Parser Change
change = do
  ch <- AP.take 2
  return $ res ch
    where res "??" = Added
          res " D" = Deleted
          res " M" = Modified
          res "R " = Renamed

fileChange :: Parser FileChanges
fileChange = FileChanges <$> (change <* AP.char ' ') <*> AP.takeText

data Commit = Commit {
  sha :: Text,
  author :: Text,
  date :: DateTime,
  message :: Text
  } deriving (Show, Eq, Generic, ToJSON)

commitP :: Parser Commit
commitP = Commit <$> (shaP <?> "sha1") <*> (authorP <?> "author") <*> (dateP <?> "date") <*> (messageP <?> "message")

messageP :: Parser Text
messageP = "\n" *> takeTill isEndOfLine <* endOfLine

shaP :: Parser Text
shaP = "commit " *> takeTill isEndOfLine <* endOfLine

authorP :: Parser Text
authorP = "Author: " *> takeTill isEndOfLine <* endOfLine

data DateTime = DateTime {
  dayOfWeek :: Text,
  month :: Text,
  day :: Integer,
  time :: Text,
  year :: Integer
  } deriving (Eq, Show, Generic, ToJSON)

dateP :: Parser DateTime
dateP = string "Date:   " *> parseDateTime <* endOfLine
  where
    parseDateTime :: Parser DateTime
    parseDateTime = do
      dayW <- AP.take 3 <* space <?> "day of week"
      month <- AP.take 3 <* space <?> "month"
      dayM <- AP.decimal <* space <?> "day of month"
      time <- AP.take 8 <* space <?> "time"
      year <- AP.decimal <* (skipWhile $ not . isEndOfLine) <?> "year"
      return $ DateTime dayW month dayM time year
