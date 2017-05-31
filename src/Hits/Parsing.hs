{-# LANGUAGE OverloadedStrings #-}

module Hits.Parsing (
  fileChange,
  commitP
  ) where

import Data.Text as T
import Data.Attoparsec.Text as AP

import Hits.Types

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

commitP :: Parser Commit
commitP = Commit <$> (shaP <?> "sha1") <*> (authorP <?> "author") <*> (dateP <?> "date") <*> (messageP <?> "message")

messageP :: Parser Text
messageP = "\n" *> takeTill isEndOfLine <* endOfLine

shaP :: Parser Text
shaP = "commit " *> takeTill isEndOfLine <* endOfLine

authorP :: Parser Text
authorP = "Author: " *> takeTill isEndOfLine <* endOfLine

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
