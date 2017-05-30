{-# LANGUAGE DeriveGeneric, OverloadedStrings, DeriveAnyClass #-}
module Hits.Core (
  status,
  FileChanges
  ) where

import Data.Aeson (ToJSON)

import Data.Attoparsec.Text as AP
import Data.Text as T
import Data.Either
import GHC.Generics
import System.Process as P

data Change = Added
            | Deleted
            | Modified
            | Renamed
            deriving (Eq, Show, Generic, ToJSON)

data FileChanges = FileChanges {
  changeType :: Change,
  fileName :: Text
  } deriving (Show, Eq, Generic, ToJSON)


--[" D app/Main.hs"," M hits.cabal"," D src/Lib.hs","?? Main.hs","?? src/Hits.hs","?? src/Hits/", "R  asdf -> qwer"]

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
  

process :: IO Text
process = pack <$> P.readCreateProcess command input
  where
    command = P.shell "git status -s"
    input = ""


parsed = parse <$> process
  where parse x = case T.lines x of
          [] -> []
          (x : xs) -> xs
          

status :: IO [FileChanges]
status = do
  lines <- parsed
  return $ rights $ AP.parseOnly fileChange <$> lines
  
