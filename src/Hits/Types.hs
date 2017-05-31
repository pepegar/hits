{-# LANGUAGE DeriveGeneric, OverloadedStrings, DeriveAnyClass #-}
module Hits.Types(
  FileChanges,
  fileChange
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
