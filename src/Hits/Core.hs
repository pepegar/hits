module Hits.Core (
  status,
  FileChanges
  ) where

import Data.Aeson (ToJSON)
import Data.Attoparsec.Text as AP
import Data.Either
import Data.Text as T
import System.Process as P

import Hits.Types (FileChanges, fileChange)
  

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
  
