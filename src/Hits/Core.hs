{-# Language OverloadedStrings #-}
module Hits.Core (
  status,
  glog
  ) where

import Data.Attoparsec.Text as AP
import Data.Either
import Data.Text as T
import System.Process as P

import Hits.Types (FileChanges, Commit)
import Hits.Parsing (fileChange, commitP)
  

status :: IO [FileChanges]
status = do
  lines <- parsed
  return $ rights $ AP.parseOnly fileChange <$> lines

  where process :: IO Text
        process = pack <$> P.readCreateProcess command input
          where
            command = P.shell "git status -s"
            input = ""

        parsed :: IO [Text]
        parsed = parse <$> process
          where parse x = case T.lines x of
                      [] -> []
                      (x : xs) -> xs


glog :: IO [Commit]
glog = do
  stdout <- gitLog
  return $ toList $ parseOnly (commitP `sepBy` AP.endOfLine) stdout

  where gitLog :: IO Text
        gitLog = pack <$> P.readCreateProcess command input
          where command = P.shell "git log"
                input = ""

        toList :: Either String [Commit] -> [Commit]
        toList (Right x) = x
        toList _ = []
