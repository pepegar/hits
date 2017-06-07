module Hits.Status.Types(
  ChangeType (..),
  FileChange (..),
  FileChanges,
  decodeFileChanges
  ) where

import Prelude
import Data.Argonaut (class DecodeJson, class EncodeJson, Json, decodeJson, encodeJson, fromArray, (.?), (:=), (~>))
import Data.Either (Either(..))
import Data.Traversable (traverse)

data ChangeType = Added
            | Deleted
            | Modified
            | Renamed

instance showChangeType :: Show ChangeType where
  show Added = "Added"
  show Deleted = "Deleted"
  show Modified = "Modified"
  show Renamed = "Renamed"

data FileChange = FileChange {
  changeType :: ChangeType,
  fileName :: String
  }

instance decodeFileChange :: DecodeJson FileChange where
  decodeJson json = do
    obj <- decodeJson json
    changeString <- obj .? "changeType"
    changeType <- changeTypeEither changeString
    fileName <- obj .? "fileName"
    pure $ FileChange { changeType, fileName }
      where
        changeTypeEither :: String -> Either String ChangeType
        changeTypeEither "Added" = Right Added
        changeTypeEither "Deleted" = Right Deleted
        changeTypeEither "Modified" = Right Modified
        changeTypeEither "Renamed" = Right Renamed
        changeTypeEither _ = Left "was not one of {Added|Deleted|Modified|Renamed}"

instance encodeFileChange :: EncodeJson FileChange where
  encodeJson (FileChange fileChange) = 
    "changeType" := (show fileChange.changeType)
    ~> "fileName" := fileChange.fileName


type FileChanges = Array FileChange

decodeFileChanges :: Json -> Either String FileChanges
decodeFileChanges json = decodeJson json >>= traverse decodeJson

encodeFileChanges :: FileChanges -> Json
encodeFileChanges fileChanges = fromArray $ encodeJson <$> fileChanges
