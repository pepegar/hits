module Hits.Status (
  status,
  Query,
  Message
  ) where



import Prelude
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Network.HTTP.Affjax as AX

data Change = Added
            | Deleted
            | Modified
            | Renamed

data FileChanges = FileChanges {
  changeType :: Change,
  fileName :: String
  }

type State = Array FileChanges
data Query a = Status [FileChanges]
data Message = Changed State


status :: forall m. H.Component HH.HTML Query Unit Message m
status =
  H.component
      { initialState: const initialState
      , render
      , eval
      , receiver: const Nothing
      }
    where
  
    initialState :: State
    initialState = []
  
    render :: State -> H.ComponentHTML Query
    render state = HH.div_
      [ HH.text "lololol"]
  
    eval :: Query ~> H.ComponentDSL State Query Message m
    eval = case _ of
      Status x -> do
        pure x
