module Hits.Status (
  status,
  Query
  ) where


import Control.Monad.Aff (Aff)
import Data.Argonaut (Json)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Functor (map)
import Halogen as H
import Halogen.HTML as HH
import Hits.Status.Types (FileChange(..), FileChanges, decodeFileChanges)
import Network.HTTP.Affjax as AX
import Prelude (type (~>), Unit, Void, bind, const, discard, pure, ($))


type State = {
     loading :: Boolean,
     changes :: FileChanges
}

data Query a = Status a


status :: forall eff. H.Component HH.HTML Query Unit Void (Aff (ajax :: AX.AJAX | eff))
status =
  H.component
      { initialState: const initialState
      , render
      , eval
      , receiver: const Nothing
      }
    where
  
    initialState :: State
    initialState = {loading: false, changes: []}
  
    render :: State -> H.ComponentHTML Query
    render state =
      HH.body_
        [ HH.h1_ [ HH.text "Hits" ]
        , HH.div_
          [ HH.h2_ [ HH.text "status" ]
          , HH.ul_ $ map listChange state.changes
          ]
        ]
        where
          listChange (FileChange change) = HH.li_ [HH.text change.fileName]
  
    eval :: Query ~> H.ComponentDSL State Query Void (Aff (ajax :: AX.AJAX | eff))
    eval = case _ of
      Status next -> do
        H.modify (_ { loading = true})
        response <- H.liftAff $ AX.get "http://localhost:8080/status"
        H.modify (_ { loading = false, changes = fileChanges response.response })
        pure next
        where
          fileChanges :: Json -> FileChanges
          fileChanges json = case (decodeFileChanges json) of
            Right x -> x
            _ -> []
