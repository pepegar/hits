module Hits.Log where

import Control.Monad.Aff (Aff)
import Data.Argonaut (Json)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Functor (map)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Hits.Log.Types
import Network.HTTP.Affjax as AX
import Prelude (type (~>), Unit, Void, bind, const, discard, pure, ($))

type State = {
     loading :: Boolean,
     log :: Array Commit
}
data Query a = Log a


log :: forall eff . H.Component HH.HTML Query Unit Void (Aff (ajax :: AX.AJAX | eff))
log =
    H.component
        { initialState : const initialState
        , render
        , eval
        , receiver : const Nothing
        }
        where

        initialState :: State
        initialState = {loading: false, log: []}

        render :: State -> H.ComponentHTML Query
        render state =
          HH.div
            [ HP.class_ (H.ClassName "log") ]
            [ HH.h2_ [ HH.text "log" ]
            , HH.ul_ $ map listCommit state.log
            ]
            where
                listCommit (Commit commit) = HH.li [HP.class_ (H.ClassName "commit") ]
                                               [HH.text commit.message]

        eval :: Query ~> H.ComponentDSL State Query Void (Aff (ajax :: AX.AJAX | eff))
        eval = case _ of
          Log next -> do
            H.modify (_ { loading = true})
            response <- H.liftAff $ AX.get "http://localhost:8080/log"
            H.modify (_ { loading = false, log = commits response.response })
            pure next
            where
              commits :: Json -> Array Commit
              commits json = case (decodeLog json) of
                Right x -> x
                Left err -> [Commit {sha: "this is the message", author: "asdf", date: DateTime { dayOfWeek: "", month: "", day: 1, time: "", year: 1 }, message: err}]
