module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Halogen as H
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Network.HTTP.Affjax as AX
import Hits.Status as HS
import Hits.Log as HL

main :: Eff (HA.HalogenEffects (ajax :: AX.AJAX)) Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  status <- runUI HS.status unit body
  log <- runUI HL.log unit body

  status.query $ H.action $ HS.Status
  log.query $ H.action $ HL.Log
