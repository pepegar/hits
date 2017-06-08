{-# Language OverloadedStrings #-}
module Main where

import Web.Scotty as S
import Network.Wai.Middleware.Static
import Network.Wai.Middleware.RequestLogger
import Network.Wai.Middleware.Cors
import Hits as H
import Data.Aeson.Text as A (encodeToLazyText)
import Control.Monad.IO.Class
import Text.Blaze.Html5 hiding (main)
import Text.Blaze.Html.Renderer.Text

webApp :: ScottyM ()
webApp = do
  middleware $ staticPolicy (noDots >-> addBase "static")
  middleware logStdoutDev
  middleware simpleCors
  get "/status" $ do
    st <- liftIO H.status
    S.text $ A.encodeToLazyText st
  get "/log" $ do
    st <- liftIO H.glog
    S.text $ A.encodeToLazyText st
  get "/" $ file "./static/index.html"



main :: IO ()
main = scotty 8080 webApp
