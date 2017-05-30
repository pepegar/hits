{-# Language OverloadedStrings #-}
module Main where

import Web.Scotty
import Hits as H (status)
import Data.Aeson.Text as A (encodeToLazyText)
import Control.Monad.IO.Class

webApp :: ScottyM ()
webApp = do
  get "/status" $ do
    st <- liftIO H.status
    text $ A.encodeToLazyText st


main :: IO ()
main = scotty 8080 webApp
