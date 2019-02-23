{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
module Main where

import           KlaraWorks.TH

import qualified Data.ByteString.Lazy     as LBS
import qualified Data.Text                as ST
import qualified Data.Text.Lazy.Encoding  as LTE
import           Network.HTTP.Types
import           Network.Wai
import           Network.Wai.Handler.Warp

import           Clay

import           Lucid

server :: Assets -> Application
server Assets{..} req respond =
  case pathInfo req of
    ["main.js"] ->
      respond $ responseLBS
      status200
      [("Content-Type", "text/javascript")]
      mainJs
    ["style.css"] ->
      respond $ responseLBS
      status200
      [("Content-Type", "text/stylesheet")]
      styleCss
    _ ->
      respond $ responseLBS
      status200
      [("Content-Type", "text/html")]
      indexHtml

data Assets = Assets
  { indexHtml :: LBS.ByteString
  , mainJs    :: LBS.ByteString
  , styleCss  :: LBS.ByteString
  }

main :: IO ()
main = do
  $(build)
  print "Running at \"http://localhost:8000\""
  run 8000 . server $ Assets
    { indexHtml = renderBS $
      doctypehtml_ $ do
        head_ $ do
          meta_ [ charset_ "utf-8" ]
          title_ [] "Klara Works"
        body_ $ do
          div_ [id_ "main" ] ""
          script_ [src_ "main.js"] ("" :: ST.Text)
    , mainJs = LTE.encodeUtf8 $(loadFile "dist/main.js") <>
               "var app = Elm.Main.init({node: document.getElementById('main')})"
    , styleCss = LTE.encodeUtf8 . renderWith compact [] $
        h1 ?
          color "#fa8"
    }

