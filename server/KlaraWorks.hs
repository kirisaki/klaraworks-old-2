{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
module Main where

import           KlaraWorks.TH
import           Paths_klaraworks

import qualified Data.ByteString.Lazy     as LBS
import           Data.FileEmbed           (embedFile)
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
      [("Content-Type", "text/css")]
      styleCss
    ["back.svg"] ->
      respond $ responseLBS
      status200
      [("Content-Type", "image/svg+xml")]
      backSvg
    _ ->
      respond $ responseLBS
      status200
      [("Content-Type", "text/html")]
      indexHtml

data Assets = Assets
  { indexHtml :: LBS.ByteString
  , mainJs    :: LBS.ByteString
  , styleCss  :: LBS.ByteString
  , backSvg   :: LBS.ByteString
  }

boot :: IO ()
boot = do
  $(build)
  run 8000 . server $ Assets
    { indexHtml = renderBS $
      doctypehtml_ $ do
        head_ $ do
          meta_ [ charset_ "utf-8" ]
          title_ [] "Klara Works"
          style_ [] $ LTE.encodeUtf8 . renderWith compact [] $ do
            ".container" ? do
              width (vw 100)
              height (vh 100)
            (body <> h1) ?
              margin nil nil nil nil
          link_  [rel_ "stylesheet", href_ "/style.css"]
        body_ $ do
          div_ [id_ "main"] ""
          script_ [src_ "/main.js"] ST.empty
    , mainJs = LTE.encodeUtf8 $(loadFile "dist/main.js") <>
               "var app = Elm.Main.init()"
    , styleCss = LTE.encodeUtf8 . renderWith compact [] $
                 h1 ?
                 color "#fa0"
    , backSvg = LBS.fromStrict $(embedFile "assets/back.svg")
    }


main :: IO ()
main =
  boot
