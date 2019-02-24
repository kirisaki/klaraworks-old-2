{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
module Main (main, boot) where

import           KlaraWorks.Style
import           KlaraWorks.TH
import           Paths_klaraworks

import qualified Data.ByteString.Lazy     as LBS
import           Data.FileEmbed           (embedFile)
import qualified Data.Text                as ST
import qualified Data.Text.Lazy.Encoding  as LTE
import           Network.HTTP.Types
import           Network.Wai
import           Network.Wai.Handler.Warp

import           Clay                     hiding (style)

import           Lucid

server :: Assets -> Application
server Assets{..} req respond' =
  let
    respond t = respond' . responseLBS status200 t
  in
    case pathInfo req of
      ["main.js"] ->
        respond
        [("Content-Type", "text/javascript")]
        mainJs
      ["style.css"] ->
        respond
        [("Content-Type", "text/css")]
        styleCss
      ["back.svg"] ->
        respond
        [("Content-Type", "image/svg+xml")]
        backSvg
      _ ->
        respond
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
    { indexHtml = index
    , mainJs = LTE.encodeUtf8 $(loadFile "dist/main.js") <>
               "var app = Elm.Main.init()"
    , styleCss =  style
    , backSvg = LBS.fromStrict $(embedFile "assets/back.svg")
    }


main :: IO ()
main =
  boot
