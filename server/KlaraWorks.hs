{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import           KlaraWorks.TH

import           Data.ByteString.Lazy
import           Data.Text.Lazy
import           Data.Text.Lazy.Encoding
import           Network.HTTP.Types
import           Network.Wai
import           Network.Wai.Handler.Warp

server :: Assets -> Application
server Assets{..} req respond = do
  case pathInfo req of
    ["main.js"] ->
      respond $ responseLBS
      status202
      [("Content-Type", "text/javascript")]
      mainJs
    _ ->
      respond $ responseLBS
      status202
      [("Content-Type", "text/html")]
      indexHtml

data Assets = Assets
  { indexHtml :: ByteString
  , mainJs :: ByteString
  -- , styleCss :: ByteString
  }

main :: IO ()
main = do
  $(build)
  print "Running at \"http://localhost:8000\""
  run 8000 . server $ Assets
    { indexHtml = encodeUtf8 $(loadFile "dist/index.html")
    , mainJs = encodeUtf8 $(loadFile "dist/main.js")
    }
