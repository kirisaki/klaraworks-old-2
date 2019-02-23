{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Main where

import           KlaraWorks.TH

import           Data.ByteString.Lazy
import           Data.Text.Lazy
import           Data.Text.Lazy.Encoding
import           Network.HTTP.Types
import           Network.Wai
import           Network.Wai.Handler.Warp

server :: Application
server req respond = do
  $(build)
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

mainJs :: ByteString
mainJs = encodeUtf8 $(loadFile "dist/main.js")

indexHtml :: ByteString
indexHtml = encodeUtf8 $(loadFile "dist/index.html")

main :: IO ()
main = do
  print "Running at \"http://localhost:8000\""
  run 8000 server
