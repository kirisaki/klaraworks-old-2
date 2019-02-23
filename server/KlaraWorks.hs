{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
module Main where

import           KlaraWorks.TH

import qualified Data.ByteString.Lazy     as LBS
import qualified Data.Text.Lazy.Encoding  as LTE
import           Network.HTTP.Types
import           Network.Wai
import           Network.Wai.Handler.Warp

server :: Assets -> Application
server Assets{..} req respond =
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
  { indexHtml :: LBS.ByteString
  , mainJs    :: LBS.ByteString
  -- , styleCss :: ByteString
  }

main :: IO ()
main = do
  $(build)
  print "Running at \"http://localhost:8000\""
  run 8000 . server $ Assets
    { indexHtml = LTE.encodeUtf8 $(loadFile "dist/index.html")
    , mainJs = LTE.encodeUtf8 $(loadFile "dist/main.js")
    }
