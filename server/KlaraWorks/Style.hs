{-# LANGUAGE OverloadedStrings #-}
module KlaraWorks.Style
  ( style
  , index
  ) where

import           Clay                    hiding (style)
import qualified Data.ByteString.Lazy    as LBS
import qualified Data.Text               as ST
import qualified Data.Text.Lazy.Encoding as LTE
import           Lucid

style :: LBS.ByteString
style = LTE.encodeUtf8 . renderWith compact [] $
  h1 ?
  color "#fa0"

index :: LBS.ByteString
index =  renderBS $
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
