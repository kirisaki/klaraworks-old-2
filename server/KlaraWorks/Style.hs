{-# LANGUAGE OverloadedStrings #-}
module KlaraWorks.Style
  ( style
  , index
  ) where

import           Clay                    hiding (div, style)
import qualified Clay                    as C (div)
import qualified Data.ByteString.Lazy    as LBS
import qualified Data.Text               as ST
import qualified Data.Text.Lazy.Encoding as LTE
import           Lucid

rgb' :: Integer -> Color
rgb' hex = rgb r g b
  where
    b = hex `mod` 0x0100
    g = hex `mod` 0x010000 `div` 0x0100
    r = hex `div` 0x010000

kDark, kGray, kWhite, kPink,kPeach :: Color
kDark = rgb' 0x444444
kGray = rgb' 0x888888
kWhite = rgb' 0xf0f0f0
kPink = rgb' 0xfc2063
kPeach = rgb' 0xfd5185

kEase :: Css
kEase = transition "" (sec 0.2) easeOut (sec 0)


style :: LBS.ByteString
style = LTE.encodeUtf8 . renderWith compact [] $ do
  a ?
    color kWhite
  nav ? do
    position absolute
    right (px 100)
    bottom (px 100)
  ".container" |>
    C.div ? do
      width (vw 100)
      height (vh 100)
  ".container.index" ?
    left (vw 0)
  ".container.about" ?
    left (vw (-100))
  ".container.works" ?
    left (vw (-200))
  ".container.contact" ?
    left (vw (-300))


index :: LBS.ByteString
index =  renderBS $
  doctypehtml_ $ do
  head_ $ do
    meta_ [ charset_ "utf-8" ]
    meta_ [ name_ "viewport", content_ "width=device-width,initial-scale=1" ]
    title_ [] "Klara Works"
    style_ [] $ LTE.encodeUtf8 . renderWith compact [] $ do
      body ? do
        backgroundColor kDark
        backgroundImage $ url "back.svg"
        color kWhite
        overflow hidden
        position fixed
        top nil
        bottom nil
        left nil
        right nil
      ".container" ? do
        width (vw 400)
        height (vh 100)
        display flex
        position absolute
        kEase
      (body <> h1) ?
        margin nil nil nil nil
    link_  [rel_ "stylesheet", href_ "/style.css"]
  body_ $ do
    div_ [id_ "main"] ""
    script_ [src_ "/main.js"] ST.empty
