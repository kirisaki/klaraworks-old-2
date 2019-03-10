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
  styleNav

styleNav :: Css
styleNav = do
  nav ?
    position absolute
  nav |> ul ? do
    position absolute
    width (vw 100)
    height (em 2)
    top nil
    left nil
    display flex
    justifyContent spaceBetween
  nav |> ul |> a ? do
    width (vw 24)
    height (pct 100)
    display block
    textAlign center
    lineHeight (em 1.7)
    boxSizing borderBox
    padding nil (em 0.1) nil (em 0.1)
    borderColor kWhite
    borderWidth (px 1)
    borderStyle solid
    textDecoration none
    transform $ skewX (deg 135)
  nav |> ul |> a |> li ? do
    display block
    transform $ skewX (deg (-135))
  ".container" |>
    C.div ? do
      width (vw 100)
      height (vh 100 @-@ em 2)
      top (em 2)
      position relative
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
        fontFamily ["futura-pt", "a-otf-ud-shin-go-pr6n"] [sansSerif]
      ".container" ? do
        width (vw 400)
        height (vh 100)
        display flex
        position absolute
        kEase
      (body <> h1 <> nav <> C.div <> ul) ? do
        margin nil nil nil nil
        padding nil nil nil nil
    link_  [rel_ "stylesheet", href_ "/style.css"]
    script_ "(function(d){var config={kitId:'lve6akb',scriptTimeout: 3000,async: true}, h=d.documentElement,t=setTimeout(function(){h.className=h.className.replace(/\bwf-loading\b/g,'')+' wf-inactive';},config.scriptTimeout),tk=d.createElement('script'),f=false,s=d.getElementsByTagName('script')[0],a;h.className+=' wf-loading';tk.src='https://use.typekit.net/'+config.kitId+'.js';tk.async=true;tk.onload=tk.onreadystatechange=function(){a=this.readyState;if(f||a&&a!='complete'&&a!='loaded')return;f=true;clearTimeout(t);try{Typekit.load(config)}catch(e){}};s.parentNode.insertBefore(tk,s)})(document);"
  body_ $ do
    div_ [id_ "main"] ""
    script_ [src_ "/main.js"] ST.empty
