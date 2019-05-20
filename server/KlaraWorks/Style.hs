{-# LANGUAGE OverloadedStrings #-}
module KlaraWorks.Style
  ( style
  , index
  ) where

import           Clay                    hiding (div, style)
import qualified Clay                    as C (div)
import qualified Clay.Flexbox            as CF
import qualified Clay.Media              as CM
import qualified Data.ByteString.Lazy    as LBS
import qualified Data.Text               as ST
import qualified Data.Text.Lazy.Encoding as LTE
import           Lucid
import           Lucid.Base
import           Prelude                 hiding (rem, span)

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

kSmall :: Size LengthUnit
kSmall = rem 0.8

kMiddle :: Size LengthUnit
kMiddle = rem 1

kLarge :: Size LengthUnit
kLarge = rem 1.4

kMobile :: Css -> Css
kMobile = query CM.screen [CM.maxWidth (px 700)]

style :: LBS.ByteString
style = LTE.encodeUtf8 . renderWith compact [] $ do
  a ?
    color kWhite
  article  ? do
    display flex
    flexWrap CF.wrap
    justifyContent CF.flexEnd
  styleNav
  styleSetting
  styleRouting
  styleIndex
  styleAbout
  styleWorks

styleNav :: Css
styleNav = do
  nav ? do
    position absolute
    width (vw 40)
    height (rem 2)
    top (rem 1.7)
    right (vw 2)
    kMobile $ do
      left (vw (-5))
      width (vw 110)
  nav |> ul ? do
    display flex
    justifyContent spaceBetween
  nav |> ul |> a ? do
    width (pct 24)
    height (pct 100)
    display block
    textAlign center
    lineHeight (rem 1.7)
    boxSizing borderBox
    padding nil (rem 0.1) nil (rem 0.1)
    borderColor kWhite
    borderWidth (px 1)
    borderStyle solid
    outlineStyle none
    textDecoration none
    transform $ skewX (deg 135)
    kEase
  nav |> ul |> "a.active" ?
    backgroundColor kPink
  nav |> ul |> a # hover ?
    backgroundColor kPink
  nav |> ul |> a |> li ? do
    position relative
    left (rem (- 0.3))
    display block
    transform $ skewX (deg (-135))

styleRouting :: Css
styleRouting = do
  ".container" |> C.div ? do
    width (vw 100)
    height (pct 100 @-@ rem 3.7)
    top (rem 3.7)
    position relative
    overflow auto
  ".container.index" ?
    left (vw 0)
  ".container.about" ?
    left (vw (-100))
  ".container.works" ?
    left (vw (-200))
  ".container.contact" ?
    left (vw (-300))
  ".container" |> C.div |> article ? do
    position absolute
    right nil
    width (vw 70)
    kMobile $
      width (vw 100)

styleSetting :: Css
styleSetting = do
  ".setting" ? do
    height (rem 1.7)
    top nil
    right (rem 0.5)
    position absolute
    fontSize kSmall
    display flex
  ".setting" |> li ? do
    lineHeight (rem 1.7)
    display block
    padding nil nil nil (rem 1)
  ".language_selector, .seed" ? do
    color kWhite
    backgroundColor transparent
    borderStyle none
    fontFamily ["Josefin Sans", "M PLUS 1p"] [sansSerif]
  ".language_selector" |> option ? do
    backgroundColor kDark
    fontFamily ["Josefin Sans", "M PLUS 1p"] [sansSerif]
  ".seed" ?
    textAlign end

styleIndex :: Css
styleIndex = do
  ".container" |> ".index" |> h1 ? do
    position absolute
    width $ rem 13
    height $ rem 3
    top $ pct 40
    right $ rem 1
    backgroundColor transparent
    backgroundImage $ url "klaraworks.svg"
    backgroundRepeat noRepeat
    display block
  ".container" |> ".index" |> h1 |> span ?
    display none

styleAbout :: Css
styleAbout = do
  ".about" |> article |> C.div ? do
    width (pct 50)
    boxSizing borderBox
    padding (rem 0.7) (rem 0.7) (rem 0.7) (rem 0.7)
    kMobile $
      width (pct 100)
  ".about" |> article |> C.div |> section ? box
  ".about" |> article |> C.div |> section |> h1 ? do
    fontWeight (weight 400)
    fontSize kLarge
    borderWidth4 nil nil (px 1) nil
    borderStyle solid
  ".about" |> article |> C.div |> section |> p ?
    textAlign justify

styleWorks :: Css
styleWorks = do
  ".works" |> article |> C.div ? do
    width (pct 33)
    boxSizing borderBox
    padding (rem 0.7) (rem 0.7) (rem 0.7) (rem 0.7)
    kMobile $
      width (pct 50)
  ".works" |> article |> C.div |> section ? do
    box
    img ?
      width (pct 100)

box :: Css
box = do
  padding (rem 1) (rem 1) (rem 1) (rem 1)
  borderWidth (px 1)
  borderStyle solid

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
        borderColor kWhite
        color kWhite
        overflow hidden
        position fixed
        top nil
        bottom nil
        left nil
        right nil
        fontFamily ["Josefin Sans", "M PLUS 1p"] [sansSerif]
        fontSize (pt 12)
      ".container" ? do
        width (vw 400)
        height (vh 100)
        display flex
        position absolute
        kEase
      (body <> h1 <> nav <> C.div <> ul) ? do
        margin nil nil nil nil
        padding nil nil nil nil
    link_  [rel_ "dns-prefetch", href_ "https://fonts.gstatic.com/"]
    link_  [rel_ "preload", href_ "/klaraworks.svg", Attribute "as" "image"]
    link_  [rel_ "preload", href_ "/back.svg", Attribute "as" "image"]
    link_  [rel_ "preload", href_ "/main.js", Attribute "as" "script"]
    link_  [rel_ "stylesheet", href_ "/JosefinSans.css"]
    link_  [rel_ "stylesheet", href_ "/MPLUS1p.css"]
    link_  [rel_ "stylesheet", href_ "/style.css"]
  body_ $ do
    div_ [id_ "main"] ""
    script_ [async_ "", src_ "/main.js"] ST.empty
