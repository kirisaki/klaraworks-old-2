{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
module Main (main, boot) where

import           KlaraWorks.Style
import           KlaraWorks.TH
import           Paths_klaraworks

import           Control.Monad.IO.Class      (liftIO)
import           Data.Binary
import qualified Data.ByteString.Lazy        as LBS
import           Data.FileEmbed              (embedFile)
import           Data.Int
import qualified Data.List                   as L
import qualified Data.Text                   as ST
import qualified Data.Text.Lazy.Encoding     as LTE
import           Data.Word
import           Network.HTTP.Types
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Network.Wai.Handler.WarpTLS

import           Clay                        hiding (style)

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
      ["klaraworks.svg"] ->
        respond
        [("Content-Type", "image/svg+xml")]
        klaraworksSvg
      ["JosefinSans.css"] ->
        respond
        [("Content-Type", "text/css")]
        josefinSansCss
      ["MPLUS1p.css"] ->
        respond
        [("Content-Type", "text/css")]
        mPlus1pCss
      ["api", "works", x] -> do
        liftIO $ print $ rawQueryString req
        case (x, rawQueryString req) of
          ("20190401-lady3", "?jpn") ->
            respond
            [("Content-Type", "application/vnd.klaraworks.work-meta")]
            ( "\x22" <>
              "\x0e" <> "20190401-lady3" <>
              "\x5c\x53\x0d\x6e" <>
              "\x00\x06" <> "\229\176\145\229\165\179" <>
              "\x00" <> "" <>
              "\x01" <> "\x12" <> "20190401-lady3.jpg"
            )
          ("20190401-lady3", "?eng") ->
            respond
            [("Content-Type", "application/vnd.klaraworks.work-meta")]
            ( "\x22" <>
              "\x0e" <> "20190401-lady3" <>
              "\x5c\x53\x0d\x6e" <>
              "\x00\x06" <> "A Girl" <>
              "\x00" <> "" <>
              "\x01" <> "\x12" <> "20190401-lady3.jpg"
            )
          ("20190301-lady2", "?jpn") ->
            respond
            [("Content-Type", "application/vnd.klaraworks.work-meta")]
            ( "\x22" <>
              "\x0e" <> "20190301-lady2" <>
              "\x5c\x53\x0d\x6e" <>
              "\x00\x0c" <> "\227\129\172\227\129\132\227\129\172\227\129\132" <>
              "\x18" <> "\232\137\166\233\154\138\227\129\147\227\130\140\227\129\143\227\129\151\227\130\135\227\130\147" <>
              "\x01" <> "\x12" <> "20190401-lady2.jpg"
            )
          ("20190301-lady2", "?eng") ->
            respond
            [("Content-Type", "application/vnd.klaraworks.work-meta")]
            ( "\x22" <>
              "\x5c\x53\x0d\x6e" <>
              "\x0e" <> "20190301-lady2" <>
              "\x00\x06" <> "Nuinui" <>
              "\x11" <> "Kantai Collection" <>
              "\x01" <> "\x12" <> "20190401-lady2.jpg"
            )
          _ ->
            respond' $
            responseLBS status404
            [("Content-Type", "application/vnd.klaraworks.work-meta")]
            "\x44"
      ["api", "works"] ->
        case rawQueryString req of
          "?jpn" ->
            respond
            [("Content-Type", "application/vnd.klaraworks.works")]
            ( "\x22" <> "\x00\x02" <>
              "\x0e" <> "20190301-lady2" <> "\x5c\x53\x0d\x6e" <> "\x00\x0c" <> "\227\129\172\227\129\132\227\129\172\227\129\132" <>
              "\x0e" <> "20190401-lady3" <> "\x5c\x53\x0d\x6e" <> "\x00\x06" <> "\229\176\145\229\165\179" <>
              ""
            )
          "?eng" ->
            respond
            [("Content-Type", "application/vnd.klaraworks.works")]
            ( "\x22" <> "\x00\x02" <>
              "\x0e" <> "20190301-lady2" <> "\x5c\x53\x0d\x6e" <> "\x00\x06" <> "Nuinui" <>
              "\x0e" <> "20190401-lady3" <> "\x5c\x53\x0d\x6e" <> "\x00\x06" <> "A Girl" <>
              ""
            )
      _ ->
        respond
        [("Content-Type", "text/html")]
        indexHtml

data Language
  = Japanese
  | English
  deriving(Show, Eq)

languageToCode :: Language -> LBS.ByteString
languageToCode = \case
  Japanese -> "jpn"
  English -> "eng"

data WorkType
  = Picture
  | Manga
  deriving(Show, Eq)

workTypeToCode :: WorkType -> LBS.ByteString
workTypeToCode = \case
  Picture -> "\x01"
  Manga -> "\x02"



data WorkMeta = WorkMeta
  { workMetaTitle  :: ST.Text
  , workMetaOrigin :: ST.Text
  }

data Work = Work
  { workId        :: ST.Text
  , workTimestanp :: Int32
  , workType      :: WorkType
  , workMeta      :: [(Language, WorkMeta)]
  }

length8 :: ST.Text -> Word8
length8 = fromIntegral . ST.length

length16 :: ST.Text -> Word16
length16 = fromIntegral . ST.length

encodeWorkDetail :: Work -> Language -> LBS.ByteString
encodeWorkDetail work lang =
  case L.lookup lang $ workMeta work of
    Nothing -> "\x44"
    Just m ->
      let
        lenId = length8 $ workId work
        lenTitle = length16 $ workMetaTitle m
        lenOrigin = length16 $ workMetaOrigin m
      in
        undefined

data Assets = Assets
  { indexHtml      :: LBS.ByteString
  , mainJs         :: LBS.ByteString
  , styleCss       :: LBS.ByteString
  , backSvg        :: LBS.ByteString
  , klaraworksSvg  :: LBS.ByteString
  , josefinSansCss :: LBS.ByteString
  , mPlus1pCss     :: LBS.ByteString
  }

boot :: IO ()
boot = do
  $(build)
  let s = setPort 8000 defaultSettings
  let t = tlsSettings "../ssl/localhost.crt" "../ssl/localhost.key"
  runTLS t s . server $ Assets
    { indexHtml = index
    , mainJs = LTE.encodeUtf8 $(loadFile "dist/main.js") <>
               "var app = Elm.Main.init();"
    , styleCss =  style
    , backSvg = LBS.fromStrict $(embedFile "assets/back.svg")
    , klaraworksSvg = LBS.fromStrict $(embedFile "assets/klaraworks.svg")
    , josefinSansCss = LBS.fromStrict $(embedFile "assets/Josefin+Sans.css")
    , mPlus1pCss = LBS.fromStrict $(embedFile "assets/M+PLUS+1p.css")
    }


main :: IO ()
main =
  boot
