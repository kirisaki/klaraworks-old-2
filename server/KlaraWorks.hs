{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
module Main (main, boot) where

import           KlaraWorks.Style
import           KlaraWorks.TH
import           Paths_klaraworks

import           Control.Monad.IO.Class      (liftIO)
import           Data.Binary.Builder
import qualified Data.ByteString             as SBS
import qualified Data.ByteString.Lazy        as LBS
import           Data.FileEmbed              (embedFile)
import           Data.Int
import qualified Data.List                   as L
import           Data.Maybe
import qualified Data.Text                   as ST
import qualified Data.Text.Encoding          as STE
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
      ["api", "works"] ->
        case rawQueryString req of
          "?jpn" ->
            respond
            [("Content-Type", "application/vnd.klaraworks.works")]
            $ encodeWorksSummary sampleWorks Japanese
          "?eng" ->
            respond
            [("Content-Type", "application/vnd.klaraworks.works")]
            $ encodeWorksSummary sampleWorks English
      _ ->
        respond
        [("Content-Type", "text/html")]
        indexHtml

sampleWorks :: [Work]
sampleWorks =
  [ Work
    "20190301-lady"
    0x5c530d6e
    Picture
    [ (Japanese, WorkMeta "少女" "")
    , (English, WorkMeta "A Girl" "")
    ]
  , Work
    "20190301-nuinui"
    0x5c530d6e
    Picture
    [ (Japanese, WorkMeta "ぬいぬい" "艦隊これくしょん")
    , (English, WorkMeta "Nuinui" "Kantai Collection")
    ]
  ]

encodeWorksSummary :: [Work] -> Language -> LBS.ByteString
encodeWorksSummary works lang =
  let
    summaries = fmap
             (\w ->
                case L.lookup lang (workMeta w) of
                  Nothing -> Nothing
                  Just m ->
                    let
                      lenId = singleton . length8 $ workId w
                      lenTitle = putWord16be . length16 $ workMetaTitle m
                    in
                      Just $
                      lenId <> (fromByteString . STE.encodeUtf8) (workId w) <>
                      putInt32be (workTimestamp w) <>
                      lenTitle <> (fromByteString . STE.encodeUtf8) (workMetaTitle m)
             ) works
  in
    case catMaybes summaries of
      [] -> "\x44"
      ss ->
        let
          num = putWord16be . fromIntegral $ L.length ss
        in
          toLazyByteString $
          "\x22" <> num <> mconcat ss

encodeWorkDetail :: Work -> Language -> LBS.ByteString
encodeWorkDetail work lang =
  case L.lookup lang $ workMeta work of
    Nothing -> "\x44"
    Just m ->
      let
        lenId = singleton . length8 $ workId work
        lenTitle = putWord16be . length16 $ workMetaTitle m
        lenOrigin = singleton . length8 $ workMetaOrigin m
      in
        toLazyByteString $
        "\x22" <>
        putInt32be (workTimestamp work) <>
        lenId <> (fromByteString . STE.encodeUtf8) (workId work) <>
        lenTitle <> (fromByteString . STE.encodeUtf8) (workMetaTitle m) <>
        lenOrigin <> (fromByteString . STE.encodeUtf8) (workMetaOrigin m)

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
  , workTimestamp :: Int32
  , workType      :: WorkType
  , workMeta      :: [(Language, WorkMeta)]
  }

length8 :: ST.Text -> Word8
length8 = fromIntegral . SBS.length . STE.encodeUtf8

length16 :: ST.Text -> Word16
length16 = fromIntegral . SBS.length . STE.encodeUtf8


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
