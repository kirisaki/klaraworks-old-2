{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TemplateHaskell            #-}
module Main (main, boot) where

import           KlaraWorks.Style
import           KlaraWorks.TH
import           Paths_klaraworks

import           Control.Exception.Safe
import           Control.Lens
import           Control.Monad.IO.Class      (MonadIO, liftIO)
import           Control.Monad.Reader
import           Data.Binary.Builder
import qualified Data.ByteString             as SBS
import qualified Data.ByteString.Lazy        as LBS
import           Data.FileEmbed              (embedFile)
import           Data.Functor
import qualified Data.HashMap.Strict         as HM
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
             ) (L.sort works)
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
  } deriving (Eq)

data Work = Work
  { workId        :: ST.Text
  , workTimestamp :: Int32
  , workType      :: WorkType
  , workMeta      :: [(Language, WorkMeta)]
  } deriving (Eq)

instance Ord Work where
  x <= y = workTimestamp x <= workTimestamp y

length8 :: ST.Text -> Word8
length8 = fromIntegral . SBS.length . STE.encodeUtf8

length16 :: ST.Text -> Word16
length16 = fromIntegral . SBS.length . STE.encodeUtf8

data ContentType
  = Html
  | JavaScript
  | Css
  | Svg
  | Jpeg
  deriving (Show, Eq)

typeToHeader :: ContentType -> Header
typeToHeader =
  let
    h t = ("Content-Type", t)
  in
    \case
      Html -> h "text/html"
      JavaScript -> h "text/javascript"
      Css -> h "text/css"
      Svg -> h "image/svg+xml"
      Jpeg -> h "image/jpeg"

data ContentData
  = File FilePath
  | Byte LBS.ByteString
  deriving (Show, Eq)

data Asset = Asset
  { contentData :: ContentData
  , contentType :: ContentType
  } deriving (Show, Eq)

makeLensesWith classyRules_ ''Asset

data AssetsEnv = AssetsEnv
  { files  :: HM.HashMap ST.Text Asset
  , images :: HM.HashMap ST.Text Asset
  } deriving(Show)

makeClassy_ ''AssetsEnv

newtype Env = Env
  { assets :: AssetsEnv
  } deriving(Show)

makeLensesWith classyRules_ ''Env

instance HasAssetsEnv Env where
  assetsEnv = _assets

type KlaraWorks env = ReaderT env IO Application

server :: (HasAssetsEnv env) => KlaraWorks env
server = do
  fs' <- view (assetsEnv . _files)
  imgs <- view (assetsEnv . _images)
  let fs = HM.union fs' imgs
  pure $
    \req res ->
      let
        notFound = res $ responseLBS status404
                   [typeToHeader Html]
                   "not found"
        index = case HM.lookup "index.html" fs of
              Just (Asset (Byte b) t) ->
                res $ responseLBS status200
                [typeToHeader t]
                b
              _ ->
                notFound
      in
        case pathInfo req of
          [] -> index
          ["about"] -> index
          ["works"] -> index
          ["contact"] -> index
          [x] ->
            case HM.lookup x fs of
              Just (Asset (Byte b) t) ->
                res $ responseLBS status200
                [typeToHeader t]
                b
              Just (Asset (File f) t) ->
                res $ responseFile status200
                [typeToHeader t]
                f Nothing
              _ ->
                notFound
          ["api", "works"] ->
            case rawQueryString req of
              "?jpn" ->
                res $ responseLBS status200
                [("Content-Type", "application/vnd.klaraworks.works")]
                $ encodeWorksSummary sampleWorks Japanese
              "?eng" ->
                res $ responseLBS status200
                [("Content-Type", "application/vnd.klaraworks.works")]
                $ encodeWorksSummary sampleWorks English
          _ ->
            notFound

boot :: IO ()
boot = do
  $(build)
  let s = setPort 8000 defaultSettings
  let t = tlsSettings "../ssl/localhost.crt" "../ssl/localhost.key"
  runTLS t s =<< runReaderT server
    (Env
     (AssetsEnv
      (HM.fromList
       [ ("index.html", Asset (Byte KlaraWorks.Style.index) Html)
       , ("main.js", Asset (Byte (LTE.encodeUtf8 $(loadFile "dist/main.js") <> "var app = Elm.Main.init();")) JavaScript)
       , ("style.css", Asset (Byte style) Css)
       , ("back.svg", Asset (File "./assets/back.svg") Svg)
       , ("klaraworks.svg", Asset (File "./assets/klaraworks.svg") Svg)
       , ("JosefinSans.css", Asset (File "./assets/Josefin+Sans.css") Css)
       , ("MPLUS1p.css", Asset (File "./assets/M+PLUS+1p.css") Css)
       ]
      )
      (HM.fromList
       [ ("20190515-yudachi-0.jpg", Asset (File "./img/20190515-yudachi-0.jpg") Jpeg)
       , ("20190406-lily-0.jpg", Asset (File "./img/20190406-lily-0.jpg") Jpeg)
      ]
      )
     )
    )

sampleWorks :: [Work]
sampleWorks =
  [ Work
    "20190515-yudachi"
    0x5c530dff
    Picture
    [ (Japanese, WorkMeta "お祈り夕立" "艦隊これくしょん")
    , (English, WorkMeta "Yudachi praying" "Kantai Collection")
    ]
  , Work
    "20190406-lily"
    0x5c530d6e
    Picture
    [ (Japanese, WorkMeta "百合の魔術師" "")
    , (English, WorkMeta "The witch of lily" "")
    ]
  ]




main :: IO ()
main =
  boot
