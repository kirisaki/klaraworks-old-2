{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module KlaraWorks.TH where

import           Control.Lens
import           Control.Monad
import           Data.Aeson
import           Data.Aeson.Lens
import qualified Data.List                  as L
import           Data.Text
import           Data.Text.Encoding         (encodeUtf8)
import qualified Data.Vector                as V
import           Data.Yaml
import           Language.Haskell.TH.Syntax
import           System.Directory
import           System.Process

loadFile :: FilePath -> Q Exp
loadFile path = do
  str <- runIO $ readFile path
  [| str |]

build :: Q Exp
build = do
  addDependentFile "package.yaml"
  y <- decodeFileThrow "package.yaml" :: Q Value
  let deps =
        L.map unpack .
        L.filter (isPrefixOf "client") .
        (\(Success t) -> t) .
        mapM fromJSON .
        V.toList $ y ^. key "extra-source-files" ._Array
  mapM_ addDependentFile deps
  runIO $ do
    createDirectoryIfMissing True "./__temp"
    createDirectoryIfMissing True "./dist"
    callCommand "npx elm make client/Main.elm --output=./__temp/main.js"
    callCommand "npx uglifyjs --compress 'pure_funcs=\"F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9\",pure_getters,keep_fargs=false,unsafe_comps,unsafe'  ./__temp/main.js | npx uglifyjs  --mangle -- > ./dist/main.js"
    removeDirectoryRecursive "./__temp"
  [| return () |]
