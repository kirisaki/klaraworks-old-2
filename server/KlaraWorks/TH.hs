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
  runIO $ callCommand "yarn"
  runIO $ callCommand "yarn run webpack"
  [| return () |]
