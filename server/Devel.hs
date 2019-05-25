{-# LANGUAGE LambdaCase #-}
module Devel where

import           Main               (boot)

import           Control.Concurrent
import           Control.Monad
import           Data.IORef
import           Foreign.Store
import           GHC.Word

lockStore :: Store (MVar ())
lockStore = Store 0

tidIndex :: Word32
tidIndex = 1

update :: IO ()
update =
  lookupStore tidIndex >>=
  \case
    Just tidStore -> restart tidStore
    _ -> do
      lock <- storeAction lockStore newEmptyMVar
      tid <- start lock
      _ <- storeAction (Store tidIndex) (newIORef tid)
      return ()
  where
    restart :: Store (IORef ThreadId) -> IO ()
    restart tidStore =
      withStore tidStore $ \ref -> do
        tid <- readIORef ref
        killThread tid
        withStore lockStore takeMVar
        readStore lockStore >>= start >>= writeIORef ref
    start :: MVar () -> IO ThreadId
    start lock =
      forkFinally
      boot
      (\_ -> putMVar lock () >> shutdown)

shutdown :: IO ()
shutdown =
  lookupStore tidIndex >>=
  \case
    Just tidStore -> do
      withStore tidStore $ readIORef >=> killThread
      putStrLn "shutdown"
    _ -> putStrLn "no application running"

