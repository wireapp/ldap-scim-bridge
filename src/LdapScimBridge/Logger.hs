{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans -Wno-missing-export-lists #-}

module LdapScimBridge.Logger where

import System.Logger (Level (..))
import qualified System.Logger as Log

type Logger = Level -> Text -> IO ()

mkLogger :: Level -> IO Logger
mkLogger lvl = do
  lgr :: Log.Logger <-
    Log.defSettings
      & Log.setLogLevel lvl
      & Log.new
  pure $ \msgLvl msgContent -> do
    Log.log lgr msgLvl (Log.msg @Text $ show msgContent)
    Log.flush lgr
