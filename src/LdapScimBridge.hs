{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans -Wno-missing-export-lists #-}

module LdapScimBridge where

import Control.Exception (ErrorCall (ErrorCall), catch, throwIO)
import qualified Data.ByteString.Char8 as ByteString
import Data.String.Conversions (cs)
import qualified Data.Yaml as Yaml
import LdapScimBridge.Config
import LdapScimBridge.Logger
import LdapScimBridge.Scim
import System.Environment (getProgName)
import System.Logger (Level (..))

parseCli :: IO BridgeConf
parseCli = do
  usage <- do
    progName <- getProgName
    let usage :: String -> ErrorCall
        usage = ErrorCall . (<> help)
        help =
          cs . unlines . fmap cs $
            [ "",
              "",
              "usage: " <> progName <> " <config.yaml>",
              "see https://github.com/wireapp/ldap-scim-bridge for a sample config."
            ]
    pure usage

  getArgs >>= \case
    [file] -> do
      content <- ByteString.readFile file `catch` \(SomeException err) -> throwIO . usage $ show err
      either (throwIO . usage . show) pure $ Yaml.decodeEither' content
    bad -> throwIO . usage $ "bad number of arguments: " <> show bad

main :: IO ()
main = do
  myconf :: BridgeConf <- parseCli
  lgr :: Logger <- mkLogger (logLevel myconf)
  lgr Debug $ show (mapping myconf)
  updateScimPeer lgr myconf `catch` logErrors lgr
  where
    logErrors :: Logger -> SomeException -> IO a
    logErrors lgr (SomeException e) = do
      lgr Fatal $ "uncaught exception: " <> show e
      throwIO e
