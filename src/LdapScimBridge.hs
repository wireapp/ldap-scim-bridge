{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans -Wno-missing-export-lists #-}

module LdapScimBridge where

import Control.Exception (ErrorCall (ErrorCall), catch, throwIO)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encode.Pretty as Aeson
import qualified Data.ByteString.Char8 as ByteString
import qualified Data.Foldable as Foldable
import qualified Data.HashMap.Lazy as HM
import qualified Data.List
import qualified Data.Map as Map
import Data.String.Conversions (cs)
import qualified Data.String.Conversions as SC
import qualified Data.Text.Encoding as Text
import qualified Data.Yaml as Yaml
import qualified GHC.Show
import Ldap.Client as Ldap
import LdapScimBridge.Config
import LdapScimBridge.Ldap
import LdapScimBridge.Logger
import LdapScimBridge.Mapping
import LdapScimBridge.Scim
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Client.TLS as HTTP
import Servant.API.ContentTypes (NoContent)
import Servant.Client (BaseUrl (..), ClientEnv (..), Scheme (..), mkClientEnv)
import System.Environment (getProgName)
import System.Logger (Level (..))
import qualified System.Logger as Log
import qualified Text.Email.Validate
import Web.Scim.Class.Auth (AuthData)
import qualified Web.Scim.Class.Auth as AuthClass
import qualified Web.Scim.Class.Group as GroupClass
import qualified Web.Scim.Class.User as ScimClass
import qualified Web.Scim.Client as ScimClient
import qualified Web.Scim.Filter as ScimFilter
import qualified Web.Scim.Schema.Common as ScimCommon
import qualified Web.Scim.Schema.ListResponse as Scim
import qualified Web.Scim.Schema.Meta as Scim
import qualified Web.Scim.Schema.Schema as Scim
import qualified Web.Scim.Schema.User as Scim
import qualified Web.Scim.Schema.User.Email as Scim

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
