{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans -Wno-missing-export-lists #-}

module LdapScimBridge.Config where

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
import LdapScimBridge.Mapping
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

data LdapConf = LdapConf
  { -- | eg. @Ldap.Tls (host conf) Ldap.defaultTlsSettings@
    ldapHost :: Host,
    -- | usually 389 for plaintext or 636 for TLS.
    ldapPort :: PortNumber,
    -- | `$ slapcat | grep ^modifiersName`, eg. @Dn "cn=admin,dc=nodomain"@.
    ldapDn :: Dn,
    ldapPassword :: Password,
    ldapSearch :: LdapSearch,
    -- | anything from "Data.Text.Encoding".
    ldapCodec :: Codec,
    ldapDeleteOnAttribute :: Maybe LdapFilterAttr,
    ldapDeleteFromDirectory :: Maybe LdapSearch
  }
  deriving stock (Show)

data LdapFilterAttr = LdapFilterAttr
  { key :: Text,
    value :: Text
  }
  deriving stock (Eq, Show, Generic)

data LdapSearch = LdapSearch
  { -- | `$ slapcat | grep ^dn`, eg. @Dn "dc=nodomain"@.
    ldapSearchBase :: Dn,
    -- | eg. @"account"@
    ldapSearchObjectClass :: Text,
    -- | eg. @[LdapFilterAttr "memberOf" "team red", LdapFilterAttr "hairColor" "yellow"]
    ldapSearchExtra :: [LdapFilterAttr]
  }
  deriving stock (Eq, Show)

data Codec = Utf8 | Latin1
  deriving stock (Eq, Show)

instance Aeson.FromJSON LdapConf where
  parseJSON = Aeson.withObject "LdapConf" $ \obj -> do
    ftls :: Bool <- obj Aeson..: "tls"
    fhost :: String <- obj Aeson..: "host"
    fport :: Int <- obj Aeson..: "port"
    fdn :: Text <- obj Aeson..: "dn"
    fpassword :: String <- obj Aeson..: "password"
    fsearch :: LdapSearch <- obj Aeson..: "search"
    fcodec :: Text <- obj Aeson..: "codec"
    fdeleteOnAttribute :: Maybe LdapFilterAttr <- obj Aeson..:? "deleteOnAttribute" -- TODO: this can go into 'fdeleteFromDirectory'.
    fdeleteFromDirectory :: Maybe LdapSearch <- obj Aeson..:? "deleteFromDirectory"

    let vhost :: Host
        vhost = case ftls of
          True -> Ldap.Tls fhost Ldap.defaultTlsSettings
          False -> Ldap.Plain fhost

        vport :: PortNumber
        vport = fromIntegral fport

    vcodec <- case fcodec of
      "utf8" -> pure Utf8
      "latin1" -> pure Latin1
      bad -> fail $ "unsupported codec: " <> show bad

    pure $
      LdapConf
        { ldapHost = vhost,
          ldapPort = vport,
          ldapDn = Dn fdn,
          ldapPassword = Password $ ByteString.pack fpassword,
          ldapSearch = fsearch,
          ldapCodec = vcodec,
          ldapDeleteOnAttribute = fdeleteOnAttribute,
          ldapDeleteFromDirectory = fdeleteFromDirectory
        }

instance Aeson.FromJSON LdapFilterAttr where
  parseJSON = Aeson.withObject "LdapFilterAttr" $ \obj -> do
    LdapFilterAttr
      <$> obj Aeson..: "key"
      <*> obj Aeson..: "value"

instance Aeson.FromJSON LdapSearch where
  parseJSON = Aeson.withObject "LdapSearch" $ \obj -> do
    fbase :: Text <- obj Aeson..: "base"
    fobjectClass :: Text <- obj Aeson..: "objectClass"

    extra :: [LdapFilterAttr] <- do
      let go :: (Text, Yaml.Value) -> Yaml.Parser LdapFilterAttr
          go (key, val) = do
            str <- Aeson.withText "val" pure val
            pure $ LdapFilterAttr key str
      go `mapM` HM.toList (HM.filterWithKey (\k _ -> k `notElem` ["base", "objectClass"]) obj)
    pure $ LdapSearch (Dn fbase) fobjectClass extra

data ScimConf = ScimConf
  { scimTls :: Bool,
    scimHost :: String,
    scimPort :: Int,
    scimPath :: String,
    scimToken :: Text
  }
  deriving stock (Eq, Show, Generic)

instance Aeson.FromJSON ScimConf where
  parseJSON = Aeson.withObject "ScimConf" $ \obj -> do
    ScimConf
      <$> obj Aeson..: "tls"
      <*> obj Aeson..: "host"
      <*> obj Aeson..: "port"
      <*> obj Aeson..: "path"
      <*> obj Aeson..: "token"

data BridgeConf = BridgeConf
  { ldapSource :: LdapConf,
    scimTarget :: ScimConf,
    mapping :: Mapping,
    logLevel :: Level
  }
  deriving stock (Show, Generic)

instance Aeson.FromJSON Level where
  parseJSON "Trace" = pure Trace
  parseJSON "Debug" = pure Debug
  parseJSON "Info" = pure Info
  parseJSON "Warn" = pure Warn
  parseJSON "Error" = pure Error
  parseJSON "Fatal" = pure Fatal
  parseJSON bad = fail $ "unknown Level: " <> show bad

instance Aeson.FromJSON BridgeConf
