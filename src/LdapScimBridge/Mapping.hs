{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans -Wno-missing-export-lists #-}

module LdapScimBridge.Mapping where

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

-- | Map attribute keys to functions from attribute values to changes to scim records.  We'll
-- start off with an empty scim record, and change it based on attributes we find that are
-- listed in the mapping.  Mappigns can fail, eg. if there is more than one attribute value
-- for the attribute mapping to externalId.
type Mapping = Mapping' Mapper

newtype Mapping' a = Mapping {fromMapping :: Map LdapKey [ScimKey a]}
  deriving stock (Show, Eq)

newtype LdapKey = LdapKey Text
  deriving stock (Show, Eq, Ord)

newtype LdapVal = LdapVal Text
  deriving stock (Show, Eq, Ord)

newtype ScimKey a = ScimKey (Text, a)
  deriving stock (Show, Eq, Ord)

-- | Function that takes 0 or more values stored under the same 'LdapKey' in an ldap
-- 'SearchEntry', and updates a given scim user or produces an error.  We provide hard-wired
-- 'Mapper's for some scim user fields (see below).
newtype Mapper = Mapper
  { fromMapper ::
      [Text] ->
      Either
        MappingError
        ( Scim.User ScimTag ->
          Scim.User ScimTag
        )
  }

data MappingError
  = WrongNumberOfAttrValues LdapKey String Int
  | CouldNotParseEmail Text String
  deriving stock (Eq, Show)

construct :: Mapping' () -> Mapping' Mapper
construct (Mapping mp) = Mapping $ Map.mapWithKey go mp
  where
    go :: LdapKey -> [ScimKey ()] -> [ScimKey Mapper]
    go k vs = go' k <$> vs

    go' :: LdapKey -> ScimKey () -> ScimKey Mapper
    --
    go' ldapKey (ScimKey ("displayName", ())) = ScimKey . ("displayName",) . Mapper $ \case
      [val] -> Right $ \usr -> usr {Scim.displayName = Just val}
      bad -> Left $ WrongNumberOfAttrValues ldapKey "1" (Prelude.length bad)
    --
    go' ldapKey (ScimKey ("userName", ())) = ScimKey . ("userName",) . Mapper $ \case
      [val] -> Right $ \usr -> usr {Scim.userName = val}
      bad -> Left $ WrongNumberOfAttrValues ldapKey "1" (Prelude.length bad)
    --
    go' ldapKey (ScimKey ("externalId", ())) = ScimKey . ("externalId",) . Mapper $ \case
      [val] -> Right $ \usr -> usr {Scim.externalId = Just val}
      bad -> Left $ WrongNumberOfAttrValues ldapKey "1" (Prelude.length bad)
    --
    go' ldapKey (ScimKey ("emails", ())) = ScimKey . ("emails",) . Mapper $ \case
      [] -> Right id
      [val] -> case Text.Email.Validate.validate (SC.cs val) of
        Right email -> Right $ \usr ->
          usr
            { Scim.emails =
                [Scim.Email Nothing (Scim.EmailAddress2 email) Nothing]
            }
        Left err -> Left $ CouldNotParseEmail val err
      bad ->
        Left $
          WrongNumberOfAttrValues
            ldapKey
            "<=1 (with more than one email, which one should be primary?)"
            (Prelude.length bad)
    go' ldapKey (ScimKey (bad, ())) = ScimKey . ("emails",) . Mapper $ \case


----------------------------------------------------------------------

instance Aeson.FromJSON (Mapping' Text) where
  parseJSON = Aeson.withObject "Mapping" $ \obj -> do
    mfdisplayName <- obj Aeson..:? "displayName"
    fuserName <- obj Aeson..: "userName"
    fexternalId <- obj Aeson..: "externalId"
    mfemail <- obj Aeson..:? "email"

    let listToMap :: [(LdapKey, scimKey)] -> Map LdapKey [scimKey]
        listToMap = foldl' go mempty
          where
            go mp (k, b) = Map.alter (Just . maybe [b] (b :)) k mp

    pure . Mapping . listToMap . catMaybes $
      [ (\fdisplayName -> (LdapKey fdisplayName, ScimKey "displayName")) <$> mfdisplayName,
        Just (LdapKey fuserName, ScimKey "userName"),
        Just (LdapKey fexternalId, ScimKey "externalId"),
        (\femail -> (LdapKey femail, ScimKey "email")) <$> mfemail
      ]

----------------------------------------------------------------------

-- | Fill in the parameters for hscim 'User' type with plausible defaults.  (You may want to
-- touch this if you're using the library for something new.)
data ScimTag

instance Scim.UserTypes ScimTag where
  type UserId ScimTag = Text
  type UserExtra ScimTag = Scim.NoUserExtra
  supportedSchemas = [Scim.User20]

instance GroupClass.GroupTypes ScimTag where
  type GroupId ScimTag = Text

instance AuthClass.AuthTypes ScimTag where
  type AuthData ScimTag = Text
  type AuthInfo ScimTag = ()
