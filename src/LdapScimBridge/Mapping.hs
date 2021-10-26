{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans -Wno-missing-export-lists #-}

module LdapScimBridge.Mapping where

import qualified Data.Aeson as Aeson
import qualified Data.Map as Map
import qualified Data.String.Conversions as SC
import qualified GHC.Show
import qualified Text.Email.Validate
import Web.Scim.Class.Auth (AuthData)
import qualified Web.Scim.Class.Auth as AuthClass
import qualified Web.Scim.Class.Group as GroupClass
import qualified Web.Scim.Schema.Schema as Scim
import qualified Web.Scim.Schema.User as Scim
import qualified Web.Scim.Schema.User.Email as Scim

-- | Map attribute keys to functions from attribute values to changes to scim records.  We'll
-- start off with an empty scim record, and change it based on attributes we find that are
-- listed in the mapping.  Mappigns can fail, eg. if there is more than one attribute value
-- for the attribute mapping to externalId.
newtype Mapping = Mapping {fromMapping :: Map Text [FieldMapping]}
  deriving stock (Show)

data FieldMapping = FieldMapping
  { fieldMappingLabel :: Text,
    fieldMappingFun ::
      [Text] ->
      Either
        MappingError
        ( Scim.User ScimTag ->
          Scim.User ScimTag
        )
  }

data MappingError
  = MissingAttr Text
  | WrongNumberOfAttrValues Text String Int
  | CouldNotParseEmail Text String
  deriving stock (Eq, Show)

instance Show FieldMapping where
  show = show . fieldMappingLabel

instance Aeson.FromJSON Mapping where
  parseJSON = Aeson.withObject "Mapping" $ \obj -> do
    mfdisplayName <- obj Aeson..:? "displayName"
    fuserName <- obj Aeson..: "userName"
    fexternalId <- obj Aeson..: "externalId"
    mfemail <- obj Aeson..:? "email"

    let listToMap :: [(Text, a)] -> Map Text [a]
        listToMap = foldl' go mempty
          where
            go mp (k, b) = Map.alter (Just . maybe [b] (b :)) k mp

    pure . Mapping . listToMap . catMaybes $
      [ (\fdisplayName -> (fdisplayName, mapDisplayName fdisplayName)) <$> mfdisplayName,
        Just (fuserName, mapUserName fuserName),
        Just (fexternalId, mapExternalId fexternalId),
        (\femail -> (femail, mapEmail femail)) <$> mfemail
      ]
    where
      -- The name that shows for this user in wire.
      mapDisplayName :: Text -> FieldMapping
      mapDisplayName ldapFieldName = FieldMapping "displayName" $
        \case
          [val] -> Right $ \usr -> usr {Scim.displayName = Just val}
          bad -> Left $ WrongNumberOfAttrValues ldapFieldName "1" (Prelude.length bad)

      -- Really, not username, but handle.
      mapUserName :: Text -> FieldMapping
      mapUserName ldapFieldName = FieldMapping "userName" $
        \case
          [val] -> Right $ \usr -> usr {Scim.userName = val}
          bad -> Left $ WrongNumberOfAttrValues ldapFieldName "1" (Prelude.length bad)

      mapExternalId :: Text -> FieldMapping
      mapExternalId ldapFieldName = FieldMapping "externalId" $
        \case
          [val] -> Right $ \usr -> usr {Scim.externalId = Just val}
          bad -> Left $ WrongNumberOfAttrValues ldapFieldName "1" (Prelude.length bad)

      mapEmail :: Text -> FieldMapping
      mapEmail ldapFieldName = FieldMapping "emails" $
        \case
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
                ldapFieldName
                "<=1 (with more than one email, which one should be primary?)"
                (Prelude.length bad)

----------------------------------------------------------------------
-- ScimTag

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
