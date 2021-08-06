{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module LdapScimBridge where

import Control.Exception (bracket_)
import Control.Monad (when)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encode.Pretty as Aeson
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as ByteString
import qualified Data.ByteString.Lazy.Char8 as LByteString
import qualified Data.Foldable as Foldable
import Data.Function (fix)
import Data.List.NonEmpty
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Data.Text.Encoding as Text
import qualified Data.Text.IO as Text
import qualified Data.Yaml as Yaml
import Ldap.Client as Ldap
import qualified Ldap.Client.Bind as Ldap
import System.Exit (die)
import qualified System.IO as IO
import qualified Web.Scim.Schema.User as ScimSchema
import qualified Web.Scim.Server.Mock as ScimServer

data SearchConf = SearchConf
  { -- | eg. @Ldap.Tls (host conf) Ldap.defaultTlsSettings@
    host :: Host,
    -- | usually 389 for plaintext or 636 for TLS.
    port :: PortNumber,
    -- | `$ slapcat | grep ^modifiersName`, eg. @Dn "cn=admin,dc=nodomain"@.
    dn :: Dn,
    password :: Password,
    -- | `$ slapcat | grep ^dn`, eg. @Dn "dc=nodomain"@.
    base :: Dn,
    -- | eg. @Attr "objectClass" := "account"@.
    fltr :: Filter,
    -- | anything from "Data.Text.Encoding".
    codec :: ByteString -> Text,
    mapping :: Mapping
  }

instance Aeson.FromJSON SearchConf where
  parseJSON = Aeson.withObject "SearchConf" $ \obj -> do
    ftls :: Bool <- obj Aeson..: "tls"
    fhost :: String <- obj Aeson..: "host"
    fport :: Int <- obj Aeson..: "port"
    fdn :: Text <- obj Aeson..: "dn"
    fpassword :: String <- obj Aeson..: "password"
    fbase :: Text <- obj Aeson..: "base"
    fobjectClass :: String <- obj Aeson..: "objectClass"
    fcodec :: Text <- obj Aeson..: "codec"
    fmapping :: Mapping <- obj Aeson..: "mapping"

    let vhost :: Host
        vhost = case ftls of
          True -> Ldap.Tls fhost Ldap.defaultTlsSettings
          False -> Ldap.Plain fhost

        vport :: PortNumber
        vport = fromIntegral fport

    vcodec :: (ByteString -> Text) <- case fcodec of
      "utf8" -> pure Text.decodeUtf8
      "latin1" -> pure Text.decodeLatin1
      bad -> fail $ "unsupported codec: " <> show bad

    pure $
      SearchConf
        { host = vhost,
          port = vport,
          dn = Dn fdn,
          password = Password $ ByteString.pack fpassword,
          base = Dn fbase,
          fltr = Attr "objectClass" := ByteString.pack fobjectClass,
          codec = vcodec,
          mapping = fmapping
        }

data MappingError
  = MissingAttr Text
  | WrongNumberOfAttrValues Text Int Int
  deriving (Eq, Show)

newtype FieldMapping
  = FieldMapping
      ( [Text] ->
        Either
          MappingError
          ( ScimSchema.User ScimServer.Mock ->
            ScimSchema.User ScimServer.Mock
          )
      )

-- | Map attribute keys to functions from attribute values to changes to scim records.  We'll
-- start off with an empty scim record, and change it based on attributes we find that are
-- listed in the mapping.  Mappigns can fail, eg. if there is more than one attribute value
-- for the attribute mapping to externalId.
newtype Mapping = Mapping {fromMapping :: Map Text FieldMapping}

instance Aeson.FromJSON Mapping where
  parseJSON = Aeson.withObject "Mapping" $ \obj -> do
    fuserName <- obj Aeson..: "userName"
    fexternalId <- obj Aeson..: "externalId"
    mfemail <- obj Aeson..:? "email"

    pure . Mapping . Map.fromList . catMaybes $
      [ Just (fuserName, mapUserName fuserName),
        Just (fexternalId, mapExternalId fexternalId),
        (\femail -> (femail, mapEmail femail)) <$> mfemail
      ]
    where
      mapUserName ldapFieldName = FieldMapping $
        \case
          [val] -> Right $ \usr -> usr {ScimSchema.userName = val}
          bad -> Left $ WrongNumberOfAttrValues ldapFieldName 1 (Prelude.length bad)

      mapExternalId ldapFieldName = FieldMapping $
        \case
          [val] -> Right $ \usr -> usr {ScimSchema.externalId = Just val}
          bad -> Left $ WrongNumberOfAttrValues ldapFieldName 1 (Prelude.length bad)

      mapEmail = error "now we can allow 0 or more than 1 values!"

type LdapResult a = IO (Either LdapError a)

searchLdapUser :: SearchConf -> Text -> LdapResult [SearchEntry]
searchLdapUser conf uid = Ldap.with (host conf) (port conf) $ \l -> do
  Ldap.bind l (dn conf) (password conf)
  Ldap.search
    l
    (base conf)
    (typesOnly True)
    (And (fltr conf :| [Attr "uid" := Text.encodeUtf8 uid]))
    []

listLdapUsers :: SearchConf -> LdapResult [SearchEntry]
listLdapUsers conf = Ldap.with (host conf) (port conf) $ \l -> do
  Ldap.bind l (dn conf) (password conf)
  Ldap.search l (base conf) mempty (fltr conf) mempty

-- | the 'undefined' is ok, the mapping is guaranteed to contain a filler for this, or the
-- mapping parser would have failed.
emptyScimUser :: ScimSchema.User ScimServer.Mock
emptyScimUser = ScimSchema.empty [] undefined ScimSchema.NoUserExtra

ldapToScim ::
  forall scim.
  scim ~ Either [MappingError] (ScimSchema.User ScimServer.Mock) =>
  SearchConf ->
  SearchEntry ->
  scim
ldapToScim conf (SearchEntry _ attrs) = Foldable.foldl' go (Right emptyScimUser) attrs
  where
    go :: scim -> (Attr, [AttrValue]) -> scim
    go scimval (Attr key, vals) = case Map.lookup key (fromMapping $ mapping conf) of
      Nothing -> scimval
      Just (FieldMapping f) -> case (scimval, f (codec conf <$> vals)) of
        (Right scimusr, Right f') -> Right (f' scimusr)
        (Right _, Left err) -> Left [err]
        (Left errs, Right _) -> Left errs
        (Left errs, Left err) -> Left (err : errs)

{-
SearchEntry (Dn "cn=me,ou=People,dc=nodomain")
[(Attr "objectClass",["top","account","posixAccount","shadowAccount"])
,(Attr "cn",["me"])
,(Attr "uid",["me"])
,(Attr "uidNumber",["10003"])
,(Attr "gidNumber",["10003"])
,(Attr "homeDirectory",["/home/me"])
,(Attr "userPassword",["notgonnatelleither"])
,(Attr "loginShell",["/bin/bash"])
]

next:
- find out what wire wants and construct that from somewhere
- write working mapping
- write and run client against wire-server
- find out what AD usually provides and make sure we can express that in a yaml file
- don't just post, but also do update and delete appropriately

-}

----------------------------------------------------------------------

main :: IO ()
main = do
  myconf :: SearchConf <- ByteString.readFile "../sample-conf.yaml" >>= either (error . show) pure . Yaml.decodeEither'
  searchLdapUser myconf "john" >>= print
  listLdapUsers myconf >>= print
  ldaps :: [SearchEntry] <- either (error . show) pure =<< listLdapUsers myconf
  let mbScims :: [Either [MappingError] (ScimSchema.User ScimServer.Mock)]
      mbScims = ldapToScim myconf <$> ldaps
  scims :: [ScimSchema.User ScimServer.Mock] <- mapM (either (error . show) pure) mbScims
  LByteString.putStrLn $ Aeson.encodePretty scims
