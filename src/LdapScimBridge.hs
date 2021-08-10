{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports -Wno-missing-export-lists #-}

module LdapScimBridge where

import Control.Exception (bracket_, throwIO)
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
import qualified Data.String.Conversions as SC
import Data.Text (Text)
import qualified Data.Text.Encoding as Text
import qualified Data.Text.IO as Text
import qualified Data.Yaml as Yaml
import Ldap.Client as Ldap
import qualified Ldap.Client.Bind as Ldap
import Servant.API (Header, (:<|>) ((:<|>)), (:>))
import Servant.API.ContentTypes (NoContent)
import Servant.API.Generic (ToServant)
import Servant.Client (ClientM, client, runClientM)
import Servant.Client.Generic (AsClientT, genericClientHoist)
import System.Exit (die)
import qualified System.IO as IO
import qualified Text.Email.Validate
import qualified Web.Scim.Class.Auth as ScimClass
import qualified Web.Scim.Class.User as ScimClass
import qualified Web.Scim.Client as ScimClient
import qualified Web.Scim.Schema.ListResponse as Scim
import qualified Web.Scim.Schema.Schema as Scim
import qualified Web.Scim.Schema.User as Scim
import qualified Web.Scim.Schema.User.Email as Scim
import qualified Web.Scim.Server as ScimServer
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
  | WrongNumberOfAttrValues Text String Int
  | CouldNotParseEmail Text String
  deriving stock (Eq, Show)

newtype FieldMapping
  = FieldMapping
      ( [Text] ->
        Either
          MappingError
          ( Scim.User ScimServer.Mock ->
            Scim.User ScimServer.Mock
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
          [val] -> Right $ \usr -> usr {Scim.userName = val}
          bad -> Left $ WrongNumberOfAttrValues ldapFieldName "1" (Prelude.length bad)

      mapExternalId ldapFieldName = FieldMapping $
        \case
          [val] -> Right $ \usr -> usr {Scim.externalId = Just val}
          bad -> Left $ WrongNumberOfAttrValues ldapFieldName "1" (Prelude.length bad)

      mapEmail ldapFieldName = FieldMapping $
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

type User = Scim.User ScimServer.Mock

type StoredUser = ScimClass.StoredUser ScimServer.Mock

-- | the 'undefined' is ok, the mapping is guaranteed to contain a filler for this, or the
-- mapping parser would have failed.
emptyScimUser :: User
emptyScimUser =
  Scim.empty schemas (error "undefined") Scim.NoUserExtra
  where
    schemas = [Scim.User20]

ldapToScim ::
  forall scim.
  scim ~ Either [MappingError] User =>
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

----------------------------------------------------------------------

main :: IO ()
main = do
  myconf :: SearchConf <- ByteString.readFile "./sample-conf.yaml" >>= either (error . show) pure . Yaml.decodeEither'
  searchLdapUser myconf "john" >>= print
  listLdapUsers myconf >>= print
  ldaps :: [SearchEntry] <- either (error . show) pure =<< listLdapUsers myconf
  let mbScims :: [Either [MappingError] User]
      mbScims = ldapToScim myconf <$> ldaps
  scims :: [User] <- mapM (either (error . show) pure) mbScims
  LByteString.putStrLn $ Aeson.encodePretty scims
  error "and now send it all to wire!  clients are ready!"
