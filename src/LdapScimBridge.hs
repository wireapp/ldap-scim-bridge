{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans -Wno-missing-export-lists #-}

module LdapScimBridge where

import Control.Exception (ErrorCall (ErrorCall), catch, throwIO)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encode.Pretty as Aeson
import qualified Data.ByteString.Char8 as ByteString
import qualified Data.Foldable as Foldable
import qualified Data.Map as Map
import Data.String.Conversions (cs)
import qualified Data.String.Conversions as SC
import qualified Data.Text.Encoding as Text
import qualified Data.Yaml as Yaml
import Ldap.Client as Ldap
import qualified Network.HTTP.Client as HTTP
import Servant.Client (BaseUrl (..), ClientEnv (..), Scheme (..), mkClientEnv)
import System.Environment (getProgName)
import System.Logger (Level (..))
import qualified System.Logger as Log
import qualified Text.Email.Validate
import qualified Web.Scim.Class.User as ScimClass
import qualified Web.Scim.Client as ScimClient
import qualified Web.Scim.Filter as ScimFilter
import qualified Web.Scim.Schema.Common as ScimCommon
import qualified Web.Scim.Schema.ListResponse as Scim
import qualified Web.Scim.Schema.Meta as Scim
import qualified Web.Scim.Schema.Schema as Scim
import qualified Web.Scim.Schema.User as Scim
import qualified Web.Scim.Schema.User.Email as Scim
import qualified Web.Scim.Server.Mock as ScimServer

data LdapConf = LdapConf
  { -- | eg. @Ldap.Tls (host conf) Ldap.defaultTlsSettings@
    ldapHost :: Host,
    -- | usually 389 for plaintext or 636 for TLS.
    ldapPort :: PortNumber,
    -- | `$ slapcat | grep ^modifiersName`, eg. @Dn "cn=admin,dc=nodomain"@.
    ldapDn :: Dn,
    ldapPassword :: Password,
    -- | `$ slapcat | grep ^dn`, eg. @Dn "dc=nodomain"@.
    ldapBase :: Dn,
    -- | eg. @Attr "objectClass" := "account"@.
    ldapFltr :: Filter,
    -- | anything from "Data.Text.Encoding".
    ldapCodec :: ByteString -> Text
  }

instance Aeson.FromJSON LdapConf where
  parseJSON = Aeson.withObject "LdapConf" $ \obj -> do
    ftls :: Bool <- obj Aeson..: "tls"
    fhost :: String <- obj Aeson..: "host"
    fport :: Int <- obj Aeson..: "port"
    fdn :: Text <- obj Aeson..: "dn"
    fpassword :: String <- obj Aeson..: "password"
    fbase :: Text <- obj Aeson..: "base"
    fobjectClass :: String <- obj Aeson..: "objectClass"
    fcodec :: Text <- obj Aeson..: "codec"

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
      LdapConf
        { ldapHost = vhost,
          ldapPort = vport,
          ldapDn = Dn fdn,
          ldapPassword = Password $ ByteString.pack fpassword,
          ldapBase = Dn fbase,
          ldapFltr = Attr "objectClass" := ByteString.pack fobjectClass,
          ldapCodec = vcodec
        }

data ScimConf = ScimConf
  { scimTls :: Bool,
    scimHost :: String,
    scimPort :: Int,
    scimPath :: String,
    scimToken :: Text
  }
  deriving stock (Generic)

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
  deriving stock (Generic)

instance Aeson.FromJSON Level where
  parseJSON "Trace" = pure Trace
  parseJSON "Debug" = pure Debug
  parseJSON "Info" = pure Info
  parseJSON "Warn" = pure Warn
  parseJSON "Error" = pure Error
  parseJSON "Fatal" = pure Fatal
  parseJSON bad = fail $ "unknown Level: " <> show bad

instance Aeson.FromJSON BridgeConf

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

searchLdapUser :: LdapConf -> Text -> LdapResult [SearchEntry]
searchLdapUser conf uid = Ldap.with (ldapHost conf) (ldapPort conf) $ \l -> do
  Ldap.bind l (ldapDn conf) (ldapPassword conf)
  Ldap.search
    l
    (ldapBase conf)
    (typesOnly True)
    (And (ldapFltr conf :| [Attr "uid" := Text.encodeUtf8 uid]))
    []

listLdapUsers :: LdapConf -> LdapResult [SearchEntry]
listLdapUsers conf = Ldap.with (ldapHost conf) (ldapPort conf) $ \l -> do
  Ldap.bind l (ldapDn conf) (ldapPassword conf)
  Ldap.search l (ldapBase conf) mempty (ldapFltr conf) mempty

type User = Scim.User ScimServer.Mock

type StoredUser = ScimClass.StoredUser ScimServer.Mock

-- | the 'undefined' is ok, the mapping is guaranteed to contain a filler for this, or the
-- mapping parser would have failed.
emptyScimUser :: User
emptyScimUser =
  Scim.empty scimSchemas (error "undefined") Scim.NoUserExtra

scimSchemas :: [Scim.Schema]
scimSchemas = [Scim.User20]

ldapToScim ::
  forall scim.
  scim ~ Either [(SearchEntry, MappingError)] User =>
  BridgeConf ->
  SearchEntry ->
  scim
ldapToScim conf entry@(SearchEntry _ attrs) = Foldable.foldl' go (Right emptyScimUser) attrs
  where
    go :: scim -> (Attr, [AttrValue]) -> scim
    go scimval (Attr key, vals) = case Map.lookup key (fromMapping $ mapping conf) of
      Nothing -> scimval
      Just (FieldMapping f) -> case (scimval, f (ldapCodec (ldapSource conf) <$> vals)) of
        (Right scimusr, Right f') -> Right (f' scimusr)
        (Right _, Left err) -> Left [(entry, err)]
        (Left errs, Right _) -> Left errs
        (Left errs, Left err) -> Left ((entry, err) : errs)

connectScim :: ScimConf -> IO ClientEnv
connectScim conf = do
  -- honor TLS settings
  manager <- HTTP.newManager HTTP.defaultManagerSettings
  let base = BaseUrl Http (scimHost conf) (scimPort conf) (scimPath conf)
  pure $ mkClientEnv manager base

updateScimPeer :: Logger -> BridgeConf -> [User] -> IO ()
updateScimPeer lgr conf scims = do
  -- TODO: delete deletees.
  clientEnv <- connectScim (scimTarget conf)
  let tok = Just . scimToken . scimTarget $ conf
  forM_ scims $ \scim -> do
    eid <- maybe (error "impossible") pure $ Scim.externalId scim
    let fltr = Just $ filterBy "externalId" eid
    mbold :: Maybe StoredUser <-
      ScimClient.getUsers @ScimServer.Mock clientEnv tok fltr
        <&> Scim.resources
        <&> listToMaybe
    case mbold of
      Just old ->
        if ScimCommon.value (Scim.thing old) == scim
          then do
            lgr Info $ "unchanged: " <> show (Scim.externalId scim)
          else do
            lgr Info $ "update: " <> show (Scim.externalId scim)
            void (ScimClient.postUser clientEnv tok `mapM` scims) `catch` \e@(SomeException _) -> do
              lgr Warn $ show e
      Nothing -> do
        lgr Info $ "new user: " <> show (Scim.externalId scim)
        void (ScimClient.postUser clientEnv tok `mapM` scims) `catch` \e@(SomeException _) -> do
          lgr Warn $ show e
  where
    filterBy :: Text -> Text -> ScimFilter.Filter
    filterBy name value =
      ScimFilter.FilterAttrCompare
        (ScimFilter.topLevelAttrPath name)
        ScimFilter.OpEq
        (ScimFilter.ValString value)

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

type Logger = Level -> Text -> IO ()

mkLogger :: Level -> IO Logger
mkLogger lvl = do
  lgr :: Log.Logger <- Log.new (Log.setLogLevel lvl Log.defSettings)
  pure $ \msgLvl msgContent -> do
    Log.log lgr msgLvl (Log.msg @Text $ show msgContent)
    Log.flush lgr

main :: IO ()
main = do
  myconf :: BridgeConf <-
    parseCli
  lgr :: Logger <-
    mkLogger (logLevel myconf)
  ldaps :: [SearchEntry] <-
    either (throwIO . ErrorCall . show) pure =<< listLdapUsers (ldapSource myconf)
  scims :: [User] <-
    mapM (either (throwIO . ErrorCall . show) pure) (ldapToScim myconf <$> ldaps)
  lgr Debug . cs $ "Pulled the following scim users:\n" <> Aeson.encodePretty scims
  updateScimPeer lgr myconf scims
