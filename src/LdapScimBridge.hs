{-# LANGUAGE OverloadedStrings #-}

module LdapScimBridge where

import Control.Exception (ErrorCall (ErrorCall), catch, throwIO)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encode.Pretty as Aeson
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Char8 as ByteString
import qualified Data.Foldable as Foldable
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
import Prelude

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
    fdeleteOnAttribute :: Maybe LdapFilterAttr <- obj Aeson..:? "deleteOnAttribute"
    fdeleteFromDirectory :: Maybe LdapSearch <- obj Aeson..:? "deleteFromDirectory"

    let vhost :: Host
        vhost = if ftls then Ldap.Tls fhost Ldap.defaultTlsSettings else Ldap.Plain fhost

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
      let go :: (KM.Key, Yaml.Value) -> Yaml.Parser LdapFilterAttr
          go (key, val) = do
            str <- Aeson.withText "val" pure val
            pure $ LdapFilterAttr (K.toText key) str
      go `mapM` KM.toList (KM.filterWithKey (\k _ -> k `notElem` ["base", "objectClass"]) obj)
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
    logLevel :: PhantomParent Level
  }
  deriving stock (Show, Generic)

-- | Work around orphan instances.  Might not be a phantom, but I like the name.  :)
newtype PhantomParent a = PhantomParent {unPhantomParent :: a}
  deriving stock (Eq, Ord, Bounded, Show, Generic)

instance Aeson.FromJSON (PhantomParent Level) where
  parseJSON =
    fmap PhantomParent . \case
      "Trace" -> pure Trace
      "Debug" -> pure Debug
      "Info" -> pure Info
      "Warn" -> pure Warn
      "Error" -> pure Error
      "Fatal" -> pure Fatal
      bad -> fail $ "unknown Level: " <> show bad

instance Aeson.FromJSON BridgeConf

data MappingError
  = MissingAttr Text
  | MissingMandatoryValue Text
  | WrongNumberOfAttrValues Text String Int
  | CouldNotParseEmail Text String
  deriving stock (Eq, Show)

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

instance Show FieldMapping where
  show = show . fieldMappingLabel

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

-- | Map attribute keys to functions from attribute values to changes to scim records.  We'll
-- start off with an empty scim record, and change it based on attributes we find that are
-- listed in the mapping.  Mappigns can fail, eg. if there is more than one attribute value
-- for the attribute mapping to externalId.
newtype Mapping = Mapping {fromMapping :: Map Text [FieldMapping]}
  deriving stock (Show)

instance Aeson.FromJSON Mapping where
  parseJSON = Aeson.withObject "Mapping" $ \obj -> do
    mfdisplayName <- obj Aeson..:? "displayName"
    fuserName <- obj Aeson..: "userName"
    fexternalId <- obj Aeson..: "externalId"
    mfemail <- obj Aeson..:? "email"
    mfrole <- obj Aeson..:? "roles"

    let listToMap :: [(Text, a)] -> Map Text [a]
        listToMap = foldl' go mempty
          where
            go mp (k, b) = Map.alter (Just . maybe [b] (b :)) k mp

    pure . Mapping . listToMap . catMaybes $
      [ (\fdisplayName -> (fdisplayName, mapDisplayName fdisplayName "displayName")) <$> mfdisplayName,
        Just (fuserName, mapUserName fuserName "userName"),
        Just (fexternalId, mapExternalId fexternalId "externalId"),
        (\femail -> (femail, mapEmail femail "email")) <$> mfemail,
        (\frole -> (frole, mapRole frole "roles")) <$> mfrole
      ]
    where
      -- The name that shows for this user in wire.
      mapDisplayName :: Text -> Text -> FieldMapping
      mapDisplayName ldapFieldName scimFieldName = FieldMapping scimFieldName $
        \case
          [val] -> Right $ \usr -> usr {Scim.displayName = Just val}
          bad -> Left $ WrongNumberOfAttrValues (ldapFieldName <> " -> " <> scimFieldName) "1" (Prelude.length bad)

      -- Wire user handle (the one with the '@').
      mapUserName :: Text -> Text -> FieldMapping
      mapUserName ldapFieldName scimFieldName = FieldMapping scimFieldName $
        \case
          [val] -> Right $ \usr -> usr {Scim.userName = val}
          bad -> Left $ WrongNumberOfAttrValues (ldapFieldName <> " -> " <> scimFieldName) "1" (Prelude.length bad)

      mapExternalId :: Text -> Text -> FieldMapping
      mapExternalId ldapFieldName scimFieldName = FieldMapping scimFieldName $
        \case
          [val] -> Right $ \usr -> usr {Scim.externalId = Just val}
          bad -> Left $ WrongNumberOfAttrValues (ldapFieldName <> " -> " <> scimFieldName) "1" (Prelude.length bad)

      mapEmail :: Text -> Text -> FieldMapping
      mapEmail ldapFieldName scimFieldName = FieldMapping scimFieldName $
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
                (ldapFieldName <> " -> " <> scimFieldName)
                "<=1 (with more than one email, which one should be primary?)"
                (Prelude.length bad)

      mapRole :: Text -> Text -> FieldMapping
      mapRole ldapFieldName scimFieldName = FieldMapping scimFieldName $
        \case
          [] -> Right id
          [val] -> Right $ \usr -> usr {Scim.roles = [val]}
          bad -> Left $ WrongNumberOfAttrValues (ldapFieldName <> " -> " <> scimFieldName) "1" (Prelude.length bad)

type LdapResult a = IO (Either LdapError a)

ldapObjectClassFilter :: Text -> Filter
ldapObjectClassFilter = (Attr "objectClass" :=) . cs

ldapFilterAttrToFilter :: LdapFilterAttr -> Filter
ldapFilterAttrToFilter (LdapFilterAttr key val) = Attr key := cs val

listLdapUsers :: LdapConf -> LdapSearch -> LdapResult [SearchEntry]
listLdapUsers conf searchConf = Ldap.with (ldapHost conf) (ldapPort conf) $ \l -> do
  Ldap.bind l (ldapDn conf) (ldapPassword conf)
  let fltr :: Filter =
        And
          ( ldapObjectClassFilter (ldapSearchObjectClass searchConf)
              :| (ldapFilterAttrToFilter <$> ldapSearchExtra searchConf)
          )
  Ldap.search l (ldapSearchBase searchConf) mempty fltr mempty

type User = Scim.User ScimTag

type StoredUser = ScimClass.StoredUser ScimTag

-- | Note that the `userName` field is mandatory in SCIM, but we gloss over this by setting it
-- to an empty Text here.  See 'RequireUserName', 'ldapToScim' if you wonder whether this is a
-- good idea.
emptyScimUser :: User
emptyScimUser =
  Scim.empty scimSchemas "" Scim.NoUserExtra

scimSchemas :: [Scim.Schema]
scimSchemas = [Scim.User20]

data RequireUserName = Lenient | Strict
  deriving stock (Eq, Show)

-- | Translate an LDAP record into a SCIM record.  If username is not provided in the LDAP
-- record, behavior is defined by the first argument: if `Lenient`, just fill in an empty
-- Text; if `Strict`, throw an error.
ldapToScim ::
  forall m.
  (m ~ Either [(SearchEntry, MappingError)]) =>
  RequireUserName ->
  BridgeConf ->
  SearchEntry ->
  m (SearchEntry, User)
ldapToScim reqUserName conf entry@(SearchEntry _ attrs) = do
  guardUserName
  (entry,) <$> Foldable.foldl' go (Right emptyScimUser) attrs
  where
    guardUserName =
      if reqUserName == Strict && Attr "userName" `notElem` (fst <$> toList attrs)
        then Left [(entry, MissingMandatoryValue "userName")]
        else Right ()

    codec = case ldapCodec (ldapSource conf) of
      Utf8 -> Text.decodeUtf8
      Latin1 -> Text.decodeLatin1

    go :: m User -> (Attr, [AttrValue]) -> m User
    go scimval (Attr key, vals) = case Map.lookup key (fromMapping $ mapping conf) of
      Nothing -> scimval
      Just fieldMappings -> foldl' (go' vals) scimval fieldMappings

    go' :: [ByteString] -> m User -> FieldMapping -> m User
    go' vals scimval (FieldMapping _ f) = case (scimval, f (codec <$> vals)) of
      (Right scimusr, Right f') -> Right (f' scimusr)
      (Right _, Left err) -> Left [(entry, err)]
      (Left errs, Right _) -> Left errs
      (Left errs, Left err) -> Left ((entry, err) : errs)

connectScim :: Logger -> ScimConf -> IO ClientEnv
connectScim lgr conf = (`catch` logErrors) $ do
  let settings =
        if scimTls conf
          then HTTP.tlsManagerSettings
          else HTTP.defaultManagerSettings
      schema =
        if scimTls conf
          then Https
          else Http
  manager <- HTTP.newManager settings
  let base = BaseUrl schema (scimHost conf) (scimPort conf) (scimPath conf)
  pure $ mkClientEnv manager base
  where
    logErrors (SomeException e) = do
      lgr Error $ "could not connect to scim peer: " <> show e
      throwIO e

isDeletee :: LdapConf -> SearchEntry -> Bool
isDeletee conf = case ldapDeleteOnAttribute conf of
  Nothing -> const False
  Just (LdapFilterAttr key value) ->
    \(SearchEntry _ attrs) ->
      maybe False (cs value `elem`) (Data.List.lookup (Attr key) attrs)

updateScimPeer :: Logger -> BridgeConf -> IO ()
updateScimPeer lgr conf = do
  clientEnv <- connectScim lgr (scimTarget conf)
  let tok = Just . scimToken . scimTarget $ conf
  ldaps :: [SearchEntry] <-
    either (throwIO . ErrorCall . show) pure =<< listLdapUsers (ldapSource conf) (ldapSearch (ldapSource conf))
  do
    -- put, post
    lgr Info "[post/put: started]"
    let ldapKeepees = filter (not . isDeletee (ldapSource conf)) ldaps
    scims :: [(SearchEntry, User)] <-
      mapM (either (throwIO . ErrorCall . show) pure) (ldapToScim Strict conf <$> ldapKeepees)
    lgr Debug $ "Pulled the following ldap users for post/put:\n" <> show (fst <$> scims)
    lgr Debug . cs $ "Translated to scim:\n" <> Aeson.encodePretty (snd <$> scims)
    updateScimPeerPostPut lgr clientEnv tok (snd <$> scims)
    lgr Info "[post/put: done]"
  do
    -- delete
    lgr Info "[delete: started]"
    let ldapDeleteesAttr = filter (isDeletee (ldapSource conf)) ldaps
    ldapDeleteesDirectory :: [SearchEntry] <- case ldapDeleteFromDirectory (ldapSource conf) of
      Just (searchConf :: LdapSearch) ->
        either (throwIO . ErrorCall . show) pure =<< listLdapUsers (ldapSource conf) searchConf
      Nothing ->
        pure mempty

    scims :: [(SearchEntry, User)] <-
      mapM (either (throwIO . ErrorCall . show) pure) (ldapToScim Lenient conf <$> (ldapDeleteesAttr <> ldapDeleteesDirectory))
    lgr Debug $ "Pulled the following ldap users for delete:\n" <> show (fst <$> scims)
    lgr Debug . cs $ "Translated to scim:\n" <> Aeson.encodePretty (snd <$> scims)
    updateScimPeerDelete lgr clientEnv tok (snd <$> scims)
    lgr Info "[delete: done]"

lookupScimByExternalId :: ClientEnv -> Maybe Text -> Scim.User tag -> IO (Maybe StoredUser)
lookupScimByExternalId clientEnv tok scim = do
  eid <- maybe (error "impossible") pure $ Scim.externalId scim
  let fltr = Just $ filterBy "externalId" eid
  mbold :: [StoredUser] <-
    ScimClient.getUsers @ScimTag clientEnv tok fltr
      <&> Scim.resources
  case mbold of
    [old] -> pure $ Just old
    [] -> pure Nothing
    (_ : _ : _) -> error "impossible" -- externalId must be unique in the scope of the scim auth token.
  where
    filterBy :: Text -> Text -> ScimFilter.Filter
    filterBy name value =
      ScimFilter.FilterAttrCompare
        (ScimFilter.topLevelAttrPath name)
        ScimFilter.OpEq
        (ScimFilter.ValString value)

updateScimPeerPostPut ::
  Logger ->
  ClientEnv ->
  Maybe (AuthData ScimTag) ->
  [User] ->
  IO ()
updateScimPeerPostPut lgr clientEnv tok = mapM_ $ \scim -> do
  case Scim.externalId scim of
    Nothing -> lgr Error $ "scim user without 'externalId' field: " <> show scim
    Just eid -> updateScimPeerPostPutStep lgr clientEnv tok scim eid

updateScimPeerPostPutStep ::
  Logger ->
  ClientEnv ->
  Maybe Text ->
  Scim.User ScimTag ->
  Text ->
  IO ()
updateScimPeerPostPutStep lgr clientEnv tok scim eid = do
  lookupScimByExternalId clientEnv tok scim >>= \case
    Just old ->
      if ScimCommon.value (Scim.thing old) == scim
        then do
          lgr Info $ "unchanged: " <> show eid
        else do
          lgr Info $ "update: " <> show eid
          process $ ScimClient.putUser @ScimTag clientEnv tok (ScimCommon.id (Scim.thing old)) scim
    Nothing -> do
      lgr Info $ "new user: " <> show eid
      process $ ScimClient.postUser clientEnv tok scim
  where
    process :: IO StoredUser -> IO ()
    process action = do
      result :: Either SomeException StoredUser <-
        (Right <$> action) `catch` (pure . Left)
      result
        & either
          (lgr Error . show)
          (\new -> lgr Debug $ "UserId: " <> (show . ScimCommon.id . Scim.thing $ new))

updateScimPeerDelete ::
  Logger ->
  ClientEnv ->
  Maybe (AuthData ScimTag) ->
  [User] ->
  IO ()
updateScimPeerDelete lgr clientEnv tok = mapM_ $ \scim -> do
  lookupScimByExternalId clientEnv tok scim >>= \case
    Just old -> do
      process (ScimClient.deleteUser @ScimTag clientEnv tok (ScimCommon.id (Scim.thing old)))
        `catch` \e@(SomeException _) -> lgr Error $ show e
    Nothing -> do
      pure ()
  where
    process :: IO NoContent -> IO ()
    process action = do
      result :: Either SomeException NoContent <-
        (Right <$> action) `catch` (pure . Left)
      result
        & either
          (lgr Error . show)
          (const $ pure ())

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
  lgr :: Log.Logger <-
    Log.defSettings
      & Log.setLogLevel lvl
      & Log.new
  pure $ \msgLvl msgContent -> do
    Log.log lgr msgLvl (Log.msg @Text $ show msgContent)
    Log.flush lgr

main :: IO ()
main = do
  myconf :: BridgeConf <- parseCli
  lgr :: Logger <- mkLogger (unPhantomParent $ logLevel myconf)
  lgr Debug $ show (mapping myconf)
  updateScimPeer lgr myconf `catch` logErrors lgr
  where
    logErrors :: Logger -> SomeException -> IO a
    logErrors lgr (SomeException e) = do
      lgr Fatal $ "uncaught exception: " <> show e
      throwIO e
