{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans -Wno-missing-export-lists #-}

module LdapScimBridge.Scim where

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

type User = Scim.User ScimTag

type StoredUser = ScimClass.StoredUser ScimTag

ldapToScim ::
  forall m.
  m ~ Either [(SearchEntry, MappingError)] =>
  BridgeConf ->
  SearchEntry ->
  m (SearchEntry, User)
ldapToScim conf entry@(SearchEntry _ attrs) = (entry,) <$> Foldable.foldl' go (Right emptyScimUser) attrs
  where
    codec = case ldapCodec (ldapSource conf) of
      Utf8 -> Text.decodeUtf8
      Latin1 -> Text.decodeLatin1

    go :: m User -> (Attr, [AttrValue]) -> m User
    go scimval (Attr key, vals) = case Map.lookup key (fromMapping $ mapping conf) of
      Nothing -> scimval
      Just fieldMappings -> foldl' (go' vals) scimval fieldMappings

    go' :: [ByteString] -> m User -> FieldMapping -> m User
    go' vals scimval (FieldMapping _ (Mapper f)) = case (scimval, f (codec <$> vals)) of
      (Right scimusr, Right f') -> Right (f' scimusr)
      (Right _, Left err) -> Left [(entry, err)]
      (Left errs, Right _) -> Left errs
      (Left errs, Left err) -> Left ((entry, err) : errs)

-- | the 'undefined' is ok, the mapping is guaranteed to contain a filler for this, or the
-- mapping parser would have failed.
emptyScimUser :: User
emptyScimUser =
  Scim.empty scimSchemas (error "undefined") Scim.NoUserExtra

scimSchemas :: [Scim.Schema]
scimSchemas = [Scim.User20]

connectScim :: Logger -> ScimConf -> IO ClientEnv
connectScim lgr conf = (`catch` logErrors) $ do
  let settings =
        if scimTls conf
          then HTTP.tlsManagerSettings
          else HTTP.defaultManagerSettings
  manager <- HTTP.newManager settings
  let base = BaseUrl Http (scimHost conf) (scimPort conf) (scimPath conf)
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
      mapM (either (throwIO . ErrorCall . show) pure) (ldapToScim conf <$> ldapKeepees)
    lgr Debug $ "Pulled the following ldap users for post/put:\n" <> show (fst <$> scims)
    lgr Debug . cs $ "Translated to scim:\n" <> Aeson.encodePretty (snd <$> scims)
    updateScimPeerPostPut lgr clientEnv tok (snd <$> scims)
    lgr Info "[post/put: done]"
  do
    -- delete
    lgr Info "[delete: started]"
    let ldapDeleteesAttr = filter (isDeletee (ldapSource conf)) ldaps
    ldapDeleteesDirectory :: [SearchEntry] <- case (ldapDeleteFromDirectory (ldapSource conf)) of
      Just (searchConf :: LdapSearch) ->
        either (throwIO . ErrorCall . show) pure =<< listLdapUsers (ldapSource conf) searchConf
      Nothing ->
        pure mempty

    scims :: [(SearchEntry, User)] <-
      mapM (either (throwIO . ErrorCall . show) pure) (ldapToScim conf <$> (ldapDeleteesAttr <> ldapDeleteesDirectory))
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
