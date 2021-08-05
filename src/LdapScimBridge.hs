{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module LdapScimBridge where

import Control.Exception (bracket_)
import Control.Monad (when)
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import qualified Data.Foldable as Foldable
import Data.Function (fix)
import Data.List.NonEmpty
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text.Encoding as Text
import qualified Data.Text.IO as Text
import Ldap.Client as Ldap
import qualified Ldap.Client.Bind as Ldap
import System.Exit (die)
import qualified System.IO as IO
import qualified Web.Scim.Schema.User as ScimSchema
import qualified Web.Scim.Server.Mock as ScimServer

data SearchConf = Conf
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

myconf :: SearchConf
myconf =
  Conf
    { host = Ldap.Plain "localhost",
      port = 389,
      dn = Dn "cn=admin,dc=nodomain",
      password = Password "geheim hoch drei",
      base = Dn "ou=People,dc=nodomain",
      fltr = Attr "objectClass" := "account",
      codec = Text.decodeUtf8,
      mapping = mymapping
    }
  where
    mymapping :: Mapping
    mymapping =
      Mapping . Map.fromList $
        [ ( "uidNumber",
            FieldMapping $
              \case
                [val] -> Right $ \usr -> usr {ScimSchema.userName = val}
                bad -> Left $ WrongNumberOfAttrValues "uidNumber" 1 (Prelude.length bad)
          ),
          ( "uid",
            FieldMapping $
              \case
                [val] -> Right $ \usr -> usr {ScimSchema.externalId = Just val}
                bad -> Left $ WrongNumberOfAttrValues "uid" 1 (Prelude.length bad)
          )
        ]

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
- make mapping entries mandatory (especially `ScimSchema.userName`)
- find out what AD usually provides and make sure we can express that in a yaml file

-}

----------------------------------------------------------------------

main :: IO ()
main = do
  searchLdapUser myconf "john" >>= print
  listLdapUsers myconf >>= print
  ldaps :: [SearchEntry] <- either (error . show) pure =<< listLdapUsers myconf
  let scims :: [Either [MappingError] (ScimSchema.User ScimServer.Mock)]
      scims = ldapToScim myconf <$> ldaps
  print scims
