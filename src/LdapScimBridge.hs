{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module LdapScimBridge where

import Control.Exception (bracket_)
import Control.Monad (when)
import Data.Function (fix)
import Data.List.NonEmpty
import Data.Text (Text)
import qualified Data.Text.Encoding as Text
import qualified Data.Text.IO as Text
import Ldap.Client as Ldap
import qualified Ldap.Client.Bind as Ldap
import System.Exit (die)
import qualified System.IO as IO

type LdapResult a = IO (Either LdapError a)

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
    fltr :: Filter
  }

myconf :: SearchConf
myconf =
  Conf
    { host = Ldap.Plain "localhost",
      port = 389,
      dn = Dn "cn=admin,dc=nodomain",
      password = Password "geheim hoch drei",
      base = Dn "ou=People,dc=nodomain",
      fltr = Attr "objectClass" := "account"
    }

main :: IO ()
main = do
  searchLdapUser myconf "john" >>= print
  listLdapUsers myconf >>= print

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
