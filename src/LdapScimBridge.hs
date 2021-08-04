{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module LdapScimBridge
  ( someFunc,
  )
where

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

data Conf = Conf
  { host :: String,
    port :: Int,
    dn :: Dn,
    password :: Password,
    base :: Dn
  }
  deriving (Show, Eq)

conf :: Conf
conf = Conf "localhost" 389 (Dn "") (Password "...") (Dn "")

-- apt-get install ldapscripts ldap-utils slapd

someFunc :: IO (Either LdapError ())
someFunc = do
  Ldap.with (Ldap.Tls (host conf) Ldap.defaultTlsSettings) (fromIntegral (port conf)) $ \l -> do
    Ldap.bind l (dn conf) (password conf)
    let uid = "fisx"
    us <-
      Ldap.search
        l
        (base conf)
        (typesOnly True)
        (And ((Attr "objectClass" := "Person") :| [Attr "uid" := Text.encodeUtf8 uid]))
        []
    print us
