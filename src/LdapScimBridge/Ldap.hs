{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans -Wno-missing-export-lists #-}

module LdapScimBridge.Ldap where

import Data.String.Conversions (cs)
import Ldap.Client as Ldap
import LdapScimBridge.Config

type LdapResult a = IO (Either LdapError a)

ldapObjectClassFilter :: Text -> Filter -- TODO: inline?
ldapObjectClassFilter = (Attr "objectClass" :=) . cs

ldapFilterAttrToFilter :: LdapFilterAttr -> Filter -- TODO: inline?  replace LdapFilterAttr with `Attr` and `:=`?
ldapFilterAttrToFilter (LdapFilterAttr key val) = Attr key := (cs val)

listLdapUsers :: LdapConf -> LdapSearch -> LdapResult [SearchEntry]
listLdapUsers conf searchConf = Ldap.with (ldapHost conf) (ldapPort conf) $ \l -> do
  Ldap.bind l (ldapDn conf) (ldapPassword conf)
  let fltr :: Filter =
        And
          ( ldapObjectClassFilter (ldapSearchObjectClass searchConf)
              :| (ldapFilterAttrToFilter <$> ldapSearchExtra searchConf)
          )
  Ldap.search l (ldapSearchBase searchConf) mempty fltr mempty
