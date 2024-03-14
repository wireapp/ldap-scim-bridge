{-# LANGUAGE OverloadedStrings #-}

import Data.ByteString (ByteString)
import Data.String.Conversions (cs)
import Data.Text
import qualified Data.Yaml as Yaml
import Ldap.Client as Ldap
import LdapScimBridge hiding (main)
import Test.Hspec
import Text.Email.Parser (unsafeEmailAddress)
import Web.Scim.Schema.Meta as Scim
import Web.Scim.Schema.Schema as Scim
import Web.Scim.Schema.User as Scim
import Web.Scim.Schema.User.Email as Scim

main :: IO ()
main = hspec $ do
  describe "LdapScimBridge" $ do
    it "map displayName userName externalId and email" $ do
      testMapping "George" "george" "george@nodomain" "george@nodomain"

testMapping :: Text -> Text -> Text -> Text -> IO ()
testMapping displayName userName externalId email = do
  let [local, domain] = splitOn "@" email
  conf <- Yaml.decodeThrow confYaml
  let searchEntry =
        SearchEntry
          (Dn "123")
          [ (Attr "displayName", [cs displayName]),
            (Attr "uidNumber", [cs userName]),
            (Attr "email", [cs email])
          ]
  let expectedScimUser =
        Scim.User
          { schemas = [User20],
            userName = userName,
            externalId = Just externalId,
            name = Nothing,
            displayName = Just displayName,
            nickName = Nothing,
            profileUrl = Nothing,
            title = Nothing,
            userType = Nothing,
            preferredLanguage = Nothing,
            locale = Nothing,
            active = Nothing,
            password = Nothing,
            emails = [Email {typ = Nothing, Scim.value = EmailAddress2 {unEmailAddress = unsafeEmailAddress (cs local) (cs domain)}, primary = Nothing}],
            phoneNumbers = [],
            ims = [],
            photos = [],
            addresses = [],
            entitlements = [],
            roles = [],
            x509Certificates = [],
            extra = NoUserExtra
          }
  let Right (actualSearchEntry, actualScimUser) = ldapToScim conf searchEntry
  actualSearchEntry `shouldBe` searchEntry
  actualScimUser `shouldBe` expectedScimUser

confYaml :: ByteString
confYaml =
  "logLevel: \"Debug\"  # one of Trace,Debug,Info,Warn,Error,Fatal; `Fatal` is least noisy, `Trace` most.\n\
  \ldapSource:\n\
  \  tls: false\n\
  \  host: \"localhost\"\n\
  \  port: 389\n\
  \  dn: \"cn=admin,dc=nodomain\"\n\
  \  password: \"geheim\"\n\
  \  search:\n\
  \    base: \"ou=People,dc=nodomain\"\n\
  \    objectClass: \"account\"\n\
  \  codec: \"utf8\"\n\
  \  deleteOnAttribute:  # optional, related to `delete-from-directory`.\n\
  \    key: \"deleted\"\n\
  \    value: \"true\"\n\
  \  deleteFromDirectory:  # optional; ok to use together with `delete-on-attribute` if you use both.\n\
  \    base: \"ou=DeletedPeople,dc=nodomain\"\n\
  \    objectClass: \"account\"\n\
  \scimTarget:\n\
  \  tls: false\n\
  \  host: \"localhost\"\n\
  \  port: 8088\n\
  \  path: \"/scim/v2\"\n\
  \  token: \"Bearer RRhtCL/VF9IYcmb3E9zaDo3rP6w3mZ3Ww3da7d2RDR8=\"\n\
  \mapping:\n\
  \  displayName: \"displayName\"\n\
  \  userName: \"uidNumber\"\n\
  \  externalId: \"email\"\n\
  \  email: \"email\""
