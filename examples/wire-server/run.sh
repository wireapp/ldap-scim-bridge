#!/bin/bash

# put this to work with https://github.com/wireapp/wire-server
#
# if you are not familiar with / interested in the wire app, but want
# to bridge ldap and scim in some other context, this file should
# still serve you as a proof of concept, just ignore the parts where
# we're talking to wire.
#
# if you have managed to mutate this file into something that works
# for a different use case, or even if you run out of steam trying,
# we'd appreciate a PR or at least an issue with a code dump.
#
# WARNING: this test is hard to run, and it does not worry about
# polluting your system state.  are cleaner, but more involved
# approach can be found in
# https://github.com/wireapp/wire-server/pull/1709.

set -x
set -o pipefail
set -o errexit
cd "$( dirname "${BASH_SOURCE[0]}" )"/../../

export BRIDGE_CONF1=./examples/wire-server/conf1.yaml
export BRIDGE_CONF2=./examples/wire-server/conf2.yaml
export WIRE_USERID
export WIRE_TEAMID
export SCIM_TOKEN
export SCIM_TOKEN_ID
export SCIM_TOKEN_FULL
export WIRE_SERVER_PATH=~/src/wire-server
export SPAR_URL=http://localhost:8088
export BRIG_URL=http://localhost:8088
export GALLEY_URL=http://localhost:8085

function install() {
  sudo apt-get install ldapscripts ldap-utils slapd
}

function clear() {
  # TODO: sometimes we want to know if this fails, but only if it's
  # other errors than "already exists".  or maybe we can test for
  # existence before we attemp to delete, and then fail on all
  # remaining errors?
  sudo ldapdelete -D "cn=admin,dc=nodomain" -w geheim -H ldapi:/// "cn=notcreated,ou=NoPeople,dc=nodomain" || true
  sudo ldapdelete -D "cn=admin,dc=nodomain" -w geheim -H ldapi:/// "cn=notcreated,ou=DeletedPeople,dc=nodomain" || true
  sudo ldapdelete -D "cn=admin,dc=nodomain" -w geheim -H ldapi:/// "cn=uses123email,ou=People,dc=nodomain" || true
  sudo ldapdelete -D "cn=admin,dc=nodomain" -w geheim -H ldapi:/// "cn=uses123email,ou=DeletedPeople,dc=nodomain" || true
  sudo ldapdelete -D "cn=admin,dc=nodomain" -w geheim -H ldapi:/// "ou=People,dc=nodomain" || true
  sudo ldapdelete -D "cn=admin,dc=nodomain" -w geheim -H ldapi:/// "ou=DeletedPeople,dc=nodomain" || true
  sudo ldapdelete -D "cn=admin,dc=nodomain" -w geheim -H ldapi:/// "ou=NoPeople,dc=nodomain" || true
}

function scaffolding1() {
  sudo ldapadd -D "cn=admin,dc=nodomain" -w geheim -H ldapi:/// -f ./ldif/dirs.ldif
  sudo ldapadd -D "cn=admin,dc=nodomain" -w geheim -H ldapi:/// -f ./ldif/new_users.ldif
}

function scaffolding2() {
  sudo ldapadd -D "cn=admin,dc=nodomain" -w geheim -H ldapi:/// -f ./ldif/deleted_users.ldif
}

function scaffolding_spar() {
  if ( curl -s $BRIG_URL/i/status ); then
    WIRE_USER=$(${WIRE_SERVER_PATH}/deploy/services-demo/create_test_team_admins.sh -c)
    WIRE_USERID=$(echo $WIRE_USER | sed 's/^\([^,]\+\),\([^,]\+\),\([^,]\+\)$/\1/')
    WIRE_PASSWD=$(echo $WIRE_USER | sed 's/^\([^,]\+\),\([^,]\+\),\([^,]\+\)$/\3/')
    WIRE_TEAMID=$(curl -s -H'content-type: application/json' -H'Z-User: '"${WIRE_USERID}" http://localhost:8082/self | jq .team | xargs echo)

    # create a saml idp (if we don't, users will not be created, but invitated, which would make the following more awkward to write down).
    curl -s -X PUT \
      --header "Z-User: $WIRE_USERID" \
      --header 'Content-Type: application/json;charset=utf-8' \
      -d '{"status": "enabled"}' \
      ${GALLEY_URL}/i/teams/$WIRE_TEAMID/features/sso >/dev/null
    export WIRE_SAMLIDP=$(curl -X POST \
      --header "Z-User: $WIRE_USERID" \
      --header 'Content-Type: application/xml;charset=utf-8' \
      -d "<EntityDescriptor xmlns:samlp=\"urn:oasis:names:tc:SAML:2.0:protocol\" xmlns:samla=\"urn:oasis:names:tc:SAML:2.0:assertion\" xmlns:samlm=\"urn:oasis:names:tc:SAML:2.0:metadata\" xmlns:ds=\"http://www.w3.org/2000/09/xmldsig#\" ID=\"_0c29ba62-a541-11e8-8042-873ef87bdcba\" entityID=\"https://issuer.net/_"`uuid`"\" xmlns=\"urn:oasis:names:tc:SAML:2.0:metadata\"><IDPSSODescriptor protocolSupportEnumeration=\"urn:oasis:names:tc:SAML:2.0:protocol\"><KeyDescriptor use=\"signing\"><ds:KeyInfo><ds:X509Data><ds:X509Certificate>MIIBOTCBxKADAgECAg4TIFmNatMeqaAE8BWQBTANBgkqhkiG9w0BAQsFADAAMB4XDTIxMDkwMzEzMjUyMVoXDTQxMDgyOTEzMjUyMVowADB6MA0GCSqGSIb3DQEBAQUAA2kAMGYCYQDPAqTk/nq2B/J0WH2FtiRh6nB8BvOc6M7d4K2KV0kXrePjeRPh+cDDf9mYrpntnjBa2LGAc0S4gjUXdvnt1Fxg2YYXYJ+N7+jxV36jUng7cGz1tEOB5RIj28Mv8/eXnjUCAREwDQYJKoZIhvcNAQELBQADYQBaIWDz832gg5jZPIy5z0CV1rWbUQALy6SUodWMezbzVF86hycUvZqAzd5Pir8084Mk/6FQK2Hbbml2LaHS8JnZpYxlgNIRNNonzScAUFclDi4NNmcxPuB6ycu9kK/0l+A=</ds:X509Certificate></ds:X509Data></ds:KeyInfo></KeyDescriptor><SingleSignOnService Binding=\"urn:oasis:names:tc:SAML:2.0:bindings:HTTP-POST\" Location=\"https://requri.net/fb9e3c14-25eb-482a-8df3-c71e3e83110b\"/></IDPSSODescriptor></EntityDescriptor>" \
      ${SPAR_URL}/identity-providers | jq .)
    if [ "$(echo $WIRE_SAMLIDP | jq .id)" == "null" ]; then
        echo "could not create idp: $WIRE_SAMLIDP"
        false
    fi

    # create a scim token
    SCIM_TOKEN_FULL=$(curl -X POST \
      --header "Z-User: $WIRE_USERID" \
      --header 'Content-Type: application/json;charset=utf-8' \
      -d '{ "description": "test '"`date`"'", "password": "'"$WIRE_PASSWD"'" }' \
      ${SPAR_URL}/scim/auth-tokens)
    SCIM_TOKEN_ID=$(echo $SCIM_TOKEN_FULL | jq -r .info.id)
    SCIM_TOKEN=$(echo $SCIM_TOKEN_FULL | jq -r .token)
    ESCAPED_SCIM_TOKEN=$(echo $SCIM_TOKEN | sed 's/\+/\\\+/g;s_/_\\/_g;s/\=/\\=/g')
    sed -i 's/^  token: \"Bearer .*$/  token: \"Bearer '"${ESCAPED_SCIM_TOKEN}"'"/' $BRIDGE_CONF1
    sed -i 's/^  token: \"Bearer .*$/  token: \"Bearer '"${ESCAPED_SCIM_TOKEN}"'"/' $BRIDGE_CONF2
  else
    # no wire-server running?
    echo "${WIRE_SERVER_PATH}/deploy/dockerephemeral/run.sh"
    echo "${WIRE_SERVER_PATH}/services/start-services-only.sh"
    false
  fi
}

function assert_num_members() {
    sleep 2 # mitigate race conditions (increase the time if this function fails)
    if [ "$(curl -s -H'content-type: application/json' -H'Z-User: '"${WIRE_USERID}" http://localhost:8085/teams/${WIRE_TEAMID}/members | jq '.members|length')" != "$1" ]; then
      echo "$2"
      false
    fi
}

# ----------------------------------------------------------------------
# main

clear

scaffolding_spar
echo WIRE_USERID: $WIRE_USERID
echo WIRE_TEAMID: $WIRE_TEAMID
echo SCIM_TOKEN: $SCIM_TOKEN

scaffolding1
sudo slapcat
cabal run ldap-scim-bridge $BRIDGE_CONF1
assert_num_members 2 "user could not be created!"

scaffolding2
sudo slapcat
cabal run ldap-scim-bridge $BRIDGE_CONF2
assert_num_members 1 "user could not be deleted!"
