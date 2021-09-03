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

set -xe
cd "$( dirname "${BASH_SOURCE[0]}" )"/..

export BRIDGE_CONF=./sample-conf.yaml

function install() {
  sudo apt-get install ldapscripts ldap-utils slapd
}

function clear() {
  # TODO: this both fails independently; I have not investigated.
  sudo ldapdelete -D "cn=admin,dc=nodomain" -w geheim -H ldapi:/// DN "ou=People,dc=nodomain"
  sudo ldapdelete -D "cn=admin,dc=nodomain" -w geheim -H ldapi:/// DN "cn=john,ou=People,dc=nodomain"
}

function scaffolding1() {
  sudo ldapadd -D "cn=admin,dc=nodomain" -w geheim -H ldapi:/// -f ./ldif/dirs.ldif
  sudo ldapadd -D "cn=admin,dc=nodomain" -w geheim -H ldapi:/// -f ./ldif/new_users.ldif
}

function scaffolding2() {
  sudo ldapadd -D "cn=admin,dc=nodomain" -w geheim -H ldapi:/// -f ./ldif/deleted_users.ldif
}

export WIRE_USERID
export WIRE_TEAMID
export SCIM_TOKEN
export SCIM_TOKEN_ID
export SCIM_TOKEN_FULL
function scaffolding_spar() {
  export WIRE_SERVER_PATH=~/src/wire-server
  export SPAR_URL=http://localhost:8088
  export BRIG_URL=http://localhost:8088
  if ( curl -q $BRIG_URL/i/status ); then
    WIRE_USER=$(${WIRE_SERVER_PATH}/deploy/services-demo/create_test_team_admins.sh -c)
    WIRE_USERID=$(echo $WIRE_USER | perl -ne '/^([^,]+),([^,]+),([^,]+)$/ && print $1')
    WIRE_PASSWD=$(echo $WIRE_USER | perl -ne '/^([^,]+),([^,]+),([^,]+)$/ && print $3')
    WIRE_TEAMID=$(curl -s -H'content-type: application/json' -H'Z-User: '"${WIRE_USERID}" http://localhost:8082/self | jq .team | xargs echo)
    SCIM_TOKEN_FULL=$(curl -v -X POST \
      --header "Z-User: $WIRE_USERID" \
      --header 'Content-Type: application/json;charset=utf-8' \
      -d '{ "description": "test '"`date`"'", "password": "'"$WIRE_PASSWD"'" }' \
      ${SPAR_URL}/scim/auth-tokens)
    SCIM_TOKEN=$(echo $SCIM_TOKEN_FULL | jq -r .token)
    SCIM_TOKEN_ID=$(echo $SCIM_TOKEN_FULL | jq -r .info.id)
  else
    echo "${WIRE_SERVER_PATH}/deploy/dockerephemeral/run.sh"
    echo "${WIRE_SERVER_PATH}/services/start-services-only.sh"
    exit 1
  fi
  perl -i.bak -ne 'if (/^\s+token: \"Bearer .*$/) { print "  token: \"Bearer '"${SCIM_TOKEN}"'\"\n" } else { print }' $BRIDGE_CONF
}

function run() {
  cabal run ldap-scim-bridge $BRIDGE_CONF
}

scaffolding_spar
echo WIRE_USERID: $WIRE_USERID
echo WIRE_TEAMID: $WIRE_TEAMID
echo SCIM_TOKEN: $SCIM_TOKEN

#scaffolding1
#sudo slapcat
#clear
#exit 0

run

export INVITATIONS=$(curl -s -H'content-type: application/json' -H'Z-User: '"${WIRE_USERID}" http://localhost:8082/teams/${WIRE_TEAMID}/invitations)
# the following two are not supposed to show anything because the user has only been invited so far:
#curl -s -H'content-type: application/json' -H'Z-User: '"${WIRE_USERID}" http://localhost:8085/teams/${WIRE_TEAMID}/members | jq .
#curl -s -H'content-type: text/csv' -H'Z-User: '"${WIRE_USERID}" http://localhost:8085/teams/${WIRE_TEAMID}/members/csv

export INVITATION_ID=$(echo $INVITATIONS | jq '.invitations[0].id')
if [ "$INVITATION_ID" == "null" ]; then
    echo "could not find invitation:"
    echo $INVITATIONS
    exit 1
else
    echo "invitation sent: $INVITATION_ID"
fi