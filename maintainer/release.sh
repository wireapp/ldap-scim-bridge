#!/bin/bash

set -x
set -eo pipefail

cd "$( dirname "${BASH_SOURCE[0]}" )/.."

function prompt() {
    echo -e "\nok? [^D / ^C]"; cat > /dev/null
}

echo "confirm that you have updated CHANGELOG.md and version in ldap-scim-bridge.cabal as eg. here:"
git show v0.7 | cat
prompt

echo "confirm that you have tagged the new version v0.<N> (or anything PVP), and push everything to origin/master."
prompt

echo "this will trigger the deployment of the docker image, see github action logs for how that went."
echo -e "ok!\n"

echo "now for hackage."
cabal test
cabal sdist

echo "now run 'cabal upload <file.tgz>' and then 'cabal upload --publish <file.tgz>'"
prompt

echo "that's it, you're done!"
