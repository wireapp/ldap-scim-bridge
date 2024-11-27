#!/bin/bash

echo "WARNING!! this script has never run!!  test carefully!!"
exit 1

# hackage
# from https://hackage.haskell.org/upload
dir=$(mktemp -d dist-docs.XXXXXX)
trap 'rm -r "$dir"' EXIT
cabal v2-haddock --builddir="$dir" --haddock-for-hackage --enable-doc
cabal upload -d --publish $dir/*-docs.tar.gz

# docker
export VERSION=0.10
docker build -t ldap-scim-bridge:${VERSION} .
docker tag ldap-scim-bridge:${VERSION} quay.io/wire/ldap-scim-bridge:${VERSION}
docker login quay.io
docker push quay.io/wire/ldap-scim-bridge:${VERSION}

