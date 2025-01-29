#!/usr/bin/env bash
# shellcheck disable=SC2317
# SC2317 (info): Command appears to be unreachable. Check usage (or ignore if invoked indirectly).
#
# FUTUREWORK: Re-enable this check when you start using this script.

echo "WARNING!! this script has never run!!  test carefully!!"
exit 1

# hackage
# from https://hackage.haskell.org/upload
dir=$(mktemp -d dist-docs.XXXXXX)
trap 'rm -r "$dir"' EXIT
cabal v2-haddock --builddir="$dir" --haddock-for-hackage --enable-doc
cabal upload -d --publish "$dir"/*-docs.tar.gz

# docker
export VERSION=0.10
docker build -t ldap-scim-bridge:"${VERSION}" .
docker tag ldap-scim-bridge:"${VERSION}" quay.io/wire/ldap-scim-bridge:"${VERSION}"
docker login quay.io
docker push quay.io/wire/ldap-scim-bridge:"${VERSION}"
