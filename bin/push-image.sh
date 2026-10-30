#!/usr/bin/env bash
# Push the image archive built by `nix build .#dockerImage` as
# <repository>:<tag> with skopeo, never overwriting a published tag (ADR-0018),
# and write the registry digest as `digest=sha256:...` to $GITHUB_OUTPUT
# (stdout outside GitHub Actions).
#
# Usage: bin/push-image.sh <archive> <repository> <tag>
#
# Environment:
#   REGISTRY_USERNAME, REGISTRY_PASSWORD  optional; log in to the registry of
#                                         <repository> first (password on stdin)
#   TLS_VERIFY                            default true; false for a local test
#                                         registry
set -euo pipefail

if [[ $# -ne 3 ]]; then
  echo "usage: $0 <archive> <repository> <tag>" >&2
  exit 64
fi
archive=$1
repository=$2
tag=$3
registry=${repository%%/*}
ref="docker://${repository}:${tag}"
tls_verify=${TLS_VERIFY:-true}

if [[ ! -f $archive ]]; then
  echo "::error::image archive $archive not found" >&2
  exit 1
fi

if [[ -n ${REGISTRY_PASSWORD:-} ]]; then
  printf '%s' "$REGISTRY_PASSWORD" |
    skopeo login --tls-verify="$tls_verify" --username "${REGISTRY_USERNAME:?REGISTRY_USERNAME is required with REGISTRY_PASSWORD}" --password-stdin "$registry"
fi

inspect() {
  skopeo inspect --tls-verify="$tls_verify" --format '{{.Digest}}' "$ref"
}

errors=$(mktemp)
trap 'rm -f "$errors"' EXIT

if digest=$(inspect 2>"$errors"); then
  # Same version = same commit: keep what is there, never push over it.
  echo "::notice::${repository}:${tag} is already published (${digest}), not overwritten"
elif grep -qiE 'manifest unknown|name unknown|not found' "$errors"; then
  # The source is the archive this build just made, so no signature policy
  # applies; --insecure-policy also spares a /etc/containers/policy.json.
  skopeo copy --insecure-policy --dest-tls-verify="$tls_verify" "docker-archive:${archive}" "$ref"
  # Read the digest back from the registry: it is what consumers pull.
  digest=$(inspect)
  echo "Pushed ${repository}:${tag} (${digest})"
else
  cat "$errors" >&2
  echo "::error::cannot check whether ${repository}:${tag} exists" >&2
  exit 1
fi

if [[ ! $digest =~ ^sha256:[0-9a-f]{64}$ ]]; then
  echo "::error::unexpected digest '${digest}' for ${repository}:${tag}" >&2
  exit 1
fi

echo "digest=${digest}" >>"${GITHUB_OUTPUT:-/dev/stdout}"
