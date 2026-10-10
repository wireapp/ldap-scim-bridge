#!/usr/bin/env bash

# Validates the workflow_dispatch inputs of prepare-release.yml before any of
# them is used: in an API path, a git ref, a file name or the run summary.
# Every input arrives through an environment variable (never `${{ }}` in a
# `run:` body), is matched against the contract shared with the release
# manager (ADR-0021 in wire-onprem-first-decisions, ADR 0006 here), and the
# first violation stops the run with an `error:` line on stderr.
#
# Usage: SOURCE_BRANCH=dev TYPE=ga VERSION_BUMP=patch FORCE=false \
#        REQUEST_ID= EXPECTED_VERSION= RELEASE_NOTE= RELEASE_DECLARATION= RM_ORIGIN= \
#        ./bin/validate-release-inputs.sh
#
#   SOURCE_BRANCH        dev, or release/X.Y (existence is checked by the workflow afterwards)
#   TYPE                 ga | alpha | beta | rc
#   VERSION_BUMP         patch | minor | major
#   FORCE                true | false (empty counts as false)
#   REQUEST_ID           empty, or the UUID4 of the release-manager request
#   EXPECTED_VERSION     empty, or X.Y.Z[-<alpha|beta|rc>.N] (a check, never a source)
#   RELEASE_NOTE         empty, or Markdown text
#   RELEASE_DECLARATION  empty, or the JSON object of a release declaration
#   RM_ORIGIN            empty, or staging | prod; required when REQUEST_ID is set
#
# The nine inputs together may not exceed 60,000 characters: GitHub refuses a
# dispatch above 65,535 and the release manager stops at the same budget.
#
# Tests: bin/test/run.sh (bin/test/validate-release-inputs.test.sh).

set -euo pipefail

budget=60000

source_branch_re='^(dev|release/[0-9]+\.[0-9]+)$'
uuid4_re='^[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}$'
version_re='^[0-9]+\.[0-9]+\.[0-9]+(-(alpha|beta|rc)\.[0-9]+)?$'
rm_origin_re='^(staging|prod)$'

fail() {
  echo "error: $*" >&2
  exit 1
}

source_branch="${SOURCE_BRANCH:-}"
type="${TYPE:-}"
version_bump="${VERSION_BUMP:-}"
force="${FORCE:-}"
request_id="${REQUEST_ID:-}"
expected_version="${EXPECTED_VERSION:-}"
release_note="${RELEASE_NOTE:-}"
release_declaration="${RELEASE_DECLARATION:-}"
rm_origin="${RM_ORIGIN:-}"

[[ "$source_branch" =~ $source_branch_re ]] \
  || fail "source_branch must be 'dev' or 'release/X.Y' (got: '${source_branch}')"

case "$type" in
  ga | alpha | beta | rc) ;;
  *) fail "type must be one of ga, alpha, beta, rc (got: '${type}')" ;;
esac

case "$version_bump" in
  patch | minor | major) ;;
  *) fail "version_bump must be one of patch, minor, major (got: '${version_bump}')" ;;
esac

case "$force" in
  '' | true | false) ;;
  *) fail "force must be true or false (got: '${force}')" ;;
esac

if [[ -n "$request_id" ]]; then
  [[ "$request_id" =~ $uuid4_re ]] \
    || fail "request_id must be a lower-case UUID4 (got: '${request_id}')"
  [[ -n "$rm_origin" ]] \
    || fail "rm_origin is required when request_id is set (staging or prod)"
fi

if [[ -n "$expected_version" ]]; then
  [[ "$expected_version" =~ $version_re ]] \
    || fail "expected_version must be X.Y.Z or X.Y.Z-<alpha|beta|rc>.N, without a leading 'v' (got: '${expected_version}')"
fi

if [[ -n "$rm_origin" ]]; then
  [[ "$rm_origin" =~ $rm_origin_re ]] \
    || fail "rm_origin must be 'staging' or 'prod' (got: '${rm_origin}')"
fi

# Only the shape is checked here: the sections, enums and types are the job of
# bin/validate-release-declaration.sh, once the text is written to a file.
if [[ -n "$release_declaration" ]]; then
  if ! jq -e 'type == "object"' <<<"$release_declaration" >/dev/null 2>&1; then
    fail "release_declaration must be a JSON object (the text does not parse as one)"
  fi
fi

# Character counts in the current locale, as the release manager counts them.
total=$((${#source_branch} + ${#type} + ${#version_bump} + ${#force} + ${#request_id} \
  + ${#expected_version} + ${#release_note} + ${#release_declaration} + ${#rm_origin}))
if ((total > budget)); then
  fail "the inputs total ${total} characters, above the ${budget} budget (release_note: ${#release_note}, release_declaration: ${#release_declaration}) — shorten the note or the declaration"
fi

show() { if [[ -n "$2" ]]; then echo "$1=$2"; else echo "$1=(empty)"; fi; }
echo "Validated prepare-release inputs:"
show "  source_branch" "$source_branch"
show "  type" "$type"
show "  version_bump" "$version_bump"
show "  force" "${force:-false}"
show "  request_id" "$request_id"
show "  expected_version" "$expected_version"
show "  rm_origin" "$rm_origin"
echo "  release_note=${#release_note} characters"
echo "  release_declaration=${#release_declaration} characters"
echo "  total=${total} characters (budget ${budget})"
