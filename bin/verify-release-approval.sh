#!/usr/bin/env bash

# Verifies with a release manager that a release request is approved, before
# publish-release promotes anything (ADR 0006 "Release from the release
# manager"). Used by the `verify-approval` job on a dispatch that carries a
# request id, and by the publish job on the recovery path when the release
# manifest records one.
#
# Usage: ./bin/verify-release-approval.sh <url> <token> <request_id> <component> <version>
#   url:         the release manager, https://host[:port] (http only for a loopback host, in tests)
#   token:       its ingest token, the one notify-rm already uses
#   request_id:  UUID4 of the release request
#   component:   the component this repository releases (ldap-scim-bridge)
#   version:     the version being published, X.Y.Z or X.Y.Z-<alpha|beta|rc>.N
#
# GET <url>/api/v1/release-requests/<request_id>, 15 s timeout, no redirects
# (an SSO-fronted host answers a machine with a 302 to a login page). Passes
# only when the answer is HTTP 200 and the request's `component` and
# `expected_version` are the ones given and its `status` is `approved` or
# `publishing`; prints who approved it. Any other answer exits 1 with the
# reason. The token reaches curl as a config file on stdin, never on a
# command line where ps shows it.
#
# Tests: bin/test/run.sh (bin/test/verify-release-approval.test.sh, with a
# fake curl on PATH).

set -euo pipefail

# Five arguments, possibly empty: an unset RM_<ORIGIN>_URL variable or
# RM_<ORIGIN>_TOKEN secret arrives as an empty string and gets its own message.
if [[ $# -ne 5 ]]; then
  echo "usage: $0 <url> <token> <request_id> <component> <version>" >&2
  exit 64
fi
url="$1"
token="$2"
request_id="$3"
component="$4"
version="$5"

uuid4_re='^[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}$'
version_re='^[0-9]+\.[0-9]+\.[0-9]+(-(alpha|beta|rc)\.[0-9]+)?$'
url_re='^(https://[^/[:space:]]+|http://(127\.0\.0\.1|localhost)(:[0-9]+)?)/?$'

fail() {
  echo "error: $*" >&2
  exit 1
}

[[ "$request_id" =~ $uuid4_re ]] || fail "request_id '${request_id}' is not a lower-case UUID4"
[[ "$version" =~ $version_re ]] || fail "version '${version}' is not X.Y.Z or X.Y.Z-<alpha|beta|rc>.N"
[[ "$component" =~ ^[a-z0-9][a-z0-9-]*$ ]] || fail "component '${component}' is not a component name"
[[ "$url" =~ $url_re ]] || fail "release manager URL '${url}' is not https://host (is the RM_<ORIGIN>_URL variable set?)"
[[ -n "$token" ]] || fail "the release manager token is empty (is the RM_<ORIGIN>_TOKEN secret set?)"
[[ "$token" == *[$'\r\n']* ]] && fail "the release manager token holds a line break"

response=$(mktemp)
trap 'rm -f "$response"' EXIT

# Quoted curl config strings take backslash escapes, hence \ and " escaped.
config_token=${token//\\/\\\\}
config_token=${config_token//\"/\\\"}
request_url="${url%/}/api/v1/release-requests/${request_id}"

# --fail-with-body: a 4xx/5xx is a failed command, and the body is still
# written for the message below. %{http_code} is printed either way; it is
# 000 when nothing answered.
code=$(printf 'header = "Authorization: Bearer %s"\n' "$config_token" |
  curl --config - --silent --show-error --max-time 15 --fail-with-body \
    -o "$response" -w '%{http_code}' \
    -H 'Accept: application/json' \
    "$request_url") || code="${code:-000}"

case "$code" in
  200) ;;
  000) fail "release manager ${url} did not answer within 15 s for request ${request_id} — not publishing" ;;
  404) fail "release manager ${url} has no release request ${request_id} (HTTP 404) — not publishing" ;;
  401 | 403) fail "release manager ${url} refused the token for request ${request_id} (HTTP ${code}) — not publishing" ;;
  3??) fail "release manager ${url} answered HTTP ${code} for request ${request_id} (a redirect: an interactive SSO in front of the API?) — not publishing" ;;
  *) fail "release manager ${url} answered HTTP ${code} for request ${request_id}: $(head -c 500 "$response" | tr -d '\r\n') — not publishing" ;;
esac

jq -e 'type == "object"' "$response" >/dev/null 2>&1 \
  || fail "release manager ${url} answered 200 for request ${request_id} but not with a JSON object — not publishing"

got_component=$(jq -r '.component // ""' "$response")
got_version=$(jq -r '.expected_version // ""' "$response")
got_status=$(jq -r '.status // ""' "$response")
approved_by=$(jq -r '.approved_by // "nobody"' "$response")
approved_at=$(jq -r '.approved_at // "n/a"' "$response")

[[ "$got_component" == "$component" ]] \
  || fail "request ${request_id} is for component '${got_component}', not '${component}' — not publishing"
[[ "$got_version" == "$version" ]] \
  || fail "request ${request_id} expects version '${got_version}', not '${version}' — not publishing"
case "$got_status" in
  approved | publishing) ;;
  *) fail "request ${request_id} is '${got_status}', not approved (approved or publishing) — not publishing" ;;
esac

echo "Release request ${request_id} on ${url}: ${component} ${version}, status '${got_status}', approved by ${approved_by} at ${approved_at}."
