#!/usr/bin/env bash

# bin/verify-release-approval.sh against bin/test/fake-curl.sh, first on PATH:
# the answer is set per case through FAKE_CURL_STATUS and FAKE_CURL_BODY, and
# the recorded arguments show what reached curl. Run with bin/test/run.sh.

set -euo pipefail
cd "$(dirname "$0")/../.."
# shellcheck source=bin/test/lib.sh
source bin/test/lib.sh

script=./bin/verify-release-approval.sh
uuid=6f1e4c2a-9b3d-4a7e-8f10-2c5d7e9a1b3c
url=https://rm.example.com
token='s3cret"with\specials'

tmp=$(mktemp -d)
trap 'rm -rf "$tmp"' EXIT
mkdir -p "$tmp/bin"
cp bin/test/fake-curl.sh "$tmp/bin/curl"
chmod +x "$tmp/bin/curl"
export FAKE_CURL_ARGS="$tmp/args" FAKE_CURL_STDIN="$tmp/stdin"

approved='{"id":"6f1e4c2a-9b3d-4a7e-8f10-2c5d7e9a1b3c","component":"ldap-scim-bridge","expected_version":"0.12.0","status":"approved","approved_by":"alice","approved_at":"2026-10-08T10:00:00Z"}'

# run <status> <body> <args...>: the script with the fake curl answering status/body.
run() {
  local status="$1" body="$2"; shift 2
  rm -f "$FAKE_CURL_ARGS" "$FAKE_CURL_STDIN"
  env PATH="$tmp/bin:$PATH" FAKE_CURL_STATUS="$status" FAKE_CURL_BODY="$body" "$script" "$@"
}

expect_out "approved" "status 'approved', approved by alice" run 200 "$approved" "$url" "$token" "$uuid" ldap-scim-bridge 0.12.0
expect_ok "publishing" run 200 "$(jq -c '.status = "publishing"' <<<"$approved")" "$url" "$token" "$uuid" ldap-scim-bridge 0.12.0
expect_ok "trailing slash on the URL" run 200 "$approved" "$url/" "$token" "$uuid" ldap-scim-bridge 0.12.0
expect_ok "loopback http" run 200 "$approved" http://127.0.0.1:8080 "$token" "$uuid" ldap-scim-bridge 0.12.0

expect_fail "requested" "is 'requested', not approved" run 200 "$(jq -c '.status = "requested"' <<<"$approved")" "$url" "$token" "$uuid" ldap-scim-bridge 0.12.0
expect_fail "published already" "is 'published', not approved" run 200 "$(jq -c '.status = "published"' <<<"$approved")" "$url" "$token" "$uuid" ldap-scim-bridge 0.12.0
expect_fail "other component" "is for component 'webapp', not 'ldap-scim-bridge'" run 200 "$(jq -c '.component = "webapp"' <<<"$approved")" "$url" "$token" "$uuid" ldap-scim-bridge 0.12.0
expect_fail "other version" "expects version '0.12.0', not '0.12.1'" run 200 "$approved" "$url" "$token" "$uuid" ldap-scim-bridge 0.12.1
expect_fail "404" "has no release request" run 404 '{"error":"not found"}' "$url" "$token" "$uuid" ldap-scim-bridge 0.12.0
expect_fail "403" "refused the token" run 403 '{"error":"forbidden"}' "$url" "$token" "$uuid" ldap-scim-bridge 0.12.0
expect_fail "302 to SSO" "a redirect" run 302 '' "$url" "$token" "$uuid" ldap-scim-bridge 0.12.0
expect_fail "500" "answered HTTP 500" run 500 'boom' "$url" "$token" "$uuid" ldap-scim-bridge 0.12.0
expect_fail "unreachable" "did not answer within 15 s" run 000 '' "$url" "$token" "$uuid" ldap-scim-bridge 0.12.0
expect_fail "200 without JSON" "not with a JSON object" run 200 '<html>' "$url" "$token" "$uuid" ldap-scim-bridge 0.12.0

# What reached curl on the last successful call: the URL built from the
# validated parts, the timeout, no redirects, and the token on stdin only.
run 200 "$approved" "$url" "$token" "$uuid" ldap-scim-bridge 0.12.0 >/dev/null
expect_ok "request URL" grep -qx "${url}/api/v1/release-requests/${uuid}" "$FAKE_CURL_ARGS"
expect_ok "a timeout" grep -qx -- '--max-time' "$FAKE_CURL_ARGS"
expect_ok "of 15 s" grep -qx -- '15' "$FAKE_CURL_ARGS"
expect_ok "fails on 4xx/5xx" grep -qx -- '--fail-with-body' "$FAKE_CURL_ARGS"
expect_ok "token on stdin, escaped for the config file" grep -qxF 'header = "Authorization: Bearer s3cret\"with\\specials"' "$FAKE_CURL_STDIN"
if grep -q 's3cret' "$FAKE_CURL_ARGS"; then
  echo "   not ok: the token is on curl's command line" >&2
  exit 1
fi
if grep -qx -- '-L' "$FAKE_CURL_ARGS"; then
  echo "   not ok: curl follows redirects" >&2
  exit 1
fi
passed=$((passed + 2))

# Rejected before any call.
expect_fail "bad request id" "is not a lower-case UUID4" run 200 "$approved" "$url" "$token" not-a-uuid ldap-scim-bridge 0.12.0
expect_fail "bad version" "is not X.Y.Z" run 200 "$approved" "$url" "$token" "$uuid" ldap-scim-bridge v0.12.0
expect_fail "bad component" "is not a component name" run 200 "$approved" "$url" "$token" "$uuid" 'Team Settings' 0.12.0
expect_fail "plain http URL" "is not https://host" run 200 "$approved" http://rm.example.com "$token" "$uuid" ldap-scim-bridge 0.12.0
expect_fail "URL with a path" "is not https://host" run 200 "$approved" "$url/api" "$token" "$uuid" ldap-scim-bridge 0.12.0
expect_fail "empty URL" "is not https://host" run 200 "$approved" '' "$token" "$uuid" ldap-scim-bridge 0.12.0
expect_fail "empty token" "token is empty" run 200 "$approved" "$url" '' "$uuid" ldap-scim-bridge 0.12.0
expect_fail "token with a line break" "holds a line break" run 200 "$approved" "$url" $'a\nb' "$uuid" ldap-scim-bridge 0.12.0
if [[ -e "$FAKE_CURL_ARGS" ]]; then
  echo "   not ok: curl was called for a rejected argument" >&2
  exit 1
fi
passed=$((passed + 1))

finish
