#!/usr/bin/env bash

# A curl stand-in, put first on PATH by the tests of the scripts that call
# the release manager. It records its arguments (one per line) in
# FAKE_CURL_ARGS and its stdin in FAKE_CURL_STDIN, writes FAKE_CURL_BODY to
# the file named by -o, prints FAKE_CURL_STATUS for -w '%{http_code}', and
# mimics curl's exit codes: 22 for a 4xx/5xx with --fail-with-body, 28 for
# status 000 (nothing answered).

set -euo pipefail

printf '%s\n' "$@" > "$FAKE_CURL_ARGS"
cat > "$FAKE_CURL_STDIN"

out=""
fail_with_body=false
args=("$@")
for ((i = 0; i < ${#args[@]}; i++)); do
  case "${args[i]}" in
    -o) out="${args[i + 1]}" ;;
    --fail-with-body) fail_with_body=true ;;
  esac
done

status="${FAKE_CURL_STATUS:-200}"
if [[ -n "$out" ]]; then
  printf '%s' "${FAKE_CURL_BODY:-}" > "$out"
fi
printf '%s' "$status"

if [[ "$status" == "000" ]]; then
  exit 28
fi
if [[ "$fail_with_body" == "true" && "$status" -ge 400 ]]; then
  exit 22
fi
