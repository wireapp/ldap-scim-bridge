#!/usr/bin/env bash

# Runs the tests of the bin/ shell scripts: every bin/test/*.test.sh, or the
# ones named on the command line. Each test file sources bin/test/lib.sh and
# fails the run when one of its expectations fails. Needs bash and jq only.
#
# Usage: bin/test/run.sh [bin/test/<name>.test.sh ...]
#
# Runs in CI from .github/workflows/workflows-lint.yml.

set -euo pipefail

root="$(cd "$(dirname "$0")/../.." && pwd)"
cd "$root"

tests=("$@")
if [[ ${#tests[@]} -eq 0 ]]; then
  tests=(bin/test/*.test.sh)
fi

failed=()
for t in "${tests[@]}"; do
  echo "== ${t}"
  if bash "$t"; then
    echo "   ok"
  else
    echo "   FAILED" >&2
    failed+=("$t")
  fi
done

if [[ ${#failed[@]} -gt 0 ]]; then
  echo "error: ${#failed[@]} test file(s) failed: ${failed[*]}" >&2
  exit 1
fi
echo "All ${#tests[@]} test file(s) passed."
