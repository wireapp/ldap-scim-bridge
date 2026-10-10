#!/usr/bin/env bash

# Shared helpers of the bin/test/*.test.sh files, sourced by each of them.
# A test file is a sequence of expectations; the first failed one aborts it.
#
#   expect_ok   <label> <command...>              the command exits 0
#   expect_fail <label> <stderr substring> <cmd>  the command exits 1 and its stderr contains the text
#   expect_out  <label> <stdout substring> <cmd>  the command exits 0 and its stdout contains the text
#
# `expected_failures` and `passed` count the expectations, for the summary line
# every test file prints through `finish`.

set -euo pipefail

passed=0

_show() {
  # $1 label, $2 what went wrong, $3 captured output
  echo "   not ok: $1 — $2" >&2
  while IFS= read -r line; do echo "        | ${line}"; done <<<"$3" >&2
  exit 1
}

expect_ok() {
  local label="$1"; shift
  local out
  if ! out=$("$@" 2>&1); then
    _show "$label" "expected exit 0" "$out"
  fi
  passed=$((passed + 1))
}

expect_fail() {
  local label="$1" needle="$2"; shift 2
  local out rc=0
  out=$("$@" 2>&1) || rc=$?
  if [[ $rc -ne 1 ]]; then
    _show "$label" "expected exit 1, got $rc" "$out"
  fi
  if [[ "$out" != *"$needle"* ]]; then
    _show "$label" "stderr does not contain '$needle'" "$out"
  fi
  passed=$((passed + 1))
}

expect_out() {
  local label="$1" needle="$2"; shift 2
  local out
  if ! out=$("$@" 2>&1); then
    _show "$label" "expected exit 0" "$out"
  fi
  if [[ "$out" != *"$needle"* ]]; then
    _show "$label" "output does not contain '$needle'" "$out"
  fi
  passed=$((passed + 1))
}

finish() {
  echo "   ${passed} expectation(s) passed"
}
