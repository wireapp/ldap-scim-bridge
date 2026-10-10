#!/usr/bin/env bash

# bin/validate-release-inputs.sh: one case per rule of the dispatch contract.
# Run with bin/test/run.sh.

set -euo pipefail
cd "$(dirname "$0")/../.."
# shellcheck source=bin/test/lib.sh
source bin/test/lib.sh

script=./bin/validate-release-inputs.sh
uuid=6f1e4c2a-9b3d-4a7e-8f10-2c5d7e9a1b3c

# Runs the script with exactly the given VAR=value pairs on top of the manual
# defaults, so each case names only what it changes.
run() {
  env SOURCE_BRANCH=dev TYPE=ga VERSION_BUMP=patch FORCE=false \
    REQUEST_ID= EXPECTED_VERSION= RELEASE_NOTE= RELEASE_DECLARATION= RM_ORIGIN= \
    "$@" "$script"
}

expect_ok "manual GA from dev" run
expect_out "prints what it validated" "source_branch=dev" run
expect_ok "release line" run SOURCE_BRANCH=release/4.24
expect_ok "pre-release with force" run TYPE=rc FORCE=true
expect_ok "force may be empty" run FORCE=
expect_ok "full release-manager set" run REQUEST_ID="$uuid" EXPECTED_VERSION=0.12.0 RM_ORIGIN=prod \
  RELEASE_NOTE=$'## Overview\n\nA note.' RELEASE_DECLARATION='{"schemaVersion":"wire.component-release/v1alpha1"}'
expect_ok "expected pre-release version" run EXPECTED_VERSION=0.12.0-beta.2
expect_ok "rm_origin alone" run RM_ORIGIN=staging

expect_fail "source_branch main" "source_branch must be" run SOURCE_BRANCH=main
expect_fail "source_branch release/4" "source_branch must be" run SOURCE_BRANCH=release/4
expect_fail "source_branch traversal" "source_branch must be" run SOURCE_BRANCH='../x'
expect_fail "source_branch with newline" "source_branch must be" run SOURCE_BRANCH=$'dev\nx'
expect_fail "source_branch empty" "source_branch must be" run SOURCE_BRANCH=
expect_fail "type" "type must be one of" run TYPE=nightly
expect_fail "version_bump" "version_bump must be one of" run VERSION_BUMP=huge
expect_fail "force" "force must be true or false" run FORCE=yes
expect_fail "request_id not a UUID" "request_id must be" run REQUEST_ID=not-a-uuid RM_ORIGIN=prod
expect_fail "request_id not version 4" "request_id must be" run REQUEST_ID=6f1e4c2a-9b3d-1a7e-8f10-2c5d7e9a1b3c RM_ORIGIN=prod
expect_fail "request_id upper case" "request_id must be" run REQUEST_ID="${uuid^^}" RM_ORIGIN=prod
expect_fail "request_id without rm_origin" "rm_origin is required" run REQUEST_ID="$uuid"
expect_fail "rm_origin dev" "rm_origin must be" run RM_ORIGIN=dev
expect_fail "expected_version with v" "expected_version must be" run EXPECTED_VERSION=v0.12.0
expect_fail "expected_version legacy alpha" "expected_version must be" run EXPECTED_VERSION=0.12.0-alpha01
expect_fail "expected_version short" "expected_version must be" run EXPECTED_VERSION=4.25
expect_fail "declaration not JSON" "release_declaration must be a JSON object" run RELEASE_DECLARATION='{'
expect_fail "declaration not an object" "release_declaration must be a JSON object" run RELEASE_DECLARATION='[]'

# Budget: the fixed inputs above weigh 15 characters (dev ga patch false).
fixed=15
note=$(head -c $((60000 - fixed)) /dev/zero | tr '\0' 'x')
expect_ok "note at the budget" run RELEASE_NOTE="$note"
expect_fail "note above the budget" "above the 60000 budget" run RELEASE_NOTE="${note}x"

finish
