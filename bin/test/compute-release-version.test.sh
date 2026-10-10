#!/usr/bin/env bash

# Tests of bin/compute-release-version.sh against throwaway git histories,
# starting from this repository's own: the lightweight v0.10.4 and the
# annotated SemVer anchor v0.11.0 on the same commit.

# shellcheck source=bin/test/lib.sh
source bin/test/lib.sh

script="$PWD/bin/compute-release-version.sh"
work=$(mktemp -d)
trap 'rm -rf "$work"' EXIT

export GIT_CONFIG_NOSYSTEM=1 GIT_CONFIG_GLOBAL=/dev/null
export GIT_AUTHOR_NAME=test GIT_AUTHOR_EMAIL=test@example.com
export GIT_COMMITTER_NAME=test GIT_COMMITTER_EMAIL=test@example.com

repo="$work/repo"
git init -q -b dev "$repo"
g() { git -C "$repo" "$@"; }
commit() { g commit -q --allow-empty -m "$1"; }
# The script reads origin/<branch>: publish the local branches there.
sync() {
  local b
  for b in $(g for-each-ref --format='%(refname:short)' refs/heads); do
    g update-ref "refs/remotes/origin/$b" "refs/heads/$b"
  done
}
run() { (cd "$repo" && "$script" "$@"); }

commit "initial"
sync
expect_fail "no release tag yet" "no explicit release reachable" run dev ga patch

g tag v0.10.4
g tag -a v0.11.0 -m "SemVer anchor"
commit "feature"
sync

expect_fail "bad type" "type must be one of" run dev final patch
expect_fail "bad bump" "version_bump must be one of" run dev ga huge

# The annotated anchor wins over the lightweight tag on the same commit.
expect_out "GA patch from the anchor" "0.11.1" run dev ga patch
expect_out "GA minor from the anchor" "0.12.0" run dev ga minor
expect_out "GA major from the anchor" "1.0.0" run dev ga major
# version_bump applies to a new pre-release line (team-settings: always patch).
expect_out "rc minor from the anchor" "0.12.0-rc.1" run dev rc minor
expect_out "alpha patch from the anchor" "0.11.1-alpha.1" run dev alpha patch

g tag -a v0.12.0-rc.1 -m "v0.12.0-rc.1"
commit "fix"
sync

expect_out "next rc" "0.12.0-rc.2" run dev rc patch
expect_out "rc bump ignored inside a line" "0.12.0-rc.2" run dev rc major
expect_fail "back to beta" "which ranks higher" run dev beta patch
expect_out "promote to GA" "0.12.0" run dev ga minor

g tag -a v0.12.0 -m "v0.12.0"
g branch release/0.12
commit "next feature"
sync

expect_out "patch on the release line" "0.12.1" run release/0.12 ga patch
expect_out "rc on the release line" "0.12.1-rc.1" run release/0.12 rc patch
expect_fail "minor on the release line" "may only produce 0.12.* releases" run release/0.12 rc minor
expect_out "next minor from dev" "0.13.0-rc.1" run dev rc minor

# A release already ahead of the target is a gap, wherever it was cut.
g checkout -q -b side
commit "side"
g tag -a v0.13.0 -m "v0.13.0"
g checkout -q dev
sync
expect_fail "dev behind a newer release" "already ahead of computed target" run dev ga patch
expect_out "release line ignores newer minors" "0.12.1" run release/0.12 ga patch

# The computed target is already tagged, off the release line.
g tag -a v0.12.1 -m "v0.12.1" side
expect_fail "existing tag" "already exists" run release/0.12 ga patch

finish
