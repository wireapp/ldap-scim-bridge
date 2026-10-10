#!/usr/bin/env bash

# Computes the next explicit release version per ADR 0006 "Version computation",
# in the pre-release form of ADR-0018: X.Y.Z-<alpha|beta|rc>.N, N from 1.
# Used only by the prepare-release workflow — never for dev builds (see
# the wireapp/wire-rm-actions version action for that).
#
# Copied from wireapp/wire-team-settings@0ee9c80 until a shared action
# computes release versions. One change: version_bump also applies when a
# pre-release line starts from a GA (v0.11.0 + rc + minor = 0.12.0-rc.1), where
# team-settings always bumps the patch. The release manager's next-version
# computation must follow the same rule.
#
# Usage: ./bin/compute-release-version.sh <source_branch> <type> <version_bump>
#   source_branch:    dev, or a maintained release/X.Y line
#   type:          ga | alpha | beta | rc
#   version_bump:  patch | minor | major (applies when the latest release is a
#                  GA; ignored when continuing or promoting a pre-release)
#
# Requires a checkout with full tag history (fetch-depth: 0) and source_branch
# fetched and resolvable as "origin/<source_branch>".
#
# On success, prints the computed version (without a leading 'v') to stdout.
# On any invariant violation (channel-rank regression, release-line violation,
# SemVer gap/collision, or no explicit release reachable at all — bootstrap,
# out of scope per ADR 0006 Open Questions) prints an error to stderr and exits 1.

set -euo pipefail

source_branch="${1:?Usage: $0 <source_branch> <type> <version_bump>}"
type="${2:?Usage: $0 <source_branch> <type> <version_bump>}"
version_bump="${3:-patch}"

case "$type" in
  ga | alpha | beta | rc) ;;
  *)
    echo "error: type must be one of ga, alpha, beta, rc (got: $type)" >&2
    exit 1
    ;;
esac

case "$version_bump" in
  patch | minor | major) ;;
  *)
    echo "error: version_bump must be one of patch, minor, major (got: $version_bump)" >&2
    exit 1
    ;;
esac

rank() {
  case "$1" in
    alpha) echo 0 ;;
    beta) echo 1 ;;
    rc) echo 2 ;;
  esac
}

bump_version() {
  # $1 = X.Y.Z, $2 = patch|minor|major
  local base="$1" kind="$2" maj min pat
  IFS='.' read -r maj min pat <<<"$base"
  case "$kind" in
    patch) echo "${maj}.${min}.$((pat + 1))" ;;
    minor) echo "${maj}.$((min + 1)).0" ;;
    major) echo "$((maj + 1)).0.0" ;;
  esac
}

# Latest explicit release (GA or named pre-release) reachable from source_branch.
latest_tag=$(git describe --tags --match "v[0-9]*.[0-9]*.[0-9]*" --abbrev=0 "origin/${source_branch}" 2>/dev/null || echo "")

if [[ -z "$latest_tag" ]]; then
  echo "error: no explicit release reachable from '${source_branch}' — bootstrapping the first release on this history is out of scope (ADR 0006 Open Questions)" >&2
  exit 1
fi

latest="${latest_tag#v}"

# Release-line constraint (ADR 0003 + ADR 0006): a release/X.Y source may only
# ever produce vX.Y.* — minor/major bumps from such a source are a hard failure.
is_release_line=false
line=""
if [[ "$source_branch" =~ ^release/([0-9]+\.[0-9]+)$ ]]; then
  is_release_line=true
  line="${BASH_REMATCH[1]}"
fi

target=""

# Pre-release tags are X.Y.Z-<channel>.N (ADR-0018). The former form
# X.Y.Z-<channel>NN is still read, so an older line keeps working, but never
# written.
if [[ "$latest" =~ ^([0-9]+\.[0-9]+\.[0-9]+)-(alpha|beta|rc)(\.?)([0-9]+)$ ]]; then
  latest_base="${BASH_REMATCH[1]}"
  latest_channel="${BASH_REMATCH[2]}"
  latest_legacy=false
  [[ -z "${BASH_REMATCH[3]}" ]] && latest_legacy=true
  latest_counter="${BASH_REMATCH[4]}"

  if [[ "$type" == "ga" ]]; then
    # Promote the pre-release to GA. version_bump does not apply here — there is
    # no "latest GA" to bump from, the version is fixed by the promoted pre-release.
    target="$latest_base"
  else
    if (($(rank "$type") < $(rank "$latest_channel"))); then
      echo "error: cannot prepare '${type}' — latest reachable release '${latest_tag}' is already at channel '${latest_channel}', which ranks higher. A release cannot move backward in channel maturity for the same target version." >&2
      exit 1
    elif [[ "$type" == "$latest_channel" ]]; then
      # SemVer sorts "alpha.5" below "alpha04": continuing a channel opened in
      # the former form would publish a version older than the previous one.
      if [[ "$latest_legacy" == "true" ]]; then
        echo "error: cannot prepare '${type}' — latest reachable release '${latest_tag}' uses the former pre-release form; '${latest_base}-${type}.$((10#${latest_counter} + 1))' would sort below it in SemVer. Move to a higher channel or to GA instead." >&2
        exit 1
      fi
      target="${latest_base}-${type}.$((10#${latest_counter} + 1))"
    else
      target="${latest_base}-${type}.1"
    fi
  fi
elif [[ "$latest" =~ ^[0-9]+\.[0-9]+\.[0-9]+$ ]]; then
  # Latest reachable release is a GA.
  if [[ "$type" == "ga" ]]; then
    target=$(bump_version "$latest" "$version_bump")
  else
    target="$(bump_version "$latest" "$version_bump")-${type}.1"
  fi
else
  echo "error: could not parse latest reachable release tag '${latest_tag}'" >&2
  exit 1
fi

if [[ "$is_release_line" == "true" ]]; then
  target_line=$(grep -oE '^[0-9]+\.[0-9]+' <<<"$target")
  if [[ "$target_line" != "$line" ]]; then
    echo "error: source_branch 'release/${line}' may only produce ${line}.* releases; computed target '${target}' falls outside that line (version_bump: minor/major is invalid from a release/X.Y source)" >&2
    exit 1
  fi
fi

# SemVer gap check: the target must not already exist, and nothing already
# released may be ahead of it.
if git rev-parse -q --verify "refs/tags/v${target}" >/dev/null; then
  echo "error: tag 'v${target}' already exists — refusing to recompute an existing release" >&2
  exit 1
fi

# Which existing tags can supersede this candidate depends on where it is cut
# from, and the two cases are genuinely different:
#
#   release/X.Y — a maintenance line is *expected* to sit behind newer minors.
#                 Releasing 4.23.3 while v4.24.0 exists is the normal backport
#                 flow, so only this line's own tags are relevant.
#
#   dev (any other branch) — the candidate must be the newest release overall.
#                 If any release is already ahead of it, the branch is behind
#                 and the candidate would be superseded the moment it shipped.
#                 Scanning only its own X.Y line missed this entirely: with
#                 v4.25.0 already out, preparing 4.24.1 from dev used to pass.
if [[ "$is_release_line" == "true" ]]; then
  tag_scope="v${line}.*"
else
  tag_scope="v[0-9]*.[0-9]*.[0-9]*"
fi

while IFS= read -r existing_tag; do
  [[ -z "$existing_tag" ]] && continue
  existing="${existing_tag#v}"
  # Skip tags that are not plain release versions — `semver compare` rejects
  # them, which under `set -e` would abort the whole computation.
  [[ "$existing" =~ ^[0-9]+\.[0-9]+\.[0-9]+(-[0-9A-Za-z.-]+)?$ ]] || continue
  if [[ "$("$(dirname "$0")/semver" compare "$existing" "$target")" == "1" ]]; then
    echo "error: existing tag '${existing_tag}' is already ahead of computed target 'v${target}' — this candidate would be superseded or create a version gap" >&2
    exit 1
  fi
done < <(git tag -l "$tag_scope")

echo "$target"
