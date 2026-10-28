#!/usr/bin/env bash
# Put a tool from the nixpkgs locked by flake.lock on the PATH of the next
# steps of a GitHub Actions job: builds nixpkgs#<attribute> and appends the
# bin directory of the output holding <binary> to $GITHUB_PATH (a package may
# have several outputs, e.g. a man page one).
#
# Usage: bin/install-nix-tool.sh <attribute> [<binary>]
#   binary defaults to the attribute name. Run from the repository root.
set -euo pipefail

attr=${1:?usage: $0 <attribute> [<binary>]}
binary=${2:-$attr}

while IFS= read -r out; do
  if [[ -x "${out}/bin/${binary}" ]]; then
    echo "${out}/bin" >>"${GITHUB_PATH:?GITHUB_PATH is not set}"
    echo "${binary}: ${out}/bin/${binary}"
    exit 0
  fi
done < <(nix build --inputs-from . "nixpkgs#${attr}" --no-link --print-out-paths)

echo "::error::nixpkgs#${attr} has no bin/${binary}" >&2
exit 1
