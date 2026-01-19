#!/usr/bin/env bash
set -euo pipefail

# Update Guix and Nix pins to current upstream heads.

guix_rev=$(git ls-remote https://git.savannah.gnu.org/git/guix.git HEAD | awk '{print $1}')
nix_rev=$(git ls-remote https://github.com/NixOS/nixpkgs.git refs/heads/nixpkgs-unstable | awk '{print $1}')

if [[ -z "${guix_rev}" || -z "${nix_rev}" ]]; then
  echo "Failed to resolve Guix or nixpkgs revisions." >&2
  exit 1
fi

# Pin Guix channel commit.
sed -i -E "s/(commit \"[0-9a-f]+\")/commit \"${guix_rev}\"/" guix/channels.scm

# Pin Nix fetchGit rev.
sed -i -E "s/(rev = \"[0-9a-f]+\")/rev = \"${nix_rev}\"/" nix/shard.nix

echo "Pinned guix: ${guix_rev}"
echo "Pinned nixpkgs: ${nix_rev}"
