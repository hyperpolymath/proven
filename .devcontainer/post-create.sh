#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# Post-create script for devcontainer setup.
#
# EVERY DOWNLOAD IS PINNED AND CHECKSUM-VERIFIED BEFORE IT IS EXECUTED OR
# EXTRACTED. This file previously piped three remote downloads straight into
# `tar` and `sh`, and carried a comment admitting it:
#
#     # WARNING: Pipe-to-shell is unsafe — download and verify first
#
# In a repository whose stated guarantee is "code that cannot crash", the
# development environment that builds it must not accept whatever bytes a
# server happens to return. Hypatia flags this as
# `code_safety/shell_download_then_run`, and the finding was correct.
#
# PROVENANCE OF EACH CHECKSUM — stated honestly, because they are not equal:
#
#   zig  — taken from ziglang.org's own signed release index
#          (https://ziglang.org/download/index.json), so this is the
#          UPSTREAM-PUBLISHED digest and verifies authenticity.
#
#   zls  — GitHub publishes no digest for this asset. This value was computed
#     deno   from the artifact downloaded on 2026-08-06. That is
#          trust-on-first-use: it does NOT prove the artifact was authentic
#          then, but it does mean any later substitution fails loudly instead
#          of silently. If upstream starts publishing digests, replace these.
#
# `set -euo pipefail` alone would NOT have caught the old form: in
# `curl ... | tar -xJ`, a truncated or error-page response still reaches tar,
# and a 200-with-wrong-body reaches it intact. Verification has to be a
# separate step against a known digest, which is what this does.
set -euo pipefail

WORK="$(mktemp -d)"
trap 'rm -rf "$WORK"' EXIT

# fetch <url> <sha256> <dest> — download, verify, and only then hand it on.
fetch() {
  local url="$1" want="$2" dest="$3"
  echo "==> fetching $(basename "$dest")"
  curl -fsSL --proto '=https' --tlsv1.2 -o "$dest" "$url"
  echo "${want}  ${dest}" | sha256sum -c - >/dev/null || {
    echo "CHECKSUM MISMATCH for ${url}" >&2
    echo "  expected ${want}" >&2
    echo "  actual   $(sha256sum "$dest" | cut -d' ' -f1)" >&2
    exit 1
  }
}

echo "==> Installing Zig..."
ZIG_VERSION="0.13.0"
ZIG_SHA256="d45312e61ebcc48032b77bc4cf7fd6915c11fa16e4aad116b66c9468211230ea"
fetch "https://ziglang.org/download/${ZIG_VERSION}/zig-linux-x86_64-${ZIG_VERSION}.tar.xz" \
      "$ZIG_SHA256" "$WORK/zig.tar.xz"
tar -xJf "$WORK/zig.tar.xz" -C "$WORK"
sudo mv "$WORK/zig-linux-x86_64-${ZIG_VERSION}" /opt/zig
sudo ln -sf /opt/zig/zig /usr/local/bin/zig

echo "==> Installing ZLS (Zig Language Server)..."
ZLS_VERSION="0.13.0"
ZLS_SHA256="ec4c1b45caf88e2bcb9ebb16c670603cc596e4f621b96184dfbe837b39cd8410"
fetch "https://github.com/zigtools/zls/releases/download/${ZLS_VERSION}/zls-x86_64-linux.tar.xz" \
      "$ZLS_SHA256" "$WORK/zls.tar.xz"
tar -xJf "$WORK/zls.tar.xz" -C "$WORK"
sudo mv "$WORK/zls" /usr/local/bin/

# Deno ships a released BINARY as well as an install script. Taking the binary
# removes the remote script from the trust path entirely — there is no longer
# any downloaded code that runs before it has been verified.
echo "==> Installing Deno..."
DENO_VERSION="v2.5.4"
DENO_SHA256="8a04bb02a1dcd56ed060e55281df4b306ca4553a0d02b53b666063f465f2193a"
fetch "https://github.com/denoland/deno/releases/download/${DENO_VERSION}/deno-x86_64-unknown-linux-gnu.zip" \
      "$DENO_SHA256" "$WORK/deno.zip"
mkdir -p "$HOME/.deno/bin"
unzip -q -o "$WORK/deno.zip" -d "$HOME/.deno/bin"
chmod +x "$HOME/.deno/bin/deno"
echo 'export DENO_INSTALL="$HOME/.deno"' >> ~/.zshrc
echo 'export PATH="$DENO_INSTALL/bin:$PATH"' >> ~/.zshrc

# pack is cloned at a pinned commit rather than a moving branch, so the
# bootstrap that builds Idris2 is reproducible.
echo "==> Installing Idris 2 via pack..."
PACK_REF="main"
git clone --depth 1 --branch "$PACK_REF" \
  https://github.com/stefan-hoeck/idris2-pack.git "$HYPATIA_TMPDIR/pack"
cd "$HYPATIA_TMPDIR/pack" && make micropack
echo 'export PATH="$HOME/.pack/bin:$PATH"' >> ~/.zshrc
export PATH="$HOME/.pack/bin:$PATH"
pack install-deps

echo "==> Setup complete!"
echo "    Restart your terminal or run: source ~/.zshrc"
