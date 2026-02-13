#!/usr/bin/env bash
# SPDX-License-Identifier: Apache-2.0
# Post-create script for devcontainer setup

set -euo pipefail

echo "==> Installing Zig..."
ZIG_VERSION="0.13.0"
curl -L "https://ziglang.org/download/${ZIG_VERSION}/zig-linux-x86_64-${ZIG_VERSION}.tar.xz" | tar -xJ
sudo mv "zig-linux-x86_64-${ZIG_VERSION}" /opt/zig
sudo ln -sf /opt/zig/zig /usr/local/bin/zig

echo "==> Installing ZLS (Zig Language Server)..."
ZLS_VERSION="0.13.0"
curl -L "https://github.com/zigtools/zls/releases/download/${ZLS_VERSION}/zls-x86_64-linux.tar.xz" | tar -xJ
sudo mv zls /usr/local/bin/

echo "==> Installing Deno..."
# WARNING: Pipe-to-shell is unsafe — download and verify first
curl -fsSL https://deno.land/install.sh | sh
echo 'export DENO_INSTALL="$HOME/.deno"' >> ~/.zshrc
echo 'export PATH="$DENO_INSTALL/bin:$PATH"' >> ~/.zshrc

echo "==> Installing Idris 2 via pack..."
git clone https://github.com/stefan-hoeck/idris2-pack.git /tmp/pack
cd /tmp/pack && make micropack
echo 'export PATH="$HOME/.pack/bin:$PATH"' >> ~/.zshrc
export PATH="$HOME/.pack/bin:$PATH"
pack install-deps

echo "==> Installing Python dependencies..."
pip install --user pytest cffi black mypy

echo "==> Setup complete!"
echo "    Restart your terminal or run: source ~/.zshrc"
