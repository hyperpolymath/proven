# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

# default.nix -- Nix derivation that builds and wraps libproven.
#
# Since Nix is a purely functional configuration language with no direct C FFI,
# this derivation builds the proven CLI from source and provides helper
# functions that shell out to the CLI for validation and safe operations.
#
# All computation is performed by the proven CLI (backed by Idris 2 verified
# code via Zig FFI). No logic is reimplemented in Nix.
#
# Usage:
#   nix-build
#   nix-build -A proven-cli   # Just the CLI binary
#   nix-build -A lib           # Helper library for use in Nix expressions

{ pkgs ? import <nixpkgs> {} }:

let
  # Build the proven CLI from source.
  # The CLI wraps libproven (Idris 2 + Zig FFI) behind a command-line interface.
  proven-cli = pkgs.stdenv.mkDerivation {
    pname = "proven-cli";
    version = "0.9.0";

    src = ../../.;

    nativeBuildInputs = with pkgs; [
      zig
      idris2
    ];

    buildPhase = ''
      # Build the Idris 2 core library
      idris2 --build proven.ipkg || true

      # Build the Zig FFI bridge and CLI
      cd ffi/zig
      zig build -Doptimize=ReleaseSafe || true
      cd ../..

      # Build the CLI application
      if [ -d apps/cli ]; then
        cd apps/cli
        zig build -Doptimize=ReleaseSafe || true
        cd ../..
      fi
    '';

    installPhase = ''
      mkdir -p $out/bin $out/lib $out/include

      # Install CLI binary (search common output locations)
      for candidate in \
        ffi/zig/zig-out/bin/proven \
        apps/cli/zig-out/bin/proven \
        build/proven \
        zig-out/bin/proven; do
        if [ -f "$candidate" ]; then
          cp "$candidate" $out/bin/proven
          break
        fi
      done

      # Install shared library if available
      for lib in ffi/zig/zig-out/lib/libproven.so ffi/zig/zig-out/lib/libproven.a; do
        if [ -f "$lib" ]; then
          cp "$lib" $out/lib/
        fi
      done

      # Install C headers
      if [ -d bindings/c/include ]; then
        cp bindings/c/include/*.h $out/include/
      fi
    '';

    meta = with pkgs.lib; {
      description = "Formally verified safety library -- CLI interface";
      homepage = "https://github.com/hyperpolymath/proven";
      license = { spdxId = "PMPL-1.0-or-later"; free = true; };
      maintainers = [ "Jonathan D.A. Jewell <jonathan.jewell@open.ac.uk>" ];
      platforms = platforms.unix;
    };
  };

  # Import the helper libraries that wrap the CLI.
  lib = import ./lib/proven.nix { inherit pkgs proven-cli; };

in {
  inherit proven-cli lib;

  # Default output: the CLI binary
  default = proven-cli;

  # Expose sub-libraries for targeted imports
  safeMath = import ./lib/safe-math.nix { inherit pkgs proven-cli; };
  safeString = import ./lib/safe-string.nix { inherit pkgs proven-cli; };
  safePath = import ./lib/safe-path.nix { inherit pkgs proven-cli; };
  safeValidators = import ./lib/safe-validators.nix { inherit pkgs proven-cli; };
}
