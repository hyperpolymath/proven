# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath)
#
# Nix flake development environment for proven.
# Usage: nix develop
{
  description = "Proven — formally verified safety via Idris2";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let pkgs = nixpkgs.legacyPackages.${system};
      in {
        devShells.default = pkgs.mkShell {
          buildInputs = with pkgs; [
            # Idris2 — formal verification and ABI definitions
            idris2

            # Zig — FFI implementation
            zig

            # Rust — cargo-based components and bindings
            rustc
            cargo
            clippy
            rustfmt

            # Build tooling
            pkg-config
            gnumake
          ];

          shellHook = ''
            echo "proven dev shell — idris2 + zig + cargo"
          '';
        };
      });
}
