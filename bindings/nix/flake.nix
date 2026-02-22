# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

# flake.nix -- Nix flake wrapping the proven library derivation.
#
# Provides the proven CLI and helper libraries as flake outputs.
# All computation is performed by the proven CLI (Idris 2 verified code).
# No logic is reimplemented in Nix.
#
# Usage:
#   nix build .#proven-cli       # Build the CLI binary
#   nix build .#lib              # Build the helper library
#   nix develop                  # Enter a dev shell with proven available
#
#   # In another flake:
#   inputs.proven.url = "github:hyperpolymath/proven?dir=bindings/nix";
#   proven.lib.safeMath.addChecked 100 200

{
  description = "Proven -- formally verified safety library (Nix bindings)";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        proven = import ./default.nix { inherit pkgs; };
      in
      {
        # Packages exposed by this flake
        packages = {
          default = proven.default;
          proven-cli = proven.proven-cli;
          lib = proven.lib;
        };

        # Library attribute set for use in other Nix expressions
        lib = proven.lib;

        # Development shell with proven CLI available
        devShells.default = pkgs.mkShell {
          buildInputs = [ proven.proven-cli ];
          shellHook = ''
            echo "proven CLI available: $(proven --version 2>/dev/null || echo 'not built yet')"
          '';
        };

        # Overlay for integration with other Nix expressions
        overlays.default = final: prev: {
          proven-cli = proven.proven-cli;
          proven-lib = proven.lib;
        };
      }
    );
}
