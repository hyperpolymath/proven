# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

# proven.nix -- Top-level Nix helper library for the proven safety library.
#
# Aggregates all safe-* sub-modules into a single attribute set.
# Each function shells out to the proven CLI, which delegates to the
# Idris 2 formally verified implementation via the Zig FFI bridge.
#
# NO logic is reimplemented in Nix. All computation is performed by the CLI.
#
# Usage:
#   let proven = import ./proven.nix { inherit pkgs proven-cli; };
#   in proven.safeMath.addChecked 100 200
#      proven.safeString.escapeHtml "<script>alert(1)</script>"
#      proven.safeValidators.isValidEmail "user@example.com"

{ pkgs, proven-cli }:

let
  safeMath = import ./safe-math.nix { inherit pkgs proven-cli; };
  safeString = import ./safe-string.nix { inherit pkgs proven-cli; };
  safePath = import ./safe-path.nix { inherit pkgs proven-cli; };
  safeValidators = import ./safe-validators.nix { inherit pkgs proven-cli; };

  # Helper to invoke the proven CLI and capture stdout.
  # Returns null on non-zero exit code.
  runProven = args:
    let
      result = pkgs.runCommand "proven-invoke" {
        nativeBuildInputs = [ proven-cli ];
      } ''
        proven ${args} > $out 2>/dev/null || echo "__PROVEN_ERROR__" > $out
      '';
      output = builtins.readFile result;
    in
      if output == "__PROVEN_ERROR__\n" then null
      else builtins.replaceStrings ["\n"] [""] output;

  # Library version
  version = "0.9.0";

in {
  inherit safeMath safeString safePath safeValidators;
  inherit runProven version;

  # Lifecycle helpers (informational; Nix evaluations are ephemeral)
  meta = {
    description = "Proven safety library -- Nix bindings via CLI";
    homepage = "https://github.com/hyperpolymath/proven";
    license = "PMPL-1.0-or-later";
    maintainer = "Jonathan D.A. Jewell <jonathan.jewell@open.ac.uk>";
    moduleCount = 41;
    cliRequired = true;
    note = ''
      Nix cannot perform direct C FFI. All proven operations shell out to
      the proven CLI binary. Results are captured as Nix strings and parsed.
      This means operations are evaluated at Nix evaluation time via IFD
      (import from derivation) or at build time via runCommand.
    '';
  };
}
