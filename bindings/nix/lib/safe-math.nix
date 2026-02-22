# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

# safe-math.nix -- Safe arithmetic operations via the proven CLI.
#
# All computation is performed by the proven CLI binary, which delegates to
# the Idris 2 formally verified implementation through the Zig FFI bridge.
# No arithmetic logic is reimplemented in Nix.
#
# Each function invokes `proven math <operation> <args>` and parses the
# output. On error (overflow, division by zero, etc.), null is returned.
#
# Usage:
#   let math = import ./safe-math.nix { inherit pkgs proven-cli; };
#   in math.addChecked 100 200        # "300"
#      math.divSafe 10 0              # null (division by zero)
#      math.clamp 0 100 150           # "100"

{ pkgs, proven-cli }:

let
  # Run a proven math subcommand and return the result as a string.
  # Returns null if the operation fails (overflow, division by zero, etc.).
  runMath = subcommand: args:
    let
      argStr = builtins.concatStringsSep " " (map toString args);
      result = pkgs.runCommand "proven-math-${subcommand}" {
        nativeBuildInputs = [ proven-cli ];
      } ''
        if proven math ${subcommand} ${argStr} > $out 2>/dev/null; then
          true
        else
          echo "" > $out
        fi
      '';
      output = builtins.replaceStrings ["\n"] [""] (builtins.readFile result);
    in
      if output == "" then null else output;

in {
  # Checked addition with overflow detection.
  # Returns the sum as a string, or null on overflow.
  addChecked = a: b: runMath "add-checked" [ a b ];

  # Checked subtraction with underflow detection.
  # Returns the difference as a string, or null on underflow.
  subChecked = a: b: runMath "sub-checked" [ a b ];

  # Checked multiplication with overflow detection.
  # Returns the product as a string, or null on overflow.
  mulChecked = a: b: runMath "mul-checked" [ a b ];

  # Safe division with zero-check.
  # Returns the quotient as a string, or null on division by zero.
  divSafe = numerator: denominator: runMath "div" [ numerator denominator ];

  # Safe modulo with zero-check.
  # Returns the remainder as a string, or null on division by zero.
  modSafe = numerator: denominator: runMath "mod" [ numerator denominator ];

  # Safe absolute value.
  # Returns the absolute value as a string, or null for INT64_MIN.
  absSafe = n: runMath "abs" [ n ];

  # Clamp a value to the range [lo, hi].
  # Always succeeds.
  clamp = lo: hi: value: runMath "clamp" [ lo hi value ];

  # Integer exponentiation with overflow checking.
  # Returns base^exp as a string, or null on overflow.
  powChecked = base: exp: runMath "pow-checked" [ base exp ];

  # Module metadata
  _meta = {
    name = "safe-math";
    description = "Safe arithmetic operations via proven CLI";
    functions = [
      "addChecked" "subChecked" "mulChecked" "divSafe"
      "modSafe" "absSafe" "clamp" "powChecked"
    ];
  };
}
