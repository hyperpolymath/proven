# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

# safe-string.nix -- Safe string operations via the proven CLI.
#
# All computation is performed by the proven CLI binary, which delegates to
# the Idris 2 formally verified implementation through the Zig FFI bridge.
# No string escaping or validation logic is reimplemented in Nix.
#
# Each function invokes `proven string <operation> <args>` and parses the
# output. On error, null is returned.
#
# Usage:
#   let str = import ./safe-string.nix { inherit pkgs proven-cli; };
#   in str.escapeHtml "<script>alert(1)</script>"
#      str.escapeSql "O'Brien"
#      str.isValidUtf8 "hello"

{ pkgs, proven-cli }:

let
  # Run a proven string subcommand with a single string argument.
  # Returns the CLI output as a string, or null on error.
  runString = subcommand: input:
    let
      # Write input to a file to avoid shell escaping issues
      inputFile = pkgs.writeText "proven-string-input" input;
      result = pkgs.runCommand "proven-string-${subcommand}" {
        nativeBuildInputs = [ proven-cli ];
      } ''
        if proven string ${subcommand} --file ${inputFile} > $out 2>/dev/null; then
          true
        elif proven string ${subcommand} "$(cat ${inputFile})" > $out 2>/dev/null; then
          true
        else
          echo "" > $out
        fi
      '';
      output = builtins.readFile result;
      trimmed = builtins.replaceStrings ["\n"] [""] output;
    in
      if trimmed == "" then null else trimmed;

  # Run a proven string subcommand that returns a boolean ("true"/"false").
  runStringBool = subcommand: input:
    let
      raw = runString subcommand input;
    in
      if raw == "true" then true
      else if raw == "false" then false
      else null;

in {
  # Check if a byte sequence is valid UTF-8.
  # Returns true/false, or null on error.
  isValidUtf8 = input: runStringBool "is-valid-utf8" input;

  # Escape a string for safe use in SQL queries.
  # Doubles single quotes. Returns escaped string, or null on error.
  escapeSql = input: runString "escape-sql" input;

  # Escape a string for safe use in HTML.
  # Escapes <, >, &, ", and ' characters. Returns escaped string, or null on error.
  escapeHtml = input: runString "escape-html" input;

  # Escape a string for safe use in JavaScript string literals.
  # Returns escaped string, or null on error.
  escapeJs = input: runString "escape-js" input;

  # Module metadata
  _meta = {
    name = "safe-string";
    description = "Safe string operations via proven CLI";
    functions = [ "isValidUtf8" "escapeSql" "escapeHtml" "escapeJs" ];
  };
}
