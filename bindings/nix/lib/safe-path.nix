# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

# safe-path.nix -- Safe filesystem path operations via the proven CLI.
#
# All computation is performed by the proven CLI binary, which delegates to
# the Idris 2 formally verified implementation through the Zig FFI bridge.
# No path validation or sanitization logic is reimplemented in Nix.
#
# These functions are particularly useful in Nix derivations that accept
# user-supplied filenames, preventing directory traversal attacks.
#
# Usage:
#   let path = import ./safe-path.nix { inherit pkgs proven-cli; };
#   in path.hasTraversal "../../../etc/passwd"   # true
#      path.sanitizeFilename "../../malicious.sh" # "malicious.sh"

{ pkgs, proven-cli }:

let
  # Run a proven path subcommand with a single path argument.
  # Returns the CLI output as a string, or null on error.
  runPath = subcommand: input:
    let
      inputFile = pkgs.writeText "proven-path-input" input;
      result = pkgs.runCommand "proven-path-${subcommand}" {
        nativeBuildInputs = [ proven-cli ];
      } ''
        if proven path ${subcommand} --file ${inputFile} > $out 2>/dev/null; then
          true
        elif proven path ${subcommand} "$(cat ${inputFile})" > $out 2>/dev/null; then
          true
        else
          echo "" > $out
        fi
      '';
      output = builtins.readFile result;
      trimmed = builtins.replaceStrings ["\n"] [""] output;
    in
      if trimmed == "" then null else trimmed;

  # Run a proven path subcommand that returns a boolean ("true"/"false").
  runPathBool = subcommand: input:
    let
      raw = runPath subcommand input;
    in
      if raw == "true" then true
      else if raw == "false" then false
      else null;

in {
  # Check if a path contains directory traversal sequences ("..").
  # Returns true if traversal is detected, false otherwise, null on error.
  hasTraversal = path: runPathBool "has-traversal" path;

  # Sanitize a filename by removing dangerous characters.
  # Strips path separators, "..", null bytes, and control characters.
  # Returns sanitized filename as string, or null on error.
  sanitizeFilename = filename: runPath "sanitize-filename" filename;

  # Convenience: check that a path is safe (no traversal detected).
  # Returns true if the path is safe, false if traversal is detected.
  isSafe = path:
    let result = runPathBool "has-traversal" path;
    in if result == true then false
       else if result == false then true
       else null;

  # Module metadata
  _meta = {
    name = "safe-path";
    description = "Safe filesystem path operations via proven CLI";
    functions = [ "hasTraversal" "sanitizeFilename" "isSafe" ];
  };
}
