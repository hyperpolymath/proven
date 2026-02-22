# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

# safe-validators.nix -- Email, URL, IPv4, and JSON validators via the proven CLI.
#
# All computation is performed by the proven CLI binary, which delegates to
# the Idris 2 formally verified implementation through the Zig FFI bridge.
# No validation logic is reimplemented in Nix.
#
# These functions are useful for validating configuration values in Nix
# expressions (e.g., NixOS module options, flake inputs, derivation args).
#
# Usage:
#   let v = import ./safe-validators.nix { inherit pkgs proven-cli; };
#   in v.isValidEmail "user@example.com"     # true
#      v.isValidEmail "not-an-email"          # false
#      v.isValidIpv4 "192.168.1.1"           # true
#      v.isValidJson "{\"key\": \"value\"}"   # true

{ pkgs, proven-cli }:

let
  # Run a proven subcommand with a string input and return a boolean.
  # Returns true/false as Nix boolean, or null on error.
  runValidateBool = module: subcommand: input:
    let
      inputFile = pkgs.writeText "proven-validate-input" input;
      result = pkgs.runCommand "proven-validate-${subcommand}" {
        nativeBuildInputs = [ proven-cli ];
      } ''
        if proven ${module} ${subcommand} --file ${inputFile} > $out 2>/dev/null; then
          true
        elif proven ${module} ${subcommand} "$(cat ${inputFile})" > $out 2>/dev/null; then
          true
        else
          echo "" > $out
        fi
      '';
      output = builtins.replaceStrings ["\n"] [""] (builtins.readFile result);
    in
      if output == "true" then true
      else if output == "false" then false
      else null;

  # Run a proven subcommand and return the raw string output.
  runValidateString = module: subcommand: input:
    let
      inputFile = pkgs.writeText "proven-validate-input" input;
      result = pkgs.runCommand "proven-validate-${subcommand}" {
        nativeBuildInputs = [ proven-cli ];
      } ''
        if proven ${module} ${subcommand} --file ${inputFile} > $out 2>/dev/null; then
          true
        elif proven ${module} ${subcommand} "$(cat ${inputFile})" > $out 2>/dev/null; then
          true
        else
          echo "" > $out
        fi
      '';
      output = builtins.replaceStrings ["\n"] [""] (builtins.readFile result);
    in
      if output == "" then null else output;

in {
  # --------------------------------------------------------------------------
  # Email validation (RFC 5321 simplified)
  # --------------------------------------------------------------------------

  # Validate an email address.
  # Returns true if valid, false if invalid, null on error.
  isValidEmail = email: runValidateBool "email" "is-valid" email;

  # --------------------------------------------------------------------------
  # URL parsing and validation
  # --------------------------------------------------------------------------

  # Parse a URL and return the scheme, or null if invalid.
  parseUrlScheme = url: runValidateString "url" "parse-scheme" url;

  # Parse a URL and return the host, or null if invalid.
  parseUrlHost = url: runValidateString "url" "parse-host" url;

  # --------------------------------------------------------------------------
  # IPv4 validation
  # --------------------------------------------------------------------------

  # Validate an IPv4 address string (e.g., "192.168.1.1").
  # Returns true if valid, false if invalid, null on error.
  isValidIpv4 = addr: runValidateBool "network" "parse-ipv4" addr;

  # Check if an IPv4 address is private (RFC 1918).
  # Returns true/false, or null on error.
  isPrivateIpv4 = addr: runValidateBool "network" "is-private" addr;

  # Check if an IPv4 address is loopback (127.0.0.0/8).
  # Returns true/false, or null on error.
  isLoopbackIpv4 = addr: runValidateBool "network" "is-loopback" addr;

  # --------------------------------------------------------------------------
  # JSON validation
  # --------------------------------------------------------------------------

  # Check if a string is valid JSON.
  # Returns true/false, or null on error.
  isValidJson = input: runValidateBool "json" "is-valid" input;

  # Get the JSON root value type ("null", "bool", "number", "string",
  # "array", "object", or "invalid").
  # Returns the type as a string, or null on error.
  jsonType = input: runValidateString "json" "get-type" input;

  # --------------------------------------------------------------------------
  # DateTime validation (ISO 8601)
  # --------------------------------------------------------------------------

  # Parse an ISO 8601 datetime string and return formatted output.
  # Returns the formatted datetime, or null on parse failure.
  parseDatetime = input: runValidateString "datetime" "parse" input;

  # Check if a year is a leap year.
  # Returns true/false, or null on error.
  isLeapYear = year:
    let
      result = pkgs.runCommand "proven-is-leap-year" {
        nativeBuildInputs = [ proven-cli ];
      } ''
        if proven datetime is-leap-year ${toString year} > $out 2>/dev/null; then
          true
        else
          echo "" > $out
        fi
      '';
      output = builtins.replaceStrings ["\n"] [""] (builtins.readFile result);
    in
      if output == "true" then true
      else if output == "false" then false
      else null;

  # --------------------------------------------------------------------------
  # Version parsing (SemVer)
  # --------------------------------------------------------------------------

  # Parse a semantic version string (e.g., "1.2.3-alpha").
  # Returns the normalized version string, or null on parse failure.
  parseVersion = input: runValidateString "version" "parse" input;

  # Module metadata
  _meta = {
    name = "safe-validators";
    description = "Email, URL, IPv4, JSON, datetime, and version validators via proven CLI";
    functions = [
      "isValidEmail" "parseUrlScheme" "parseUrlHost"
      "isValidIpv4" "isPrivateIpv4" "isLoopbackIpv4"
      "isValidJson" "jsonType"
      "parseDatetime" "isLeapYear"
      "parseVersion"
    ];
  };
}
