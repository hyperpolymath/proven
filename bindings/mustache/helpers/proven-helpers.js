// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
//
// Proven Safety Library - Mustache Lambda Provider (JavaScript / Deno)
//
// Provides Mustache lambda functions that call libproven via proven-cli.
// Designed for use with Deno and mustache.js (or any spec-compliant renderer).
//
// ALL computation delegates to proven-cli. This file only handles:
//   - Spawning the CLI subprocess
//   - Parsing CLI output
//   - Returning results for Mustache template rendering
//
// Usage with Deno:
//   import { provenHelpers } from "./proven-helpers.js";
//   import Mustache from "npm:mustache";
//
//   const template = '{{#proven_validate_email}}user@example.com{{/proven_validate_email}}';
//   const output = Mustache.render(template, provenHelpers());
//
// Prerequisites:
//   - Deno runtime (https://deno.land)
//   - proven-cli on PATH or PROVEN_CLI env var set

// ---------------------------------------------------------------------------
// CLI invocation
// ---------------------------------------------------------------------------

// Resolve the proven-cli binary path.
const PROVEN_CLI = Deno.env.get("PROVEN_CLI") || "proven-cli";

// Call proven-cli synchronously and return stdout as a trimmed string.
// Returns the error message on failure (never throws).
function callProvenCli(...args) {
  try {
    const command = new Deno.Command(PROVEN_CLI, {
      args: args,
      stdout: "piped",
      stderr: "piped",
    });
    const result = command.outputSync();

    if (result.success) {
      return new TextDecoder().decode(result.stdout).trim();
    }

    const stderr = new TextDecoder().decode(result.stderr).trim();
    return `[proven error: ${stderr || "unknown failure"}]`;
  } catch (err) {
    return `[proven error: ${err.message}]`;
  }
}

// ---------------------------------------------------------------------------
// Mustache lambda factory
//
// Each lambda receives the enclosed template text as its argument, calls
// proven-cli, and returns the result string for interpolation.
// ---------------------------------------------------------------------------

// Create a lambda that calls a proven-cli subcommand with the enclosed text
// split into arguments.
function makeLambda(module, subcommand) {
  return function () {
    return function (text, render) {
      const rendered = render(text).trim();
      const args = rendered.split(/\s+/);
      return callProvenCli(module, subcommand, ...args);
    };
  };
}

// Create a lambda that passes the entire enclosed text as a single argument.
function makeSingleArgLambda(module, subcommand) {
  return function () {
    return function (text, render) {
      const rendered = render(text).trim();
      return callProvenCli(module, subcommand, rendered);
    };
  };
}

// ---------------------------------------------------------------------------
// Exported helpers object
// ---------------------------------------------------------------------------

// Returns a context object with all proven lambda functions for use
// as Mustache template data.
export function provenHelpers() {
  return {
    // --- SafeMath ---
    // Usage: {{#proven_safe_add}}10 20{{/proven_safe_add}}
    proven_safe_add: makeLambda("math", "add"),

    // Usage: {{#proven_safe_sub}}30 10{{/proven_safe_sub}}
    proven_safe_sub: makeLambda("math", "sub"),

    // Usage: {{#proven_safe_mul}}6 7{{/proven_safe_mul}}
    proven_safe_mul: makeLambda("math", "mul"),

    // Usage: {{#proven_safe_div}}100 3{{/proven_safe_div}}
    proven_safe_div: makeLambda("math", "div"),

    // Usage: {{#proven_safe_mod}}17 5{{/proven_safe_mod}}
    proven_safe_mod: makeLambda("math", "mod"),

    // Usage: {{#proven_safe_abs}}-42{{/proven_safe_abs}}
    proven_safe_abs: makeSingleArgLambda("math", "abs"),

    // Usage: {{#proven_safe_pow}}2 10{{/proven_safe_pow}}
    proven_safe_pow: makeLambda("math", "pow"),

    // Usage: {{#proven_safe_clamp}}0 100 150{{/proven_safe_clamp}}
    proven_safe_clamp: makeLambda("math", "clamp"),

    // --- Validation ---
    // Usage: {{#proven_validate_email}}user@example.com{{/proven_validate_email}}
    proven_validate_email: makeSingleArgLambda("validate", "email"),

    // Usage: {{#proven_validate_url}}https://example.com{{/proven_validate_url}}
    proven_validate_url: makeSingleArgLambda("validate", "url"),

    // Usage: {{#proven_validate_ipv4}}192.168.1.1{{/proven_validate_ipv4}}
    proven_validate_ipv4: makeSingleArgLambda("network", "parse-ipv4"),

    // Usage: {{#proven_validate_path}}../etc/passwd{{/proven_validate_path}}
    proven_validate_path: makeSingleArgLambda("path", "has-traversal"),

    // Usage: {{#proven_validate_json}}{"key": "value"}{{/proven_validate_json}}
    proven_validate_json: makeSingleArgLambda("json", "is-valid"),

    // Usage: {{#proven_validate_password}}MyP@ssw0rd!{{/proven_validate_password}}
    proven_validate_password: makeSingleArgLambda("password", "validate"),

    // --- SafeString ---
    // Usage: {{#proven_sanitize_string}}<script>alert(1)</script>{{/proven_sanitize_string}}
    proven_sanitize_string: makeSingleArgLambda("string", "escape-html"),

    // Usage: {{#proven_escape_sql}}Robert'; DROP TABLE--{{/proven_escape_sql}}
    proven_escape_sql: makeSingleArgLambda("string", "escape-sql"),

    // Usage: {{#proven_escape_js}}alert('xss'){{/proven_escape_js}}
    proven_escape_js: makeSingleArgLambda("string", "escape-js"),

    // --- SafeCrypto ---
    // Usage: {{#proven_hash_sha256}}hello world{{/proven_hash_sha256}}
    proven_hash_sha256: makeSingleArgLambda("crypto", "sha256"),

    // Usage: {{#proven_random_hex}}32{{/proven_random_hex}}
    proven_random_hex: makeSingleArgLambda("crypto", "random-bytes"),

    // Usage: {{#proven_checksum_crc32}}data{{/proven_checksum_crc32}}
    proven_checksum_crc32: makeSingleArgLambda("checksum", "crc32"),

    // --- Hex encoding ---
    // Usage: {{#proven_hex_encode}}Hello{{/proven_hex_encode}}
    proven_hex_encode: makeSingleArgLambda("hex", "encode"),

    // Usage: {{#proven_hex_decode}}48656c6c6f{{/proven_hex_decode}}
    proven_hex_decode: makeSingleArgLambda("hex", "decode"),

    // --- SafeDateTime ---
    // Usage: {{#proven_format_datetime}}2026-01-15T10:30:00Z{{/proven_format_datetime}}
    proven_format_datetime: makeSingleArgLambda("datetime", "parse"),

    // Usage: {{#proven_is_leap_year}}2024{{/proven_is_leap_year}}
    proven_is_leap_year: makeSingleArgLambda("datetime", "is-leap-year"),

    // --- SafeColor ---
    // Usage: {{#proven_parse_color}}#FF5733{{/proven_parse_color}}
    proven_parse_color: makeSingleArgLambda("color", "parse-hex"),

    // --- SafeVersion ---
    // Usage: {{#proven_parse_version}}1.2.3-alpha{{/proven_parse_version}}
    proven_parse_version: makeSingleArgLambda("version", "parse"),

    // Usage: {{#proven_version_compare}}1.2.3 2.0.0{{/proven_version_compare}}
    proven_version_compare: makeLambda("version", "compare"),

    // --- SafeUUID ---
    // Usage: {{#proven_uuid_v4}}{{/proven_uuid_v4}}
    proven_uuid_v4: function () {
      return function (_text, _render) {
        return callProvenCli("uuid", "v4");
      };
    },

    // Usage: {{#proven_parse_uuid}}550e8400-e29b-41d4-a716-446655440000{{/proven_parse_uuid}}
    proven_parse_uuid: makeSingleArgLambda("uuid", "parse"),
  };
}

// ---------------------------------------------------------------------------
// Convenience: merge proven helpers into an existing context
// ---------------------------------------------------------------------------

// Merge proven lambdas into an existing Mustache context object.
// Does not overwrite existing keys.
export function withProvenHelpers(context) {
  const helpers = provenHelpers();
  for (const [key, value] of Object.entries(helpers)) {
    if (!(key in context)) {
      context[key] = value;
    }
  }
  return context;
}

// ---------------------------------------------------------------------------
// CLI self-test (run directly: deno run --allow-run proven-helpers.js --test)
// ---------------------------------------------------------------------------

if (import.meta.main && Deno.args.includes("--test")) {
  console.log("Proven Mustache helpers - self-test");
  console.log("CLI binary:", PROVEN_CLI);
  console.log("");

  const tests = [
    ["math", "add", "10", "20"],
    ["math", "div", "100", "0"],
    ["validate", "email", "user@example.com"],
    ["hex", "encode", "Hello"],
    ["version", "compare", "1.0.0", "2.0.0"],
  ];

  for (const args of tests) {
    const result = callProvenCli(...args);
    console.log(`  ${args.join(" ")} => ${result}`);
  }

  console.log("");
  console.log("Self-test complete.");
}
