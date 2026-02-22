# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
#
# Proven Safety Library - Mustache Lambda Provider (Ruby)
#
# Provides Mustache lambda functions that call proven-cli via shell execution.
# Designed for use with the 'mustache' gem (or any spec-compliant renderer).
#
# ALL computation delegates to proven-cli. This file only handles:
#   - Spawning the CLI subprocess
#   - Parsing CLI output
#   - Returning results for Mustache template rendering
#
# Usage:
#   require_relative 'proven-helpers'
#   require 'mustache'
#
#   template = '{{#proven_validate_email}}user@example.com{{/proven_validate_email}}'
#   output = Mustache.render(template, ProvenHelpers.context)
#
# Prerequisites:
#   - Ruby >= 2.7
#   - mustache gem: gem install mustache
#   - proven-cli on PATH or PROVEN_CLI env var set

require 'open3'
require 'shellwords'

# Mustache lambda helpers for the proven safety library.
# Every lambda invokes proven-cli as a subprocess; no safety logic
# is reimplemented in Ruby.
module ProvenHelpers
  # Path to the proven-cli binary.
  PROVEN_CLI = ENV.fetch('PROVEN_CLI', 'proven-cli').freeze

  # --------------------------------------------------------------------------
  # CLI invocation
  # --------------------------------------------------------------------------

  # Call proven-cli with the given arguments and return stdout (trimmed).
  # Returns an error message string on failure (never raises).
  def self.call_cli(*args)
    cmd = [PROVEN_CLI, *args]
    stdout, stderr, status = Open3.capture3(*cmd)

    if status.success?
      stdout.strip
    else
      "[proven error: #{stderr.strip.empty? ? 'unknown failure' : stderr.strip}]"
    end
  rescue Errno::ENOENT
    '[proven error: proven-cli not found]'
  rescue StandardError => e
    "[proven error: #{e.message}]"
  end

  # --------------------------------------------------------------------------
  # Lambda factories
  # --------------------------------------------------------------------------

  # Create a lambda that splits the enclosed text into arguments for the CLI.
  def self.make_lambda(mod, subcommand)
    lambda do |text|
      args = text.strip.split(/\s+/)
      call_cli(mod, subcommand, *args)
    end
  end

  # Create a lambda that passes the enclosed text as a single argument.
  def self.make_single_arg_lambda(mod, subcommand)
    lambda do |text|
      call_cli(mod, subcommand, text.strip)
    end
  end

  # --------------------------------------------------------------------------
  # Context hash with all proven lambdas
  # --------------------------------------------------------------------------

  # Returns a Hash suitable for passing to Mustache.render as template data.
  # All values are lambdas that call proven-cli at render time.
  def self.context
    {
      # --- SafeMath ---
      # Usage: {{#proven_safe_add}}10 20{{/proven_safe_add}}
      'proven_safe_add'   => make_lambda('math', 'add'),

      # Usage: {{#proven_safe_sub}}30 10{{/proven_safe_sub}}
      'proven_safe_sub'   => make_lambda('math', 'sub'),

      # Usage: {{#proven_safe_mul}}6 7{{/proven_safe_mul}}
      'proven_safe_mul'   => make_lambda('math', 'mul'),

      # Usage: {{#proven_safe_div}}100 3{{/proven_safe_div}}
      'proven_safe_div'   => make_lambda('math', 'div'),

      # Usage: {{#proven_safe_mod}}17 5{{/proven_safe_mod}}
      'proven_safe_mod'   => make_lambda('math', 'mod'),

      # Usage: {{#proven_safe_abs}}-42{{/proven_safe_abs}}
      'proven_safe_abs'   => make_single_arg_lambda('math', 'abs'),

      # Usage: {{#proven_safe_pow}}2 10{{/proven_safe_pow}}
      'proven_safe_pow'   => make_lambda('math', 'pow'),

      # Usage: {{#proven_safe_clamp}}0 100 150{{/proven_safe_clamp}}
      'proven_safe_clamp' => make_lambda('math', 'clamp'),

      # --- Validation ---
      # Usage: {{#proven_validate_email}}user@example.com{{/proven_validate_email}}
      'proven_validate_email'    => make_single_arg_lambda('validate', 'email'),

      # Usage: {{#proven_validate_url}}https://example.com{{/proven_validate_url}}
      'proven_validate_url'      => make_single_arg_lambda('validate', 'url'),

      # Usage: {{#proven_validate_ipv4}}192.168.1.1{{/proven_validate_ipv4}}
      'proven_validate_ipv4'     => make_single_arg_lambda('network', 'parse-ipv4'),

      # Usage: {{#proven_validate_path}}../etc/passwd{{/proven_validate_path}}
      'proven_validate_path'     => make_single_arg_lambda('path', 'has-traversal'),

      # Usage: {{#proven_validate_json}}{"key": "value"}{{/proven_validate_json}}
      'proven_validate_json'     => make_single_arg_lambda('json', 'is-valid'),

      # Usage: {{#proven_validate_password}}MyP@ssw0rd!{{/proven_validate_password}}
      'proven_validate_password' => make_single_arg_lambda('password', 'validate'),

      # --- SafeString ---
      # Usage: {{#proven_sanitize_string}}<script>alert(1)</script>{{/proven_sanitize_string}}
      'proven_sanitize_string' => make_single_arg_lambda('string', 'escape-html'),

      # Usage: {{#proven_escape_sql}}Robert'; DROP TABLE--{{/proven_escape_sql}}
      'proven_escape_sql'      => make_single_arg_lambda('string', 'escape-sql'),

      # Usage: {{#proven_escape_js}}alert('xss'){{/proven_escape_js}}
      'proven_escape_js'       => make_single_arg_lambda('string', 'escape-js'),

      # --- SafeCrypto ---
      # Usage: {{#proven_hash_sha256}}hello world{{/proven_hash_sha256}}
      'proven_hash_sha256'    => make_single_arg_lambda('crypto', 'sha256'),

      # Usage: {{#proven_random_hex}}32{{/proven_random_hex}}
      'proven_random_hex'     => make_single_arg_lambda('crypto', 'random-bytes'),

      # Usage: {{#proven_checksum_crc32}}data{{/proven_checksum_crc32}}
      'proven_checksum_crc32' => make_single_arg_lambda('checksum', 'crc32'),

      # --- Hex encoding ---
      # Usage: {{#proven_hex_encode}}Hello{{/proven_hex_encode}}
      'proven_hex_encode' => make_single_arg_lambda('hex', 'encode'),

      # Usage: {{#proven_hex_decode}}48656c6c6f{{/proven_hex_decode}}
      'proven_hex_decode' => make_single_arg_lambda('hex', 'decode'),

      # --- SafeDateTime ---
      # Usage: {{#proven_format_datetime}}2026-01-15T10:30:00Z{{/proven_format_datetime}}
      'proven_format_datetime' => make_single_arg_lambda('datetime', 'parse'),

      # Usage: {{#proven_is_leap_year}}2024{{/proven_is_leap_year}}
      'proven_is_leap_year'    => make_single_arg_lambda('datetime', 'is-leap-year'),

      # --- SafeColor ---
      # Usage: {{#proven_parse_color}}#FF5733{{/proven_parse_color}}
      'proven_parse_color' => make_single_arg_lambda('color', 'parse-hex'),

      # --- SafeVersion ---
      # Usage: {{#proven_parse_version}}1.2.3-alpha{{/proven_parse_version}}
      'proven_parse_version'   => make_single_arg_lambda('version', 'parse'),

      # Usage: {{#proven_version_compare}}1.2.3 2.0.0{{/proven_version_compare}}
      'proven_version_compare' => make_lambda('version', 'compare'),

      # --- SafeUUID ---
      # Usage: {{#proven_uuid_v4}}{{/proven_uuid_v4}}
      'proven_uuid_v4'    => ->(_text) { call_cli('uuid', 'v4') },

      # Usage: {{#proven_parse_uuid}}550e8400-e29b-41d4-a716-446655440000{{/proven_parse_uuid}}
      'proven_parse_uuid' => make_single_arg_lambda('uuid', 'parse'),
    }
  end

  # --------------------------------------------------------------------------
  # Convenience: merge proven helpers into an existing context
  # --------------------------------------------------------------------------

  # Merge proven lambdas into an existing Hash context.
  # Does not overwrite existing keys.
  def self.merge_into(existing_context)
    context.each do |key, value|
      existing_context[key] = value unless existing_context.key?(key)
    end
    existing_context
  end
end

# ---------------------------------------------------------------------------
# Self-test (run directly: ruby proven-helpers.rb --test)
# ---------------------------------------------------------------------------

if __FILE__ == $PROGRAM_NAME && ARGV.include?('--test')
  puts 'Proven Mustache helpers (Ruby) - self-test'
  puts "CLI binary: #{ProvenHelpers::PROVEN_CLI}"
  puts ''

  tests = [
    %w[math add 10 20],
    %w[math div 100 0],
    %w[validate email user@example.com],
    %w[hex encode Hello],
    %w[version compare 1.0.0 2.0.0],
  ]

  tests.each do |args|
    result = ProvenHelpers.call_cli(*args)
    puts "  #{args.join(' ')} => #{result}"
  end

  puts ''
  puts 'Self-test complete.'
end
