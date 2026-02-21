# SPDX-License-Identifier: PMPL-1.0-or-later
# SPDX-FileCopyrightText: 2025 Hyperpolymath

module Proven
  # Safe string operations for XSS prevention and sanitization.
  module SafeString
    # HTML entity map for escaping.
    HTML_ENTITIES = {
      '&' => "&amp;",
      '<' => "&lt;",
      '>' => "&gt;",
      '"' => "&quot;",
      '\'' => "&#x27;",
    }

    # Escape HTML special characters.
    def self.escape_html(input : String) : String
      result = String::Builder.new
      input.each_char do |c|
        if entity = HTML_ENTITIES[c]?
          result << entity
        else
          result << c
        end
      end
      result.to_s
    end

    # Escape for SQL (single quotes).
    def self.escape_sql(input : String) : String
      input.gsub("'", "''")
    end

    # Escape for JavaScript strings.
    def self.escape_js(input : String) : String
      result = String::Builder.new
      input.each_char do |c|
        case c
        when '\\'
          result << "\\\\"
        when '"'
          result << "\\\""
        when '\''
          result << "\\'"
        when '\n'
          result << "\\n"
        when '\r'
          result << "\\r"
        when '\t'
          result << "\\t"
        when '<'
          result << "\\x3c"
        when '>'
          result << "\\x3e"
        else
          result << c
        end
      end
      result.to_s
    end

    # URL encode a string.
    def self.url_encode(input : String) : String
      URI.encode_www_form(input)
    end

    # URL decode a string.
    def self.url_decode(input : String) : String?
      begin
        URI.decode_www_form(input)
      rescue
        nil
      end
    end

    # Sanitize string to only allow safe characters.
    def self.sanitize(input : String, allowed : String = "a-zA-Z0-9_-") : String
      regex = Regex.new("[^#{allowed}]")
      input.gsub(regex, "")
    end

    # Default sanitization (alphanumeric + underscore + hyphen).
    def self.sanitize_default(input : String) : String
      sanitize(input)
    end

    # Convert to slug.
    def self.slugify(input : String) : String
      input.downcase
        .gsub(/[^a-z0-9\s-]/, "")
        .gsub(/[\s_]+/, "-")
        .gsub(/-+/, "-")
        .strip("-")
    end

    # Truncate string safely.
    def self.truncate(input : String, max_length : Int32, suffix : String = "...") : String
      return input if input.size <= max_length
      return suffix if max_length <= suffix.size
      input[0, max_length - suffix.size] + suffix
    end

    # Remove control characters.
    def self.strip_control_chars(input : String) : String
      input.gsub(/[\x00-\x1f\x7f]/, "")
    end

    # Normalize whitespace.
    def self.normalize_whitespace(input : String) : String
      input.gsub(/\s+/, " ").strip
    end

    # Check if string contains only ASCII.
    def self.ascii_only?(input : String) : Bool
      input.each_char.all? { |c| c.ord < 128 }
    end

    # Check if string contains only printable ASCII.
    def self.printable_ascii?(input : String) : Bool
      input.each_char.all? { |c| c.ord >= 32 && c.ord < 127 }
    end

    # Base64 encode.
    def self.base64_encode(input : String) : String
      Base64.strict_encode(input)
    end

    # Base64 decode.
    def self.base64_decode(input : String) : String?
      begin
        Base64.decode_string(input)
      rescue
        nil
      end
    end

    # Hex encode bytes.
    def self.hex_encode(input : Bytes) : String
      input.hexstring
    end

    # Hex decode to bytes.
    def self.hex_decode(input : String) : Bytes?
      return nil if input.size % 2 != 0
      begin
        input.hexbytes
      rescue
        nil
      end
    end
  end
end

require "uri"
require "base64"
