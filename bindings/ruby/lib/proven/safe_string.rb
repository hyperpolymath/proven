# SPDX-License-Identifier: Apache-2.0
# SPDX-FileCopyrightText: 2025 Hyperpolymath
# frozen_string_literal: true

require "cgi"
require "uri"

module Proven
  # Safe string operations for escaping and sanitization.
  module SafeString
    class << self
      # Escape a string for safe HTML insertion.
      #
      # @param value [String]
      # @return [String]
      def escape_html(value)
        CGI.escapeHTML(value).gsub("'", "&#x27;")
      end

      # Escape a string for safe SQL interpolation.
      # Note: Prefer parameterized queries over string interpolation.
      #
      # @param value [String]
      # @return [String]
      def escape_sql(value)
        value.gsub("'", "''")
      end

      # Escape a string for safe JavaScript string literal insertion.
      #
      # @param value [String]
      # @return [String]
      def escape_js(value)
        value
          .gsub("\\", "\\\\")
          .gsub('"', '\\"')
          .gsub("'", "\\'")
          .gsub("\n", "\\n")
          .gsub("\r", "\\r")
          .gsub("\t", "\\t")
      end

      # Percent-encode a string for safe URL inclusion.
      #
      # @param value [String]
      # @return [String]
      def escape_url(value)
        URI.encode_www_form_component(value)
      end

      # Safely truncate a string to a maximum length.
      #
      # @param value [String]
      # @param max_length [Integer]
      # @param suffix [String]
      # @return [String]
      def truncate_safe(value, max_length, suffix: "...")
        return "" if max_length.negative?
        return value if value.length <= max_length
        return value[0, max_length] if max_length <= suffix.length

        value[0, max_length - suffix.length] + suffix
      end
    end
  end
end
