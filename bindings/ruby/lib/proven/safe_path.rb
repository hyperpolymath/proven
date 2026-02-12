# SPDX-License-Identifier: Apache-2.0
# SPDX-FileCopyrightText: 2025 Hyperpolymath
# frozen_string_literal: true

module Proven
  # Safe filesystem path operations with traversal attack prevention.
  module SafePath
    class << self
      # Check if a path contains directory traversal sequences.
      #
      # @param path [String]
      # @return [Boolean]
      def traversal?(path)
        path.include?("..") || path.include?("~")
      end

      # Check if a path is safe (no traversal attacks).
      #
      # @param path [String]
      # @return [Boolean]
      def safe?(path)
        !traversal?(path)
      end

      # Sanitize a filename by removing dangerous characters.
      #
      # @param filename [String]
      # @return [String]
      def sanitize_filename(filename)
        filename
          .gsub("..", "_")
          .gsub(%r{[/\\<>:"|?*\x00]}, "_")
      end

      # Safely join path components, rejecting traversal attempts.
      #
      # @param base [String]
      # @param parts [Array<String>]
      # @return [String, nil] Returns nil if traversal detected
      def safe_join(base, *parts)
        return nil if parts.any? { |part| traversal?(part) }

        sanitized = parts.map { |part| sanitize_filename(part) }
        File.join(base, *sanitized)
      end
    end
  end
end
