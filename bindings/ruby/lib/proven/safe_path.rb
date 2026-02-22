# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
# frozen_string_literal: true

# Safe path operations via FFI to libproven.
# ALL computation delegates to Idris 2 compiled code.

module Proven
  module SafePath
    class << self
      # Check if a path contains directory traversal sequences.
      #
      # @param path [String]
      # @return [Boolean, nil]
      def traversal?(path)
        return nil if path.nil?
        ptr, len = FFI.str_to_ptr(path)
        result = FFI.invoke_bool_result(
          "proven_path_has_traversal",
          [Fiddle::TYPE_VOIDP, Fiddle::TYPE_SIZE_T],
          [ptr, len]
        )
        return nil unless result
        status, val = result
        status == FFI::STATUS_OK ? val : nil
      end

      # Check if a path is safe (no traversal attacks).
      #
      # @param path [String]
      # @return [Boolean, nil]
      def safe?(path)
        has_traversal = traversal?(path)
        return nil if has_traversal.nil?
        !has_traversal
      end

      # Sanitize a filename by removing dangerous characters.
      # Returns nil on error.
      #
      # @param filename [String]
      # @return [String, nil]
      def sanitize_filename(filename)
        return nil if filename.nil?
        ptr, len = FFI.str_to_ptr(filename)
        result = FFI.invoke_string_result(
          "proven_path_sanitize_filename",
          [Fiddle::TYPE_VOIDP, Fiddle::TYPE_SIZE_T],
          [ptr, len]
        )
        return nil unless result
        status, str = result
        status == FFI::STATUS_OK ? str : nil
      end
    end
  end
end
