# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
# frozen_string_literal: true

# Safe string operations via FFI to libproven.
# ALL computation delegates to Idris 2 compiled code.

module Proven
  module SafeString
    class << self
      # Check if a byte string is valid UTF-8.
      #
      # @param value [String]
      # @return [Boolean, nil]
      def valid_utf8?(value)
        return nil if value.nil?
        ptr, len = FFI.str_to_ptr(value)
        result = FFI.invoke_bool_result(
          "proven_string_is_valid_utf8",
          [Fiddle::TYPE_VOIDP, Fiddle::TYPE_SIZE_T],
          [ptr, len]
        )
        return nil unless result
        status, val = result
        status == FFI::STATUS_OK ? val : nil
      end

      # Escape a string for safe HTML insertion.
      # Returns nil on error.
      #
      # @param value [String]
      # @return [String, nil]
      def escape_html(value)
        return nil if value.nil?
        ptr, len = FFI.str_to_ptr(value)
        result = FFI.invoke_string_result(
          "proven_string_escape_html",
          [Fiddle::TYPE_VOIDP, Fiddle::TYPE_SIZE_T],
          [ptr, len]
        )
        return nil unless result
        status, str = result
        status == FFI::STATUS_OK ? str : nil
      end

      # Escape a string for safe SQL interpolation.
      # Returns nil on error.
      #
      # @param value [String]
      # @return [String, nil]
      def escape_sql(value)
        return nil if value.nil?
        ptr, len = FFI.str_to_ptr(value)
        result = FFI.invoke_string_result(
          "proven_string_escape_sql",
          [Fiddle::TYPE_VOIDP, Fiddle::TYPE_SIZE_T],
          [ptr, len]
        )
        return nil unless result
        status, str = result
        status == FFI::STATUS_OK ? str : nil
      end

      # Escape a string for safe JavaScript string literal insertion.
      # Returns nil on error.
      #
      # @param value [String]
      # @return [String, nil]
      def escape_js(value)
        return nil if value.nil?
        ptr, len = FFI.str_to_ptr(value)
        result = FFI.invoke_string_result(
          "proven_string_escape_js",
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
