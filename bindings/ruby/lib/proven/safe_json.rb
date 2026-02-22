# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
# frozen_string_literal: true

# Safe JSON operations via FFI to libproven.
# ALL computation delegates to Idris 2 compiled code.

module Proven
  module SafeJson
    # JSON type enum values (must match ffi/zig/src/main.zig JsonType).
    module JsonType
      NULL    = 0
      BOOLEAN = 1
      NUMBER  = 2
      STRING  = 3
      ARRAY   = 4
      OBJECT  = 5
      INVALID = -1
    end

    # Map from enum to symbol names.
    TYPE_NAMES = {
      JsonType::NULL => :null,
      JsonType::BOOLEAN => :boolean,
      JsonType::NUMBER => :number,
      JsonType::STRING => :string,
      JsonType::ARRAY => :array,
      JsonType::OBJECT => :object,
      JsonType::INVALID => :invalid,
    }.freeze

    class << self
      # Check if a string is valid JSON.
      #
      # @param json_string [String]
      # @return [Boolean, nil]
      def valid?(json_string)
        return nil if json_string.nil?
        ptr, len = FFI.str_to_ptr(json_string)
        result = FFI.invoke_bool_result(
          "proven_json_is_valid",
          [Fiddle::TYPE_VOIDP, Fiddle::TYPE_SIZE_T],
          [ptr, len]
        )
        return nil unless result
        status, val = result
        status == FFI::STATUS_OK ? val : nil
      end

      # Get the top-level JSON type of a string.
      # Returns a symbol: :null, :boolean, :number, :string, :array, :object, :invalid.
      # Returns nil on error.
      #
      # @param json_string [String]
      # @return [Symbol, nil]
      def type_of(json_string)
        return nil if json_string.nil?
        ptr, len = FFI.str_to_ptr(json_string)
        type_val = FFI.invoke_i32(
          "proven_json_get_type",
          [Fiddle::TYPE_VOIDP, Fiddle::TYPE_SIZE_T],
          [ptr, len]
        )
        return nil if type_val.nil?
        TYPE_NAMES.fetch(type_val, :invalid)
      end
    end
  end
end
