# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
# frozen_string_literal: true

# Safe currency operations via FFI to libproven.
# ALL computation delegates to Idris 2 compiled code.

module Proven
  module SafeCurrency
    # Parsed currency result from libproven.
    CurrencyInfo = Struct.new(:amount_minor, :currency_code, :decimal_places, keyword_init: true)

    class << self
      # Parse a currency string (e.g., "$10.50", "EUR 25.00").
      # Returns nil on invalid input.
      #
      # @param input [String]
      # @return [CurrencyInfo, nil]
      def parse(input)
        return nil if input.nil?
        ptr, len = FFI.str_to_ptr(input)

        # CurrencyResult = { i32 status, i64 amount_minor, [3]u8 code, u8 decimals }
        # = 4 + 4pad + 8 + 3 + 1 = 20 bytes (padded to 24)
        buf = Fiddle::Pointer.malloc(24, Fiddle::RUBY_FREE)

        fn = Fiddle::Function.new(
          FFI.handler["proven_currency_parse"],
          [Fiddle::TYPE_VOIDP, Fiddle::TYPE_VOIDP, Fiddle::TYPE_SIZE_T],
          Fiddle::TYPE_VOID,
          need_gvl: true
        )
        fn.call(buf, ptr, len)

        packed = buf.to_str(24)
        status = packed[0, 4].unpack1("l!")
        return nil unless status == FFI::STATUS_OK

        amount_minor = packed[8, 8].unpack1("q") # i64
        code_bytes = packed[16, 3]
        decimal_places = packed[19, 1].unpack1("C")

        CurrencyInfo.new(
          amount_minor: amount_minor,
          currency_code: code_bytes,
          decimal_places: decimal_places
        )
      rescue Fiddle::DLError, TypeError
        nil
      end

      # Format a monetary amount to string.
      # Returns nil on error.
      #
      # @param amount_minor [Integer] amount in minor units (cents, etc.)
      # @param currency_code [String] 3-letter ISO 4217 code
      # @param decimal_places [Integer] number of decimal places
      # @return [String, nil]
      def format(amount_minor, currency_code, decimal_places)
        return nil if currency_code.nil? || currency_code.length != 3

        # Pack the 3-byte currency code
        code_bytes = currency_code.bytes
        # proven_currency_format(amount_minor: i64, code: [3]u8, decimal_places: u8) -> StringResult
        # [3]u8 passed as i32 (packed), u8 as int
        code_packed = code_bytes[0] | (code_bytes[1] << 8) | (code_bytes[2] << 16)

        result = FFI.invoke_string_result(
          "proven_currency_format",
          [Fiddle::TYPE_LONG_LONG, Fiddle::TYPE_INT, Fiddle::TYPE_INT],
          [amount_minor, code_packed, decimal_places]
        )
        return nil unless result
        status, str = result
        status == FFI::STATUS_OK ? str : nil
      rescue Fiddle::DLError, TypeError
        nil
      end
    end
  end
end
