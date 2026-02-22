# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
# frozen_string_literal: true

# Safe phone number operations via FFI to libproven.
# ALL computation delegates to Idris 2 compiled code.

module Proven
  module SafePhone
    # Parsed phone result from libproven.
    PhoneInfo = Struct.new(:country_code, :national_number, keyword_init: true)

    class << self
      # Parse a phone number string.
      # Returns nil on invalid input.
      #
      # @param input [String]
      # @return [PhoneInfo, nil]
      def parse(input)
        return nil if input.nil?
        ptr, len = FFI.str_to_ptr(input)

        # PhoneResult = { i32 status, u16 country_code, u64 national_number, bool is_valid }
        # Layout: 4(status) + 2(cc) + 2pad + 8(nn) + 1(valid) + 7pad = 24 bytes
        buf = Fiddle::Pointer.malloc(24, Fiddle::RUBY_FREE)

        fn = Fiddle::Function.new(
          FFI.handler["proven_phone_parse"],
          [Fiddle::TYPE_VOIDP, Fiddle::TYPE_VOIDP, Fiddle::TYPE_SIZE_T],
          Fiddle::TYPE_VOID,
          need_gvl: true
        )
        fn.call(buf, ptr, len)

        packed = buf.to_str(24)
        status = packed[0, 4].unpack1("l!")
        return nil unless status == FFI::STATUS_OK

        country_code = packed[4, 2].unpack1("S") # u16
        national_number = packed[8, 8].unpack1("Q") # u64
        is_valid = packed[16, 1].unpack1("C") != 0

        return nil unless is_valid

        PhoneInfo.new(
          country_code: country_code,
          national_number: national_number
        )
      rescue Fiddle::DLError, TypeError
        nil
      end

      # Check if a phone number string is valid.
      #
      # @param input [String]
      # @return [Boolean]
      def valid?(input)
        !parse(input).nil?
      end

      # Format a phone number in E.164 format.
      # Returns nil on error.
      #
      # @param country_code [Integer]
      # @param national_number [Integer]
      # @return [String, nil]
      def format_e164(country_code, national_number)
        result = FFI.invoke_string_result(
          "proven_phone_format_e164",
          [Fiddle::TYPE_SHORT, Fiddle::TYPE_LONG_LONG],
          [country_code, national_number]
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
