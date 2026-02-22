# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
# frozen_string_literal: true

# Safe datetime operations via FFI to libproven.
# ALL computation delegates to Idris 2 compiled code.

module Proven
  module SafeDatetime
    # Parsed datetime from libproven.
    DateTimeInfo = Struct.new(
      :year, :month, :day, :hour, :minute, :second,
      :nanosecond, :tz_offset_minutes,
      keyword_init: true
    )

    class << self
      # Parse an ISO 8601 datetime string.
      # Returns nil on invalid input.
      #
      # @param datetime_string [String]
      # @return [DateTimeInfo, nil]
      def parse(datetime_string)
        return nil if datetime_string.nil?
        ptr, len = FFI.str_to_ptr(datetime_string)

        # DateTimeResult = { i32 status, DateTime { i32 year, u8 month, u8 day,
        #   u8 hour, u8 minute, u8 second, u32 nanosecond, i16 tz_offset } }
        # Layout: 4(status) + 4(year) + 1+1+1+1+1(month..second) + 3pad + 4(nano) + 2(tz) + 2pad = 24 bytes
        buf = Fiddle::Pointer.malloc(24, Fiddle::RUBY_FREE)

        fn = Fiddle::Function.new(
          FFI.handler["proven_datetime_parse"],
          [Fiddle::TYPE_VOIDP, Fiddle::TYPE_VOIDP, Fiddle::TYPE_SIZE_T],
          Fiddle::TYPE_VOID,
          need_gvl: true
        )
        fn.call(buf, ptr, len)

        packed = buf.to_str(24)
        status = packed[0, 4].unpack1("l!")
        return nil unless status == FFI::STATUS_OK

        year = packed[4, 4].unpack1("l!") # i32
        month = packed[8, 1].unpack1("C")
        day = packed[9, 1].unpack1("C")
        hour = packed[10, 1].unpack1("C")
        minute = packed[11, 1].unpack1("C")
        second = packed[12, 1].unpack1("C")
        # 3 bytes padding (13-15)
        nanosecond = packed[16, 4].unpack1("L") # u32
        tz_offset = packed[20, 2].unpack1("s!") # i16

        DateTimeInfo.new(
          year: year,
          month: month,
          day: day,
          hour: hour,
          minute: minute,
          second: second,
          nanosecond: nanosecond,
          tz_offset_minutes: tz_offset
        )
      rescue Fiddle::DLError, TypeError
        nil
      end

      # Check if a year is a leap year.
      #
      # @param year [Integer]
      # @return [Boolean, nil]
      def leap_year?(year)
        FFI.invoke_bool(
          "proven_datetime_is_leap_year",
          [Fiddle::TYPE_INT],
          [year]
        )
      end

      # Get the number of days in a month.
      #
      # @param year [Integer]
      # @param month [Integer] 1-12
      # @return [Integer, nil]
      def days_in_month(year, month)
        FFI.invoke_u8(
          "proven_datetime_days_in_month",
          [Fiddle::TYPE_INT, Fiddle::TYPE_CHAR],
          [year, month]
        )
      end
    end
  end
end
