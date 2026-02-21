# SPDX-License-Identifier: PMPL-1.0-or-later
# SPDX-FileCopyrightText: 2025 Hyperpolymath
# frozen_string_literal: true

require "time"
require "date"

module Proven
  # Safe datetime operations with validation.
  #
  # Provides validated datetime parsing, formatting, and arithmetic
  # with protection against invalid dates and overflow.
  module SafeDatetime
    # Minimum supported timestamp (year 1970)
    MIN_TIMESTAMP = 0

    # Maximum supported timestamp (year 2100)
    MAX_TIMESTAMP = 4_102_444_800

    # Parse ISO 8601 datetime string.
    #
    # @param datetime_string [String]
    # @return [Result] containing Time or error
    def self.parse_iso8601(datetime_string)
      return Result.error(InvalidInputError.new("Datetime cannot be nil")) if datetime_string.nil?
      return Result.error(InvalidInputError.new("Datetime cannot be empty")) if datetime_string.strip.empty?

      begin
        time = Time.iso8601(datetime_string.strip)
        Result.ok(time)
      rescue ArgumentError => e
        Result.error(InvalidFormatError.new("Invalid ISO 8601 datetime: #{e.message}"))
      end
    end

    # Parse RFC 2822 datetime string.
    #
    # @param datetime_string [String]
    # @return [Result]
    def self.parse_rfc2822(datetime_string)
      return Result.error(InvalidInputError.new("Datetime cannot be nil")) if datetime_string.nil?

      begin
        time = Time.rfc2822(datetime_string.strip)
        Result.ok(time)
      rescue ArgumentError => e
        Result.error(InvalidFormatError.new("Invalid RFC 2822 datetime: #{e.message}"))
      end
    end

    # Parse Unix timestamp.
    #
    # @param timestamp [Integer, Float, String]
    # @return [Result]
    def self.from_unix(timestamp)
      ts = case timestamp
           when Integer, Float
             timestamp.to_i
           when String
             Integer(timestamp) rescue nil
           else
             nil
           end

      return Result.error(InvalidInputError.new("Invalid timestamp")) if ts.nil?
      return Result.error(OutOfRangeError.new("Timestamp out of range")) if ts < MIN_TIMESTAMP || ts > MAX_TIMESTAMP

      Result.ok(Time.at(ts).utc)
    end

    # Parse Unix timestamp in milliseconds.
    #
    # @param timestamp_ms [Integer, String]
    # @return [Result]
    def self.from_unix_ms(timestamp_ms)
      ts = case timestamp_ms
           when Integer
             timestamp_ms
           when String
             Integer(timestamp_ms) rescue nil
           else
             nil
           end

      return Result.error(InvalidInputError.new("Invalid timestamp")) if ts.nil?

      from_unix(ts / 1000)
    end

    # Format time as ISO 8601.
    #
    # @param time [Time]
    # @return [String]
    def self.to_iso8601(time)
      time.utc.iso8601
    end

    # Format time as RFC 2822.
    #
    # @param time [Time]
    # @return [String]
    def self.to_rfc2822(time)
      time.rfc2822
    end

    # Convert to Unix timestamp.
    #
    # @param time [Time]
    # @return [Integer]
    def self.to_unix(time)
      time.to_i
    end

    # Convert to Unix timestamp in milliseconds.
    #
    # @param time [Time]
    # @return [Integer]
    def self.to_unix_ms(time)
      (time.to_f * 1000).to_i
    end

    # Safely add duration to time.
    #
    # @param time [Time]
    # @param seconds [Integer]
    # @return [Result]
    def self.add_seconds(time, seconds)
      new_ts = time.to_i + seconds
      return Result.error(OutOfRangeError.new("Result out of range")) if new_ts < MIN_TIMESTAMP || new_ts > MAX_TIMESTAMP

      Result.ok(Time.at(new_ts).utc)
    end

    # Safely add days to time.
    #
    # @param time [Time]
    # @param days [Integer]
    # @return [Result]
    def self.add_days(time, days)
      add_seconds(time, days * 86_400)
    end

    # Safely add months to time.
    #
    # @param time [Time]
    # @param months [Integer]
    # @return [Result]
    def self.add_months(time, months)
      date = time.to_date
      new_date = date >> months

      # Handle end of month edge cases
      if date.day > new_date.day
        new_date = Date.new(new_date.year, new_date.month, -1)
      end

      Result.ok(Time.utc(new_date.year, new_date.month, new_date.day, time.hour, time.min, time.sec))
    rescue ArgumentError => e
      Result.error(OutOfRangeError.new("Date arithmetic error: #{e.message}"))
    end

    # Calculate difference between two times in seconds.
    #
    # @param time1 [Time]
    # @param time2 [Time]
    # @return [Integer]
    def self.diff_seconds(time1, time2)
      (time1 - time2).to_i.abs
    end

    # Calculate difference in days.
    #
    # @param time1 [Time]
    # @param time2 [Time]
    # @return [Integer]
    def self.diff_days(time1, time2)
      diff_seconds(time1, time2) / 86_400
    end

    # Check if time is in the past.
    #
    # @param time [Time]
    # @return [Boolean]
    def self.past?(time)
      time < Time.now
    end

    # Check if time is in the future.
    #
    # @param time [Time]
    # @return [Boolean]
    def self.future?(time)
      time > Time.now
    end

    # Get start of day (midnight UTC).
    #
    # @param time [Time]
    # @return [Time]
    def self.start_of_day(time)
      Time.utc(time.year, time.month, time.day, 0, 0, 0)
    end

    # Get end of day (23:59:59 UTC).
    #
    # @param time [Time]
    # @return [Time]
    def self.end_of_day(time)
      Time.utc(time.year, time.month, time.day, 23, 59, 59)
    end

    # Check if date is valid.
    #
    # @param year [Integer]
    # @param month [Integer]
    # @param day [Integer]
    # @return [Boolean]
    def self.valid_date?(year, month, day)
      Date.valid_date?(year, month, day)
    end

    # Check if year is a leap year.
    #
    # @param year [Integer]
    # @return [Boolean]
    def self.leap_year?(year)
      Date.leap?(year)
    end

    # Get current UTC time.
    #
    # @return [Time]
    def self.now_utc
      Time.now.utc
    end

    # Get current Unix timestamp.
    #
    # @return [Integer]
    def self.now_unix
      Time.now.to_i
    end
  end
end
