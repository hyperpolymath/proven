# SPDX-License-Identifier: PMPL-1.0-or-later
# SPDX-FileCopyrightText: 2025 Hyperpolymath

module Proven
  # Safe date/time operations with validation.
  module SafeDateTime
    # Parse ISO 8601 datetime string.
    def self.parse_iso8601(input : String) : Time?
      return nil if input.empty? || input.size > 50

      begin
        Time.parse_iso8601(input)
      rescue
        nil
      end
    end

    # Parse RFC 2822 datetime string.
    def self.parse_rfc2822(input : String) : Time?
      return nil if input.empty? || input.size > 100

      begin
        Time.parse_rfc2822(input)
      rescue
        nil
      end
    end

    # Parse datetime with custom format.
    def self.parse(input : String, format : String, location : Time::Location = Time::Location::UTC) : Time?
      return nil if input.empty?

      begin
        Time.parse(input, format, location)
      rescue
        nil
      end
    end

    # Format time as ISO 8601.
    def self.to_iso8601(time : Time) : String
      time.to_rfc3339
    end

    # Format time as RFC 2822.
    def self.to_rfc2822(time : Time) : String
      time.to_rfc2822
    end

    # Format time with custom format.
    def self.format(time : Time, format : String) : String
      time.to_s(format)
    end

    # Get current UTC time.
    def self.now_utc : Time
      Time.utc
    end

    # Get current local time.
    def self.now_local : Time
      Time.local
    end

    # Add duration safely.
    def self.add_duration(time : Time, span : Time::Span) : Time
      time + span
    end

    # Add days safely.
    def self.add_days(time : Time, days : Int32) : Time
      time + days.days
    end

    # Add hours safely.
    def self.add_hours(time : Time, hours : Int32) : Time
      time + hours.hours
    end

    # Add minutes safely.
    def self.add_minutes(time : Time, minutes : Int32) : Time
      time + minutes.minutes
    end

    # Add seconds safely.
    def self.add_seconds(time : Time, seconds : Int32) : Time
      time + seconds.seconds
    end

    # Calculate difference between times in seconds.
    def self.diff_seconds(a : Time, b : Time) : Int64
      (a - b).total_seconds.to_i64
    end

    # Calculate difference in days.
    def self.diff_days(a : Time, b : Time) : Int32
      (a - b).total_days.to_i32
    end

    # Check if time is in the past.
    def self.is_past?(time : Time) : Bool
      time < Time.utc
    end

    # Check if time is in the future.
    def self.is_future?(time : Time) : Bool
      time > Time.utc
    end

    # Check if two times are on the same day.
    def self.same_day?(a : Time, b : Time) : Bool
      a.year == b.year && a.day_of_year == b.day_of_year
    end

    # Check if date is a weekend.
    def self.is_weekend?(time : Time) : Bool
      time.day_of_week.saturday? || time.day_of_week.sunday?
    end

    # Check if date is a weekday.
    def self.is_weekday?(time : Time) : Bool
      !is_weekend?(time)
    end

    # Get start of day.
    def self.start_of_day(time : Time) : Time
      Time.utc(time.year, time.month, time.day, 0, 0, 0)
    end

    # Get end of day.
    def self.end_of_day(time : Time) : Time
      Time.utc(time.year, time.month, time.day, 23, 59, 59)
    end

    # Get start of month.
    def self.start_of_month(time : Time) : Time
      Time.utc(time.year, time.month, 1, 0, 0, 0)
    end

    # Get start of year.
    def self.start_of_year(time : Time) : Time
      Time.utc(time.year, 1, 1, 0, 0, 0)
    end

    # Check if year is a leap year.
    def self.is_leap_year?(year : Int32) : Bool
      Time.leap_year?(year)
    end

    # Get days in month.
    def self.days_in_month(year : Int32, month : Int32) : Int32
      Time.days_in_month(year, month)
    end

    # Convert to Unix timestamp (seconds).
    def self.to_unix(time : Time) : Int64
      time.to_unix
    end

    # Convert to Unix timestamp (milliseconds).
    def self.to_unix_ms(time : Time) : Int64
      time.to_unix_ms
    end

    # Create time from Unix timestamp.
    def self.from_unix(timestamp : Int64) : Time
      Time.unix(timestamp)
    end

    # Create time from Unix timestamp (milliseconds).
    def self.from_unix_ms(timestamp : Int64) : Time
      Time.unix_ms(timestamp)
    end

    # Convert to different timezone.
    def self.in_location(time : Time, location_name : String) : Time?
      begin
        location = Time::Location.load(location_name)
        time.in(location)
      rescue
        nil
      end
    end

    # Check if time is within range.
    def self.in_range?(time : Time, start_time : Time, end_time : Time) : Bool
      time >= start_time && time <= end_time
    end

    # Clamp time to range.
    def self.clamp(time : Time, min_time : Time, max_time : Time) : Time
      return min_time if time < min_time
      return max_time if time > max_time
      time
    end
  end
end
