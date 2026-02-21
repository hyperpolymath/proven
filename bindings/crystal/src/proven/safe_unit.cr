# SPDX-License-Identifier: PMPL-1.0-or-later
# SPDX-FileCopyrightText: 2025 Hyperpolymath

module Proven
  # Safe physical unit conversions.
  module SafeUnit
    # Length units.
    enum LengthUnit
      Meters
      Kilometers
      Centimeters
      Millimeters
      Miles
      Yards
      Feet
      Inches

      def to_meters(value : Float64) : Float64
        case self
        when Meters      then value
        when Kilometers  then value * 1000.0
        when Centimeters then value / 100.0
        when Millimeters then value / 1000.0
        when Miles       then value * 1609.344
        when Yards       then value * 0.9144
        when Feet        then value * 0.3048
        when Inches      then value * 0.0254
        else                  value
        end
      end

      def from_meters(meters : Float64) : Float64
        case self
        when Meters      then meters
        when Kilometers  then meters / 1000.0
        when Centimeters then meters * 100.0
        when Millimeters then meters * 1000.0
        when Miles       then meters / 1609.344
        when Yards       then meters / 0.9144
        when Feet        then meters / 0.3048
        when Inches      then meters / 0.0254
        else                  meters
        end
      end
    end

    # Mass units.
    enum MassUnit
      Kilograms
      Grams
      Milligrams
      Pounds
      Ounces
      Stones

      def to_kilograms(value : Float64) : Float64
        case self
        when Kilograms  then value
        when Grams      then value / 1000.0
        when Milligrams then value / 1_000_000.0
        when Pounds     then value * 0.453592
        when Ounces     then value * 0.0283495
        when Stones     then value * 6.35029
        else                 value
        end
      end

      def from_kilograms(kg : Float64) : Float64
        case self
        when Kilograms  then kg
        when Grams      then kg * 1000.0
        when Milligrams then kg * 1_000_000.0
        when Pounds     then kg / 0.453592
        when Ounces     then kg / 0.0283495
        when Stones     then kg / 6.35029
        else                 kg
        end
      end
    end

    # Temperature units.
    enum TemperatureUnit
      Celsius
      Fahrenheit
      Kelvin
    end

    # Time units.
    enum TimeUnit
      Seconds
      Milliseconds
      Microseconds
      Nanoseconds
      Minutes
      Hours
      Days
      Weeks

      def to_seconds(value : Float64) : Float64
        case self
        when Seconds      then value
        when Milliseconds then value / 1000.0
        when Microseconds then value / 1_000_000.0
        when Nanoseconds  then value / 1_000_000_000.0
        when Minutes      then value * 60.0
        when Hours        then value * 3600.0
        when Days         then value * 86400.0
        when Weeks        then value * 604800.0
        else                   value
        end
      end

      def from_seconds(secs : Float64) : Float64
        case self
        when Seconds      then secs
        when Milliseconds then secs * 1000.0
        when Microseconds then secs * 1_000_000.0
        when Nanoseconds  then secs * 1_000_000_000.0
        when Minutes      then secs / 60.0
        when Hours        then secs / 3600.0
        when Days         then secs / 86400.0
        when Weeks        then secs / 604800.0
        else                   secs
        end
      end
    end

    # Data size units.
    enum DataUnit
      Bytes
      Kilobytes
      Megabytes
      Gigabytes
      Terabytes
      Kibibytes
      Mebibytes
      Gibibytes
      Tebibytes

      def to_bytes(value : Float64) : Float64
        case self
        when Bytes     then value
        when Kilobytes then value * 1000.0
        when Megabytes then value * 1_000_000.0
        when Gigabytes then value * 1_000_000_000.0
        when Terabytes then value * 1_000_000_000_000.0
        when Kibibytes then value * 1024.0
        when Mebibytes then value * 1_048_576.0
        when Gibibytes then value * 1_073_741_824.0
        when Tebibytes then value * 1_099_511_627_776.0
        else                value
        end
      end

      def from_bytes(bytes : Float64) : Float64
        case self
        when Bytes     then bytes
        when Kilobytes then bytes / 1000.0
        when Megabytes then bytes / 1_000_000.0
        when Gigabytes then bytes / 1_000_000_000.0
        when Terabytes then bytes / 1_000_000_000_000.0
        when Kibibytes then bytes / 1024.0
        when Mebibytes then bytes / 1_048_576.0
        when Gibibytes then bytes / 1_073_741_824.0
        when Tebibytes then bytes / 1_099_511_627_776.0
        else                bytes
        end
      end
    end

    # Convert length between units.
    def self.convert_length(value : Float64, from : LengthUnit, to : LengthUnit) : Float64
      meters = from.to_meters(value)
      to.from_meters(meters)
    end

    # Convert mass between units.
    def self.convert_mass(value : Float64, from : MassUnit, to : MassUnit) : Float64
      kg = from.to_kilograms(value)
      to.from_kilograms(kg)
    end

    # Convert temperature between units.
    def self.convert_temperature(value : Float64, from : TemperatureUnit, to : TemperatureUnit) : Float64
      # Convert to Kelvin first
      kelvin = case from
               when TemperatureUnit::Celsius    then value + 273.15
               when TemperatureUnit::Fahrenheit then (value - 32.0) * 5.0 / 9.0 + 273.15
               when TemperatureUnit::Kelvin     then value
               else                                  value
               end

      # Convert from Kelvin
      case to
      when TemperatureUnit::Celsius    then kelvin - 273.15
      when TemperatureUnit::Fahrenheit then (kelvin - 273.15) * 9.0 / 5.0 + 32.0
      when TemperatureUnit::Kelvin     then kelvin
      else                                  kelvin
      end
    end

    # Convert time between units.
    def self.convert_time(value : Float64, from : TimeUnit, to : TimeUnit) : Float64
      secs = from.to_seconds(value)
      to.from_seconds(secs)
    end

    # Convert data size between units.
    def self.convert_data(value : Float64, from : DataUnit, to : DataUnit) : Float64
      bytes = from.to_bytes(value)
      to.from_bytes(bytes)
    end

    # Human-readable data size.
    def self.human_readable_bytes(bytes : Float64, use_binary : Bool = false) : String
      units = use_binary ? ["B", "KiB", "MiB", "GiB", "TiB"] : ["B", "KB", "MB", "GB", "TB"]
      divisor = use_binary ? 1024.0 : 1000.0

      value = bytes.abs
      unit_index = 0

      while value >= divisor && unit_index < units.size - 1
        value /= divisor
        unit_index += 1
      end

      sign = bytes < 0 ? "-" : ""
      "#{sign}#{value.round(2)} #{units[unit_index]}"
    end

    # Human-readable duration.
    def self.human_readable_duration(seconds : Float64) : String
      return "0s" if seconds == 0

      result = [] of String
      remaining = seconds.abs

      if remaining >= 86400
        days = (remaining / 86400).floor.to_i
        result << "#{days}d"
        remaining %= 86400
      end

      if remaining >= 3600
        hours = (remaining / 3600).floor.to_i
        result << "#{hours}h"
        remaining %= 3600
      end

      if remaining >= 60
        minutes = (remaining / 60).floor.to_i
        result << "#{minutes}m"
        remaining %= 60
      end

      if remaining > 0
        if remaining == remaining.floor
          result << "#{remaining.to_i}s"
        else
          result << "#{remaining.round(2)}s"
        end
      end

      sign = seconds < 0 ? "-" : ""
      "#{sign}#{result.join(" ")}"
    end
  end
end
