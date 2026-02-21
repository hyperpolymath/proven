# SPDX-License-Identifier: PMPL-1.0-or-later
# SPDX-FileCopyrightText: 2025 Hyperpolymath
# frozen_string_literal: true

module Proven
  # Safe physical unit conversions.
  #
  # Provides type-safe unit conversions for length, mass,
  # temperature, time, and other physical quantities.
  module SafeUnit
    # Length unit definitions (to meters)
    LENGTH_TO_METERS = {
      meters: 1.0,
      kilometers: 1000.0,
      centimeters: 0.01,
      millimeters: 0.001,
      miles: 1609.344,
      yards: 0.9144,
      feet: 0.3048,
      inches: 0.0254,
      nautical_miles: 1852.0
    }.freeze

    # Mass unit definitions (to kilograms)
    MASS_TO_KG = {
      kilograms: 1.0,
      grams: 0.001,
      milligrams: 0.000001,
      pounds: 0.453592,
      ounces: 0.0283495,
      stones: 6.35029,
      metric_tons: 1000.0
    }.freeze

    # Time unit definitions (to seconds)
    TIME_TO_SECONDS = {
      seconds: 1.0,
      milliseconds: 0.001,
      microseconds: 0.000001,
      nanoseconds: 0.000000001,
      minutes: 60.0,
      hours: 3600.0,
      days: 86_400.0,
      weeks: 604_800.0
    }.freeze

    # Data unit definitions (to bytes)
    DATA_TO_BYTES = {
      bytes: 1.0,
      kilobytes: 1000.0,
      megabytes: 1_000_000.0,
      gigabytes: 1_000_000_000.0,
      terabytes: 1_000_000_000_000.0,
      kibibytes: 1024.0,
      mebibytes: 1_048_576.0,
      gibibytes: 1_073_741_824.0,
      tebibytes: 1_099_511_627_776.0
    }.freeze

    # Convert length between units.
    #
    # @param value [Float]
    # @param from [Symbol]
    # @param to [Symbol]
    # @return [Result]
    def self.convert_length(value, from:, to:)
      return Result.error(InvalidInputError.new("Unknown unit: #{from}")) unless LENGTH_TO_METERS.key?(from)
      return Result.error(InvalidInputError.new("Unknown unit: #{to}")) unless LENGTH_TO_METERS.key?(to)

      meters = value * LENGTH_TO_METERS[from]
      Result.ok(meters / LENGTH_TO_METERS[to])
    end

    # Convert mass between units.
    #
    # @param value [Float]
    # @param from [Symbol]
    # @param to [Symbol]
    # @return [Result]
    def self.convert_mass(value, from:, to:)
      return Result.error(InvalidInputError.new("Unknown unit: #{from}")) unless MASS_TO_KG.key?(from)
      return Result.error(InvalidInputError.new("Unknown unit: #{to}")) unless MASS_TO_KG.key?(to)

      kg = value * MASS_TO_KG[from]
      Result.ok(kg / MASS_TO_KG[to])
    end

    # Convert temperature between units.
    #
    # @param value [Float]
    # @param from [Symbol] :celsius, :fahrenheit, or :kelvin
    # @param to [Symbol] :celsius, :fahrenheit, or :kelvin
    # @return [Result]
    def self.convert_temperature(value, from:, to:)
      # Convert to Kelvin first
      kelvin = case from
               when :celsius
                 value + 273.15
               when :fahrenheit
                 (value - 32.0) * 5.0 / 9.0 + 273.15
               when :kelvin
                 value
               else
                 return Result.error(InvalidInputError.new("Unknown temperature unit: #{from}"))
               end

      return Result.error(OutOfRangeError.new("Temperature below absolute zero")) if kelvin.negative?

      # Convert from Kelvin
      result = case to
               when :celsius
                 kelvin - 273.15
               when :fahrenheit
                 (kelvin - 273.15) * 9.0 / 5.0 + 32.0
               when :kelvin
                 kelvin
               else
                 return Result.error(InvalidInputError.new("Unknown temperature unit: #{to}"))
               end

      Result.ok(result)
    end

    # Convert time between units.
    #
    # @param value [Float]
    # @param from [Symbol]
    # @param to [Symbol]
    # @return [Result]
    def self.convert_time(value, from:, to:)
      return Result.error(InvalidInputError.new("Unknown unit: #{from}")) unless TIME_TO_SECONDS.key?(from)
      return Result.error(InvalidInputError.new("Unknown unit: #{to}")) unless TIME_TO_SECONDS.key?(to)

      seconds = value * TIME_TO_SECONDS[from]
      Result.ok(seconds / TIME_TO_SECONDS[to])
    end

    # Convert data size between units.
    #
    # @param value [Float]
    # @param from [Symbol]
    # @param to [Symbol]
    # @return [Result]
    def self.convert_data(value, from:, to:)
      return Result.error(InvalidInputError.new("Unknown unit: #{from}")) unless DATA_TO_BYTES.key?(from)
      return Result.error(InvalidInputError.new("Unknown unit: #{to}")) unless DATA_TO_BYTES.key?(to)

      bytes = value * DATA_TO_BYTES[from]
      Result.ok(bytes / DATA_TO_BYTES[to])
    end

    # Format bytes to human-readable string.
    #
    # @param bytes [Integer]
    # @param binary [Boolean] use binary (KiB, MiB) vs decimal (KB, MB)
    # @return [String]
    def self.format_bytes(bytes, binary: true)
      return "0 B" if bytes.zero?

      units = binary ? %w[B KiB MiB GiB TiB PiB] : %w[B KB MB GB TB PB]
      base = binary ? 1024.0 : 1000.0

      exp = (Math.log(bytes) / Math.log(base)).floor
      exp = [exp, units.length - 1].min

      size = bytes / (base**exp)
      format("%.2f %s", size, units[exp])
    end

    # Parse bytes from human-readable string.
    #
    # @param string [String] e.g., "10 MB", "1.5 GiB"
    # @return [Result]
    def self.parse_bytes(string)
      return Result.error(InvalidInputError.new("Input cannot be nil")) if string.nil?

      match = string.strip.match(/\A([\d.]+)\s*([A-Za-z]+)\z/)
      return Result.error(InvalidFormatError.new("Invalid byte format")) unless match

      value = Float(match[1])
      unit = match[2].downcase.to_sym

      unit_map = {
        b: 1,
        kb: 1000,
        mb: 1_000_000,
        gb: 1_000_000_000,
        tb: 1_000_000_000_000,
        kib: 1024,
        mib: 1_048_576,
        gib: 1_073_741_824,
        tib: 1_099_511_627_776
      }

      factor = unit_map[unit]
      return Result.error(InvalidInputError.new("Unknown unit: #{match[2]}")) unless factor

      Result.ok((value * factor).round)
    rescue ArgumentError
      Result.error(InvalidFormatError.new("Invalid number format"))
    end

    # Format duration to human-readable string.
    #
    # @param seconds [Float]
    # @return [String]
    def self.format_duration(seconds)
      return "0s" if seconds.zero?

      parts = []

      if seconds >= 86_400
        days = (seconds / 86_400).floor
        parts << "#{days}d"
        seconds %= 86_400
      end

      if seconds >= 3600
        hours = (seconds / 3600).floor
        parts << "#{hours}h"
        seconds %= 3600
      end

      if seconds >= 60
        mins = (seconds / 60).floor
        parts << "#{mins}m"
        seconds %= 60
      end

      parts << format("%.1fs", seconds) if seconds.positive? || parts.empty?

      parts.join(" ")
    end
  end
end
