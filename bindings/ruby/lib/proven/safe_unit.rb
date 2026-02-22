# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
# frozen_string_literal: true

# Safe unit conversion operations via FFI to libproven.
# ALL computation delegates to Idris 2 compiled code.

module Proven
  module SafeUnit
    # LengthUnit enum values (must match ffi/zig/src/main.zig LengthUnit).
    module LengthUnit
      METERS      = 0
      KILOMETERS  = 1
      CENTIMETERS = 2
      MILLIMETERS = 3
      MILES       = 4
      YARDS       = 5
      FEET        = 6
      INCHES      = 7
      NAUTICAL    = 8
    end

    # TempUnit enum values (must match ffi/zig/src/main.zig TempUnit).
    module TempUnit
      CELSIUS    = 0
      FAHRENHEIT = 1
      KELVIN     = 2
    end

    # Map from symbol names to LengthUnit enum values.
    LENGTH_MAP = {
      meters: LengthUnit::METERS,
      kilometers: LengthUnit::KILOMETERS,
      centimeters: LengthUnit::CENTIMETERS,
      millimeters: LengthUnit::MILLIMETERS,
      miles: LengthUnit::MILES,
      yards: LengthUnit::YARDS,
      feet: LengthUnit::FEET,
      inches: LengthUnit::INCHES,
      nautical_miles: LengthUnit::NAUTICAL,
    }.freeze

    # Map from symbol names to TempUnit enum values.
    TEMP_MAP = {
      celsius: TempUnit::CELSIUS,
      fahrenheit: TempUnit::FAHRENHEIT,
      kelvin: TempUnit::KELVIN,
    }.freeze

    class << self
      # Convert length between units.
      # Returns nil on error (unknown units, etc.).
      #
      # @param value [Float]
      # @param from [Symbol] e.g., :meters, :feet
      # @param to [Symbol] e.g., :kilometers, :miles
      # @return [Float, nil]
      def convert_length(value, from:, to:)
        from_enum = LENGTH_MAP[from]
        to_enum = LENGTH_MAP[to]
        return nil if from_enum.nil? || to_enum.nil?

        result = FFI.invoke_float_result(
          "proven_unit_convert_length",
          [Fiddle::TYPE_DOUBLE, Fiddle::TYPE_INT, Fiddle::TYPE_INT],
          [value.to_f, from_enum, to_enum]
        )
        return nil unless result
        status, val = result
        status == FFI::STATUS_OK ? val : nil
      end

      # Convert temperature between units.
      # Returns nil on error (below absolute zero, unknown units, etc.).
      #
      # @param value [Float]
      # @param from [Symbol] :celsius, :fahrenheit, or :kelvin
      # @param to [Symbol] :celsius, :fahrenheit, or :kelvin
      # @return [Float, nil]
      def convert_temperature(value, from:, to:)
        from_enum = TEMP_MAP[from]
        to_enum = TEMP_MAP[to]
        return nil if from_enum.nil? || to_enum.nil?

        result = FFI.invoke_float_result(
          "proven_unit_convert_temp",
          [Fiddle::TYPE_DOUBLE, Fiddle::TYPE_INT, Fiddle::TYPE_INT],
          [value.to_f, from_enum, to_enum]
        )
        return nil unless result
        status, val = result
        status == FFI::STATUS_OK ? val : nil
      end
    end
  end
end
