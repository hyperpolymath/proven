# SPDX-License-Identifier: Apache-2.0
# SPDX-FileCopyrightText: 2025 Hyperpolymath
# frozen_string_literal: true

module Proven
  # Result type for operations that may fail.
  # Provides a functional approach to error handling.
  class Result
    attr_reader :value, :error

    def initialize(value: nil, error: nil)
      @value = value
      @error = error
    end

    # Check if the result is successful.
    #
    # @return [Boolean]
    def ok?
      @error.nil?
    end

    # Check if the result is an error.
    #
    # @return [Boolean]
    def error?
      !@error.nil?
    end

    # Unwrap the value, raising an error if the result is an error.
    #
    # @return [Object]
    # @raise [RuntimeError] if the result is an error
    def unwrap!
      raise @error if error?

      @value
    end

    # Unwrap the value or return a default.
    #
    # @param default [Object] the default value to return if error
    # @return [Object]
    def unwrap_or(default)
      ok? ? @value : default
    end

    # Map over the value if successful.
    #
    # @yield [value] block to transform the value
    # @return [Result]
    def map
      return self if error?

      Result.ok(yield(@value))
    end

    # Map over the error if failed.
    #
    # @yield [error] block to transform the error
    # @return [Result]
    def map_error
      return self if ok?

      Result.error(yield(@error))
    end

    # Chain operations that return Results.
    #
    # @yield [value] block that returns a Result
    # @return [Result]
    def and_then
      return self if error?

      yield(@value)
    end

    # Create a successful result.
    #
    # @param value [Object]
    # @return [Result]
    def self.ok(value)
      new(value: value)
    end

    # Create an error result.
    #
    # @param error [String, StandardError]
    # @return [Result]
    def self.error(error_value)
      new(error: error_value)
    end
  end

  # Maybe type for optional values.
  class Maybe
    attr_reader :value

    def initialize(value)
      @value = value
    end

    # Check if value is present.
    #
    # @return [Boolean]
    def some?
      !@value.nil?
    end

    # Check if value is absent.
    #
    # @return [Boolean]
    def none?
      @value.nil?
    end

    # Unwrap the value, raising if none.
    #
    # @return [Object]
    # @raise [RuntimeError] if none
    def unwrap!
      raise "Attempted to unwrap None" if none?

      @value
    end

    # Unwrap with default.
    #
    # @param default [Object]
    # @return [Object]
    def unwrap_or(default)
      some? ? @value : default
    end

    # Map over the value if present.
    #
    # @yield [value]
    # @return [Maybe]
    def map
      return self if none?

      Maybe.some(yield(@value))
    end

    # Create a Some value.
    #
    # @param value [Object]
    # @return [Maybe]
    def self.some(value)
      new(value)
    end

    # Create a None value.
    #
    # @return [Maybe]
    def self.none
      new(nil)
    end
  end

  # Error types for Proven operations.
  class ProvenError < StandardError; end

  class ValidationError < ProvenError; end

  class OverflowError < ProvenError; end

  class OutOfRangeError < ProvenError; end

  class InvalidInputError < ProvenError; end

  class InvalidFormatError < ProvenError; end

  class TooLongError < ProvenError; end

  class EmptyInputError < ProvenError; end
end
