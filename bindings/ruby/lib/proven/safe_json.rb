# SPDX-License-Identifier: Apache-2.0
# SPDX-FileCopyrightText: 2025 Hyperpolymath
# frozen_string_literal: true

require "json"

module Proven
  # Safe JSON operations with validation and size limits.
  #
  # Provides safe JSON parsing and serialization with protection
  # against excessively large payloads and invalid data.
  module SafeJson
    # Default maximum JSON size (1MB)
    DEFAULT_MAX_SIZE = 1_048_576

    # Default maximum nesting depth
    DEFAULT_MAX_DEPTH = 100

    # Parse JSON string safely with size and depth limits.
    #
    # @param json_string [String] the JSON to parse
    # @param max_size [Integer] maximum allowed size in bytes
    # @param max_depth [Integer] maximum nesting depth
    # @return [Result] containing parsed data or error
    def self.parse(json_string, max_size: DEFAULT_MAX_SIZE, max_depth: DEFAULT_MAX_DEPTH)
      return Result.error(InvalidInputError.new("JSON cannot be nil")) if json_string.nil?
      return Result.error(TooLongError.new("JSON exceeds max size")) if json_string.bytesize > max_size

      begin
        data = JSON.parse(json_string, max_nesting: max_depth)
        Result.ok(data)
      rescue JSON::NestingError
        Result.error(ValidationError.new("JSON exceeds max nesting depth"))
      rescue JSON::ParserError => e
        Result.error(InvalidFormatError.new("Invalid JSON: #{e.message}"))
      end
    end

    # Serialize data to JSON safely.
    #
    # @param data [Object] the data to serialize
    # @param max_size [Integer] maximum allowed output size
    # @param pretty [Boolean] whether to pretty-print
    # @return [Result] containing JSON string or error
    def self.stringify(data, max_size: DEFAULT_MAX_SIZE, pretty: false)
      return Result.error(InvalidInputError.new("Data cannot be nil")) if data.nil?

      begin
        json = if pretty
                 JSON.pretty_generate(data)
               else
                 JSON.generate(data)
               end

        if json.bytesize > max_size
          Result.error(TooLongError.new("Generated JSON exceeds max size"))
        else
          Result.ok(json)
        end
      rescue JSON::GeneratorError => e
        Result.error(InvalidFormatError.new("Cannot serialize to JSON: #{e.message}"))
      end
    end

    # Check if a string is valid JSON.
    #
    # @param json_string [String]
    # @return [Boolean]
    def self.valid?(json_string)
      parse(json_string).ok?
    end

    # Safely get a nested value from parsed JSON.
    #
    # @param data [Hash, Array] the parsed JSON data
    # @param path [Array<String, Integer>] the path to the value
    # @return [Maybe] containing the value or none
    def self.get(data, *path)
      current = data

      path.each do |key|
        case current
        when Hash
          return Maybe.none unless current.key?(key.to_s) || current.key?(key.to_sym)

          current = current[key.to_s] || current[key.to_sym]
        when Array
          return Maybe.none unless key.is_a?(Integer) && key >= 0 && key < current.length

          current = current[key]
        else
          return Maybe.none
        end
      end

      current.nil? ? Maybe.none : Maybe.some(current)
    end

    # Get a string value from JSON.
    #
    # @param data [Hash, Array]
    # @param path [Array<String, Integer>]
    # @return [Maybe]
    def self.get_string(data, *path)
      get(data, *path).and_then do |value|
        value.is_a?(String) ? Maybe.some(value) : Maybe.none
      end
    end

    # Get an integer value from JSON.
    #
    # @param data [Hash, Array]
    # @param path [Array<String, Integer>]
    # @return [Maybe]
    def self.get_integer(data, *path)
      get(data, *path).and_then do |value|
        value.is_a?(Integer) ? Maybe.some(value) : Maybe.none
      end
    end

    # Get a float value from JSON.
    #
    # @param data [Hash, Array]
    # @param path [Array<String, Integer>]
    # @return [Maybe]
    def self.get_float(data, *path)
      get(data, *path).and_then do |value|
        value.is_a?(Numeric) ? Maybe.some(value.to_f) : Maybe.none
      end
    end

    # Get a boolean value from JSON.
    #
    # @param data [Hash, Array]
    # @param path [Array<String, Integer>]
    # @return [Maybe]
    def self.get_boolean(data, *path)
      get(data, *path).and_then do |value|
        [true, false].include?(value) ? Maybe.some(value) : Maybe.none
      end
    end

    # Get an array value from JSON.
    #
    # @param data [Hash, Array]
    # @param path [Array<String, Integer>]
    # @return [Maybe]
    def self.get_array(data, *path)
      get(data, *path).and_then do |value|
        value.is_a?(Array) ? Maybe.some(value) : Maybe.none
      end
    end

    # Get an object (hash) value from JSON.
    #
    # @param data [Hash, Array]
    # @param path [Array<String, Integer>]
    # @return [Maybe]
    def self.get_object(data, *path)
      get(data, *path).and_then do |value|
        value.is_a?(Hash) ? Maybe.some(value) : Maybe.none
      end
    end

    # Merge two JSON objects deeply.
    #
    # @param base [Hash]
    # @param overlay [Hash]
    # @return [Hash]
    def self.deep_merge(base, overlay)
      return overlay unless base.is_a?(Hash) && overlay.is_a?(Hash)

      base.merge(overlay) do |_key, old_val, new_val|
        if old_val.is_a?(Hash) && new_val.is_a?(Hash)
          deep_merge(old_val, new_val)
        else
          new_val
        end
      end
    end

    # Escape a string for safe JSON embedding.
    #
    # @param string [String]
    # @return [String]
    def self.escape_string(string)
      return "" if string.nil?

      JSON.generate(string)[1..-2] # Remove surrounding quotes
    end
  end
end
