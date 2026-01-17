# SPDX-License-Identifier: PMPL-1.0
# SPDX-FileCopyrightText: 2025 Hyperpolymath

require "json"

module Proven
  # Safe JSON operations with size limits and validation.
  module SafeJson
    # Maximum allowed JSON size in bytes.
    MAX_SIZE = 10_000_000 # 10 MB

    # Maximum nesting depth.
    MAX_DEPTH = 100

    # Parse JSON safely with size check.
    def self.parse(input : String) : JSON::Any?
      return nil if input.size > MAX_SIZE
      return nil if input.empty?

      begin
        JSON.parse(input)
      rescue
        nil
      end
    end

    # Check if string is valid JSON.
    def self.valid?(input : String) : Bool
      !parse(input).nil?
    end

    # Serialize to JSON string.
    def self.to_json(value : JSON::Any) : String
      value.to_json
    end

    # Serialize to pretty JSON string.
    def self.to_pretty_json(value : JSON::Any) : String
      value.to_pretty_json
    end

    # Get value at path (dot-separated).
    def self.get_path(json : JSON::Any, path : String) : JSON::Any?
      return json if path.empty?

      current = json
      path.split('.').each do |key|
        case current.raw
        when Hash
          if current.as_h.has_key?(key)
            current = current[key]
          else
            return nil
          end
        when Array
          idx = key.to_i?
          return nil if idx.nil?
          arr = current.as_a
          return nil if idx < 0 || idx >= arr.size
          current = arr[idx]
        else
          return nil
        end
      end
      current
    end

    # Check if path exists in JSON.
    def self.has_path?(json : JSON::Any, path : String) : Bool
      !get_path(json, path).nil?
    end

    # Get string value at path.
    def self.get_string(json : JSON::Any, path : String) : String?
      value = get_path(json, path)
      return nil if value.nil?
      value.as_s?
    end

    # Get integer value at path.
    def self.get_int(json : JSON::Any, path : String) : Int64?
      value = get_path(json, path)
      return nil if value.nil?
      value.as_i64?
    end

    # Get float value at path.
    def self.get_float(json : JSON::Any, path : String) : Float64?
      value = get_path(json, path)
      return nil if value.nil?
      value.as_f?
    end

    # Get boolean value at path.
    def self.get_bool(json : JSON::Any, path : String) : Bool?
      value = get_path(json, path)
      return nil if value.nil?
      value.as_bool?
    end

    # Get array value at path.
    def self.get_array(json : JSON::Any, path : String) : Array(JSON::Any)?
      value = get_path(json, path)
      return nil if value.nil?
      value.as_a?
    end

    # Get object value at path.
    def self.get_object(json : JSON::Any, path : String) : Hash(String, JSON::Any)?
      value = get_path(json, path)
      return nil if value.nil?
      value.as_h?
    end

    # Merge two JSON objects (shallow).
    def self.merge(base : JSON::Any, overlay : JSON::Any) : JSON::Any?
      base_h = base.as_h?
      overlay_h = overlay.as_h?

      return nil if base_h.nil? || overlay_h.nil?

      result = base_h.dup
      overlay_h.each do |key, value|
        result[key] = value
      end

      JSON::Any.new(result)
    end

    # Extract keys from JSON object.
    def self.keys(json : JSON::Any) : Array(String)?
      json.as_h?.try(&.keys)
    end

    # Check type of JSON value.
    def self.type_of(json : JSON::Any) : String
      case json.raw
      when Nil          then "null"
      when Bool         then "boolean"
      when Int64        then "integer"
      when Float64      then "number"
      when String       then "string"
      when Array        then "array"
      when Hash         then "object"
      else                   "unknown"
      end
    end

    # Count elements in JSON (array length or object keys).
    def self.count(json : JSON::Any) : Int32?
      case json.raw
      when Array
        json.as_a.size
      when Hash
        json.as_h.size
      else
        nil
      end
    end

    # Deep clone JSON value.
    def self.clone(json : JSON::Any) : JSON::Any
      JSON.parse(json.to_json)
    end
  end
end
