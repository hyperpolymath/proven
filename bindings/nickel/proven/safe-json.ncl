# SPDX-License-Identifier: PMPL-1.0-or-later
# SPDX-FileCopyrightText: 2025 Hyperpolymath
#
# SafeJson - JSON validation and path access for Nickel
#
# Provides safe JSON operations with validation, path access,
# and schema checking. Prevents injection and malformed data.

{
  # Result type
  Result = {
    value | Dyn,
    ok | Bool,
  },

  # JSON value types
  JsonType = [| 'Null, 'Boolean, 'Number, 'String, 'Array, 'Object |],

  # Maximum JSON depth to prevent stack overflow
  MAX_DEPTH = 100,

  # Maximum string length in JSON values
  MAX_STRING_LENGTH = 1048576,  # 1 MB

  # Get JSON value type
  get_type
    : Dyn -> String
    = fun value =>
      if value == null then "null"
      else if std.is_bool value then "boolean"
      else if std.is_number value then "number"
      else if std.is_string value then "string"
      else if std.is_array value then "array"
      else if std.is_record value then "object"
      else "unknown",

  # Check if value is a valid JSON primitive
  is_primitive
    : Dyn -> Bool
    = fun value =>
      value == null ||
      std.is_bool value ||
      std.is_number value ||
      std.is_string value,

  # Safe get from record with default
  get_or_default
    : String -> Dyn -> Dyn -> Dyn
    = fun key default obj =>
      if std.is_record obj && std.record.has_field key obj then
        std.record.get key obj
      else
        default,

  # Safe get from record, returns Result
  get_safe
    : String -> Dyn -> { value : Dyn, ok : Bool }
    = fun key obj =>
      if std.is_record obj && std.record.has_field key obj then
        { value = std.record.get key obj, ok = true }
      else
        { value = null, ok = false },

  # Safe array index access
  get_index
    : Number -> Array Dyn -> { value : Dyn, ok : Bool }
    = fun idx arr =>
      if std.is_array arr then
        let len = std.array.length arr in
        if idx >= 0 && idx < len then
          { value = std.array.at idx arr, ok = true }
        else
          { value = null, ok = false }
      else
        { value = null, ok = false },

  # Check if object has required keys
  has_keys
    : Array String -> Dyn -> Bool
    = fun keys obj =>
      if std.is_record obj then
        std.array.all (fun k => std.record.has_field k obj) keys
      else
        false,

  # Check if object has only allowed keys
  only_keys
    : Array String -> Dyn -> Bool
    = fun allowed obj =>
      if std.is_record obj then
        let obj_keys = std.record.fields obj in
        std.array.all (fun k => std.array.any (fun a => a == k) allowed) obj_keys
      else
        false,

  # Merge two JSON objects (shallow)
  merge
    : Dyn -> Dyn -> { value : Dyn, ok : Bool }
    = fun a b =>
      if std.is_record a && std.is_record b then
        { value = a & b, ok = true }
      else
        { value = a, ok = false },

  # Pick specific keys from object
  pick
    : Array String -> Dyn -> Dyn
    = fun keys obj =>
      if std.is_record obj then
        std.array.fold_left
          (fun acc k =>
            if std.record.has_field k obj then
              std.record.insert k (std.record.get k obj) acc
            else
              acc
          )
          {}
          keys
      else
        {},

  # Omit specific keys from object
  omit
    : Array String -> Dyn -> Dyn
    = fun keys obj =>
      if std.is_record obj then
        std.array.fold_left
          (fun acc k => std.record.remove k acc)
          obj
          keys
      else
        obj,

  # Safe deep get using path (e.g., ["user", "name"])
  get_path
    : Array String -> Dyn -> { value : Dyn, ok : Bool }
    = fun path obj =>
      std.array.fold_left
        (fun acc key =>
          if acc.ok then
            get_safe key acc.value
          else
            acc
        )
        { value = obj, ok = true }
        path,

  # Validate value against simple schema
  # Schema is a record with field types
  validate_schema
    : Dyn -> Dyn -> Bool
    = fun schema value =>
      if !(std.is_record schema) || !(std.is_record value) then
        false
      else
        let schema_keys = std.record.fields schema in
        std.array.all
          (fun key =>
            if std.record.has_field key value then
              let expected_type = std.record.get key schema in
              let actual_type = get_type (std.record.get key value) in
              expected_type == actual_type
            else
              false
          )
          schema_keys,

  # Check if JSON string length is safe
  is_safe_string
    : String -> Bool
    = fun s =>
      std.string.length s <= MAX_STRING_LENGTH,

  # Check for dangerous patterns in JSON strings
  has_dangerous_pattern
    : String -> Bool
    = fun s =>
      std.string.contains "__proto__" s ||
      std.string.contains "constructor" s ||
      std.string.contains "<script" (std.string.lowercase s),

  # Contracts
  SafeJsonString = std.contract.from_predicate (fun s =>
    is_safe_string s && !(has_dangerous_pattern s)
  ),

  JsonObject = std.contract.from_predicate std.is_record,
  JsonArray = std.contract.from_predicate std.is_array,
}
