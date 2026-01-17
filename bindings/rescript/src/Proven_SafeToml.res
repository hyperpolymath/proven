// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

/**
 * SafeToml - Safe TOML parsing and validation operations that cannot crash.
 *
 * Provides bounded TOML parsing with depth limits, key validation,
 * and type-safe value extraction. All operations return errors
 * rather than crashing on malformed input.
 */

/** TOML error types */
type tomlError =
  | ParseError
  | InvalidUtf8
  | MaxDepthExceeded
  | MaxKeysExceeded
  | InvalidKey
  | InvalidValue
  | DuplicateKey
  | TypeMismatch
  | KeyNotFound

/** TOML value types */
type rec tomlValue =
  | String(string)
  | Integer(int)
  | Float(float)
  | Boolean(bool)
  | Datetime(string)
  | Array(array<tomlValue>)
  | Table(Js.Dict.t<tomlValue>)

/** Maximum allowed nesting depth for tables */
let maxDepth = 32

/** Maximum keys per table */
let maxKeysPerTable = 256

/** Safe TOML parsing options */
type parseOptions = {
  maxDepth: int,
  maxKeys: int,
  allowDuplicateKeys: bool,
}

/** Default parse options */
let defaultParseOptions: parseOptions = {
  maxDepth,
  maxKeys: maxKeysPerTable,
  allowDuplicateKeys: false,
}

/** Validate a TOML key (bare keys: A-Za-z0-9_-) */
let isValidKey = (key: string): bool => {
  if Js.String2.length(key) == 0 {
    false
  } else {
    let valid = ref(true)
    let length = Js.String2.length(key)
    for index in 0 to length - 1 {
      if valid.contents {
        let charCode = Js.String2.charCodeAt(key, index)->Belt.Float.toInt
        let isValidChar =
          (charCode >= 65 && charCode <= 90) || // A-Z
          (charCode >= 97 && charCode <= 122) || // a-z
          (charCode >= 48 && charCode <= 57) || // 0-9
          charCode == 95 || // _
          charCode == 45 // -
        if !isValidChar {
          valid := false
        }
      }
    }
    valid.contents
  }
}

/** Validate a TOML string is valid UTF-8 */
let isValidString = (input: string): bool => {
  // JavaScript strings are inherently valid UTF-16
  // Check for any invalid surrogate pairs
  let valid = ref(true)
  let length = Js.String2.length(input)
  for index in 0 to length - 1 {
    if valid.contents {
      let charCode = Js.String2.charCodeAt(input, index)->Belt.Float.toInt
      // Check for lone surrogates
      if charCode >= 0xD800 && charCode <= 0xDBFF {
        // High surrogate - must be followed by low surrogate
        if index + 1 >= length {
          valid := false
        } else {
          let nextCode = Js.String2.charCodeAt(input, index + 1)->Belt.Float.toInt
          if nextCode < 0xDC00 || nextCode > 0xDFFF {
            valid := false
          }
        }
      } else if charCode >= 0xDC00 && charCode <= 0xDFFF {
        // Low surrogate without preceding high surrogate
        valid := false
      }
    }
  }
  valid.contents
}

/** Parse a TOML value string */
let parseValue = (input: string): result<tomlValue, tomlError> => {
  let trimmed = Js.String2.trim(input)
  if Js.String2.length(trimmed) == 0 {
    Error(InvalidValue)
  } else if trimmed == "true" {
    Ok(Boolean(true))
  } else if trimmed == "false" {
    Ok(Boolean(false))
  } else if
    Js.String2.charAt(trimmed, 0) == "\"" &&
    Js.String2.length(trimmed) >= 2 &&
    Js.String2.charAt(trimmed, Js.String2.length(trimmed) - 1) == "\""
  {
    // Double-quoted string
    Ok(String(Js.String2.slice(trimmed, ~from=1, ~to_=Js.String2.length(trimmed) - 1)))
  } else if
    Js.String2.charAt(trimmed, 0) == "'" &&
    Js.String2.length(trimmed) >= 2 &&
    Js.String2.charAt(trimmed, Js.String2.length(trimmed) - 1) == "'"
  {
    // Single-quoted string
    Ok(String(Js.String2.slice(trimmed, ~from=1, ~to_=Js.String2.length(trimmed) - 1)))
  } else {
    // Try integer
    switch Belt.Int.fromString(trimmed) {
    | Some(intValue) => Ok(Integer(intValue))
    | None =>
      // Try float
      switch Js.Float.fromString(trimmed) {
      | value if !Js.Float.isNaN(value) => Ok(Float(value))
      | _ => Error(InvalidValue)
      }
    }
  }
}

/** Parse a simple TOML key-value line (e.g., "key = value") */
let parseKeyValue = (line: string): result<(string, tomlValue), tomlError> => {
  switch Js.String2.indexOf(line, "=") {
  | -1 => Error(ParseError)
  | eqPos =>
    let key = Js.String2.trim(Js.String2.slice(line, ~from=0, ~to_=eqPos))
    let valueStr = Js.String2.trim(Js.String2.sliceToEnd(line, ~from=eqPos + 1))

    if Js.String2.length(key) == 0 {
      Error(InvalidKey)
    } else if Js.String2.length(valueStr) == 0 {
      Error(InvalidValue)
    } else {
      switch parseValue(valueStr) {
      | Ok(parsedValue) => Ok((key, parsedValue))
      | Error(err) => Error(err)
      }
    }
  }
}

/** Check if a string looks like valid TOML syntax */
let isValidToml = (input: string): bool => {
  if !isValidString(input) {
    false
  } else {
    let depth = ref(0)
    let valid = ref(true)
    let lines = Js.String2.split(input, "\n")

    Belt.Array.forEach(lines, line => {
      if valid.contents {
        let trimmed = Js.String2.trim(line)

        // Skip empty lines and comments
        if Js.String2.length(trimmed) > 0 && Js.String2.charAt(trimmed, 0) != "#" {
          // Table header
          if Js.String2.charAt(trimmed, 0) == "[" {
            // Check for array of tables
            if Js.String2.length(trimmed) > 1 && Js.String2.charAt(trimmed, 1) == "[" {
              depth := depth.contents + 1
              if depth.contents > maxDepth {
                valid := false
              }
            } else {
              depth := 1
            }
          } else {
            // Key-value pair
            if Js.String2.indexOf(trimmed, "=") == -1 {
              valid := false
            }
          }
        }
      }
    })

    valid.contents
  }
}

/** Escape a string for TOML output */
let escapeString = (input: string): string => {
  input
  ->Js.String2.replaceByRe(%re("/\\\\/g"), "\\\\")
  ->Js.String2.replaceByRe(%re("/\"/g"), "\\\"")
  ->Js.String2.replaceByRe(%re("/\n/g"), "\\n")
  ->Js.String2.replaceByRe(%re("/\r/g"), "\\r")
  ->Js.String2.replaceByRe(%re("/\t/g"), "\\t")
}

/** Format a tomlValue as a TOML string */
let rec formatValue = (value: tomlValue): string => {
  switch value {
  | String(stringValue) => "\"" ++ escapeString(stringValue) ++ "\""
  | Integer(intValue) => Belt.Int.toString(intValue)
  | Float(floatValue) => Js.Float.toString(floatValue)
  | Boolean(boolValue) =>
    if boolValue {
      "true"
    } else {
      "false"
    }
  | Datetime(dtValue) => dtValue
  | Array(arrValue) =>
    let items = Belt.Array.map(arrValue, formatValue)
    "[" ++ Js.Array2.joinWith(items, ", ") ++ "]"
  | Table(_) => "{ }"
  }
}

/** Get value from tomlValue as string */
let asString = (value: tomlValue): option<string> => {
  switch value {
  | String(stringValue) => Some(stringValue)
  | _ => None
  }
}

/** Get value from tomlValue as int */
let asInteger = (value: tomlValue): option<int> => {
  switch value {
  | Integer(intValue) => Some(intValue)
  | _ => None
  }
}

/** Get value from tomlValue as float */
let asFloat = (value: tomlValue): option<float> => {
  switch value {
  | Float(floatValue) => Some(floatValue)
  | _ => None
  }
}

/** Get value from tomlValue as bool */
let asBool = (value: tomlValue): option<bool> => {
  switch value {
  | Boolean(boolValue) => Some(boolValue)
  | _ => None
  }
}

/** Get value from tomlValue as array */
let asArray = (value: tomlValue): option<array<tomlValue>> => {
  switch value {
  | Array(arrValue) => Some(arrValue)
  | _ => None
  }
}

/** Get value from tomlValue as table */
let asTable = (value: tomlValue): option<Js.Dict.t<tomlValue>> => {
  switch value {
  | Table(tableValue) => Some(tableValue)
  | _ => None
  }
}

/** Check if value is a string */
let isString = (value: tomlValue): bool => {
  switch value {
  | String(_) => true
  | _ => false
  }
}

/** Check if value is an integer */
let isInteger = (value: tomlValue): bool => {
  switch value {
  | Integer(_) => true
  | _ => false
  }
}

/** Check if value is a float */
let isFloat = (value: tomlValue): bool => {
  switch value {
  | Float(_) => true
  | _ => false
  }
}

/** Check if value is a boolean */
let isBoolean = (value: tomlValue): bool => {
  switch value {
  | Boolean(_) => true
  | _ => false
  }
}

/** Check if value is a datetime */
let isDatetime = (value: tomlValue): bool => {
  switch value {
  | Datetime(_) => true
  | _ => false
  }
}

/** Check if value is an array */
let isArray = (value: tomlValue): bool => {
  switch value {
  | Array(_) => true
  | _ => false
  }
}

/** Check if value is a table */
let isTable = (value: tomlValue): bool => {
  switch value {
  | Table(_) => true
  | _ => false
  }
}

/** Create an empty table */
let emptyTable = (): tomlValue => {
  Table(Js.Dict.empty())
}

/** Get a value from a table by key */
let get = (table: tomlValue, key: string): option<tomlValue> => {
  switch table {
  | Table(dict) => Js.Dict.get(dict, key)
  | _ => None
  }
}

/** Set a value in a table */
let set = (table: tomlValue, key: string, value: tomlValue): result<tomlValue, tomlError> => {
  switch table {
  | Table(dict) =>
    let newDict = Js.Dict.fromArray(Js.Dict.entries(dict))
    Js.Dict.set(newDict, key, value)
    Ok(Table(newDict))
  | _ => Error(TypeMismatch)
  }
}

/** Check if a table contains a key */
let contains = (table: tomlValue, key: string): bool => {
  switch table {
  | Table(dict) => Js.Dict.get(dict, key)->Belt.Option.isSome
  | _ => false
  }
}

/** Get the number of keys in a table */
let size = (table: tomlValue): int => {
  switch table {
  | Table(dict) => Belt.Array.length(Js.Dict.keys(dict))
  | _ => 0
  }
}

/** Get a value at a dotted path (e.g., "server.host") */
let getPath = (table: tomlValue, path: string): option<tomlValue> => {
  let parts = Js.String2.split(path, ".")
  Belt.Array.reduce(parts, Some(table), (acc, key) => {
    switch acc {
    | None => None
    | Some(currentValue) =>
      switch currentValue {
      | Table(dict) => Js.Dict.get(dict, key)
      | _ => None
      }
    }
  })
}

/** Create a string value */
let string = (value: string): tomlValue => String(value)

/** Create an integer value */
let integer = (value: int): tomlValue => Integer(value)

/** Create a float value */
let float = (value: float): tomlValue => Float(value)

/** Create a boolean value */
let boolean = (value: bool): tomlValue => Boolean(value)

/** Create a datetime value */
let datetime = (value: string): tomlValue => Datetime(value)

/** Create an array value */
let array = (values: array<tomlValue>): tomlValue => Array(values)

/** Create a table from key-value pairs */
let table = (entries: array<(string, tomlValue)>): tomlValue => {
  Table(Js.Dict.fromArray(entries))
}
