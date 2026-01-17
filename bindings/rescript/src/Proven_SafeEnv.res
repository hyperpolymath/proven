// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

/**
 * SafeEnv - Environment variable handling with validation and sanitization.
 *
 * Provides safe access to environment variables with:
 * - Validation of variable names and values
 * - Protection against injection attacks
 * - Type-safe parsing of common value types
 * - Default value handling without crashes
 */

/** Error types for environment variable operations */
type envError =
  | InvalidName
  | InvalidValue
  | NotFound
  | ParseError
  | ValueTooLong
  | EmptyValue

/** Maximum allowed length for environment variable names */
let maxNameLength = 255

/** Maximum allowed length for environment variable values */
let maxValueLength = 32767

/** External binding to get environment variable (Node.js) */
@val @scope(("process", "env"))
external getEnvUnsafe: string => Js.Nullable.t<string> = ""

/** Get all environment variables (Node.js) */
@val @scope("process")
external envObject: Js.Dict.t<string> = "env"

/** Check if an environment variable name is valid */
let isValidName = (name: string): bool => {
  let len = Js.String2.length(name)
  if len == 0 || len > maxNameLength {
    false
  } else {
    // Check for forbidden characters: '=' and null bytes
    !Js.String2.includes(name, "=") && !Js.String2.includes(name, "\x00")
  }
}

/** Check if an environment variable value is valid */
let isValidValue = (value: string): bool => {
  let len = Js.String2.length(value)
  if len > maxValueLength {
    false
  } else {
    // Check for null bytes
    !Js.String2.includes(value, "\x00")
  }
}

/** Safely get an environment variable, returning None if not found or invalid */
let get = (name: string): option<string> => {
  if !isValidName(name) {
    None
  } else {
    let value = Js.Dict.get(envObject, name)
    value
  }
}

/** Get an environment variable with a default value if not found */
let getOrDefault = (name: string, default: string): string => {
  switch get(name) {
  | Some(v) => v
  | None => default
  }
}

/** Get an environment variable, returning error if not found */
let getRequired = (name: string): result<string, envError> => {
  if !isValidName(name) {
    Error(InvalidName)
  } else {
    switch get(name) {
    | Some(v) => Ok(v)
    | None => Error(NotFound)
    }
  }
}

/** Parse an environment variable as an integer */
let getInt = (name: string): result<int, envError> => {
  switch get(name) {
  | None => Error(NotFound)
  | Some(value) =>
    switch Belt.Int.fromString(value) {
    | Some(i) => Ok(i)
    | None => Error(ParseError)
    }
  }
}

/** Parse an environment variable as an integer with a default value */
let getIntOrDefault = (name: string, default: int): int => {
  switch getInt(name) {
  | Ok(i) => i
  | Error(_) => default
  }
}

/** Parse an environment variable as a boolean */
let getBool = (name: string): result<bool, envError> => {
  switch get(name) {
  | None => Error(NotFound)
  | Some(value) =>
    let lower = Js.String2.toLowerCase(value)
    if lower == "true" || lower == "1" || lower == "yes" {
      Ok(true)
    } else if lower == "false" || lower == "0" || lower == "no" {
      Ok(false)
    } else {
      Error(ParseError)
    }
  }
}

/** Parse an environment variable as a boolean with a default value */
let getBoolOrDefault = (name: string, default: bool): bool => {
  switch getBool(name) {
  | Ok(b) => b
  | Error(_) => default
  }
}

/** Parse an environment variable as a float */
let getFloat = (name: string): result<float, envError> => {
  switch get(name) {
  | None => Error(NotFound)
  | Some(value) =>
    let f = Js.Float.fromString(value)
    if Js.Float.isNaN(f) {
      Error(ParseError)
    } else {
      Ok(f)
    }
  }
}

/** Parse an environment variable as a float with a default value */
let getFloatOrDefault = (name: string, default: float): float => {
  switch getFloat(name) {
  | Ok(f) => f
  | Error(_) => default
  }
}

/** Split an environment variable value by a delimiter */
let getSplit = (name: string, delimiter: string): result<array<string>, envError> => {
  switch get(name) {
  | None => Error(NotFound)
  | Some(value) => Ok(Js.String2.split(value, delimiter))
  }
}

/** Split an environment variable value by a delimiter with default */
let getSplitOrDefault = (name: string, delimiter: string, default: array<string>): array<string> => {
  switch getSplit(name, delimiter) {
  | Ok(parts) => parts
  | Error(_) => default
  }
}

/** Sanitize an environment variable value by removing control characters */
let sanitizeValue = (value: string): string => {
  let result = ref("")
  let len = Js.String2.length(value)

  for i in 0 to len - 1 {
    let code = Js.String2.charCodeAt(value, i)->Belt.Float.toInt
    // Skip control characters except tab, newline, carriage return
    if code >= 32 || code == 9 || code == 10 || code == 13 {
      // Skip DEL character (127)
      if code != 127 {
        result := result.contents ++ Js.String2.charAt(value, i)
      }
    }
  }

  result.contents
}

/** Check if an environment variable exists */
let exists = (name: string): bool => {
  Belt.Option.isSome(get(name))
}

/** Check if an environment variable exists and has a non-empty value */
let existsNonEmpty = (name: string): bool => {
  switch get(name) {
  | None => false
  | Some(value) => Js.String2.length(value) > 0
  }
}

/** Get an environment variable, returning error if empty */
let getNonEmpty = (name: string): result<string, envError> => {
  switch getRequired(name) {
  | Error(e) => Error(e)
  | Ok(value) =>
    if Js.String2.length(value) == 0 {
      Error(EmptyValue)
    } else {
      Ok(value)
    }
  }
}

/** Get all environment variable names */
let getNames = (): array<string> => {
  Js.Dict.keys(envObject)
}

/** Get all environment variables as key-value pairs */
let getAll = (): array<(string, string)> => {
  Js.Dict.entries(envObject)
}

/** Check if running in production environment */
let isProduction = (): bool => {
  switch get("NODE_ENV") {
  | Some(env) => Js.String2.toLowerCase(env) == "production"
  | None => false
  }
}

/** Check if running in development environment */
let isDevelopment = (): bool => {
  switch get("NODE_ENV") {
  | Some(env) => Js.String2.toLowerCase(env) == "development"
  | None => false
  }
}

/** Check if running in test environment */
let isTest = (): bool => {
  switch get("NODE_ENV") {
  | Some(env) => Js.String2.toLowerCase(env) == "test"
  | None => false
  }
}

/** Check if debug mode is enabled */
let isDebug = (): bool => {
  getBoolOrDefault("DEBUG", false)
}

/** Get the current environment name */
let getEnvironment = (): string => {
  getOrDefault("NODE_ENV", "development")
}

/** Get PORT environment variable as integer */
let getPort = (default: int): int => {
  getIntOrDefault("PORT", default)
}

/** Get HOST environment variable */
let getHost = (default: string): string => {
  getOrDefault("HOST", default)
}

/** Validate that all required environment variables are set */
let validateRequired = (names: array<string>): result<unit, array<string>> => {
  let missing = names->Belt.Array.keep(name => !existsNonEmpty(name))
  if Belt.Array.length(missing) == 0 {
    Ok()
  } else {
    Error(missing)
  }
}

/** Get a typed configuration value from environment */
type configValue =
  | StringValue(string)
  | IntValue(int)
  | FloatValue(float)
  | BoolValue(bool)
  | ArrayValue(array<string>)

/** Parse an environment variable into a config value */
let getConfigValue = (name: string, valueType: string): result<configValue, envError> => {
  switch valueType {
  | "string" =>
    switch get(name) {
    | Some(v) => Ok(StringValue(v))
    | None => Error(NotFound)
    }
  | "int" =>
    switch getInt(name) {
    | Ok(i) => Ok(IntValue(i))
    | Error(e) => Error(e)
    }
  | "float" =>
    switch getFloat(name) {
    | Ok(f) => Ok(FloatValue(f))
    | Error(e) => Error(e)
    }
  | "bool" =>
    switch getBool(name) {
    | Ok(b) => Ok(BoolValue(b))
    | Error(e) => Error(e)
    }
  | "array" =>
    switch getSplit(name, ",") {
    | Ok(arr) => Ok(ArrayValue(arr))
    | Error(e) => Error(e)
    }
  | _ => Error(ParseError)
  }
}
