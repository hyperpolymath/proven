// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
/**
 * SafeJson - JSON operations that cannot crash
 *
 * ReScript bindings to proven's formally verified JSON module
 */

open ProvenResult

// JSON value types
type rec jsonValue =
  | @as("null") Null
  | @as("boolean") Boolean(bool)
  | @as("number") Number(float)
  | @as("string") String(string)
  | @as("array") Array(array<jsonValue>)
  | @as("object") Object(Js.Dict.t<jsonValue>)

// JavaScript bindings to proven/safe_json
module SafeJsonJs = {
  @module("proven/safe_json") @scope("SafeJson")
  external parse: string => jsResult<Js.Json.t> = "parse"

  @module("proven/safe_json") @scope("SafeJson")
  external stringify: (Js.Json.t, option<int>) => jsResult<string> = "stringify"

  @module("proven/safe_json") @scope("SafeJson")
  external isValid: string => bool = "isValid"

  @module("proven/safe_json") @scope("SafeJson")
  external get: (Js.Json.t, string) => jsResult<option<Js.Json.t>> = "get"

  @module("proven/safe_json") @scope("SafeJson")
  external getPath: (Js.Json.t, array<string>) => jsResult<option<Js.Json.t>> = "getPath"

  @module("proven/safe_json") @scope("SafeJson")
  external merge: (Js.Json.t, Js.Json.t) => jsResult<Js.Json.t> = "merge"

  @module("proven/safe_json") @scope("SafeJson")
  external validate: (Js.Json.t, Js.Json.t) => jsResult<bool> = "validate"
}

// Type-safe ReScript API

/**
 * Parse JSON string safely
 *
 * @param jsonString JSON string to parse
 * @returns Result with parsed JSON or error
 */
let parse = (jsonString: string) => {
  SafeJsonJs.parse(jsonString)->fromJs
}

/**
 * Stringify JSON value safely
 *
 * @param json JSON value to stringify
 * @param indent Optional indentation (number of spaces)
 * @returns Result with JSON string or error
 */
let stringify = (json: Js.Json.t, ~indent: option<int>=?) => {
  SafeJsonJs.stringify(json, indent)->fromJs
}

/**
 * Check if string is valid JSON
 *
 * @param jsonString String to validate
 * @returns true if valid JSON, false otherwise
 */
let isValid = SafeJsonJs.isValid

/**
 * Get property from JSON object
 *
 * @param json JSON object
 * @param key Property name
 * @returns Result with optional value or error
 */
let get = (json: Js.Json.t, key: string) => {
  SafeJsonJs.get(json, key)->fromJs
}

/**
 * Get value at path in nested JSON
 *
 * @param json JSON object
 * @param path Array of keys (e.g., ["user", "name"])
 * @returns Result with optional value or error
 */
let getPath = (json: Js.Json.t, path: array<string>) => {
  SafeJsonJs.getPath(json, path)->fromJs
}

/**
 * Merge two JSON objects
 *
 * @param json1 First JSON object
 * @param json2 Second JSON object (values override first)
 * @returns Result with merged JSON or error
 */
let merge = (json1: Js.Json.t, json2: Js.Json.t) => {
  SafeJsonJs.merge(json1, json2)->fromJs
}

/**
 * Validate JSON against schema
 *
 * @param json JSON value to validate
 * @param schema JSON schema
 * @returns Result with validation result or error
 */
let validate = (json: Js.Json.t, schema: Js.Json.t) => {
  SafeJsonJs.validate(json, schema)->fromJs
}

// Helper functions for common JSON operations

/**
 * Parse and extract a value at path
 */
let parseAndGetPath = (jsonString: string, path: array<string>) => {
  switch parse(jsonString) {
  | Ok(json) => getPath(json, path)
  | Error(e) => Error(e)
  }
}

/**
 * Parse and get property
 */
let parseAndGet = (jsonString: string, key: string) => {
  switch parse(jsonString) {
  | Ok(json) => get(json, key)
  | Error(e) => Error(e)
  }
}
