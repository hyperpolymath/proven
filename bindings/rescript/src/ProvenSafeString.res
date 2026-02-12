// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
/**
 * SafeString - String operations that cannot crash
 *
 * ReScript bindings to proven's formally verified string module
 */

open ProvenResult

// JavaScript bindings to proven/safe_string
module SafeStringJs = {
  @module("proven/safe_string") @scope("SafeString")
  external isEmpty: string => bool = "isEmpty"

  @module("proven/safe_string") @scope("SafeString")
  external length: string => int = "length"

  @module("proven/safe_string") @scope("SafeString")
  external charAt: (string, int) => jsResult<string> = "charAt"

  @module("proven/safe_string") @scope("SafeString")
  external substring: (string, int, option<int>) => jsResult<string> = "substring"

  @module("proven/safe_string") @scope("SafeString")
  external trim: string => string = "trim"

  @module("proven/safe_string") @scope("SafeString")
  external toUpperCase: string => string = "toUpperCase"

  @module("proven/safe_string") @scope("SafeString")
  external toLowerCase: string => string = "toLowerCase"

  @module("proven/safe_string") @scope("SafeString")
  external contains: (string, string) => bool = "contains"

  @module("proven/safe_string") @scope("SafeString")
  external startsWith: (string, string) => bool = "startsWith"

  @module("proven/safe_string") @scope("SafeString")
  external endsWith: (string, string) => bool = "endsWith"

  @module("proven/safe_string") @scope("SafeString")
  external split: (string, string) => array<string> = "split"

  @module("proven/safe_string") @scope("SafeString")
  external join: (array<string>, string) => string = "join"

  @module("proven/safe_string") @scope("SafeString")
  external replace: (string, string, string) => string = "replace"

  @module("proven/safe_string") @scope("SafeString")
  external replaceAll: (string, string, string) => string = "replaceAll"

  @module("proven/safe_string") @scope("SafeString")
  external repeat: (string, int) => jsResult<string> = "repeat"

  @module("proven/safe_string") @scope("SafeString")
  external padStart: (string, int, option<string>) => string = "padStart"

  @module("proven/safe_string") @scope("SafeString")
  external padEnd: (string, int, option<string>) => string = "padEnd"
}

// Type-safe ReScript API

/**
 * Check if string is empty
 */
let isEmpty = SafeStringJs.isEmpty

/**
 * Get string length
 */
let length = SafeStringJs.length

/**
 * Get character at index (safe)
 *
 * @param str String to access
 * @param index Character index
 * @returns Result with character or error if index out of bounds
 */
let charAt = (str: string, index: int) => {
  SafeStringJs.charAt(str, index)->fromJs
}

/**
 * Get substring (safe)
 *
 * @param str String to slice
 * @param start Start index
 * @param end Optional end index
 * @returns Result with substring or error if indices invalid
 */
let substring = (str: string, ~start: int, ~end: option<int>=?) => {
  SafeStringJs.substring(str, start, end)->fromJs
}

/**
 * Trim whitespace from both ends
 */
let trim = SafeStringJs.trim

/**
 * Convert to uppercase
 */
let toUpperCase = SafeStringJs.toUpperCase

/**
 * Convert to lowercase
 */
let toLowerCase = SafeStringJs.toLowerCase

/**
 * Check if string contains substring
 */
let contains = SafeStringJs.contains

/**
 * Check if string starts with prefix
 */
let startsWith = SafeStringJs.startsWith

/**
 * Check if string ends with suffix
 */
let endsWith = SafeStringJs.endsWith

/**
 * Split string by delimiter
 */
let split = SafeStringJs.split

/**
 * Join array of strings with delimiter
 */
let join = SafeStringJs.join

/**
 * Replace first occurrence of substring
 */
let replace = SafeStringJs.replace

/**
 * Replace all occurrences of substring
 */
let replaceAll = SafeStringJs.replaceAll

/**
 * Repeat string n times (safe)
 *
 * @param str String to repeat
 * @param count Number of repetitions
 * @returns Result with repeated string or error if count invalid
 */
let repeat = (str: string, count: int) => {
  SafeStringJs.repeat(str, count)->fromJs
}

/**
 * Pad string at start
 *
 * @param str String to pad
 * @param targetLength Target length
 * @param padString Optional padding string (default: space)
 */
let padStart = (str: string, targetLength: int, ~padString: option<string>=?) => {
  SafeStringJs.padStart(str, targetLength, padString)
}

/**
 * Pad string at end
 *
 * @param str String to pad
 * @param targetLength Target length
 * @param padString Optional padding string (default: space)
 */
let padEnd = (str: string, targetLength: int, ~padString: option<string>=?) => {
  SafeStringJs.padEnd(str, targetLength, padString)
}
