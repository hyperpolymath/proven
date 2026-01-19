// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

/**
 * SafeArgs - Safe command-line argument parsing with bounds checking.
 *
 * Provides safe functions to parse and validate command-line arguments
 * without risk of buffer overflows or undefined behavior. Supports
 * flag parsing, positional arguments, and value extraction.
 */

/** Error types for argument parsing operations */
type argsError =
  | InvalidArgument
  | MissingValue
  | UnknownOption
  | DuplicateOption
  | TooManyArguments
  | InvalidNumber
  | ValueOutOfRange
  | EmptyArgument

/** Argument type classification */
type argType =
  | ShortFlag // -f
  | LongFlag // --flag
  | ShortOption // -o value or -ovalue
  | LongOption // --option=value or --option value
  | Positional // regular argument
  | Separator // --

/** Result of classifying a single argument */
type classifiedArg = {
  argType: argType,
  name: string,
  value: option<string>,
  original: string,
}

/** Check if argument type is a flag (no value expected) */
let isFlag = (argType: argType): bool => {
  switch argType {
  | ShortFlag | LongFlag => true
  | _ => false
  }
}

/** Check if argument type is an option (value expected) */
let isOption = (argType: argType): bool => {
  switch argType {
  | ShortOption | LongOption => true
  | _ => false
  }
}

/** Classify a single argument string */
let classifyArg = (arg: string): classifiedArg => {
  let len = Js.String2.length(arg)

  if len == 0 {
    {argType: Positional, name: "", value: None, original: arg}
  } else if arg == "--" {
    {argType: Separator, name: "", value: None, original: arg}
  } else if len >= 2 && Js.String2.charAt(arg, 0) == "-" && Js.String2.charAt(arg, 1) == "-" {
    // Long option (--something)
    let rest = Js.String2.sliceToEnd(arg, ~from=2)
    let restLen = Js.String2.length(rest)

    if restLen == 0 {
      {argType: Separator, name: "", value: None, original: arg}
    } else {
      // Check for --option=value
      switch Js.String2.indexOf(rest, "=") {
      | -1 => {argType: LongFlag, name: rest, value: None, original: arg}
      | eqPos => {
          let name = Js.String2.slice(rest, ~from=0, ~to_=eqPos)
          let value = Js.String2.sliceToEnd(rest, ~from=eqPos + 1)
          {argType: LongOption, name, value: Some(value), original: arg}
        }
      }
    }
  } else if len >= 2 && Js.String2.charAt(arg, 0) == "-" && Js.String2.charAt(arg, 1) != "-" {
    // Short option (-x or -xvalue)
    let rest = Js.String2.sliceToEnd(arg, ~from=1)
    let restLen = Js.String2.length(rest)

    if restLen == 1 {
      {argType: ShortFlag, name: rest, value: None, original: arg}
    } else {
      // -xvalue (short option with attached value)
      let name = Js.String2.slice(rest, ~from=0, ~to_=1)
      let value = Js.String2.sliceToEnd(rest, ~from=1)
      {argType: ShortOption, name, value: Some(value), original: arg}
    }
  } else {
    // Positional argument
    {argType: Positional, name: arg, value: None, original: arg}
  }
}

/** Safe argument iterator state */
type argIterator = {
  args: array<string>,
  mutable index: int,
  mutable pastSeparator: bool,
  mutable positionalCount: int,
  maxArgs: int,
}

/** Create a new argument iterator */
let createIterator = (~maxArgs: int=1000, args: array<string>): argIterator => {
  {
    args,
    index: 0,
    pastSeparator: false,
    positionalCount: 0,
    maxArgs,
  }
}

/** Get the next argument from iterator */
let nextArg = (iter: argIterator): option<classifiedArg> => {
  if iter.index >= Belt.Array.length(iter.args) {
    None
  } else if iter.index >= iter.maxArgs {
    None
  } else {
    let arg = Belt.Array.getUnsafe(iter.args, iter.index)
    iter.index = iter.index + 1

    if iter.pastSeparator {
      iter.positionalCount = iter.positionalCount + 1
      Some({argType: Positional, name: arg, value: None, original: arg})
    } else {
      let classified = classifyArg(arg)

      if classified.argType == Separator {
        iter.pastSeparator = true
      } else if classified.argType == Positional {
        iter.positionalCount = iter.positionalCount + 1
      }

      Some(classified)
    }
  }
}

/** Peek at the next argument without consuming it */
let peekArg = (iter: argIterator): option<string> => {
  if iter.index >= Belt.Array.length(iter.args) || iter.index >= iter.maxArgs {
    None
  } else {
    Some(Belt.Array.getUnsafe(iter.args, iter.index))
  }
}

/** Skip the next argument */
let skipArg = (iter: argIterator): unit => {
  if iter.index < Belt.Array.length(iter.args) && iter.index < iter.maxArgs {
    iter.index = iter.index + 1
  }
}

/** Get current position in iterator */
let position = (iter: argIterator): int => {
  iter.index
}

/** Get remaining argument count */
let remaining = (iter: argIterator): int => {
  let maxRemaining = iter.maxArgs - min(iter.index, iter.maxArgs)
  let actualRemaining = Belt.Array.length(iter.args) - min(iter.index, Belt.Array.length(iter.args))
  min(maxRemaining, actualRemaining)
}

/** Reset iterator to beginning */
let resetIterator = (iter: argIterator): unit => {
  iter.index = 0
  iter.pastSeparator = false
  iter.positionalCount = 0
}

/** Parse an integer from a string with bounds checking */
let parseInt = (str: string): result<int, argsError> => {
  if Js.String2.length(str) == 0 {
    Error(EmptyArgument)
  } else {
    switch Belt.Int.fromString(str) {
    | Some(n) => Ok(n)
    | None => Error(InvalidNumber)
    }
  }
}

/** Parse an integer with minimum and maximum bounds */
let parseIntBounded = (str: string, minVal: int, maxVal: int): result<int, argsError> => {
  switch parseInt(str) {
  | Error(e) => Error(e)
  | Ok(value) =>
    if value < minVal || value > maxVal {
      Error(ValueOutOfRange)
    } else {
      Ok(value)
    }
  }
}

/** Parse a float from a string */
let parseFloat = (str: string): result<float, argsError> => {
  if Js.String2.length(str) == 0 {
    Error(EmptyArgument)
  } else {
    switch Belt.Float.fromString(str) {
    | Some(n) => Ok(n)
    | None => Error(InvalidNumber)
    }
  }
}

/** Parse a boolean from common string representations */
let parseBool = (str: string): result<bool, argsError> => {
  let lower = Js.String2.toLowerCase(str)
  switch lower {
  | "true" | "yes" | "1" | "on" => Ok(true)
  | "false" | "no" | "0" | "off" => Ok(false)
  | _ => Error(InvalidArgument)
  }
}

/** Validate that a string is a valid option name (alphanumeric and hyphens) */
let isValidOptionName = (name: string): bool => {
  let len = Js.String2.length(name)
  if len == 0 {
    false
  } else {
    // First character must be alphanumeric
    let firstCode = Js.String2.charCodeAt(name, 0)->Belt.Float.toInt
    let firstIsAlphanumeric =
      (firstCode >= 48 && firstCode <= 57) || // 0-9
      (firstCode >= 65 && firstCode <= 90) || // A-Z
      (firstCode >= 97 && firstCode <= 122) // a-z

    if !firstIsAlphanumeric {
      false
    } else {
      // Cannot end with hyphen
      let lastChar = Js.String2.charAt(name, len - 1)
      if lastChar == "-" {
        false
      } else {
        // All characters must be alphanumeric, hyphen, or underscore
        let valid = ref(true)
        for i in 0 to len - 1 {
          if valid.contents {
            let charCode = Js.String2.charCodeAt(name, i)->Belt.Float.toInt
            let isValidChar =
              (charCode >= 48 && charCode <= 57) || // 0-9
              (charCode >= 65 && charCode <= 90) || // A-Z
              (charCode >= 97 && charCode <= 122) || // a-z
              charCode == 45 || // -
              charCode == 95 // _
            if !isValidChar {
              valid := false
            }
          }
        }
        valid.contents
      }
    }
  }
}

/** Result of extracting a value */
type extractValueResult = {value: string, consumedNext: bool}

/** Extract value for an option, either attached or from next argument */
let extractValue = (
  current: classifiedArg,
  nextArgValue: option<string>,
): result<extractValueResult, argsError> => {
  // Value already attached (--opt=val or -oval)
  switch current.value {
  | Some(v) => Ok({value: v, consumedNext: false})
  | None =>
    // Need to get from next argument
    switch nextArgValue {
    | Some(v) =>
      // Don't consume if next looks like an option
      if Js.String2.length(v) > 0 && Js.String2.charAt(v, 0) == "-" {
        Error(MissingValue)
      } else {
        Ok({value: v, consumedNext: true})
      }
    | None => Error(MissingValue)
    }
  }
}

/** Count the number of positional arguments in an argument list */
let countPositional = (args: array<string>): int => {
  let count = ref(0)
  let pastSeparator = ref(false)

  Belt.Array.forEach(args, arg => {
    if arg == "--" {
      pastSeparator := true
    } else if pastSeparator.contents {
      count := count.contents + 1
    } else {
      let classified = classifyArg(arg)
      if classified.argType == Positional {
        count := count.contents + 1
      }
    }
  })

  count.contents
}

/** Check if an argument matches a short or long option name */
let matchesOption = (arg: classifiedArg, short: option<string>, long: option<string>): bool => {
  switch arg.argType {
  | ShortFlag | ShortOption =>
    switch short {
    | Some(s) => arg.name == s
    | None => false
    }
  | LongFlag | LongOption =>
    switch long {
    | Some(l) => arg.name == l
    | None => false
    }
  | _ => false
  }
}

/** Check if argument needs quoting for shell display */
let needsQuoting = (arg: string): bool => {
  if Js.String2.length(arg) == 0 {
    true
  } else {
    let specialChars = [
      " ",
      "\t",
      "\n",
      "\r",
      "\"",
      "'",
      "\\",
      "$",
      "`",
      "!",
      "*",
      "?",
      "[",
      "]",
      "(",
      ")",
      "{",
      "}",
      "|",
      "&",
      ";",
      "<",
      ">",
    ]
    let chars = Js.String2.split(arg, "")
    Js.Array2.some(chars, char => Js.Array2.includes(specialChars, char))
  }
}

/** Safely join arguments with spaces (for display/logging) */
let joinArgs = (args: array<string>): string => {
  Js.Array2.joinWith(args, " ")
}

/** Option entry for storage */
type optionEntry = {
  name: string,
  value: option<string>,
  count: int,
}

/** Option set for storing parsed options */
type optionSet = {
  mutable entries: array<optionEntry>,
  maxOptions: int,
}

/** Create a new option set */
let createOptionSet = (~maxOptions: int=100): optionSet => {
  {entries: [], maxOptions}
}

/** Set an option in the option set */
let setOption = (set: optionSet, name: string, value: option<string>): result<unit, argsError> => {
  // Check if already exists
  let existingIndex = Belt.Array.getIndexBy(set.entries, entry => entry.name == name)

  switch existingIndex {
  | Some(idx) =>
    // Update existing
    let entry = Belt.Array.getUnsafe(set.entries, idx)
    let updatedEntry = {...entry, value, count: entry.count + 1}
    set.entries = Belt.Array.mapWithIndex(set.entries, (i, e) =>
      if i == idx {
        updatedEntry
      } else {
        e
      }
    )
    Ok()
  | None =>
    // Add new
    if Belt.Array.length(set.entries) >= set.maxOptions {
      Error(TooManyArguments)
    } else {
      set.entries = Belt.Array.concat(set.entries, [{name, value, count: 1}])
      Ok()
    }
  }
}

/** Get an option's value from the option set */
let getOption = (set: optionSet, name: string): option<string> => {
  switch Belt.Array.getBy(set.entries, entry => entry.name == name) {
  | Some(entry) => entry.value
  | None => None
  }
}

/** Check if an option was set */
let hasOption = (set: optionSet, name: string): bool => {
  Belt.Array.some(set.entries, entry => entry.name == name)
}

/** Get how many times an option was specified */
let getOptionCount = (set: optionSet, name: string): int => {
  switch Belt.Array.getBy(set.entries, entry => entry.name == name) {
  | Some(entry) => entry.count
  | None => 0
  }
}

/** Convert argument type to string */
let argTypeToString = (argType: argType): string => {
  switch argType {
  | ShortFlag => "short_flag"
  | LongFlag => "long_flag"
  | ShortOption => "short_option"
  | LongOption => "long_option"
  | Positional => "positional"
  | Separator => "separator"
  }
}

/** Convert error to string */
let argsErrorToString = (error: argsError): string => {
  switch error {
  | InvalidArgument => "Invalid argument"
  | MissingValue => "Missing value for option"
  | UnknownOption => "Unknown option"
  | DuplicateOption => "Duplicate option"
  | TooManyArguments => "Too many arguments"
  | InvalidNumber => "Invalid number"
  | ValueOutOfRange => "Value out of range"
  | EmptyArgument => "Empty argument"
  }
}

/** Collect all positional arguments from an argument list */
let collectPositional = (args: array<string>): array<string> => {
  let result = ref([])
  let pastSeparator = ref(false)

  Belt.Array.forEach(args, arg => {
    if arg == "--" {
      pastSeparator := true
    } else if pastSeparator.contents {
      result := Belt.Array.concat(result.contents, [arg])
    } else {
      let classified = classifyArg(arg)
      if classified.argType == Positional {
        result := Belt.Array.concat(result.contents, [arg])
      }
    }
  })

  result.contents
}

/** Parse a list of comma-separated values */
let parseCommaSeparated = (str: string): array<string> => {
  if Js.String2.length(str) == 0 {
    []
  } else {
    Js.String2.split(str, ",")
    ->Js.Array2.map(s => Js.String2.trim(s))
    ->Js.Array2.filter(s => Js.String2.length(s) > 0)
  }
}

/** Check if a string looks like a negative number (not an option) */
let isNegativeNumber = (str: string): bool => {
  let len = Js.String2.length(str)
  if len < 2 {
    false
  } else if Js.String2.charAt(str, 0) != "-" {
    false
  } else {
    let rest = Js.String2.sliceToEnd(str, ~from=1)
    switch Belt.Float.fromString(rest) {
    | Some(_) => true
    | None => false
    }
  }
}

/** Classify argument with negative number detection */
let classifyArgWithNumbers = (arg: string): classifiedArg => {
  if isNegativeNumber(arg) {
    {argType: Positional, name: arg, value: None, original: arg}
  } else {
    classifyArg(arg)
  }
}

/** Get help-style usage string for an option */
let formatOptionHelp = (short: option<string>, long: option<string>, description: string): string => {
  let shortPart = switch short {
  | Some(s) => "-" ++ s
  | None => ""
  }
  let longPart = switch long {
  | Some(l) => "--" ++ l
  | None => ""
  }

  let optionPart = switch (short, long) {
  | (Some(_), Some(_)) => shortPart ++ ", " ++ longPart
  | (Some(_), None) => shortPart
  | (None, Some(_)) => longPart
  | (None, None) => ""
  }

  "  " ++ optionPart ++ "    " ++ description
}
