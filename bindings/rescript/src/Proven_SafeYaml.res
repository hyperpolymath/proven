// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

/**
 * SafeYaml - Safe YAML parsing and validation operations that cannot crash.
 *
 * Provides secure YAML handling with protection against common YAML
 * vulnerabilities including billion laughs attacks, arbitrary code execution
 * via tags, and deeply nested structures. Implements a safe subset of YAML
 * suitable for configuration files.
 */

/** YAML error types */
type yamlError =
  | ParseError
  | InvalidIndentation
  | MaxDepthExceeded
  | MaxSizeExceeded
  | InvalidCharacter
  | DuplicateKey
  | InvalidReference
  | UnsupportedFeature

/** YAML value types (safe subset) */
type rec yamlValue =
  | Null
  | Boolean(bool)
  | Integer(int)
  | Float(float)
  | String(string)
  | Sequence(array<yamlValue>)
  | Mapping(Js.Dict.t<yamlValue>)

/** Maximum allowed nesting depth */
let maxDepth = 32

/** Maximum document size in bytes */
let maxSize = 1024 * 1024

/** Safe YAML parsing options */
type parseOptions = {
  maxDepth: int,
  maxSize: int,
  allowAnchors: bool,
  allowTags: bool,
  strictIndentation: bool,
}

/** Default parse options (secure defaults) */
let defaultParseOptions: parseOptions = {
  maxDepth,
  maxSize,
  allowAnchors: false,
  allowTags: false,
  strictIndentation: true,
}

/** Check if a string represents a YAML null value */
let isNullValue = (input: string): bool => {
  let trimmed = Js.String2.trim(input)
  Js.String2.length(trimmed) == 0 ||
  trimmed == "null" ||
  trimmed == "Null" ||
  trimmed == "NULL" ||
  trimmed == "~"
}

/** Check if a string represents a YAML boolean value */
let isBoolValue = (input: string): bool => {
  let trimmed = Js.String2.trim(input)
  switch trimmed {
  | "true" | "True" | "TRUE" | "false" | "False" | "FALSE" => true
  | "yes" | "Yes" | "YES" | "no" | "No" | "NO" => true
  | "on" | "On" | "ON" | "off" | "Off" | "OFF" => true
  | _ => false
  }
}

/** Parse a boolean from a YAML string */
let parseBool = (input: string): option<bool> => {
  let trimmed = Js.String2.trim(input)
  switch trimmed {
  | "true" | "True" | "TRUE" | "yes" | "Yes" | "YES" | "on" | "On" | "ON" => Some(true)
  | "false" | "False" | "FALSE" | "no" | "No" | "NO" | "off" | "Off" | "OFF" => Some(false)
  | _ => None
  }
}

/** Check if a string represents a YAML integer */
let isIntegerValue = (input: string): bool => {
  let trimmed = Js.String2.trim(input)
  if Js.String2.length(trimmed) == 0 {
    false
  } else {
    // Check for hex, octal, or decimal
    switch Belt.Int.fromString(trimmed) {
    | Some(_) => true
    | None =>
      // Try hex prefix
      if Js.String2.startsWith(trimmed, "0x") || Js.String2.startsWith(trimmed, "0X") {
        let hexPart = Js.String2.sliceToEnd(trimmed, ~from=2)
        switch Js.Float.fromStringWithRadix(hexPart, ~radix=16) {
        | value if !Js.Float.isNaN(value) => true
        | _ => false
        }
      } else if Js.String2.startsWith(trimmed, "0o") {
        let octPart = Js.String2.sliceToEnd(trimmed, ~from=2)
        switch Js.Float.fromStringWithRadix(octPart, ~radix=8) {
        | value if !Js.Float.isNaN(value) => true
        | _ => false
        }
      } else {
        false
      }
    }
  }
}

/** Parse an integer from a YAML string */
let parseInteger = (input: string): option<int> => {
  let trimmed = Js.String2.trim(input)
  if Js.String2.length(trimmed) == 0 {
    None
  } else {
    switch Belt.Int.fromString(trimmed) {
    | Some(intValue) => Some(intValue)
    | None =>
      if Js.String2.startsWith(trimmed, "0x") || Js.String2.startsWith(trimmed, "0X") {
        let hexPart = Js.String2.sliceToEnd(trimmed, ~from=2)
        switch Js.Float.fromStringWithRadix(hexPart, ~radix=16) {
        | value if !Js.Float.isNaN(value) => Some(Belt.Float.toInt(value))
        | _ => None
        }
      } else if Js.String2.startsWith(trimmed, "0o") {
        let octPart = Js.String2.sliceToEnd(trimmed, ~from=2)
        switch Js.Float.fromStringWithRadix(octPart, ~radix=8) {
        | value if !Js.Float.isNaN(value) => Some(Belt.Float.toInt(value))
        | _ => None
        }
      } else {
        None
      }
    }
  }
}

/** Check if a string represents a YAML float */
let isFloatValue = (input: string): bool => {
  let trimmed = Js.String2.trim(input)
  if Js.String2.length(trimmed) == 0 {
    false
  } else {
    switch trimmed {
    | ".inf" | ".Inf" | ".INF" | "-.inf" | "-.Inf" | "-.INF" => true
    | ".nan" | ".NaN" | ".NAN" => true
    | _ =>
      switch Js.Float.fromString(trimmed) {
      | value if !Js.Float.isNaN(value) => true
      | _ => false
      }
    }
  }
}

/** Parse a float from a YAML string */
let parseFloat = (input: string): option<float> => {
  let trimmed = Js.String2.trim(input)
  switch trimmed {
  | ".inf" | ".Inf" | ".INF" => Some(infinity)
  | "-.inf" | "-.Inf" | "-.INF" => Some(neg_infinity)
  | ".nan" | ".NaN" | ".NAN" => Some(Js.Float._NaN)
  | _ =>
    switch Js.Float.fromString(trimmed) {
    | value if !Js.Float.isNaN(value) => Some(value)
    | _ => None
    }
  }
}

/** Escape a string for safe inclusion in YAML */
let escapeString = (input: string): string => {
  let needsQuoting =
    Js.String2.length(input) == 0 ||
    isNullValue(input) ||
    isBoolValue(input) ||
    isIntegerValue(input) ||
    isFloatValue(input)

  // Check for special characters that need quoting
  let hasSpecialChars =
    Js.String2.includes(input, ":") ||
    Js.String2.includes(input, "#") ||
    Js.String2.includes(input, "{") ||
    Js.String2.includes(input, "}") ||
    Js.String2.includes(input, "[") ||
    Js.String2.includes(input, "]") ||
    Js.String2.includes(input, ",") ||
    Js.String2.includes(input, "&") ||
    Js.String2.includes(input, "*") ||
    Js.String2.includes(input, "!") ||
    Js.String2.includes(input, "|") ||
    Js.String2.includes(input, ">") ||
    Js.String2.includes(input, "'") ||
    Js.String2.includes(input, "\"") ||
    Js.String2.includes(input, "%") ||
    Js.String2.includes(input, "@") ||
    Js.String2.includes(input, "`")

  let hasEscapeChars =
    Js.String2.includes(input, "\n") ||
    Js.String2.includes(input, "\r") ||
    Js.String2.includes(input, "\t") ||
    Js.String2.includes(input, "\\")

  let hasLeadingOrTrailingSpace =
    Js.String2.length(input) > 0 &&
    (Js.String2.charAt(input, 0) == " " ||
      Js.String2.charAt(input, Js.String2.length(input) - 1) == " ")

  if !needsQuoting && !hasSpecialChars && !hasEscapeChars && !hasLeadingOrTrailingSpace {
    input
  } else {
    // Use double quotes with escaping
    let escaped =
      input
      ->Js.String2.replaceByRe(%re("/\\\\/g"), "\\\\")
      ->Js.String2.replaceByRe(%re("/\"/g"), "\\\"")
      ->Js.String2.replaceByRe(%re("/\n/g"), "\\n")
      ->Js.String2.replaceByRe(%re("/\r/g"), "\\r")
      ->Js.String2.replaceByRe(%re("/\t/g"), "\\t")
    "\"" ++ escaped ++ "\""
  }
}

/** Unquote and unescape a YAML string */
let unescapeString = (input: string): string => {
  let trimmed = Js.String2.trim(input)
  if Js.String2.length(trimmed) == 0 {
    ""
  } else {
    let isDoubleQuoted =
      Js.String2.charAt(trimmed, 0) == "\"" &&
      Js.String2.length(trimmed) > 1 &&
      Js.String2.charAt(trimmed, Js.String2.length(trimmed) - 1) == "\""
    let isSingleQuoted =
      Js.String2.charAt(trimmed, 0) == "'" &&
      Js.String2.length(trimmed) > 1 &&
      Js.String2.charAt(trimmed, Js.String2.length(trimmed) - 1) == "'"

    if isSingleQuoted {
      // Single-quoted strings only escape '' as '
      let content = Js.String2.slice(trimmed, ~from=1, ~to_=Js.String2.length(trimmed) - 1)
      Js.String2.replaceByRe(content, %re("/''/g"), "'")
    } else if isDoubleQuoted {
      // Double-quoted strings support escape sequences
      let content = Js.String2.slice(trimmed, ~from=1, ~to_=Js.String2.length(trimmed) - 1)
      content
      ->Js.String2.replaceByRe(%re("/\\\\n/g"), "\n")
      ->Js.String2.replaceByRe(%re("/\\\\r/g"), "\r")
      ->Js.String2.replaceByRe(%re("/\\\\t/g"), "\t")
      ->Js.String2.replaceByRe(%re("/\\\\\"/g"), "\"")
      ->Js.String2.replaceByRe(%re("/\\\\\\\\/g"), "\\")
    } else {
      trimmed
    }
  }
}

/** Validate a string for safe YAML key */
let isValidKey = (key: string): bool => {
  if Js.String2.length(key) == 0 {
    false
  } else {
    let firstChar = Js.String2.charAt(key, 0)
    if firstChar == "-" || firstChar == "?" || firstChar == ":" || firstChar == "[" || firstChar == "{" {
      false
    } else {
      !Js.String2.includes(key, "\n") && !Js.String2.includes(key, "\r")
    }
  }
}

/** Check if content contains potentially dangerous YAML features */
let hasDangerousContent = (input: string): bool => {
  // Check for anchors and aliases (billion laughs attack vector)
  Js.String2.includes(input, "&") ||
  Js.String2.includes(input, "*") ||
  // Check for tags (code execution attack vector)
  Js.String2.includes(input, "!") ||
  // Check for directives
  Js.String2.includes(input, "%")
}

/** Validate YAML indentation */
let validateIndentation = (input: string): result<unit, yamlError> => {
  let lines = Js.String2.split(input, "\n")
  let expectedIndent = ref(None)
  let valid = ref(true)
  let errorFound = ref(None)

  Belt.Array.forEach(lines, line => {
    if valid.contents && Js.String2.length(line) > 0 {
      // Count leading spaces
      let spaceCount = ref(0)
      let hasTab = ref(false)
      let lineLength = Js.String2.length(line)

      while spaceCount.contents < lineLength && valid.contents {
        let currentChar = Js.String2.charAt(line, spaceCount.contents)
        if currentChar == " " {
          spaceCount := spaceCount.contents + 1
        } else if currentChar == "\t" {
          hasTab := true
          valid := false
          errorFound := Some(InvalidIndentation)
        } else {
          // Exit loop when non-whitespace found
          ignore()
        }
        if currentChar != " " && currentChar != "\t" {
          // Break out of while loop
          spaceCount := lineLength + 1
        }
      }
      // Reset spaceCount if we broke out
      if spaceCount.contents > lineLength {
        spaceCount := {
          let count = ref(0)
          for index in 0 to lineLength - 1 {
            if Js.String2.charAt(line, index) == " " {
              count := count.contents + 1
            } else {
              index + lineLength // Break
            }
          }
          count.contents
        }
      }

      // Skip comment-only lines
      if !hasTab.contents && spaceCount.contents < lineLength {
        let firstNonSpace = Js.String2.charAt(line, spaceCount.contents)
        if firstNonSpace != "#" {
          // Determine indent size
          switch expectedIndent.contents {
          | None =>
            if spaceCount.contents > 0 {
              expectedIndent := Some(spaceCount.contents)
            }
          | Some(baseIndent) =>
            if spaceCount.contents > 0 && mod(spaceCount.contents, baseIndent) != 0 {
              valid := false
              errorFound := Some(InvalidIndentation)
            }
          }
        }
      }
    }
  })

  switch errorFound.contents {
  | Some(err) => Error(err)
  | None => Ok()
  }
}

/** Calculate depth of nested structure */
let calculateDepth = (input: string): int => {
  let maxDepthFound = ref(0)
  let currentDepth = ref(0)
  let length = Js.String2.length(input)

  for index in 0 to length - 1 {
    let currentChar = Js.String2.charAt(input, index)
    if currentChar == "{" || currentChar == "[" {
      currentDepth := currentDepth.contents + 1
      if currentDepth.contents > maxDepthFound.contents {
        maxDepthFound := currentDepth.contents
      }
    } else if currentChar == "}" || currentChar == "]" {
      if currentDepth.contents > 0 {
        currentDepth := currentDepth.contents - 1
      }
    }
  }

  maxDepthFound.contents
}

/** Check if a string is valid YAML (basic structural validation) */
let isValid = (input: string, ~options: parseOptions=defaultParseOptions, ()): bool => {
  if Js.String2.length(input) > options.maxSize {
    false
  } else if !options.allowAnchors && !options.allowTags && hasDangerousContent(input) {
    false
  } else if calculateDepth(input) > options.maxDepth {
    false
  } else if options.strictIndentation {
    switch validateIndentation(input) {
    | Ok(_) => true
    | Error(_) => false
    }
  } else {
    true
  }
}

/** Get value from yamlValue as bool */
let asBool = (value: yamlValue): option<bool> => {
  switch value {
  | Boolean(boolValue) => Some(boolValue)
  | _ => None
  }
}

/** Get value from yamlValue as int */
let asInteger = (value: yamlValue): option<int> => {
  switch value {
  | Integer(intValue) => Some(intValue)
  | _ => None
  }
}

/** Get value from yamlValue as float */
let asFloat = (value: yamlValue): option<float> => {
  switch value {
  | Float(floatValue) => Some(floatValue)
  | Integer(intValue) => Some(Belt.Int.toFloat(intValue))
  | _ => None
  }
}

/** Get value from yamlValue as string */
let asString = (value: yamlValue): option<string> => {
  switch value {
  | String(stringValue) => Some(stringValue)
  | _ => None
  }
}

/** Get value from yamlValue as sequence */
let asSequence = (value: yamlValue): option<array<yamlValue>> => {
  switch value {
  | Sequence(seqValue) => Some(seqValue)
  | _ => None
  }
}

/** Get value from yamlValue as mapping */
let asMapping = (value: yamlValue): option<Js.Dict.t<yamlValue>> => {
  switch value {
  | Mapping(mapValue) => Some(mapValue)
  | _ => None
  }
}

/** Check if value is null */
let isNull = (value: yamlValue): bool => {
  switch value {
  | Null => true
  | _ => false
  }
}

/** Get a value from a mapping by key */
let get = (value: yamlValue, key: string): option<yamlValue> => {
  switch value {
  | Mapping(mapValue) => Js.Dict.get(mapValue, key)
  | _ => None
  }
}

/** Get a value at a path (e.g., "foo.bar.baz") */
let getPath = (value: yamlValue, path: string): option<yamlValue> => {
  let parts = Js.String2.split(path, ".")
  Belt.Array.reduce(parts, Some(value), (acc, key) => {
    switch acc {
    | None => None
    | Some(currentValue) =>
      switch currentValue {
      | Mapping(mapValue) => Js.Dict.get(mapValue, key)
      | Sequence(seqValue) =>
        switch Belt.Int.fromString(key) {
        | Some(idx) => Belt.Array.get(seqValue, idx)
        | None => None
        }
      | _ => None
      }
    }
  })
}
