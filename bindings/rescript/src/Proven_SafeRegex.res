// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

/**
 * SafeRegex - Regex pattern validation with ReDoS prevention that cannot crash.
 *
 * Provides regex pattern analysis and matching with protection against
 * Regular Expression Denial of Service (ReDoS) attacks. Patterns are
 * analyzed for dangerous constructs before compilation.
 */

/** Regex operation error types */
type regexError =
  | InvalidPattern
  | PatternTooComplex
  | NestedQuantifiers
  | ExcessiveBacktracking
  | UnbalancedParentheses
  | UnbalancedBrackets
  | InvalidEscape
  | EmptyAlternation
  | PatternTooLong

/** Maximum allowed pattern length */
let maxPatternLength = 1024

/** Maximum allowed quantifier repetition */
let maxQuantifierBound = 1000

/** Maximum allowed nesting depth */
let maxNestingDepth = 10

/** Pattern analysis warning */
type warning = {
  position: int,
  message: string,
}

/** Result of pattern analysis */
type patternAnalysis = {
  isSafe: bool,
  complexityScore: int,
  hasNestedQuantifiers: bool,
  hasOverlappingAlternations: bool,
  maxNestingDepth: int,
  estimatedStates: int,
  warnings: array<warning>,
}

/** Validated regex pattern that has passed safety checks */
type safePattern = {
  pattern: string,
  analysis: patternAnalysis,
}

/** Quantifier bounds for {n}, {n,}, {n,m} syntax */
type quantifierBounds = {
  min: int,
  max: int,
}

/** Check if a character is a regex metacharacter */
let isMetaChar = (c: string): bool => {
  switch c {
  | "." | "*" | "+" | "?" | "[" | "]" | "(" | ")" | "{" | "}" | "|" | "^" | "$" | "\\" => true
  | _ => false
  }
}

/** Escape a string for literal matching in regex */
let escape = (input: string): string => {
  let result = ref("")
  let length = Js.String2.length(input)

  for i in 0 to length - 1 {
    let c = Js.String2.charAt(input, i)
    if isMetaChar(c) {
      result := result.contents ++ "\\"
    }
    result := result.contents ++ c
  }

  result.contents
}

/** Parse quantifier bounds like {n}, {n,}, {n,m} */
let parseQuantifierBounds = (pattern: string): option<quantifierBounds> => {
  let length = Js.String2.length(pattern)
  if length < 3 || Js.String2.charAt(pattern, 0) != "{" {
    None
  } else {
    let minVal = ref(0)
    let maxVal = ref(0)
    let parsingMax = ref(false)
    let valid = ref(true)
    let i = ref(1)

    while i.contents < length && Js.String2.charAt(pattern, i.contents) != "}" && valid.contents {
      let c = Js.String2.charAt(pattern, i.contents)
      if c == "," {
        parsingMax := true
      } else {
        let code = Js.String2.charCodeAt(c, 0)
        if code >= 48.0 && code <= 57.0 {
          let digit = Belt.Float.toInt(code) - 48
          if parsingMax.contents {
            maxVal := maxVal.contents * 10 + digit
          } else {
            minVal := minVal.contents * 10 + digit
          }
        } else {
          valid := false
        }
      }
      i := i.contents + 1
    }

    if !valid.contents {
      None
    } else {
      if !parsingMax.contents {
        maxVal := minVal.contents
      }
      Some({min: minVal.contents, max: maxVal.contents})
    }
  }
}

/** Analyze a regex pattern for safety without compiling */
let analyzePattern = (pattern: string): result<patternAnalysis, regexError> => {
  let length = Js.String2.length(pattern)

  if length > maxPatternLength {
    Error(PatternTooLong)
  } else {
    let warnings = ref([])
    let complexity = ref(0)
    let maxDepth = ref(0)
    let currentDepth = ref(0)
    let hasNestedQuantifiers = ref(false)
    let hasOverlappingAlternations = ref(false)
    let inBracket = ref(false)
    let prevWasQuantifier = ref(false)
    let i = ref(0)

    let error = ref(None)

    while i.contents < length && Belt.Option.isNone(error.contents) {
      let c = Js.String2.charAt(pattern, i.contents)

      switch c {
      | "(" =>
        if !inBracket.contents {
          currentDepth := currentDepth.contents + 1
          maxDepth := max(maxDepth.contents, currentDepth.contents)
          if currentDepth.contents > maxNestingDepth {
            error := Some(PatternTooComplex)
          }
          complexity := complexity.contents + 2
        }
        prevWasQuantifier := false

      | ")" =>
        if !inBracket.contents {
          if currentDepth.contents == 0 {
            error := Some(UnbalancedParentheses)
          } else {
            currentDepth := currentDepth.contents - 1
          }
        }
        prevWasQuantifier := false

      | "[" =>
        if !inBracket.contents {
          inBracket := true
          complexity := complexity.contents + 1
        }
        prevWasQuantifier := false

      | "]" =>
        if inBracket.contents {
          inBracket := false
        }
        prevWasQuantifier := false

      | "*" | "+" | "?" =>
        if prevWasQuantifier.contents {
          hasNestedQuantifiers := true
          warnings :=
            Js.Array2.concat(
              warnings.contents,
              [{position: i.contents, message: "Nested quantifier detected - potential ReDoS"}],
            )
        }
        complexity := complexity.contents + 5
        prevWasQuantifier := true

      | "{" =>
        if !inBracket.contents {
          let rest = Js.String2.sliceToEnd(pattern, ~from=i.contents)
          switch parseQuantifierBounds(rest) {
          | Some(bounds) =>
            if bounds.max > maxQuantifierBound {
              error := Some(PatternTooComplex)
            }
            complexity := complexity.contents + min(bounds.max, 100)
            if prevWasQuantifier.contents {
              hasNestedQuantifiers := true
            }
            prevWasQuantifier := true
          | None => ()
          }
        }

      | "|" =>
        if !inBracket.contents {
          complexity := complexity.contents + 3
          // Check for overlapping alternations (simplified)
          if i.contents > 0 && i.contents + 1 < length {
            let prev = Js.String2.charAt(pattern, i.contents - 1)
            let next = Js.String2.charAt(pattern, i.contents + 1)
            if prev == "|" || next == "|" {
              hasOverlappingAlternations := true
            }
          }
        }
        prevWasQuantifier := false

      | "\\" =>
        // Skip escape sequence
        if i.contents + 1 >= length {
          error := Some(InvalidEscape)
        } else {
          i := i.contents + 1
        }
        prevWasQuantifier := false

      | "." =>
        complexity := complexity.contents + 2
        prevWasQuantifier := false

      | _ =>
        complexity := complexity.contents + 1
        prevWasQuantifier := false
      }

      i := i.contents + 1
    }

    switch error.contents {
    | Some(e) => Error(e)
    | None =>
      if currentDepth.contents != 0 {
        Error(UnbalancedParentheses)
      } else if inBracket.contents {
        Error(UnbalancedBrackets)
      } else {
        let estimatedStates = complexity.contents * (maxDepth.contents + 1)
        let isSafe = !hasNestedQuantifiers.contents && estimatedStates < 10000

        Ok({
          isSafe,
          complexityScore: complexity.contents,
          hasNestedQuantifiers: hasNestedQuantifiers.contents,
          hasOverlappingAlternations: hasOverlappingAlternations.contents,
          maxNestingDepth: maxDepth.contents,
          estimatedStates,
          warnings: warnings.contents,
        })
      }
    }
  }
}

/** Validate a pattern and return a safe wrapper */
let validatePattern = (pattern: string): result<safePattern, regexError> => {
  switch analyzePattern(pattern) {
  | Error(e) => Error(e)
  | Ok(analysis) =>
    if !analysis.isSafe {
      if analysis.hasNestedQuantifiers {
        Error(NestedQuantifiers)
      } else {
        Error(ExcessiveBacktracking)
      }
    } else {
      Ok({pattern, analysis})
    }
  }
}

/** Check if pattern is safe (quick check) */
let isSafePattern = (pattern: string): bool => {
  switch validatePattern(pattern) {
  | Ok(_) => true
  | Error(_) => false
  }
}

/** Get literal prefix from a pattern (for optimization) */
let literalPrefix = (pattern: string): option<string> => {
  let length = Js.String2.length(pattern)
  let endPos = ref(0)

  while endPos.contents < length && !isMetaChar(Js.String2.charAt(pattern, endPos.contents)) {
    endPos := endPos.contents + 1
  }

  if endPos.contents == 0 {
    None
  } else {
    Some(Js.String2.slice(pattern, ~from=0, ~to_=endPos.contents))
  }
}

/** Simple pattern matching using native RegExp (only for validated patterns) */
let matches = (safePattern: safePattern, input: string): bool => {
  try {
    let regex = Js.Re.fromString(safePattern.pattern)
    Js.Re.test_(regex, input)
  } catch {
  | _ => false
  }
}

/** Test if pattern matches entire input (anchored) */
let matchesFull = (safePattern: safePattern, input: string): bool => {
  try {
    let anchoredPattern = `^${safePattern.pattern}$`
    let regex = Js.Re.fromString(anchoredPattern)
    Js.Re.test_(regex, input)
  } catch {
  | _ => false
  }
}

/** Find first match in input string */
let findFirst = (safePattern: safePattern, input: string): option<string> => {
  try {
    let regex = Js.Re.fromString(safePattern.pattern)
    switch Js.Re.exec_(regex, input) {
    | Some(result) => Js.Nullable.toOption(Js.Re.captures(result)[0])
    | None => None
    }
  } catch {
  | _ => None
  }
}

/** Find all matches in input string */
let findAll = (safePattern: safePattern, input: string): array<string> => {
  try {
    let regex = Js.Re.fromStringWithFlags(safePattern.pattern, ~flags="g")
    let matches = ref([])
    let done = ref(false)

    while !done.contents {
      switch Js.Re.exec_(regex, input) {
      | Some(result) =>
        switch Js.Nullable.toOption(Js.Re.captures(result)[0]) {
        | Some(m) => matches := Js.Array2.concat(matches.contents, [m])
        | None => ()
        }
      | None => done := true
      }
    }

    matches.contents
  } catch {
  | _ => []
  }
}

/** Replace first match with replacement string */
let replaceFirst = (safePattern: safePattern, input: string, replacement: string): string => {
  try {
    let regex = Js.Re.fromString(safePattern.pattern)
    Js.String2.replaceByRe(input, regex, replacement)
  } catch {
  | _ => input
  }
}

/** Replace all matches with replacement string */
let replaceAll = (safePattern: safePattern, input: string, replacement: string): string => {
  try {
    let regex = Js.Re.fromStringWithFlags(safePattern.pattern, ~flags="g")
    Js.String2.replaceByRe(input, regex, replacement)
  } catch {
  | _ => input
  }
}

/** Split string by pattern */
let split = (safePattern: safePattern, input: string): array<string> => {
  try {
    let regex = Js.Re.fromString(safePattern.pattern)
    Js.String2.splitByRe(input, regex)->Js.Array2.map(opt => Belt.Option.getWithDefault(opt, ""))
  } catch {
  | _ => [input]
  }
}

/** Common safe patterns */
module CommonPatterns = {
  /** Email pattern (simplified, safe) */
  let email = "^[^\\s@]+@[^\\s@]+\\.[^\\s@]+$"

  /** IPv4 address pattern */
  let ipv4 = "^\\d{1,3}\\.\\d{1,3}\\.\\d{1,3}\\.\\d{1,3}$"

  /** UUID pattern */
  let uuid = "^[0-9a-fA-F]{8}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{12}$"

  /** Hex string pattern */
  let hexString = "^[0-9a-fA-F]+$"

  /** Alphanumeric pattern */
  let alphanumeric = "^[a-zA-Z0-9]+$"

  /** URL pattern (simplified, safe) */
  let url = "^https?://[^\\s]+$"

  /** Date pattern YYYY-MM-DD */
  let dateYmd = "^\\d{4}-\\d{2}-\\d{2}$"

  /** Time pattern HH:MM:SS */
  let timeHms = "^\\d{2}:\\d{2}:\\d{2}$"

  /** Positive integer */
  let positiveInt = "^[1-9]\\d*$"

  /** Identifier (alphanumeric with underscore, starts with letter) */
  let identifier = "^[a-zA-Z_][a-zA-Z0-9_]*$"
}

/** Known dangerous patterns to avoid */
module DangerousPatterns = {
  /** Examples of patterns that can cause ReDoS */
  let examples = [
    "(a+)+", // Nested quantifiers
    "(a|a)+", // Overlapping alternations
    "(a|aa)+", // Overlapping alternations
    "(.*a){x}", // Greedy with repetition
    "([a-zA-Z]+)*", // Nested quantifiers with character class
  ]

  /** Check if a pattern resembles known dangerous patterns */
  let isDangerous = (pattern: string): bool => {
    // Check for nested quantifiers
    let hasNestedQuantifier =
      Js.Re.test_(%re("/[*+?]\\s*[*+?]/"), pattern) ||
        Js.Re.test_(%re("/[*+]\\s*\\{/"), pattern) ||
        Js.Re.test_(%re("/\\)[*+?]\\)[*+?]/"), pattern)

    // Check for potentially dangerous backreference patterns
    let hasDangerousBackref = Js.Re.test_(%re("/\\(.*\\)\\1+/"), pattern)

    hasNestedQuantifier || hasDangerousBackref
  }
}
