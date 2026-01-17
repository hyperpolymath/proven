// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

/**
 * SafeMarkdown - Markdown escaping and validation operations that cannot crash.
 *
 * Provides utilities for escaping special Markdown characters,
 * sanitizing user input for safe Markdown rendering, and validating
 * Markdown structure. All operations are memory-safe and handle edge cases.
 */

/** Error types for Markdown operations */
type markdownError =
  | InvalidUtf8
  | NestingTooDeep
  | UnclosedElement
  | InvalidLinkUrl
  | InvalidImageUrl

/** Block types for Markdown content */
type blockType =
  | Paragraph
  | Heading
  | CodeBlock
  | Blockquote
  | UnorderedList
  | OrderedList
  | HorizontalRule
  | Table

/** Maximum nesting depth for elements like lists and blockquotes */
let maxNestingDepth = 16

/** Characters that need escaping in inline text */
let inlineEscapeChars = ['\\', '`', '*', '_', '[', ']', '(', ')', '<', '>', '|', '~']

/** Characters that need escaping at the start of a line */
let lineStartEscapeChars = ['#', '+', '-', '.', '!']

// =============================================================================
// Escaping Functions
// =============================================================================

/** Escape special characters in inline Markdown text.
 *
 * This prevents user input from being interpreted as Markdown formatting.
 */
let escapeInline = (input: string): string => {
  let result = ref("")
  let inputLength = Js.String2.length(input)

  for i in 0 to inputLength - 1 {
    let char = Js.String2.charAt(input, i)
    let shouldEscape = Belt.Array.some(inlineEscapeChars, escapeChar => {
      char == Js.String2.make(escapeChar)
    })

    if shouldEscape {
      result := result.contents ++ "\\" ++ char
    } else {
      result := result.contents ++ char
    }
  }

  result.contents
}

/** Escape special characters at the start of lines.
 *
 * This prevents user input from creating headers, lists, etc.
 */
let escapeLineStart = (input: string): string => {
  let result = ref("")
  let atLineStart = ref(true)
  let inputLength = Js.String2.length(input)

  for i in 0 to inputLength - 1 {
    let char = Js.String2.charAt(input, i)

    if atLineStart.contents {
      // Check if this character needs escaping at line start
      let shouldEscape = Belt.Array.some(lineStartEscapeChars, escapeChar => {
        char == Js.String2.make(escapeChar)
      })

      if shouldEscape {
        result := result.contents ++ "\\" ++ char
      } else {
        result := result.contents ++ char
      }
    } else {
      result := result.contents ++ char
    }

    atLineStart := char == "\n"
  }

  result.contents
}

/** Fully escape text for safe inclusion in Markdown.
 *
 * Combines inline and line-start escaping.
 */
let escapeText = (input: string): string => {
  input->escapeInline->escapeLineStart
}

/** Escape text for use in a Markdown link title */
let escapeLinkTitle = (title: string): string => {
  title
  ->Js.String2.replaceByRe(%re("/\\\\/g"), "\\\\")
  ->Js.String2.replaceByRe(%re("/\"/g"), "\\\"")
  ->Js.String2.replaceByRe(%re("/\\n/g"), " ")
}

/** Escape text for use in a code span (backticks) */
let escapeCodeSpan = (code: string): string => {
  // Count maximum consecutive backticks in the input
  let maxBackticks = ref(0)
  let currentBackticks = ref(0)
  let codeLength = Js.String2.length(code)

  for i in 0 to codeLength - 1 {
    let char = Js.String2.charAt(code, i)
    if char == "`" {
      currentBackticks := currentBackticks.contents + 1
      if currentBackticks.contents > maxBackticks.contents {
        maxBackticks := currentBackticks.contents
      }
    } else {
      currentBackticks := 0
    }
  }

  // Use one more backtick than the maximum found
  let fenceCount = maxBackticks.contents + 1
  let fence = Js.String2.repeat("`", fenceCount)

  // Add spaces if code starts/ends with backtick
  let startsWithBacktick = codeLength > 0 && Js.String2.charAt(code, 0) == "`"
  let endsWithBacktick = codeLength > 0 && Js.String2.charAt(code, codeLength - 1) == "`"

  let prefix = startsWithBacktick ? " " : ""
  let suffix = endsWithBacktick ? " " : ""

  fence ++ prefix ++ code ++ suffix ++ fence
}

// =============================================================================
// URL Validation Functions
// =============================================================================

/** Dangerous URL protocols that can execute code */
let dangerousProtocols = ["javascript:", "vbscript:", "data:", "file:"]

/** Check if a URL is safe for use in Markdown links */
let isValidLinkUrl = (url: string): bool => {
  if Js.String2.length(url) == 0 {
    false
  } else {
    let lowerUrl = Js.String2.toLowerCase(url)

    // Check for dangerous protocols
    let hasDangerousProtocol = Belt.Array.some(dangerousProtocols, protocol => {
      Js.String2.startsWith(lowerUrl, protocol)
    })

    if hasDangerousProtocol {
      false
    } else {
      // Check for control characters (ASCII < 32 or 127)
      let hasControlChars = ref(false)
      let urlLength = Js.String2.length(url)

      for i in 0 to urlLength - 1 {
        let charCode = Js.String2.charCodeAt(url, i)->Belt.Float.toInt
        if charCode < 32 || charCode == 127 {
          hasControlChars := true
        }
      }

      !hasControlChars.contents
    }
  }
}

/** Check if a URL is safe for use in Markdown images */
let isValidImageUrl = (url: string): bool => {
  // Images have the same restrictions as links
  isValidLinkUrl(url)
}

/** Sanitize a URL for safe use in Markdown */
let sanitizeUrl = (url: string): option<string> => {
  if !isValidLinkUrl(url) {
    None
  } else {
    // Escape parentheses and spaces
    let sanitized =
      url
      ->Js.String2.replaceByRe(%re("/\\(/g"), "%28")
      ->Js.String2.replaceByRe(%re("/\\)/g"), "%29")
      ->Js.String2.replaceByRe(%re("/ /g"), "%20")

    Some(sanitized)
  }
}

// =============================================================================
// Link and Image Creation
// =============================================================================

/** Create a safe Markdown link */
let createLink = (
  text: string,
  url: string,
  ~title: option<string>=?,
): result<string, markdownError> => {
  if !isValidLinkUrl(url) {
    Error(InvalidLinkUrl)
  } else {
    let escapedText = escapeInline(text)

    switch sanitizeUrl(url) {
    | None => Error(InvalidLinkUrl)
    | Some(sanitizedUrl) =>
      let titlePart = switch title {
      | Some(t) => " \"" ++ escapeLinkTitle(t) ++ "\""
      | None => ""
      }

      Ok("[" ++ escapedText ++ "](" ++ sanitizedUrl ++ titlePart ++ ")")
    }
  }
}

/** Create a safe Markdown image */
let createImage = (
  altText: string,
  url: string,
  ~title: option<string>=?,
): result<string, markdownError> => {
  if !isValidImageUrl(url) {
    Error(InvalidImageUrl)
  } else {
    let escapedAlt = escapeInline(altText)

    switch sanitizeUrl(url) {
    | None => Error(InvalidImageUrl)
    | Some(sanitizedUrl) =>
      let titlePart = switch title {
      | Some(t) => " \"" ++ escapeLinkTitle(t) ++ "\""
      | None => ""
      }

      Ok("![" ++ escapedAlt ++ "](" ++ sanitizedUrl ++ titlePart ++ ")")
    }
  }
}

// =============================================================================
// Formatting Detection and Stripping
// =============================================================================

/** Strip all Markdown formatting from text */
let stripFormatting = (input: string): string => {
  let result = ref("")
  let inputLength = Js.String2.length(input)
  let index = ref(0)

  while index.contents < inputLength {
    let char = Js.String2.charAt(input, index.contents)

    // Handle escape sequences
    if char == "\\" && index.contents + 1 < inputLength {
      result := result.contents ++ Js.String2.charAt(input, index.contents + 1)
      index := index.contents + 2
    } else {
      // Skip formatting characters
      let skip = switch char {
      | "*" | "_" | "~" | "`" => true
      | _ => false
      }

      if !skip {
        result := result.contents ++ char
      }

      index := index.contents + 1
    }
  }

  result.contents
}

/** Check if text contains any Markdown formatting */
let hasFormatting = (input: string): bool => {
  let inputLength = Js.String2.length(input)
  let index = ref(0)
  let foundFormatting = ref(false)

  while index.contents < inputLength && !foundFormatting.contents {
    let char = Js.String2.charAt(input, index.contents)

    // Skip escape sequences
    if char == "\\" && index.contents + 1 < inputLength {
      index := index.contents + 2
    } else {
      // Check for formatting characters
      switch char {
      | "*" | "_" | "~" | "`" | "#" | "[" | "]" | "!" | "|" => foundFormatting := true
      | _ => ()
      }

      index := index.contents + 1
    }
  }

  foundFormatting.contents
}

// =============================================================================
// UTF-8 Validation
// =============================================================================

/** Validate UTF-8 encoding of Markdown text */
let isValidUtf8 = (input: string): bool => {
  // In JavaScript/ReScript, strings are already UTF-16, so we check for replacement chars
  !Js.Re.test_(%re("/\\uFFFD/"), input)
}

// =============================================================================
// Block Type Detection
// =============================================================================

/** Detect the type of a Markdown block from its first line */
let detectBlockType = (line: string): blockType => {
  let trimmed = Js.String2.trim(line)

  if Js.String2.length(trimmed) == 0 {
    Paragraph
  } else {
    let firstChar = Js.String2.charAt(trimmed, 0)

    // Heading
    if firstChar == "#" {
      Heading
    // Code block (fenced)
    } else if Js.String2.startsWith(trimmed, "```") || Js.String2.startsWith(trimmed, "~~~") {
      CodeBlock
    // Blockquote
    } else if firstChar == ">" {
      Blockquote
    // Unordered list
    } else if Js.String2.length(trimmed) >= 2 {
      let secondChar = Js.String2.charAt(trimmed, 1)
      if (firstChar == "-" || firstChar == "*" || firstChar == "+") && secondChar == " " {
        UnorderedList
      // Ordered list (digit followed by dot and space)
      } else if firstChar >= "0" && firstChar <= "9" {
        // Look for pattern like "1. "
        if Js.Re.test_(%re("/^\\d+\\. /"), trimmed) {
          OrderedList
        } else {
          Paragraph
        }
      // Horizontal rule (---, ***, ___)
      } else if Js.Re.test_(%re("/^[-*_]{3,}$/"), Js.String2.replaceByRe(trimmed, %re("/ /g"), "")) {
        HorizontalRule
      // Table (starts with |)
      } else if firstChar == "|" {
        Table
      } else {
        Paragraph
      }
    } else {
      Paragraph
    }
  }
}

// =============================================================================
// Error Handling
// =============================================================================

/** Convert error to string */
let errorToString = (err: markdownError): string => {
  switch err {
  | InvalidUtf8 => "Invalid UTF-8 sequence"
  | NestingTooDeep => "Nesting depth exceeds maximum"
  | UnclosedElement => "Unclosed Markdown element"
  | InvalidLinkUrl => "Invalid or dangerous URL for link"
  | InvalidImageUrl => "Invalid or dangerous URL for image"
  }
}
