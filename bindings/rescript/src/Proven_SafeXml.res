// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

/**
 * SafeXml - Safe XML escaping and validation operations that cannot crash.
 *
 * Provides secure XML handling including entity escaping, attribute value
 * encoding, and validation of XML names and content. All operations are
 * designed to prevent XML injection attacks and produce well-formed output.
 */

/** External parseInt for radix-based parsing */
@val external parseInt: (string, int) => float = "parseInt"

/** XML escape mode type */
type escapeMode =
  | Text
  | Attribute
  | Cdata

/** XML error types */
type xmlError =
  | InvalidCharacter
  | InvalidName
  | MalformedXml
  | MaxDepthExceeded
  | InvalidEntity
  | UnclosedElement

/** Escape a string for safe inclusion in XML text content.
 * Escapes: < > & ' "
 */
let escapeText = (input: string): string => {
  input
  ->Js.String2.replaceByRe(%re("/&/g"), "&amp;")
  ->Js.String2.replaceByRe(%re("/</g"), "&lt;")
  ->Js.String2.replaceByRe(%re("/>/g"), "&gt;")
  ->Js.String2.replaceByRe(%re("/'/g"), "&apos;")
  ->Js.String2.replaceByRe(%re("/\"/g"), "&quot;")
}

/** Escape a string for safe inclusion in XML attribute values.
 * Also escapes control characters.
 */
let escapeAttribute = (input: string): string => {
  let escaped = escapeText(input)
  // Also escape control characters for attributes
  let result = ref(escaped)
  for characterCode in 0 to 31 {
    if characterCode != 9 && characterCode != 10 && characterCode != 13 {
      let charPattern = Js.String2.fromCharCode(characterCode)
      let replacement = `&#x${Js.Int.toStringWithRadix(characterCode, ~radix=16)->Js.String2.toUpperCase};`
      result := Js.String2.replaceByRe(result.contents, Js.Re.fromString(charPattern), replacement)
    }
  }
  result.contents
}

/** Escape with specified mode */
let escapeWithMode = (input: string, mode: escapeMode): string => {
  switch mode {
  | Text => escapeText(input)
  | Attribute => escapeAttribute(input)
  | Cdata => input // CDATA sections don't need escaping (except for ]]>)
  }
}

/** Unescape XML entities in a string */
let unescape = (input: string): string => {
  input
  ->Js.String2.replaceByRe(%re("/&lt;/g"), "<")
  ->Js.String2.replaceByRe(%re("/&gt;/g"), ">")
  ->Js.String2.replaceByRe(%re("/&apos;/g"), "'")
  ->Js.String2.replaceByRe(%re("/&quot;/g"), "\"")
  ->Js.String2.replaceByRe(%re("/&amp;/g"), "&")
}

/** Check if a character code point is valid in XML content (XML 1.0) */
let isValidXmlCharCode = (codePoint: int): bool => {
  codePoint == 0x9 ||
  codePoint == 0xA ||
  codePoint == 0xD ||
  (codePoint >= 0x20 && codePoint <= 0xD7FF) ||
  (codePoint >= 0xE000 && codePoint <= 0xFFFD) ||
  (codePoint >= 0x10000 && codePoint <= 0x10FFFF)
}

/** Check if a character is valid as the start of an XML name */
let isValidNameStartChar = (codePoint: int): bool => {
  codePoint == 0x3A || // :
  (codePoint >= 0x41 && codePoint <= 0x5A) || // A-Z
  codePoint == 0x5F || // _
  (codePoint >= 0x61 && codePoint <= 0x7A) || // a-z
  (codePoint >= 0xC0 && codePoint <= 0xD6) ||
  (codePoint >= 0xD8 && codePoint <= 0xF6) ||
  (codePoint >= 0xF8 && codePoint <= 0x2FF) ||
  (codePoint >= 0x370 && codePoint <= 0x37D) ||
  (codePoint >= 0x37F && codePoint <= 0x1FFF) ||
  (codePoint >= 0x200C && codePoint <= 0x200D) ||
  (codePoint >= 0x2070 && codePoint <= 0x218F) ||
  (codePoint >= 0x2C00 && codePoint <= 0x2FEF) ||
  (codePoint >= 0x3001 && codePoint <= 0xD7FF) ||
  (codePoint >= 0xF900 && codePoint <= 0xFDCF) ||
  (codePoint >= 0xFDF0 && codePoint <= 0xFFFD) ||
  (codePoint >= 0x10000 && codePoint <= 0xEFFFF)
}

/** Check if a character is valid within an XML name (after first character) */
let isValidNameChar = (codePoint: int): bool => {
  isValidNameStartChar(codePoint) ||
  codePoint == 0x2D || // -
  codePoint == 0x2E || // .
  (codePoint >= 0x30 && codePoint <= 0x39) || // 0-9
  codePoint == 0xB7 ||
  (codePoint >= 0x0300 && codePoint <= 0x036F) ||
  (codePoint >= 0x203F && codePoint <= 0x2040)
}

/** Validate an XML name (element name or attribute name) */
let isValidName = (name: string): bool => {
  if Js.String2.length(name) == 0 {
    false
  } else {
    let firstCode = Js.String2.charCodeAt(name, 0)->Belt.Float.toInt
    if !isValidNameStartChar(firstCode) {
      false
    } else {
      let valid = ref(true)
      let length = Js.String2.length(name)
      for index in 1 to length - 1 {
        if valid.contents {
          let charCode = Js.String2.charCodeAt(name, index)->Belt.Float.toInt
          if !isValidNameChar(charCode) {
            valid := false
          }
        }
      }
      valid.contents
    }
  }
}

/** Validate that a string contains only valid XML characters */
let isValidContent = (content: string): bool => {
  let valid = ref(true)
  let length = Js.String2.length(content)
  for index in 0 to length - 1 {
    if valid.contents {
      let charCode = Js.String2.charCodeAt(content, index)->Belt.Float.toInt
      if !isValidXmlCharCode(charCode) {
        valid := false
      }
    }
  }
  valid.contents
}

/** Remove invalid XML characters from a string */
let sanitizeContent = (input: string): string => {
  let result = ref("")
  let length = Js.String2.length(input)
  for index in 0 to length - 1 {
    let charCode = Js.String2.charCodeAt(input, index)->Belt.Float.toInt
    if isValidXmlCharCode(charCode) {
      result := result.contents ++ Js.String2.charAt(input, index)
    }
  }
  result.contents
}

/** XML element attribute type */
type xmlAttribute = {
  name: string,
  value: string,
}

/** Create a safe XML element string */
let createElement = (
  ~tagName: string,
  ~content: option<string>=None,
  ~attributes: array<xmlAttribute>=[],
  (),
): result<string, xmlError> => {
  if !isValidName(tagName) {
    Error(InvalidName)
  } else {
    // Validate all attribute names
    let attributesValid = Belt.Array.every(attributes, attr => isValidName(attr.name))
    if !attributesValid {
      Error(InvalidName)
    } else {
      let result = ref("<" ++ tagName)

      // Add attributes
      Belt.Array.forEach(attributes, attr => {
        result := result.contents ++ " " ++ attr.name ++ "=\"" ++ escapeAttribute(attr.value) ++ "\""
      })

      // Content or self-closing
      switch content {
      | Some(innerContent) =>
        result := result.contents ++ ">" ++ escapeText(innerContent) ++ "</" ++ tagName ++ ">"
      | None => result := result.contents ++ "/>"
      }

      Ok(result.contents)
    }
  }
}

/** Create a CDATA section */
let createCDATA = (content: string): result<string, xmlError> => {
  if Js.String2.includes(content, "]]>") {
    Error(InvalidCharacter)
  } else {
    Ok("<![CDATA[" ++ content ++ "]]>")
  }
}

/** Create an XML comment */
let createComment = (content: string): result<string, xmlError> => {
  if Js.String2.includes(content, "--") {
    Error(InvalidCharacter)
  } else if Js.String2.length(content) > 0 && Js.String2.endsWith(content, "-") {
    Error(InvalidCharacter)
  } else {
    Ok("<!-- " ++ content ++ " -->")
  }
}

/** Check if a string looks like well-formed XML (basic validation) */
let isWellFormed = (input: string): bool => {
  let trimmed = Js.String2.trim(input)
  if Js.String2.length(trimmed) == 0 {
    false
  } else if !Js.String2.startsWith(trimmed, "<") {
    false
  } else {
    // Basic tag matching using depth tracking
    let depth = ref(0)
    let valid = ref(true)
    let length = Js.String2.length(trimmed)
    let index = ref(0)

    while index.contents < length && valid.contents {
      let currentChar = Js.String2.charAt(trimmed, index.contents)
      if currentChar == "<" {
        if index.contents + 1 >= length {
          valid := false
        } else {
          let nextChar = Js.String2.charAt(trimmed, index.contents + 1)
          if nextChar == "/" {
            // Closing tag
            if depth.contents == 0 {
              valid := false
            } else {
              depth := depth.contents - 1
            }
          } else if nextChar == "?" || nextChar == "!" {
            // Declaration, comment, or CDATA - skip
            ()
          } else {
            // Opening tag - check for self-closing
            let tagEnd = Js.String2.indexOfFrom(trimmed, ">", index.contents)
            if tagEnd == -1 {
              valid := false
            } else {
              let prevChar = Js.String2.charAt(trimmed, tagEnd - 1)
              if prevChar != "/" {
                depth := depth.contents + 1
              }
            }
          }
        }
      }
      index := index.contents + 1
    }

    valid.contents && depth.contents == 0
  }
}

/** Create an XML declaration */
let createDeclaration = (
  ~version: string="1.0",
  ~encoding: option<string>=Some("UTF-8"),
  ~standalone: option<bool>=None,
  (),
): string => {
  let result = ref(`<?xml version="${version}"`)
  switch encoding {
  | Some(enc) => result := result.contents ++ ` encoding="${enc}"`
  | None => ()
  }
  switch standalone {
  | Some(true) => result := result.contents ++ ` standalone="yes"`
  | Some(false) => result := result.contents ++ ` standalone="no"`
  | None => ()
  }
  result.contents ++ "?>"
}

/** Parse numeric XML entity (&#123; or &#xAB;) */
let parseNumericEntity = (entity: string): option<string> => {
  if Js.String2.length(entity) < 3 {
    None
  } else if Js.String2.charAt(entity, 0) != "#" {
    None
  } else {
    let isHex = Js.String2.charAt(entity, 1) == "x" || Js.String2.charAt(entity, 1) == "X"
    let numericPart = if isHex {
      Js.String2.sliceToEnd(entity, ~from=2)
    } else {
      Js.String2.sliceToEnd(entity, ~from=1)
    }
    let radix = if isHex { 16 } else { 10 }
    let value = parseInt(numericPart, radix)
    if !Js.Float.isNaN(value) {
      let codePoint = Belt.Float.toInt(value)
      if isValidXmlCharCode(codePoint) {
        Some(Js.String2.fromCodePoint(codePoint))
      } else {
        None
      }
    } else {
      None
    }
  }
}
