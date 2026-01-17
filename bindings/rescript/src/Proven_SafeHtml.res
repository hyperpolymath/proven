// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

/**
 * SafeHtml - HTML escaping and sanitization operations that cannot crash.
 *
 * Provides safe functions for escaping user input before insertion into HTML contexts,
 * checking for dangerous URLs, and sanitizing HTML content. All operations handle
 * edge cases without throwing exceptions.
 */

/** Error types for HTML operations */
type htmlError =
  | BufferTooSmall
  | InvalidUtf8
  | MalformedHtml

/** HTML escaping context determines which characters to escape */
type escapeContext =
  | TextContent // Content between tags (escapes < > & " ')
  | SingleQuotedAttr // Inside a single-quoted attribute (escapes ' & < >)
  | DoubleQuotedAttr // Inside a double-quoted attribute (escapes " & < >)
  | UnquotedAttr // Inside unquoted attribute (escapes many characters)

// =============================================================================
// Basic Escaping Functions
// =============================================================================

/** Escape a string for safe insertion as HTML text content.
 *
 * Escapes: & < > " '
 */
let escapeText = (input: string): string => {
  input
  ->Js.String2.replaceByRe(%re("/&/g"), "&amp;")
  ->Js.String2.replaceByRe(%re("/</g"), "&lt;")
  ->Js.String2.replaceByRe(%re("/>/g"), "&gt;")
  ->Js.String2.replaceByRe(%re("/\"/g"), "&quot;")
  ->Js.String2.replaceByRe(%re("/'/g"), "&#x27;")
}

/** Escape a string for safe insertion into an HTML attribute (double-quoted).
 *
 * Escapes: & < > "
 */
let escapeAttribute = (input: string): string => {
  input
  ->Js.String2.replaceByRe(%re("/&/g"), "&amp;")
  ->Js.String2.replaceByRe(%re("/</g"), "&lt;")
  ->Js.String2.replaceByRe(%re("/>/g"), "&gt;")
  ->Js.String2.replaceByRe(%re("/\"/g"), "&quot;")
}

/** Escape a string for safe insertion into a single-quoted HTML attribute.
 *
 * Escapes: & < > '
 */
let escapeSingleQuotedAttr = (input: string): string => {
  input
  ->Js.String2.replaceByRe(%re("/&/g"), "&amp;")
  ->Js.String2.replaceByRe(%re("/</g"), "&lt;")
  ->Js.String2.replaceByRe(%re("/>/g"), "&gt;")
  ->Js.String2.replaceByRe(%re("/'/g"), "&#x27;")
}

/** Escape a string for safe insertion into an unquoted HTML attribute.
 *
 * Escapes many characters including whitespace.
 */
let escapeUnquotedAttr = (input: string): string => {
  input
  ->Js.String2.replaceByRe(%re("/&/g"), "&amp;")
  ->Js.String2.replaceByRe(%re("/</g"), "&lt;")
  ->Js.String2.replaceByRe(%re("/>/g"), "&gt;")
  ->Js.String2.replaceByRe(%re("/\"/g"), "&quot;")
  ->Js.String2.replaceByRe(%re("/'/g"), "&#x27;")
  ->Js.String2.replaceByRe(%re("/`/g"), "&#x60;")
  ->Js.String2.replaceByRe(%re("/=/g"), "&#x3D;")
  ->Js.String2.replaceByRe(%re("/\\s/g"), "&#x20;")
}

/** Escape a string for a specific HTML context */
let escapeForContext = (input: string, context: escapeContext): string => {
  switch context {
  | TextContent => escapeText(input)
  | SingleQuotedAttr => escapeSingleQuotedAttr(input)
  | DoubleQuotedAttr => escapeAttribute(input)
  | UnquotedAttr => escapeUnquotedAttr(input)
  }
}

// =============================================================================
// URL Safety Functions
// =============================================================================

/** Dangerous URL schemes that can execute code */
let dangerousSchemes = ["javascript:", "vbscript:", "data:", "blob:"]

/** Check if a URL scheme is considered safe (no javascript:, data:, vbscript:, etc.) */
let isSafeUrlScheme = (url: string): bool => {
  let trimmed = Js.String2.trim(url)->Js.String2.toLowerCase

  let isDangerous = ref(false)
  Belt.Array.forEach(dangerousSchemes, scheme => {
    if !isDangerous.contents {
      let schemeLen = Js.String2.length(scheme)
      if Js.String2.length(trimmed) >= schemeLen {
        let prefix = Js.String2.slice(trimmed, ~from=0, ~to_=schemeLen)
        if prefix == scheme {
          isDangerous := true
        }
      }
    }
  })

  !isDangerous.contents
}

/** Escape special characters in a URL for use in href/src attributes.
 *
 * This escapes HTML entities but preserves URL structure.
 */
let escapeUrl = (input: string): string => {
  input
  ->Js.String2.replaceByRe(%re("/&/g"), "&amp;")
  ->Js.String2.replaceByRe(%re("/\"/g"), "&quot;")
  ->Js.String2.replaceByRe(%re("/'/g"), "&#x27;")
  ->Js.String2.replaceByRe(%re("/</g"), "&lt;")
  ->Js.String2.replaceByRe(%re("/>/g"), "&gt;")
}

/** Sanitize a URL for safe use in href/src attributes.
 *
 * Returns None if the URL uses a dangerous scheme.
 */
let sanitizeUrl = (input: string): option<string> => {
  if !isSafeUrlScheme(input) {
    None
  } else {
    Some(escapeUrl(input))
  }
}

// =============================================================================
// Tag and Attribute Safety
// =============================================================================

/** Dangerous HTML tag names */
let dangerousTags = [
  "script",
  "style",
  "iframe",
  "frame",
  "frameset",
  "object",
  "embed",
  "applet",
  "form",
  "input",
  "button",
  "select",
  "textarea",
  "link",
  "meta",
  "base",
  "svg",
  "math",
]

/** Check if an HTML tag name is considered dangerous */
let isDangerousTag = (tagName: string): bool => {
  let lowerTag = Js.String2.toLowerCase(tagName)
  Belt.Array.some(dangerousTags, t => t == lowerTag)
}

/** Dangerous HTML attribute names */
let dangerousAttributes = [
  "onclick",
  "ondblclick",
  "onmousedown",
  "onmouseup",
  "onmouseover",
  "onmousemove",
  "onmouseout",
  "onmouseenter",
  "onmouseleave",
  "onkeydown",
  "onkeyup",
  "onkeypress",
  "onload",
  "onerror",
  "onabort",
  "onsubmit",
  "onreset",
  "onselect",
  "onblur",
  "onfocus",
  "onchange",
  "oninput",
  "onscroll",
  "onwheel",
  "oncopy",
  "oncut",
  "onpaste",
  "ondrag",
  "ondragstart",
  "ondragend",
  "ondragover",
  "ondragenter",
  "ondragleave",
  "ondrop",
  "onanimationstart",
  "onanimationend",
  "onanimationiteration",
  "ontransitionend",
  "formaction",
  "xlink:href",
]

/** Check if an HTML attribute name is considered dangerous */
let isDangerousAttribute = (attrName: string): bool => {
  let lowerAttr = Js.String2.toLowerCase(attrName)

  // Check explicit list
  if Belt.Array.some(dangerousAttributes, a => a == lowerAttr) {
    true
  } else {
    // Any attribute starting with "on" is potentially an event handler
    Js.String2.length(lowerAttr) >= 2 && Js.String2.slice(lowerAttr, ~from=0, ~to_=2) == "on"
  }
}

// =============================================================================
// HTML Stripping and Sanitization
// =============================================================================

/** Strip all HTML tags from input, leaving only text content */
let stripTags = (input: string): string => {
  Js.String2.replaceByRe(input, %re("/<[^>]*>/g"), "")
}

/** Strip specific HTML tags (by name) */
let stripTagsByName = (input: string, tagNames: array<string>): string => {
  let result = ref(input)

  Belt.Array.forEach(tagNames, tagName => {
    let pattern = Js.Re.fromStringWithFlags(
      "<" ++ tagName ++ "[^>]*>|</" ++ tagName ++ ">",
      ~flags="gi",
    )
    result := Js.String2.replaceByRe(result.contents, pattern, "")
  })

  result.contents
}

/** Strip dangerous tags while preserving safe content */
let stripDangerousTags = (input: string): string => {
  stripTagsByName(input, dangerousTags)
}

// =============================================================================
// JavaScript/CSS Context Escaping
// =============================================================================

/** Encode a string for safe insertion into a JavaScript string literal within HTML.
 *
 * Escapes both HTML entities and JavaScript special characters.
 */
let escapeJsInHtml = (input: string): string => {
  input
  ->Js.String2.replaceByRe(%re("/\\\\/g"), "\\\\")
  ->Js.String2.replaceByRe(%re("/\"/g"), "\\\"")
  ->Js.String2.replaceByRe(%re("/'/g"), "\\'")
  ->Js.String2.replaceByRe(%re("/\n/g"), "\\n")
  ->Js.String2.replaceByRe(%re("/\r/g"), "\\r")
  ->Js.String2.replaceByRe(%re("/\t/g"), "\\t")
  ->Js.String2.replaceByRe(%re("/</g"), "\\u003C")
  ->Js.String2.replaceByRe(%re("/>/g"), "\\u003E")
  ->Js.String2.replaceByRe(%re("/&/g"), "\\u0026")
  ->Js.String2.replaceByRe(%re("/\\//g"), "\\/") // Prevent </script> injection
}

/** Encode data for safe insertion into a CSS context within HTML */
let escapeCssInHtml = (input: string): string => {
  input
  ->Js.String2.replaceByRe(%re("/\\\\/g"), "\\\\")
  ->Js.String2.replaceByRe(%re("/\"/g"), "\\\"")
  ->Js.String2.replaceByRe(%re("/'/g"), "\\'")
  ->Js.String2.replaceByRe(%re("/</g"), "\\3C ")
  ->Js.String2.replaceByRe(%re("/>/g"), "\\3E ")
  ->Js.String2.replaceByRe(%re("/&/g"), "\\26 ")
  ->Js.String2.replaceByRe(%re("/\\//g"), "\\2F ")
  ->Js.String2.replaceByRe(%re("/\n/g"), "\\A ")
  ->Js.String2.replaceByRe(%re("/\r/g"), "\\D ")
}

// =============================================================================
// Entity Handling
// =============================================================================

/** Common HTML entities and their decoded values */
let entityMap: array<(string, string)> = [
  ("&amp;", "&"),
  ("&lt;", "<"),
  ("&gt;", ">"),
  ("&quot;", "\""),
  ("&#x27;", "'"),
  ("&apos;", "'"),
  ("&#39;", "'"),
  ("&nbsp;", " "),
  ("&#160;", " "),
  ("&#xa0;", " "),
  ("&#x20;", " "),
  ("&#32;", " "),
]

/** Unescape common HTML entities to their character equivalents */
let unescapeEntities = (input: string): string => {
  let result = ref(input)

  Belt.Array.forEach(entityMap, ((entity, char)) => {
    result := Js.String2.replaceByRe(result.contents, Js.Re.fromStringWithFlags(entity, ~flags="gi"), char)
  })

  // Handle numeric entities &#NN; and &#xNN;
  // Decimal entities
  result :=
    result.contents->Js.String2.unsafeReplaceBy1(
      %re("/&#(\d+);/g"),
      (_, numStr, _, _) => {
        switch Belt.Int.fromString(numStr) {
        | Some(code) if code >= 0 && code <= 255 => Js.String2.fromCharCode(code)
        | _ => "&#" ++ numStr ++ ";"
        }
      },
    )

  // Hex entities
  result :=
    result.contents->Js.String2.unsafeReplaceBy1(
      %re("/&#x([0-9a-fA-F]+);/gi"),
      (_, hexStr, _, _) => {
        let code = Js.Float.fromString("0x" ++ hexStr)->Belt.Float.toInt
        if code >= 0 && code <= 255 {
          Js.String2.fromCharCode(code)
        } else {
          "&#x" ++ hexStr ++ ";"
        }
      },
    )

  result.contents
}

// =============================================================================
// Utility Functions
// =============================================================================

/** Calculate the maximum buffer size needed for escaping */
let maxEscapedLength = (inputLength: int): int => {
  inputLength * 6 // Worst case: all chars become &#xNN;
}

/** Check if a string contains any HTML tags */
let containsHtmlTags = (input: string): bool => {
  Js.Re.test_(%re("/<[^>]+>/"), input)
}

/** Check if a string contains potentially dangerous HTML */
let containsDangerousHtml = (input: string): bool => {
  // Check for script tags
  if Js.Re.test_(%re("/<script/i"), input) {
    true
  } else if Js.Re.test_(%re("/javascript:/i"), input) {
    // Check for javascript: URLs
    true
  } else if Js.Re.test_(%re("/on\\w+\\s*=/i"), input) {
    // Check for event handlers (onclick, onerror, etc.)
    true
  } else {
    // Check for data: URLs in attributes
    Js.Re.test_(%re("/data:[^\"'\\s]+/i"), input)
  }
}

/** Extract text content from HTML (strips tags and unescapes entities) */
let extractText = (input: string): string => {
  input->stripTags->unescapeEntities
}

/** Create a safe HTML snippet by escaping text and wrapping in a tag */
let safeElement = (tagName: string, content: string): result<string, htmlError> => {
  if isDangerousTag(tagName) {
    Error(MalformedHtml)
  } else {
    let escapedContent = escapeText(content)
    Ok("<" ++ tagName ++ ">" ++ escapedContent ++ "</" ++ tagName ++ ">")
  }
}

/** Create a safe HTML link element */
let safeLink = (href: string, text: string): option<string> => {
  switch sanitizeUrl(href) {
  | None => None
  | Some(safeHref) =>
    let escapedText = escapeText(text)
    Some("<a href=\"" ++ safeHref ++ "\">" ++ escapedText ++ "</a>")
  }
}

/** Create a safe HTML image element */
let safeImage = (src: string, alt: string): option<string> => {
  switch sanitizeUrl(src) {
  | None => None
  | Some(safeSrc) =>
    let escapedAlt = escapeAttribute(alt)
    Some("<img src=\"" ++ safeSrc ++ "\" alt=\"" ++ escapedAlt ++ "\" />")
  }
}

/** Convert error to string */
let errorToString = (err: htmlError): string => {
  switch err {
  | BufferTooSmall => "Buffer too small for operation"
  | InvalidUtf8 => "Invalid UTF-8 sequence"
  | MalformedHtml => "Malformed HTML structure"
  }
}
