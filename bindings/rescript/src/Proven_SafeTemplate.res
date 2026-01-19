// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

open Proven_Bitwise

/**
 * SafeTemplate - Template string interpolation with automatic escaping that cannot crash.
 *
 * Provides utilities for rendering templates with variables while automatically
 * escaping output based on context (HTML, URL, JavaScript, etc.). Designed to
 * prevent XSS and injection attacks.
 */

/** Error types for template operations */
type templateError =
  | InvalidTemplate
  | UnknownVariable
  | UnclosedDelimiter
  | NestedDelimiters
  | InvalidEscapeSequence
  | MaxDepthExceeded
  | CircularReference

/** Escape context for automatic output escaping */
type escapeContext =
  | EscapeNone
  | EscapeHtml
  | EscapeHtmlAttribute
  | EscapeUrl
  | EscapeJavaScript
  | EscapeCss
  | EscapeSql

/** Template variable value */
type rec value =
  | VString(string)
  | VInteger(int)
  | VFloat(float)
  | VBoolean(bool)
  | VNull
  | VArray(array<value>)

/** Template configuration options */
type config = {
  openDelim: string,
  closeDelim: string,
  escapeContext: escapeContext,
  maxDepth: int,
  stripWhitespace: bool,
}

/** Default template configuration */
let defaultConfig = {
  openDelim: "{{",
  closeDelim: "}}",
  escapeContext: EscapeHtml,
  maxDepth: 10,
  stripWhitespace: false,
}

/** Convert value to string representation */
let valueToString = (value: value): string => {
  switch value {
  | VString(s) => s
  | VInteger(i) => Belt.Int.toString(i)
  | VFloat(f) => Belt.Float.toString(f)
  | VBoolean(b) => b ? "true" : "false"
  | VNull => ""
  | VArray(_) => "[Array]"
  }
}

/** Check if a value is truthy */
let valueIsTruthy = (value: value): bool => {
  switch value {
  | VString(s) => Js.String2.length(s) > 0
  | VInteger(i) => i != 0
  | VFloat(f) => f != 0.0
  | VBoolean(b) => b
  | VNull => false
  | VArray(a) => Belt.Array.length(a) > 0
  }
}

/** Escape string for HTML context */
let escapeHtml = (value: string): string => {
  let length = Js.String2.length(value)
  let result = ref("")

  for i in 0 to length - 1 {
    let char = Js.String2.charAt(value, i)
    switch char {
    | "&" => result := result.contents ++ "&amp;"
    | "<" => result := result.contents ++ "&lt;"
    | ">" => result := result.contents ++ "&gt;"
    | "\"" => result := result.contents ++ "&quot;"
    | "'" => result := result.contents ++ "&#x27;"
    | "/" => result := result.contents ++ "&#x2F;"
    | _ => result := result.contents ++ char
    }
  }

  result.contents
}

/** Escape string for HTML attribute context */
let escapeHtmlAttribute = (value: string): string => {
  let length = Js.String2.length(value)
  let result = ref("")

  for i in 0 to length - 1 {
    let char = Js.String2.charAt(value, i)
    switch char {
    | "&" => result := result.contents ++ "&amp;"
    | "<" => result := result.contents ++ "&lt;"
    | ">" => result := result.contents ++ "&gt;"
    | "\"" => result := result.contents ++ "&quot;"
    | "'" => result := result.contents ++ "&#x27;"
    | "`" => result := result.contents ++ "&#x60;"
    | "=" => result := result.contents ++ "&#x3D;"
    | _ => result := result.contents ++ char
    }
  }

  result.contents
}

/** Escape string for URL context (percent encoding) */
let escapeUrl = (value: string): string => {
  let safeChars = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_.~"
  let hexChars = "0123456789ABCDEF"
  let length = Js.String2.length(value)
  let result = ref("")

  for i in 0 to length - 1 {
    let char = Js.String2.charAt(value, i)
    if Js.String2.includes(safeChars, char) {
      result := result.contents ++ char
    } else {
      let code = Js.String2.charCodeAt(char, 0)->Belt.Float.toInt
      let high = lsr(code, 4)
      let low = land(code, 0x0f)
      result :=
        result.contents ++
        "%" ++
        Js.String2.charAt(hexChars, high) ++
        Js.String2.charAt(hexChars, low)
    }
  }

  result.contents
}

/** Escape string for JavaScript context */
let escapeJavaScript = (value: string): string => {
  let length = Js.String2.length(value)
  let result = ref("")
  let hexChars = "0123456789ABCDEF"

  for i in 0 to length - 1 {
    let char = Js.String2.charAt(value, i)
    switch char {
    | "\\" => result := result.contents ++ "\\\\"
    | "\"" => result := result.contents ++ "\\\""
    | "'" => result := result.contents ++ "\\'"
    | "\n" => result := result.contents ++ "\\n"
    | "\r" => result := result.contents ++ "\\r"
    | "\t" => result := result.contents ++ "\\t"
    | "<" => result := result.contents ++ "\\u003C"
    | ">" => result := result.contents ++ "\\u003E"
    | "/" => result := result.contents ++ "\\/"
    | _ =>
      let code = Js.String2.charCodeAt(char, 0)->Belt.Float.toInt
      if code < 0x20 {
        // Format as \u00XX
        let h1 = lsr(code, 12)
        let h2 = land(lsr(code, 8), 0x0f)
        let h3 = land(lsr(code, 4), 0x0f)
        let h4 = land(code, 0x0f)
        result :=
          result.contents ++
          "\\u" ++
          Js.String2.charAt(hexChars, h1) ++
          Js.String2.charAt(hexChars, h2) ++
          Js.String2.charAt(hexChars, h3) ++
          Js.String2.charAt(hexChars, h4)
      } else {
        result := result.contents ++ char
      }
    }
  }

  result.contents
}

/** Escape string for CSS context */
let escapeCss = (value: string): string => {
  let length = Js.String2.length(value)
  let result = ref("")
  let hexChars = "0123456789ABCDEF"

  for i in 0 to length - 1 {
    let char = Js.String2.charAt(value, i)
    let code = Js.String2.charCodeAt(char, 0)->Belt.Float.toInt
    let isAlnum =
      (code >= 48 && code <= 57) || (code >= 65 && code <= 90) || (code >= 97 && code <= 122)

    if isAlnum {
      result := result.contents ++ char
    } else {
      // Escape as \HH
      let h1 = lsr(code, 4)
      let h2 = land(code, 0x0f)
      result :=
        result.contents ++ "\\" ++ Js.String2.charAt(hexChars, h1) ++ Js.String2.charAt(hexChars, h2) ++ " "
    }
  }

  result.contents
}

/** Escape string for SQL context (single quotes) */
let escapeSql = (value: string): string => {
  let length = Js.String2.length(value)
  let result = ref("")

  for i in 0 to length - 1 {
    let char = Js.String2.charAt(value, i)
    if char == "'" {
      result := result.contents ++ "''"
    } else {
      result := result.contents ++ char
    }
  }

  result.contents
}

/** Escape a value based on context */
let escapeForContext = (value: string, context: escapeContext): string => {
  switch context {
  | EscapeNone => value
  | EscapeHtml => escapeHtml(value)
  | EscapeHtmlAttribute => escapeHtmlAttribute(value)
  | EscapeUrl => escapeUrl(value)
  | EscapeJavaScript => escapeJavaScript(value)
  | EscapeCss => escapeCss(value)
  | EscapeSql => escapeSql(value)
  }
}

/** Variable lookup type */
type variableLookup = string => option<value>

/** Render a template with a lookup function */
let renderWithLookup = (
  template: string,
  lookup: variableLookup,
  config: config,
): result<string, templateError> => {
  let result = ref("")
  let pos = ref(0)
  let templateLen = Js.String2.length(template)
  let error = ref(None)

  while pos.contents < templateLen && Belt.Option.isNone(error.contents) {
    // Find next opening delimiter
    switch Js.String2.indexOfFrom(template, config.openDelim, pos.contents) {
    | -1 =>
      // No more delimiters, append rest of template
      result := result.contents ++ Js.String2.sliceToEnd(template, ~from=pos.contents)
      pos := templateLen
    | start =>
      // Append text before delimiter
      result := result.contents ++ Js.String2.slice(template, ~from=pos.contents, ~to_=start)

      let varStart = start + Js.String2.length(config.openDelim)

      // Find closing delimiter
      switch Js.String2.indexOfFrom(template, config.closeDelim, varStart) {
      | -1 => error := Some(UnclosedDelimiter)
      | endPos =>
        // Check for nested delimiters
        let varContent = Js.String2.slice(template, ~from=varStart, ~to_=endPos)
        if Js.String2.includes(varContent, config.openDelim) {
          error := Some(NestedDelimiters)
        } else {
          // Strip whitespace if configured
          let varName = if config.stripWhitespace {
            Js.String2.trim(varContent)
          } else {
            varContent
          }

          // Parse escape context modifier
          let (escapeCtx, actualName) = if Js.String2.startsWith(varName, "!") {
            (EscapeNone, Js.String2.sliceToEnd(varName, ~from=1))
          } else if Js.String2.startsWith(varName, "html:") {
            (EscapeHtml, Js.String2.sliceToEnd(varName, ~from=5))
          } else if Js.String2.startsWith(varName, "url:") {
            (EscapeUrl, Js.String2.sliceToEnd(varName, ~from=4))
          } else if Js.String2.startsWith(varName, "js:") {
            (EscapeJavaScript, Js.String2.sliceToEnd(varName, ~from=3))
          } else if Js.String2.startsWith(varName, "css:") {
            (EscapeCss, Js.String2.sliceToEnd(varName, ~from=4))
          } else if Js.String2.startsWith(varName, "sql:") {
            (EscapeSql, Js.String2.sliceToEnd(varName, ~from=4))
          } else {
            (config.escapeContext, varName)
          }

          let trimmedName = Js.String2.trim(actualName)

          // Look up variable
          switch lookup(trimmedName) {
          | Some(value) =>
            let strValue = valueToString(value)
            let escaped = escapeForContext(strValue, escapeCtx)
            result := result.contents ++ escaped
          | None => error := Some(UnknownVariable)
          }

          pos := endPos + Js.String2.length(config.closeDelim)
        }
      }
    }
  }

  switch error.contents {
  | Some(e) => Error(e)
  | None => Ok(result.contents)
  }
}

/** Render a template with a dictionary of variables */
let render = (
  template: string,
  variables: Js.Dict.t<value>,
  config: config,
): result<string, templateError> => {
  let lookup = name => Js.Dict.get(variables, name)
  renderWithLookup(template, lookup, config)
}

/** Render a template with default configuration */
let renderSimple = (template: string, variables: Js.Dict.t<value>): result<string, templateError> => {
  render(template, variables, defaultConfig)
}

/** Check if a template string is valid (has balanced delimiters) */
let isValid = (template: string, config: config): bool => {
  let depth = ref(0)
  let pos = ref(0)
  let templateLen = Js.String2.length(template)
  let valid = ref(true)

  while pos.contents < templateLen && valid.contents {
    switch Js.String2.indexOfFrom(template, config.openDelim, pos.contents) {
    | -1 => pos := templateLen
    | openPos =>
      depth := depth.contents + 1
      pos := openPos + Js.String2.length(config.openDelim)

      switch Js.String2.indexOfFrom(template, config.closeDelim, pos.contents) {
      | -1 => valid := false // Unclosed delimiter
      | closePos =>
        depth := depth.contents - 1
        pos := closePos + Js.String2.length(config.closeDelim)
      }
    }
  }

  valid.contents && depth.contents == 0
}

/** Extract variable names from a template */
let extractVariables = (template: string, config: config): array<string> => {
  let variables = ref([])
  let pos = ref(0)
  let templateLen = Js.String2.length(template)

  while pos.contents < templateLen {
    switch Js.String2.indexOfFrom(template, config.openDelim, pos.contents) {
    | -1 => pos := templateLen
    | start =>
      let varStart = start + Js.String2.length(config.openDelim)

      switch Js.String2.indexOfFrom(template, config.closeDelim, varStart) {
      | -1 => pos := templateLen
      | endPos =>
        let varContent = Js.String2.slice(template, ~from=varStart, ~to_=endPos)
        let varName = Js.String2.trim(varContent)

        // Remove escape context prefix
        let actualName = if Js.String2.startsWith(varName, "!") {
          Js.String2.sliceToEnd(varName, ~from=1)
        } else {
          switch Js.String2.indexOf(varName, ":") {
          | -1 => varName
          | colonPos => Js.String2.sliceToEnd(varName, ~from=colonPos + 1)
          }
        }

        let trimmedName = Js.String2.trim(actualName)

        // Check for duplicates
        let exists = Belt.Array.some(variables.contents, v => v == trimmedName)
        if !exists {
          variables := Belt.Array.concat(variables.contents, [trimmedName])
        }

        pos := endPos + Js.String2.length(config.closeDelim)
      }
    }
  }

  variables.contents
}

/** Escape context to string representation */
let escapeContextToString = (ctx: escapeContext): string => {
  switch ctx {
  | EscapeNone => "none"
  | EscapeHtml => "html"
  | EscapeHtmlAttribute => "html_attribute"
  | EscapeUrl => "url"
  | EscapeJavaScript => "javascript"
  | EscapeCss => "css"
  | EscapeSql => "sql"
  }
}

/** Get the default escape context */
let defaultEscapeContext = (): escapeContext => EscapeHtml

/** Template error to string representation */
let templateErrorToString = (error: templateError): string => {
  switch error {
  | InvalidTemplate => "Invalid template"
  | UnknownVariable => "Unknown variable"
  | UnclosedDelimiter => "Unclosed delimiter"
  | NestedDelimiters => "Nested delimiters are not allowed"
  | InvalidEscapeSequence => "Invalid escape sequence"
  | MaxDepthExceeded => "Maximum nesting depth exceeded"
  | CircularReference => "Circular reference detected"
  }
}

/** Create a value from a string */
let fromString = (s: string): value => VString(s)

/** Create a value from an integer */
let fromInt = (i: int): value => VInteger(i)

/** Create a value from a float */
let fromFloat = (f: float): value => VFloat(f)

/** Create a value from a boolean */
let fromBool = (b: bool): value => VBoolean(b)

/** Create a null value */
let null = (): value => VNull

/** Create a value from an array */
let fromArray = (a: array<value>): value => VArray(a)
