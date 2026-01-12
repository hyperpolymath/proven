// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

/**
 * SafeHeader - HTTP header operations that cannot crash.
 *
 * All operations handle injection attacks (CRLF) and size limits
 * without throwing exceptions. Operations return None on failure.
 */

/** Header name/value pair */
type header = {
  name: string,
  value: string,
}

/** Check for CRLF injection characters */
let hasCRLF = (s: string): bool => {
  Js.String2.includes(s, "\r") || Js.String2.includes(s, "\n")
}

/** Valid token characters per RFC 7230 */
let isValidTokenChar = (c: string): bool => {
  let code = Js.String2.charCodeAt(c, 0)
  code >= 33.0 && code <= 126.0 && !Js.String2.includes("()<>@,;:\\\"/[]?=", c)
}

/** Validate header name is valid token */
let isValidName = (name: string): bool => {
  if Js.String2.length(name) == 0 || Js.String2.length(name) > 256 {
    false
  } else {
    Js.Array2.every(Js.String2.split(name, ""), isValidTokenChar)
  }
}

/** Dangerous headers that should not be set by user code */
let dangerousHeaders = [
  "proxy-authorization",
  "proxy-authenticate",
  "proxy-connection",
  "transfer-encoding",
  "content-length",
  "host",
  "connection",
  "keep-alive",
  "upgrade",
  "te",
  "trailer",
]

/** Check if header name is dangerous */
let isDangerous = (name: string): bool => {
  Js.Array2.includes(dangerousHeaders, Js.String2.toLowerCase(name))
}

/** Create a validated header */
let make = (name: string, value: string): option<header> => {
  let trimmedName = Js.String2.trim(name)
  let trimmedValue = Js.String2.trim(value)

  if !isValidName(trimmedName) {
    None
  } else if hasCRLF(trimmedValue) {
    None
  } else if Js.String2.length(trimmedValue) > 8192 {
    None
  } else {
    Some({name: trimmedName, value: trimmedValue})
  }
}

/** Create header, blocking dangerous headers */
let makeSafe = (name: string, value: string): option<header> => {
  if isDangerous(name) {
    None
  } else {
    make(name, value)
  }
}

/** Render header to "Name: Value" format */
let render = (h: header): string => {
  h.name ++ ": " ++ h.value
}

/** Build Content-Security-Policy header value */
let buildCSP = (directives: array<(string, array<string>)>): string => {
  Js.Array2.joinWith(
    Js.Array2.map(directives, ((name, sources)) => {
      if Js.Array2.length(sources) == 0 {
        name
      } else {
        name ++ " " ++ Js.Array2.joinWith(sources, " ")
      }
    }),
    "; ",
  )
}

/** Build Strict-Transport-Security header value */
let buildHSTS = (maxAge: int, includeSubDomains: bool, preload: bool): string => {
  let base = "max-age=" ++ Belt.Int.toString(maxAge)
  let withSub = includeSubDomains ? base ++ "; includeSubDomains" : base
  preload ? withSub ++ "; preload" : withSub
}

/** Common security headers preset */
let securityHeaders = (): array<header> => {
  [
    {name: "X-Frame-Options", value: "DENY"},
    {name: "X-Content-Type-Options", value: "nosniff"},
    {name: "Referrer-Policy", value: "strict-origin-when-cross-origin"},
    {name: "X-XSS-Protection", value: "1; mode=block"},
  ]
}
