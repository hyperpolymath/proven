// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

/**
 * SafeHttp - HTTP method and status code validation that cannot crash.
 *
 * Provides type-safe handling of HTTP semantics with proper validation.
 * All operations return None/Error on invalid input instead of throwing.
 */
/** HTTP methods per RFC 7231 and RFC 5789 */
type method =
  | GET
  | HEAD
  | POST
  | PUT
  | DELETE
  | CONNECT
  | OPTIONS
  | TRACE
  | PATCH

/** HTTP status code categories */
type statusCategory =
  | Informational
  | Successful
  | Redirection
  | ClientError
  | ServerError

/** HTTP version */
type httpVersion =
  | Http09
  | Http10
  | Http11
  | Http2
  | Http3

/** HTTP status code with reason phrase */
type statusCode = {
  code: int,
  reasonPhrase: string,
  category: statusCategory,
}

/** Check if a method is safe (does not modify server state) */
let isSafe = (m: method): bool => {
  switch m {
  | GET | HEAD | OPTIONS | TRACE => true
  | _ => false
  }
}

/** Check if a method is idempotent (same request = same effect) */
let isIdempotent = (m: method): bool => {
  switch m {
  | GET | HEAD | PUT | DELETE | OPTIONS | TRACE => true
  | _ => false
  }
}

/** Check if a method typically has a request body */
let hasRequestBody = (m: method): bool => {
  switch m {
  | POST | PUT | PATCH => true
  | _ => false
  }
}

/** Check if a method typically has a response body */
let hasResponseBody = (m: method): bool => {
  switch m {
  | HEAD => false
  | _ => true
  }
}

/** Check if a method is cacheable by default */
let isCacheable = (m: method): bool => {
  switch m {
  | GET | HEAD => true
  | _ => false
  }
}

/** Convert a method to its string representation */
let methodToString = (m: method): string => {
  switch m {
  | GET => "GET"
  | HEAD => "HEAD"
  | POST => "POST"
  | PUT => "PUT"
  | DELETE => "DELETE"
  | CONNECT => "CONNECT"
  | OPTIONS => "OPTIONS"
  | TRACE => "TRACE"
  | PATCH => "PATCH"
  }
}

/** Parse a string into an HTTP method (case-insensitive) */
let parseMethod = (str: string): option<method> => {
  switch Js.String2.toUpperCase(str) {
  | "GET" => Some(GET)
  | "HEAD" => Some(HEAD)
  | "POST" => Some(POST)
  | "PUT" => Some(PUT)
  | "DELETE" => Some(DELETE)
  | "CONNECT" => Some(CONNECT)
  | "OPTIONS" => Some(OPTIONS)
  | "TRACE" => Some(TRACE)
  | "PATCH" => Some(PATCH)
  | _ => None
  }
}

/** Check if a string is a valid HTTP method */
let isValidMethod = (str: string): bool => {
  Belt.Option.isSome(parseMethod(str))
}

/** Get the category for a status code */
let getStatusCategory = (code: int): option<statusCategory> => {
  if code >= 100 && code < 200 {
    Some(Informational)
  } else if code >= 200 && code < 300 {
    Some(Successful)
  } else if code >= 300 && code < 400 {
    Some(Redirection)
  } else if code >= 400 && code < 500 {
    Some(ClientError)
  } else if code >= 500 && code < 600 {
    Some(ServerError)
  } else {
    None
  }
}

/** Check if a status code indicates an error */
let isErrorStatus = (code: int): bool => {
  code >= 400 && code < 600
}

/** Check if a status code indicates success */
let isSuccessStatus = (code: int): bool => {
  code >= 200 && code < 300
}

/** Check if a status code is a redirect */
let isRedirectStatus = (code: int): bool => {
  code >= 300 && code < 400
}

/** Check if a status code typically has a response body */
let statusHasBody = (code: int): bool => {
  !(code == 204 || code == 304 || code == 100 || code == 101)
}

/** Get the standard reason phrase for a status code */
let getReasonPhrase = (code: int): option<string> => {
  switch code {
  // 1xx Informational
  | 100 => Some("Continue")
  | 101 => Some("Switching Protocols")
  | 102 => Some("Processing")
  | 103 => Some("Early Hints")
  // 2xx Successful
  | 200 => Some("OK")
  | 201 => Some("Created")
  | 202 => Some("Accepted")
  | 203 => Some("Non-Authoritative Information")
  | 204 => Some("No Content")
  | 205 => Some("Reset Content")
  | 206 => Some("Partial Content")
  | 207 => Some("Multi-Status")
  | 208 => Some("Already Reported")
  | 226 => Some("IM Used")
  // 3xx Redirection
  | 300 => Some("Multiple Choices")
  | 301 => Some("Moved Permanently")
  | 302 => Some("Found")
  | 303 => Some("See Other")
  | 304 => Some("Not Modified")
  | 305 => Some("Use Proxy")
  | 307 => Some("Temporary Redirect")
  | 308 => Some("Permanent Redirect")
  // 4xx Client Error
  | 400 => Some("Bad Request")
  | 401 => Some("Unauthorized")
  | 402 => Some("Payment Required")
  | 403 => Some("Forbidden")
  | 404 => Some("Not Found")
  | 405 => Some("Method Not Allowed")
  | 406 => Some("Not Acceptable")
  | 407 => Some("Proxy Authentication Required")
  | 408 => Some("Request Timeout")
  | 409 => Some("Conflict")
  | 410 => Some("Gone")
  | 411 => Some("Length Required")
  | 412 => Some("Precondition Failed")
  | 413 => Some("Payload Too Large")
  | 414 => Some("URI Too Long")
  | 415 => Some("Unsupported Media Type")
  | 416 => Some("Range Not Satisfiable")
  | 417 => Some("Expectation Failed")
  | 418 => Some("I'm a teapot")
  | 421 => Some("Misdirected Request")
  | 422 => Some("Unprocessable Entity")
  | 423 => Some("Locked")
  | 424 => Some("Failed Dependency")
  | 425 => Some("Too Early")
  | 426 => Some("Upgrade Required")
  | 428 => Some("Precondition Required")
  | 429 => Some("Too Many Requests")
  | 431 => Some("Request Header Fields Too Large")
  | 451 => Some("Unavailable For Legal Reasons")
  // 5xx Server Error
  | 500 => Some("Internal Server Error")
  | 501 => Some("Not Implemented")
  | 502 => Some("Bad Gateway")
  | 503 => Some("Service Unavailable")
  | 504 => Some("Gateway Timeout")
  | 505 => Some("HTTP Version Not Supported")
  | 506 => Some("Variant Also Negotiates")
  | 507 => Some("Insufficient Storage")
  | 508 => Some("Loop Detected")
  | 510 => Some("Not Extended")
  | 511 => Some("Network Authentication Required")
  | _ => None
  }
}

/** Parse an integer into a validated status code */
let parseStatusCode = (code: int): option<statusCode> => {
  switch (getStatusCategory(code), getReasonPhrase(code)) {
  | (Some(category), Some(reasonPhrase)) => Some({code, reasonPhrase, category})
  | (Some(category), None) =>
    // Allow unknown codes within valid ranges
    Some({code, reasonPhrase: "Unknown", category})
  | _ => None
  }
}

/** Check if an integer is a valid HTTP status code */
let isValidStatusCode = (code: int): bool => {
  code >= 100 && code <= 599
}

/** Convert HTTP version to string */
let versionToString = (v: httpVersion): string => {
  switch v {
  | Http09 => "HTTP/0.9"
  | Http10 => "HTTP/1.0"
  | Http11 => "HTTP/1.1"
  | Http2 => "HTTP/2"
  | Http3 => "HTTP/3"
  }
}

/** Parse HTTP version from string */
let parseVersion = (str: string): option<httpVersion> => {
  switch str {
  | "HTTP/0.9" => Some(Http09)
  | "HTTP/1.0" => Some(Http10)
  | "HTTP/1.1" => Some(Http11)
  | "HTTP/2" | "HTTP/2.0" => Some(Http2)
  | "HTTP/3" | "HTTP/3.0" => Some(Http3)
  | _ => None
  }
}

/** Check if HTTP version supports persistent connections by default */
let supportsKeepAlive = (v: httpVersion): bool => {
  switch v {
  | Http09 | Http10 => false
  | Http11 | Http2 | Http3 => true
  }
}

/** Check if HTTP version supports request pipelining */
let supportsPipelining = (v: httpVersion): bool => {
  switch v {
  | Http11 => true
  | _ => false
  }
}

/** Check if HTTP version supports multiplexing */
let supportsMultiplexing = (v: httpVersion): bool => {
  switch v {
  | Http2 | Http3 => true
  | _ => false
  }
}

/** Format a status line (e.g., "HTTP/1.1 200 OK") */
let formatStatusLine = (version: httpVersion, code: int): option<string> => {
  switch getReasonPhrase(code) {
  | Some(reason) => Some(`${versionToString(version)} ${Belt.Int.toString(code)} ${reason}`)
  | None => None
  }
}

/** Format a request line (e.g., "GET /path HTTP/1.1") */
let formatRequestLine = (m: method, path: string, version: httpVersion): string => {
  `${methodToString(m)} ${path} ${versionToString(version)}`
}

/** Check if a method is allowed for a given set of allowed methods */
let isMethodAllowed = (m: method, allowed: array<method>): bool => {
  Js.Array2.includes(allowed, m)
}

/** Common status codes as constants */
module StatusCodes = {
  let ok = 200
  let created = 201
  let accepted = 202
  let noContent = 204
  let movedPermanently = 301
  let found = 302
  let notModified = 304
  let badRequest = 400
  let unauthorized = 401
  let forbidden = 403
  let notFound = 404
  let methodNotAllowed = 405
  let conflict = 409
  let gone = 410
  let unprocessableEntity = 422
  let tooManyRequests = 429
  let internalServerError = 500
  let notImplemented = 501
  let badGateway = 502
  let serviceUnavailable = 503
  let gatewayTimeout = 504
}
