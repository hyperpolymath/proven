// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

/**
 * SafeCookie - HTTP cookie operations that cannot crash.
 *
 * All operations handle injection attacks (semicolons, newlines) and
 * enforce security requirements without throwing exceptions.
 */

/** SameSite attribute values */
type sameSite = Strict | Lax | SameSiteNone

/** Cookie prefix type */
type cookiePrefix = NoPrefix | SecurePrefix | HostPrefix

/** Cookie attributes */
type cookieAttributes = {
  domain: option<string>,
  path: option<string>,
  maxAge: option<int>,
  secure: bool,
  httpOnly: bool,
  sameSite: option<sameSite>,
  partitioned: bool,
}

/** Complete cookie */
type cookie = {
  name: string,
  value: string,
  attributes: cookieAttributes,
}

/** Check for injection characters */
let hasInjection = (s: string): bool => {
  Js.String2.includes(s, ";") || Js.String2.includes(s, "\r") || Js.String2.includes(s, "\n")
}

/** Get prefix from cookie name */
let getPrefix = (name: string): cookiePrefix => {
  if Js.String2.startsWith(name, "__Host-") {
    HostPrefix
  } else if Js.String2.startsWith(name, "__Secure-") {
    SecurePrefix
  } else {
    NoPrefix
  }
}

/** Default secure attributes */
let defaultAttributes: cookieAttributes = {
  domain: None,
  path: Some("/"),
  maxAge: None,
  secure: true,
  httpOnly: true,
  sameSite: Some(Lax),
  partitioned: false,
}

/** Session cookie attributes */
let sessionAttributes: cookieAttributes = {
  domain: None,
  path: Some("/"),
  maxAge: None,
  secure: true,
  httpOnly: true,
  sameSite: Some(Strict),
  partitioned: false,
}

/** Strict security attributes */
let strictAttributes: cookieAttributes = {
  domain: None,
  path: Some("/"),
  maxAge: None,
  secure: true,
  httpOnly: true,
  sameSite: Some(Strict),
  partitioned: false,
}

/** Validate cookie name */
let validateName = (name: string): option<string> => {
  if Js.String2.length(name) == 0 || Js.String2.length(name) > 256 {
    None
  } else if hasInjection(name) {
    None
  } else {
    Some(name)
  }
}

/** Validate cookie value */
let validateValue = (value: string): option<string> => {
  if Js.String2.length(value) > 4096 {
    None
  } else if hasInjection(value) {
    None
  } else {
    Some(value)
  }
}

/** Validate prefix requirements */
let validatePrefix = (name: string, attrs: cookieAttributes): bool => {
  switch getPrefix(name) {
  | NoPrefix => true
  | SecurePrefix => attrs.secure
  | HostPrefix =>
    attrs.secure && Belt.Option.isNone(attrs.domain) && attrs.path == Some("/")
  }
}

/** Validate SameSite=None requires Secure */
let validateSameSite = (attrs: cookieAttributes): bool => {
  switch attrs.sameSite {
  | Some(SameSiteNone) => attrs.secure
  | _ => true
  }
}

/** Create a validated cookie */
let make = (name: string, value: string, attrs: cookieAttributes): option<cookie> => {
  switch (validateName(name), validateValue(value)) {
  | (Some(n), Some(v)) =>
    if !validatePrefix(n, attrs) {
      None
    } else if !validateSameSite(attrs) {
      None
    } else {
      Some({name: n, value: v, attributes: attrs})
    }
  | _ => None
  }
}

/** Create cookie with default attributes */
let makeDefault = (name: string, value: string): option<cookie> => {
  make(name, value, defaultAttributes)
}

/** Create session cookie */
let makeSession = (name: string, value: string): option<cookie> => {
  make(name, value, sessionAttributes)
}

/** Render SameSite attribute */
let sameSiteToString = (ss: sameSite): string => {
  switch ss {
  | Strict => "Strict"
  | Lax => "Lax"
  | SameSiteNone => "None"
  }
}

/** Build Set-Cookie header value */
let buildSetCookie = (c: cookie): string => {
  let base = c.name ++ "=" ++ c.value
  let parts = [base]

  let parts = switch c.attributes.domain {
  | Some(d) => Js.Array2.concat(parts, ["Domain=" ++ d])
  | None => parts
  }

  let parts = switch c.attributes.path {
  | Some(p) => Js.Array2.concat(parts, ["Path=" ++ p])
  | None => parts
  }

  let parts = switch c.attributes.maxAge {
  | Some(age) => Js.Array2.concat(parts, ["Max-Age=" ++ Belt.Int.toString(age)])
  | None => parts
  }

  let parts = c.attributes.secure ? Js.Array2.concat(parts, ["Secure"]) : parts
  let parts = c.attributes.httpOnly ? Js.Array2.concat(parts, ["HttpOnly"]) : parts

  let parts = switch c.attributes.sameSite {
  | Some(ss) => Js.Array2.concat(parts, ["SameSite=" ++ sameSiteToString(ss)])
  | None => parts
  }

  let parts = c.attributes.partitioned ? Js.Array2.concat(parts, ["Partitioned"]) : parts

  Js.Array2.joinWith(parts, "; ")
}

/** Build delete cookie header */
let buildDeleteCookie = (name: string): string => {
  name ++ "=; Max-Age=0; Path=/; Expires=Thu, 01 Jan 1970 00:00:00 GMT"
}

/** Parse Cookie header into name-value pairs */
let parseCookieHeader = (header: string): array<(string, string)> => {
  Js.Array2.map(
    Js.Array2.filter(Js.String2.split(header, ";"), s => Js.String2.length(Js.String2.trim(s)) > 0),
    pair => {
      let trimmed = Js.String2.trim(pair)
      switch Js.String2.indexOf(trimmed, "=") {
      | -1 => (trimmed, "")
      | idx => (
          Js.String2.trim(Js.String2.slice(trimmed, ~from=0, ~to_=idx)),
          Js.String2.trim(Js.String2.sliceToEnd(trimmed, ~from=idx + 1)),
        )
      }
    },
  )
}

/** Common expiration: 1 hour in seconds */
let oneHour = 3600

/** Common expiration: 1 day in seconds */
let oneDay = 86400

/** Common expiration: 1 week in seconds */
let oneWeek = 604800

/** Common expiration: 30 days in seconds */
let thirtyDays = 2592000

/** Common expiration: 1 year in seconds */
let oneYear = 31536000
