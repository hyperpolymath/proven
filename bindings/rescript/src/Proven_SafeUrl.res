// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

/**
 * SafeUrl - URL parsing that cannot crash.
 */

type parsedUrl = {
  scheme: string,
  host: string,
  port: option<int>,
  path: string,
  query: option<string>,
  fragment: option<string>,
}

// Web API URL bindings
type url
@new external makeUrl: string => url = "URL"
@get external protocol: url => string = "protocol"
@get external hostname: url => string = "hostname"
@get external port: url => string = "port"
@get external pathname: url => string = "pathname"
@get external search: url => string = "search"
@get external hash: url => string = "hash"

/** Parse a URL into its components */
let parse = (urlString: string): option<parsedUrl> => {
  try {
    let jsUrl = makeUrl(urlString)
    Some({
      scheme: protocol(jsUrl)->Js.String2.replace(":", ""),
      host: hostname(jsUrl),
      port: {
        let portStr = port(jsUrl)
        if portStr == "" {
          None
        } else {
          Belt.Int.fromString(portStr)
        }
      },
      path: {
        let p = pathname(jsUrl)
        if p == "" {
          "/"
        } else {
          p
        }
      },
      query: {
        let q = search(jsUrl)
        if q == "" || q == "?" {
          None
        } else {
          Some(Js.String2.sliceToEnd(q, ~from=1))
        }
      },
      fragment: {
        let f = hash(jsUrl)
        if f == "" || f == "#" {
          None
        } else {
          Some(Js.String2.sliceToEnd(f, ~from=1))
        }
      },
    })
  } catch {
  | _ => None
  }
}

/** Check if a URL is valid */
let isValid = (url: string): bool => {
  switch parse(url) {
  | Some(_) => true
  | None => false
  }
}

/** Validate and normalize a URL */
let normalize = (url: string): option<string> => {
  switch parse(url) {
  | Some(parsed) =>
    let portPart = switch parsed.port {
    | Some(p) => ":" ++ Belt.Int.toString(p)
    | None => ""
    }
    let queryPart = switch parsed.query {
    | Some(q) => "?" ++ q
    | None => ""
    }
    let fragmentPart = switch parsed.fragment {
    | Some(f) => "#" ++ f
    | None => ""
    }
    Some(parsed.scheme ++ "://" ++ parsed.host ++ portPart ++ parsed.path ++ queryPart ++ fragmentPart)
  | None => None
  }
}

/** Get just the host from a URL */
let getHost = (url: string): option<string> => {
  switch parse(url) {
  | Some(parsed) => Some(parsed.host)
  | None => None
  }
}

/** Get just the path from a URL */
let getPath = (url: string): option<string> => {
  switch parse(url) {
  | Some(parsed) => Some(parsed.path)
  | None => None
  }
}

/** Check if URL uses HTTPS */
let isHttps = (url: string): bool => {
  switch parse(url) {
  | Some(parsed) => parsed.scheme == "https"
  | None => false
  }
}

/** Check if URL uses HTTP or HTTPS */
let isHttp = (url: string): bool => {
  switch parse(url) {
  | Some(parsed) => parsed.scheme == "http" || parsed.scheme == "https"
  | None => false
  }
}
