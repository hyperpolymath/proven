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

/** Parse a URL into its components */
let parse = (url: string): option<parsedUrl> => {
  try {
    let jsUrl = Js.Url.make(url)
    Some({
      scheme: Js.Url.protocol(jsUrl)->Js.String2.replace(":", ""),
      host: Js.Url.hostname(jsUrl),
      port: {
        let portStr = Js.Url.port(jsUrl)
        if portStr == "" {
          None
        } else {
          Belt.Int.fromString(portStr)
        }
      },
      path: {
        let p = Js.Url.pathname(jsUrl)
        if p == "" {
          "/"
        } else {
          p
        }
      },
      query: {
        let q = Js.Url.search(jsUrl)
        if q == "" || q == "?" {
          None
        } else {
          Some(Js.String2.sliceToEnd(q, ~from=1))
        }
      },
      fragment: {
        let f = Js.Url.hash(jsUrl)
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

/** Extract the host from a URL */
let getHost = (url: string): option<string> => {
  switch parse(url) {
  | Some({host}) => Some(host)
  | None => None
  }
}

/** Extract the scheme from a URL */
let getScheme = (url: string): option<string> => {
  switch parse(url) {
  | Some({scheme}) => Some(scheme)
  | None => None
  }
}

/** Check if a URL uses HTTPS */
let isHttps = (url: string): bool => {
  switch getScheme(url) {
  | Some(s) => Js.String2.toLowerCase(s) == "https"
  | None => false
  }
}

/** Reconstruct a URL from its components */
let toString = (parsed: parsedUrl): string => {
  let result = ref(`${parsed.scheme}://${parsed.host}`)
  switch parsed.port {
  | Some(p) => result := result.contents ++ `:${Belt.Int.toString(p)}`
  | None => ()
  }
  result := result.contents ++ parsed.path
  switch parsed.query {
  | Some(q) => result := result.contents ++ `?${q}`
  | None => ()
  }
  switch parsed.fragment {
  | Some(f) => result := result.contents ++ `#${f}`
  | None => ()
  }
  result.contents
}
