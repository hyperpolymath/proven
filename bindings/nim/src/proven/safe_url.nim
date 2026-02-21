# SPDX-License-Identifier: PMPL-1.0-or-later
# SPDX-FileCopyrightText: 2025 Hyperpolymath

## Safe URL parsing and validation.

import std/[options, strutils, uri]

type
  SafeUrl* = object
    ## A validated URL with parsed components.
    scheme*: string
    host*: string
    port*: Option[int]
    path*: string
    query*: string
    fragment*: string

  UrlError* = enum
    ## URL parsing errors.
    ueNone,
    ueEmptyUrl,
    ueInvalidScheme,
    ueNoHost,
    ueTooLong,
    ueInvalidPort

const
  MaxUrlLength* = 8192
  AllowedSchemes* = ["http", "https", "ftp", "ftps", "mailto", "file"]

proc parseUrl*(raw: string): Option[SafeUrl] =
  ## Parse a URL string into a SafeUrl.
  ## Returns None if the URL is invalid.
  if raw.len == 0 or raw.len > MaxUrlLength:
    return none(SafeUrl)

  try:
    let parsed = parseUri(raw)

    if parsed.scheme.len == 0:
      return none(SafeUrl)

    let scheme = parsed.scheme.toLowerAscii()
    if scheme notin AllowedSchemes:
      return none(SafeUrl)

    var port = none(int)
    if parsed.port.len > 0:
      try:
        let p = parseInt(parsed.port)
        if p >= 0 and p <= 65535:
          port = some(p)
        else:
          return none(SafeUrl)
      except ValueError:
        return none(SafeUrl)

    result = some(SafeUrl(
      scheme: scheme,
      host: parsed.hostname,
      port: port,
      path: parsed.path,
      query: parsed.query,
      fragment: parsed.anchor
    ))
  except:
    return none(SafeUrl)

proc isValidUrl*(raw: string): bool =
  ## Check if a string is a valid URL.
  parseUrl(raw).isSome

proc isHttps*(url: SafeUrl): bool =
  ## Check if the URL uses HTTPS.
  url.scheme == "https"

proc isSecure*(url: SafeUrl): bool =
  ## Check if the URL uses a secure scheme.
  url.scheme in ["https", "ftps"]

proc hasPort*(url: SafeUrl): bool =
  ## Check if the URL has an explicit port.
  url.port.isSome

proc getDefaultPort*(scheme: string): int =
  ## Get the default port for a scheme.
  case scheme.toLowerAscii()
  of "http": 80
  of "https": 443
  of "ftp": 21
  of "ftps": 990
  else: 0

proc effectivePort*(url: SafeUrl): int =
  ## Get the effective port (explicit or default).
  if url.port.isSome:
    url.port.get()
  else:
    getDefaultPort(url.scheme)

proc origin*(url: SafeUrl): string =
  ## Get the origin (scheme + host + port) of a URL.
  result = url.scheme & "://" & url.host
  if url.port.isSome:
    let p = url.port.get()
    let defaultPort = getDefaultPort(url.scheme)
    if p != defaultPort:
      result.add(":" & $p)

proc toString*(url: SafeUrl): string =
  ## Convert a SafeUrl back to a string.
  result = url.scheme & "://" & url.host
  if url.port.isSome:
    result.add(":" & $url.port.get())
  result.add(url.path)
  if url.query.len > 0:
    result.add("?" & url.query)
  if url.fragment.len > 0:
    result.add("#" & url.fragment)

proc sameOrigin*(a, b: SafeUrl): bool =
  ## Check if two URLs have the same origin.
  a.scheme == b.scheme and
  a.host == b.host and
  a.effectivePort() == b.effectivePort()
