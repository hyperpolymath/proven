# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
#
# Safe URL parsing and validation.
# Thin wrapper over libproven FFI -- all logic lives in Idris.

import std/options
import lib_proven

type
  SafeUrl* = object
    ## A validated URL with parsed components.
    scheme*: string
    host*: string
    port*: Option[uint16]
    path*: string
    query*: string
    fragment*: string

proc parseUrl*(raw: string): Option[SafeUrl] =
  ## Parse a URL string into a SafeUrl.
  ## Returns None if the URL is invalid.
  if raw.len == 0:
    return none(SafeUrl)
  var res = provenUrlParse(unsafeAddr raw[0], csize_t(raw.len))
  if res.status != PROVEN_OK:
    return none(SafeUrl)

  # Marshal C strings to Nim strings before freeing
  let scheme = if res.components.scheme != nil and res.components.scheme_len > 0:
    $res.components.scheme
  else: ""
  let host = if res.components.host != nil and res.components.host_len > 0:
    $res.components.host
  else: ""
  let port = if res.components.has_port: some(res.components.port) else: none(uint16)
  let path = if res.components.path != nil and res.components.path_len > 0:
    $res.components.path
  else: ""
  let query = if res.components.query != nil and res.components.query_len > 0:
    $res.components.query
  else: ""
  let fragment = if res.components.fragment != nil and res.components.fragment_len > 0:
    $res.components.fragment
  else: ""

  provenUrlFree(addr res.components)

  some(SafeUrl(
    scheme: scheme,
    host: host,
    port: port,
    path: path,
    query: query,
    fragment: fragment
  ))

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
