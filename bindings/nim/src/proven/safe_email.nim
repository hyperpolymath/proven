# SPDX-License-Identifier: PMPL-1.0-or-later
# SPDX-FileCopyrightText: 2025 Hyperpolymath

## Safe email validation and parsing operations.

import std/[strutils, options]

type
  EmailParts* = object
    ## Represents the parts of an email address.
    localPart*: string
    domain*: string

proc isValid*(email: string): bool =
  ## Check if an email address is valid (basic check).
  let parts = email.split('@')
  if parts.len != 2:
    return false

  let localPart = parts[0]
  let domain = parts[1]

  if localPart.len == 0:
    return false
  if domain.len < 3:
    return false
  if '.' notin domain:
    return false
  if domain.startsWith('.'):
    return false
  if domain.endsWith('.'):
    return false

  result = true

proc splitEmail*(email: string): Option[EmailParts] =
  ## Split an email into local part and domain.
  if not isValid(email):
    return none(EmailParts)

  let parts = email.split('@')
  result = some(EmailParts(
    localPart: parts[0],
    domain: parts[1]
  ))

proc getDomain*(email: string): Option[string] =
  ## Extract the domain from an email address.
  let parts = splitEmail(email)
  if parts.isSome:
    return some(parts.get.domain)
  result = none(string)

proc getLocalPart*(email: string): Option[string] =
  ## Extract the local part from an email address.
  let parts = splitEmail(email)
  if parts.isSome:
    return some(parts.get.localPart)
  result = none(string)

proc normalize*(email: string): Option[string] =
  ## Normalize an email address (lowercase domain).
  let parts = splitEmail(email)
  if parts.isNone:
    return none(string)

  let p = parts.get
  result = some(p.localPart & "@" & p.domain.toLowerAscii)
