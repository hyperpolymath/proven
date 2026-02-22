# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
#
# Safe JSON validation operations.
# Thin wrapper over libproven FFI -- all logic lives in Idris.

import std/options
import lib_proven

proc isValidJson*(raw: string): bool =
  ## Check if a string is valid JSON.
  ## Validation logic is provided by libproven.
  if raw.len == 0:
    return false
  let res = provenJsonIsValid(unsafeAddr raw[0], csize_t(raw.len))
  if res.status == PROVEN_OK:
    return res.value
  false

proc getJsonType*(raw: string): Option[JsonType] =
  ## Determine the top-level JSON type of a string.
  ## Returns None if the string is not valid JSON.
  if raw.len == 0:
    return none(JsonType)
  let typeCode = provenJsonGetType(unsafeAddr raw[0], csize_t(raw.len))
  case typeCode
  of 0: some(jtNull)
  of 1: some(jtBool)
  of 2: some(jtNumber)
  of 3: some(jtString)
  of 4: some(jtArray)
  of 5: some(jtObject)
  else: none(JsonType)
