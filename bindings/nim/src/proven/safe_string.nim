# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
#
# Safe string operations for security-sensitive contexts.
# Thin wrapper over libproven FFI -- all logic lives in Idris.

import std/options
import lib_proven

proc isValidUtf8*(value: string): bool =
  ## Check if a string contains valid UTF-8 bytes.
  if value.len == 0:
    return true
  let res = provenStringIsValidUtf8(unsafeAddr value[0], csize_t(value.len))
  if res.status == PROVEN_OK:
    return res.value
  false

proc escapeSql*(value: string): Option[string] =
  ## Escape single quotes for SQL strings.
  ## Note: Use parameterized queries when possible.
  if value.len == 0:
    return some("")
  let res = provenStringEscapeSql(unsafeAddr value[0], csize_t(value.len))
  if res.status == PROVEN_OK and res.value != nil:
    let escaped = $res.value
    provenFreeString(res.value)
    return some(escaped)
  none(string)

proc escapeHtml*(value: string): Option[string] =
  ## Escape HTML special characters to prevent XSS attacks.
  if value.len == 0:
    return some("")
  let res = provenStringEscapeHtml(unsafeAddr value[0], csize_t(value.len))
  if res.status == PROVEN_OK and res.value != nil:
    let escaped = $res.value
    provenFreeString(res.value)
    return some(escaped)
  none(string)

proc escapeJs*(value: string): Option[string] =
  ## Escape JavaScript special characters.
  if value.len == 0:
    return some("")
  let res = provenStringEscapeJs(unsafeAddr value[0], csize_t(value.len))
  if res.status == PROVEN_OK and res.value != nil:
    let escaped = $res.value
    provenFreeString(res.value)
    return some(escaped)
  none(string)
