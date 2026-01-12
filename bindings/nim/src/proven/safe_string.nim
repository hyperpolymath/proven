# SPDX-License-Identifier: PMPL-1.0
# SPDX-FileCopyrightText: 2025 Hyperpolymath

## Safe string operations for security-sensitive contexts.

import std/[strutils, uri]

proc escapeHtml*(value: string): string =
  ## Escape HTML special characters to prevent XSS attacks.
  result = value
    .replace("&", "&amp;")
    .replace("<", "&lt;")
    .replace(">", "&gt;")
    .replace("\"", "&quot;")
    .replace("'", "&#x27;")

proc escapeSql*(value: string): string =
  ## Escape single quotes for SQL strings.
  ## Note: Use parameterized queries when possible.
  result = value.replace("'", "''")

proc escapeJs*(value: string): string =
  ## Escape JavaScript special characters.
  result = value
    .replace("\\", "\\\\")
    .replace("\"", "\\\"")
    .replace("'", "\\'")
    .replace("\n", "\\n")
    .replace("\r", "\\r")
    .replace("\t", "\\t")

proc escapeUrl*(value: string): string =
  ## URL-encode a string.
  result = encodeUrl(value)

proc truncateSafe*(value: string, maxLen: int, suffix: string = "..."): string =
  ## Safely truncate a string with a suffix.
  if value.len <= maxLen:
    return value

  let suffixLen = suffix.len
  if maxLen <= suffixLen:
    return suffix[0 ..< maxLen]

  result = value[0 ..< maxLen - suffixLen] & suffix
