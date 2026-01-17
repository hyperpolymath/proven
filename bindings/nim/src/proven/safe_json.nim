# SPDX-License-Identifier: PMPL-1.0
# SPDX-FileCopyrightText: 2025 Hyperpolymath

## Safe JSON operations with bounds checking.

import std/[options, json, strutils]

type
  SafeJsonError* = enum
    ## JSON parsing errors.
    jseNone,
    jseEmptyInput,
    jseParseFailed,
    jseKeyNotFound,
    jseTypeMismatch,
    jseTooDeep,
    jseTooLarge

const
  MaxJsonDepth* = 100
  MaxJsonSize* = 10 * 1024 * 1024  # 10MB

proc safeParseJson*(raw: string): Option[JsonNode] =
  ## Safely parse a JSON string.
  ## Returns None on parse failure or if too large.
  if raw.len == 0 or raw.len > MaxJsonSize:
    return none(JsonNode)

  try:
    result = some(parseJson(raw))
  except JsonParsingError:
    return none(JsonNode)
  except:
    return none(JsonNode)

proc safeGetStr*(node: JsonNode, key: string): Option[string] =
  ## Safely get a string value from a JSON object.
  if node.isNil or node.kind != JObject:
    return none(string)

  if not node.hasKey(key):
    return none(string)

  let val = node[key]
  if val.kind != JString:
    return none(string)

  result = some(val.getStr())

proc safeGetInt*(node: JsonNode, key: string): Option[int64] =
  ## Safely get an integer value from a JSON object.
  if node.isNil or node.kind != JObject:
    return none(int64)

  if not node.hasKey(key):
    return none(int64)

  let val = node[key]
  if val.kind != JInt:
    return none(int64)

  result = some(val.getBiggestInt())

proc safeGetFloat*(node: JsonNode, key: string): Option[float64] =
  ## Safely get a float value from a JSON object.
  if node.isNil or node.kind != JObject:
    return none(float64)

  if not node.hasKey(key):
    return none(float64)

  let val = node[key]
  case val.kind
  of JFloat:
    result = some(val.getFloat())
  of JInt:
    result = some(float64(val.getBiggestInt()))
  else:
    return none(float64)

proc safeGetBool*(node: JsonNode, key: string): Option[bool] =
  ## Safely get a boolean value from a JSON object.
  if node.isNil or node.kind != JObject:
    return none(bool)

  if not node.hasKey(key):
    return none(bool)

  let val = node[key]
  if val.kind != JBool:
    return none(bool)

  result = some(val.getBool())

proc safeGetArray*(node: JsonNode, key: string): Option[seq[JsonNode]] =
  ## Safely get an array from a JSON object.
  if node.isNil or node.kind != JObject:
    return none(seq[JsonNode])

  if not node.hasKey(key):
    return none(seq[JsonNode])

  let val = node[key]
  if val.kind != JArray:
    return none(seq[JsonNode])

  var arr: seq[JsonNode] = @[]
  for item in val:
    arr.add(item)
  result = some(arr)

proc safeGetObject*(node: JsonNode, key: string): Option[JsonNode] =
  ## Safely get a nested object from a JSON object.
  if node.isNil or node.kind != JObject:
    return none(JsonNode)

  if not node.hasKey(key):
    return none(JsonNode)

  let val = node[key]
  if val.kind != JObject:
    return none(JsonNode)

  result = some(val)

proc safeIndex*(node: JsonNode, index: int): Option[JsonNode] =
  ## Safely get an element from a JSON array by index.
  if node.isNil or node.kind != JArray:
    return none(JsonNode)

  if index < 0 or index >= node.len:
    return none(JsonNode)

  result = some(node[index])

proc isValidJson*(raw: string): bool =
  ## Check if a string is valid JSON.
  safeParseJson(raw).isSome

proc safeLen*(node: JsonNode): int =
  ## Safely get the length of a JSON array or object.
  if node.isNil:
    return 0
  case node.kind
  of JArray, JObject:
    return node.len
  else:
    return 0

proc hasKey*(node: JsonNode, key: string): bool =
  ## Safely check if a JSON object has a key.
  if node.isNil or node.kind != JObject:
    return false
  result = node.hasKey(key)
