// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

/**
 * SafeJson - Safe JSON operations that cannot crash.
 *
 * Provides safe JSON parsing and manipulation with option/result types.
 */

/** JSON value type */
type rec jsonValue =
  | Null
  | Bool(bool)
  | Number(float)
  | String(string)
  | Array(array<jsonValue>)
  | Object(Js.Dict.t<jsonValue>)

/** Parse a JSON string safely */
let parse = (jsonString: string): option<jsonValue> => {
  try {
    let parsed = Js.Json.parseExn(jsonString)
    Some(fromJsJson(parsed))
  } catch {
  | _ => None
  }
}
and fromJsJson = (json: Js.Json.t): jsonValue => {
  switch Js.Json.classify(json) {
  | Js.Json.JSONNull => Null
  | Js.Json.JSONFalse => Bool(false)
  | Js.Json.JSONTrue => Bool(true)
  | Js.Json.JSONNumber(n) => Number(n)
  | Js.Json.JSONString(s) => String(s)
  | Js.Json.JSONArray(arr) => Array(Belt.Array.map(arr, fromJsJson))
  | Js.Json.JSONObject(dict) =>
    let converted = Js.Dict.map((. v) => fromJsJson(v), dict)
    Object(converted)
  }
}

/** Convert jsonValue to Js.Json.t */
let rec toJsJson = (value: jsonValue): Js.Json.t => {
  switch value {
  | Null => Js.Json.null
  | Bool(b) => Js.Json.boolean(b)
  | Number(n) => Js.Json.number(n)
  | String(s) => Js.Json.string(s)
  | Array(arr) => Js.Json.array(Belt.Array.map(arr, toJsJson))
  | Object(dict) =>
    let converted = Js.Dict.map((. v) => toJsJson(v), dict)
    Js.Json.object_(converted)
  }
}

/** Stringify a JSON value */
let stringify = (value: jsonValue): string => {
  Js.Json.stringify(toJsJson(value))
}

/** Stringify with pretty printing */
let stringifyPretty = (value: jsonValue, indent: int): string => {
  Js.Json.stringifyWithSpace(toJsJson(value), indent)
}

/** Get a string from JSON value */
let asString = (value: jsonValue): option<string> => {
  switch value {
  | String(s) => Some(s)
  | _ => None
  }
}

/** Get a number from JSON value */
let asNumber = (value: jsonValue): option<float> => {
  switch value {
  | Number(n) => Some(n)
  | _ => None
  }
}

/** Get a bool from JSON value */
let asBool = (value: jsonValue): option<bool> => {
  switch value {
  | Bool(b) => Some(b)
  | _ => None
  }
}

/** Get an array from JSON value */
let asArray = (value: jsonValue): option<array<jsonValue>> => {
  switch value {
  | Array(arr) => Some(arr)
  | _ => None
  }
}

/** Get an object from JSON value */
let asObject = (value: jsonValue): option<Js.Dict.t<jsonValue>> => {
  switch value {
  | Object(obj) => Some(obj)
  | _ => None
  }
}

/** Check if JSON value is null */
let isNull = (value: jsonValue): bool => {
  switch value {
  | Null => true
  | _ => false
  }
}

/** Get a nested value by path (dot notation) */
let getPath = (value: jsonValue, path: string): option<jsonValue> => {
  let parts = Js.String2.split(path, ".")
  Belt.Array.reduce(parts, Some(value), (acc, key) => {
    switch acc {
    | None => None
    | Some(current) =>
      switch current {
      | Object(obj) => Js.Dict.get(obj, key)
      | Array(arr) =>
        switch Belt.Int.fromString(key) {
        | Some(idx) => Belt.Array.get(arr, idx)
        | None => None
        }
      | _ => None
      }
    }
  })
}

/** Set a value at a path in an object */
let setPath = (value: jsonValue, path: string, newValue: jsonValue): option<jsonValue> => {
  let parts = Js.String2.split(path, ".")
  let rec setHelper = (current: jsonValue, pathParts: array<string>, idx: int): option<jsonValue> => {
    if idx >= Belt.Array.length(pathParts) {
      Some(newValue)
    } else {
      let key = Belt.Array.getUnsafe(pathParts, idx)
      switch current {
      | Object(obj) =>
        let existing = Js.Dict.get(obj, key)->Belt.Option.getWithDefault(Null)
        switch setHelper(existing, pathParts, idx + 1) {
        | None => None
        | Some(updated) =>
          let newObj = Js.Dict.fromArray(Js.Dict.entries(obj))
          Js.Dict.set(newObj, key, updated)
          Some(Object(newObj))
        }
      | _ => None
      }
    }
  }
  setHelper(value, parts, 0)
}

/** Merge two JSON objects (shallow) */
let merge = (a: jsonValue, b: jsonValue): option<jsonValue> => {
  switch (a, b) {
  | (Object(objA), Object(objB)) =>
    let merged = Js.Dict.fromArray(Js.Dict.entries(objA))
    Js.Dict.entries(objB)->Belt.Array.forEach(((k, v)) => Js.Dict.set(merged, k, v))
    Some(Object(merged))
  | _ => None
  }
}

/** Deep merge two JSON objects */
let rec deepMerge = (a: jsonValue, b: jsonValue): jsonValue => {
  switch (a, b) {
  | (Object(objA), Object(objB)) =>
    let merged = Js.Dict.fromArray(Js.Dict.entries(objA))
    Js.Dict.entries(objB)->Belt.Array.forEach(((k, v)) => {
      let existing = Js.Dict.get(merged, k)
      let newVal = switch existing {
      | Some(existingVal) => deepMerge(existingVal, v)
      | None => v
      }
      Js.Dict.set(merged, k, newVal)
    })
    Object(merged)
  | (_, b) => b
  }
}
