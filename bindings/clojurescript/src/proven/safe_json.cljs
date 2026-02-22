;; SPDX-License-Identifier: PMPL-1.0-or-later
;; Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

;; SafeJson - JSON validation and type detection.
;;
;; All functions delegate to libproven via JS interop (transitive FFI).
;; Returns nil on error. NEVER reimplements logic.

(ns proven.safe-json
  "Safe JSON validation and type detection.
   All computation delegates to the Idris 2 core via FFI."
  (:require [proven.ffi :as ffi]))


;; JSON type constants (from ProvenJsonType enum)
(def JSON-NULL     0)
(def JSON-BOOL     1)
(def JSON-NUMBER   2)
(def JSON-STRING   3)
(def JSON-ARRAY    4)
(def JSON-OBJECT   5)
(def JSON-INVALID -1)


(defn valid-json?
  "Check if a string is valid JSON.
  Returns true if valid, false if invalid, nil on FFI error.

  Example:
    (valid-json? \"{\\\"key\\\": 1}\")  ;=> true
    (valid-json? \"not json\")          ;=> false"
  [s]
  (when-let [lib (ffi/get-lib)]
    (let [result (.proven_json_is_valid lib s (count s))]
      (when (ffi/ok? (.-status result))
        (.-value result)))))


(defn json-type
  "Get the JSON value type at root level.
  Returns one of: JSON-NULL, JSON-BOOL, JSON-NUMBER, JSON-STRING,
  JSON-ARRAY, JSON-OBJECT, or JSON-INVALID.

  Example:
    (json-type \"42\")        ;=> JSON-NUMBER (2)
    (json-type \"not json\")  ;=> JSON-INVALID (-1)"
  [s]
  (when-let [lib (ffi/get-lib)]
    (.proven_json_get_type lib s (count s))))


(defn json-type-name
  "Convert a JSON type code to a human-readable name.

  Example:
    (json-type-name JSON-OBJECT)  ;=> \"object\""
  [type-code]
  (case type-code
    0  "null"
    1  "boolean"
    2  "number"
    3  "string"
    4  "array"
    5  "object"
    -1 "invalid"
    "unknown"))
