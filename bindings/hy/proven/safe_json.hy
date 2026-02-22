; SPDX-License-Identifier: PMPL-1.0-or-later
; Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

;; SafeJson - JSON validation and type detection.
;;
;; All functions delegate to libproven via FFI.
;; Returns None on error. NEVER reimplements logic.

(import proven.ffi [get-lib ok? encode-str])


;; JSON type constants (from ProvenJsonType enum)
(setv JSON-NULL    0)
(setv JSON-BOOL    1)
(setv JSON-NUMBER  2)
(setv JSON-STRING  3)
(setv JSON-ARRAY   4)
(setv JSON-OBJECT  5)
(setv JSON-INVALID -1)


(defn is-valid-json [s]
  "Check if a string is valid JSON.
  Returns True if valid, False if invalid, None on FFI error.

  Example:
    (is-valid-json \"{\\\"key\\\": 1}\")  ; => True
    (is-valid-json \"not json\")          ; => False
  "
  (setv #(b n) (encode-str s))
  (setv result (.proven_json_is_valid (get-lib) b n))
  (when (ok? result.status)
    (return result.value))
  None)


(defn json-type [s]
  "Get the JSON value type at root level.
  Returns one of: JSON-NULL, JSON-BOOL, JSON-NUMBER, JSON-STRING,
  JSON-ARRAY, JSON-OBJECT, or JSON-INVALID.

  Example:
    (json-type \"42\")        ; => JSON-NUMBER (2)
    (json-type \"\\\"hello\\\"\")  ; => JSON-STRING (3)
    (json-type \"not json\")  ; => JSON-INVALID (-1)
  "
  (setv #(b n) (encode-str s))
  (.proven_json_get_type (get-lib) b n))


(defn json-type-name [type-code]
  "Convert a JSON type code to a human-readable name.

  Example:
    (json-type-name JSON-OBJECT)  ; => \"object\"
  "
  (setv names {JSON-NULL "null"
               JSON-BOOL "boolean"
               JSON-NUMBER "number"
               JSON-STRING "string"
               JSON-ARRAY "array"
               JSON-OBJECT "object"
               JSON-INVALID "invalid"})
  (.get names type-code "unknown"))
