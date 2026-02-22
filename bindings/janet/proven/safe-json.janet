# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
#
# proven/safe-json.janet - Safe JSON validation wrappers for libproven.
# All computation delegates to the Idris 2 verified core via Janet FFI.
# Do NOT reimplement any JSON logic in Janet.

(import ./ffi :prefix "ffi/")

## ============================================================================
## JSON type name lookup
## ============================================================================

(def- type-names
  {0  :null
   1  :boolean
   2  :number
   3  :string
   4  :array
   5  :object
   -1 :invalid})

## ============================================================================
## Safe JSON operations
## ============================================================================

(defn valid?
  "Check if string is valid JSON. Returns nil on error."
  [json-string]
  (ffi/extract-bool-result (ffi/proven-json-is-valid json-string (length json-string))))

(defn get-type
  "Get JSON value type at root level.
  Returns a keyword (:null, :boolean, :number, :string, :array, :object, :invalid)."
  [json-string]
  (let [t (ffi/proven-json-get-type json-string (length json-string))]
    (or (get type-names t) :invalid)))
