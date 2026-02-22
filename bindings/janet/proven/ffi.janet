# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
#
# proven/ffi.janet - FFI declarations for libproven via Janet's ffi/ module.
# All computation is performed in Idris 2 via the Zig FFI layer.
# This module loads the shared library and declares C function signatures.
# Do NOT reimplement any logic.

## ============================================================================
## Load the native library
## ============================================================================

(def- libproven (ffi/native "libproven"))

## ============================================================================
## Status code constants
## ============================================================================

(def proven-ok 0)
(def proven-err-null-pointer -1)
(def proven-err-invalid-argument -2)
(def proven-err-overflow -3)
(def proven-err-underflow -4)
(def proven-err-division-by-zero -5)
(def proven-err-parse-failure -6)
(def proven-err-validation-failed -7)
(def proven-err-out-of-bounds -8)
(def proven-err-encoding-error -9)
(def proven-err-allocation-failed -10)
(def proven-err-not-implemented -99)

## ============================================================================
## C struct type definitions for Janet FFI
## ============================================================================

# IntResult: { int32_t status; int64_t value; }
(def int-result-type (ffi/struct :s32 :s64))

# BoolResult: { int32_t status; bool value; }
(def bool-result-type (ffi/struct :s32 :bool))

# StringResult: { int32_t status; char* value; size_t length; }
(def string-result-type (ffi/struct :s32 :ptr :size))

# FloatResult: { int32_t status; double value; }
(def float-result-type (ffi/struct :s32 :double))

## ============================================================================
## Runtime lifecycle bindings
## ============================================================================

(ffi/defbind proven-init :s32 [] :native libproven
  "Initialize the Proven runtime. Returns 0 on success.")

(ffi/defbind proven-deinit :void [] :native libproven
  "Shut down the Proven runtime.")

(ffi/defbind proven-is-initialized :bool [] :native libproven
  "Check if the runtime is initialized.")

(ffi/defbind proven-ffi-abi-version :u32 [] :native libproven
  "Get FFI ABI version.")

(ffi/defbind proven-version-major :u32 [] :native libproven
  "Get major version number.")

(ffi/defbind proven-version-minor :u32 [] :native libproven
  "Get minor version number.")

(ffi/defbind proven-version-patch :u32 [] :native libproven
  "Get patch version number.")

(ffi/defbind proven-module-count :u32 [] :native libproven
  "Get total module count.")

## ============================================================================
## Memory management
## ============================================================================

(ffi/defbind proven-free-string :void [:ptr] :native libproven
  "Free a string allocated by libproven.")

## ============================================================================
## SafeMath bindings
## ============================================================================

(ffi/defbind proven-math-div int-result-type [:s64 :s64] :native libproven
  "Safe integer division.")

(ffi/defbind proven-math-mod int-result-type [:s64 :s64] :native libproven
  "Safe modulo operation.")

(ffi/defbind proven-math-add-checked int-result-type [:s64 :s64] :native libproven
  "Checked addition with overflow detection.")

(ffi/defbind proven-math-sub-checked int-result-type [:s64 :s64] :native libproven
  "Checked subtraction with underflow detection.")

(ffi/defbind proven-math-mul-checked int-result-type [:s64 :s64] :native libproven
  "Checked multiplication with overflow detection.")

(ffi/defbind proven-math-abs-safe int-result-type [:s64] :native libproven
  "Safe absolute value.")

(ffi/defbind proven-math-clamp :s64 [:s64 :s64 :s64] :native libproven
  "Clamp value to [lo, hi] range.")

(ffi/defbind proven-math-pow-checked int-result-type [:s64 :u32] :native libproven
  "Integer exponentiation with overflow checking.")

## ============================================================================
## SafeString bindings
## ============================================================================

(ffi/defbind proven-string-is-valid-utf8 bool-result-type [:ptr :size] :native libproven
  "Check if bytes are valid UTF-8.")

(ffi/defbind proven-string-escape-sql string-result-type [:ptr :size] :native libproven
  "Escape string for SQL.")

(ffi/defbind proven-string-escape-html string-result-type [:ptr :size] :native libproven
  "Escape string for HTML.")

(ffi/defbind proven-string-escape-js string-result-type [:ptr :size] :native libproven
  "Escape string for JavaScript.")

## ============================================================================
## SafePath bindings
## ============================================================================

(ffi/defbind proven-path-has-traversal bool-result-type [:ptr :size] :native libproven
  "Check if path contains traversal sequences.")

(ffi/defbind proven-path-sanitize-filename string-result-type [:ptr :size] :native libproven
  "Sanitize a filename.")

## ============================================================================
## SafeEmail bindings
## ============================================================================

(ffi/defbind proven-email-is-valid bool-result-type [:ptr :size] :native libproven
  "Validate email address.")

## ============================================================================
## SafeCrypto bindings
## ============================================================================

(ffi/defbind proven-crypto-constant-time-eq bool-result-type [:ptr :size :ptr :size]
  :native libproven
  "Constant-time byte comparison.")

(ffi/defbind proven-crypto-random-bytes :s32 [:ptr :size] :native libproven
  "Fill buffer with cryptographically secure random bytes.")

## ============================================================================
## SafeUrl bindings
## ============================================================================

(ffi/defbind proven-url-parse (ffi/struct :s32
  (ffi/struct :ptr :size :ptr :size :u16 :bool :ptr :size :ptr :size :ptr :size))
  [:ptr :size] :native libproven
  "Parse a URL into components.")

(ffi/defbind proven-url-free :void [:ptr] :native libproven
  "Free URL components.")

## ============================================================================
## SafeJson bindings
## ============================================================================

(ffi/defbind proven-json-is-valid bool-result-type [:ptr :size] :native libproven
  "Check if string is valid JSON.")

(ffi/defbind proven-json-get-type :s32 [:ptr :size] :native libproven
  "Get JSON value type at root level.")

## ============================================================================
## SafeFloat bindings
## ============================================================================

(ffi/defbind proven-float-div float-result-type [:double :double] :native libproven
  "Safe floating-point division.")

(ffi/defbind proven-float-is-finite :bool [:double] :native libproven
  "Check if float is finite.")

(ffi/defbind proven-float-is-nan :bool [:double] :native libproven
  "Check if float is NaN.")

(ffi/defbind proven-float-sqrt float-result-type [:double] :native libproven
  "Safe square root.")

(ffi/defbind proven-float-ln float-result-type [:double] :native libproven
  "Safe natural logarithm.")

## ============================================================================
## SafeHex bindings
## ============================================================================

(ffi/defbind proven-hex-encode string-result-type [:ptr :size :bool] :native libproven
  "Encode bytes to hex string.")

(def hex-decode-result-type (ffi/struct :s32 :ptr :size))

(ffi/defbind proven-hex-decode hex-decode-result-type [:ptr :size] :native libproven
  "Decode hex string to bytes.")

(ffi/defbind proven-hex-free :void [:ptr] :native libproven
  "Free hex decode result.")

## ============================================================================
## SafeChecksum bindings
## ============================================================================

(ffi/defbind proven-checksum-crc32 int-result-type [:ptr :size] :native libproven
  "Calculate CRC32 checksum.")

(ffi/defbind proven-checksum-verify-crc32 bool-result-type [:ptr :size :u32]
  :native libproven
  "Verify CRC32 matches expected value.")

## ============================================================================
## SafeHeader bindings
## ============================================================================

(ffi/defbind proven-header-has-crlf bool-result-type [:ptr :size] :native libproven
  "Check for CRLF injection characters.")

(ffi/defbind proven-header-is-valid-name bool-result-type [:ptr :size] :native libproven
  "Check if header name is valid.")

(ffi/defbind proven-header-is-dangerous bool-result-type [:ptr :size] :native libproven
  "Check if header name is dangerous.")

(ffi/defbind proven-header-render string-result-type [:ptr :size :ptr :size]
  :native libproven
  "Create validated header string.")

(ffi/defbind proven-header-build-csp string-result-type [:ptr :size] :native libproven
  "Build Content-Security-Policy header.")

(ffi/defbind proven-header-build-hsts string-result-type [:s64 :bool :bool]
  :native libproven
  "Build HSTS header value.")

## ============================================================================
## SafeDateTime bindings
## ============================================================================

(def datetime-type (ffi/struct :s32 :u8 :u8 :u8 :u8 :u8 :u32 :s16))
(def datetime-result-type (ffi/struct :s32 datetime-type))

(ffi/defbind proven-datetime-parse datetime-result-type [:ptr :size] :native libproven
  "Parse ISO 8601 date string.")

(ffi/defbind proven-datetime-format-iso8601 string-result-type [datetime-type]
  :native libproven
  "Format DateTime as ISO 8601 string.")

(ffi/defbind proven-datetime-is-leap-year :bool [:s32] :native libproven
  "Check if year is a leap year.")

(ffi/defbind proven-datetime-days-in-month :u8 [:s32 :u8] :native libproven
  "Get number of days in a month.")

## ============================================================================
## SafeVersion bindings
## ============================================================================

(def semantic-version-type (ffi/struct :u32 :u32 :u32 :size :ptr))
(def version-result-type (ffi/struct :s32 semantic-version-type))

(ffi/defbind proven-version-parse version-result-type [:ptr :size] :native libproven
  "Parse semantic version string.")

(ffi/defbind proven-version-compare :s32 [semantic-version-type semantic-version-type]
  :native libproven
  "Compare two semantic versions.")

(ffi/defbind proven-version-free :void [:ptr] :native libproven
  "Free version result resources.")

## ============================================================================
## SafePassword bindings
## ============================================================================

(def password-result-type (ffi/struct :s32 :bool :bool :bool :bool :size))

(ffi/defbind proven-password-validate password-result-type [:ptr :size] :native libproven
  "Validate password strength.")

(ffi/defbind proven-password-is-common :bool [:ptr :size] :native libproven
  "Check if password is in common list.")

## ============================================================================
## SafeColor bindings
## ============================================================================

(def rgb-color-type (ffi/struct :u8 :u8 :u8))
(def hsl-color-type (ffi/struct :double :double :double))
(def color-result-type (ffi/struct :s32 rgb-color-type))

(ffi/defbind proven-color-parse-hex color-result-type [:ptr :size] :native libproven
  "Parse hex color string.")

(ffi/defbind proven-color-rgb-to-hsl hsl-color-type [rgb-color-type] :native libproven
  "Convert RGB to HSL.")

(ffi/defbind proven-color-to-hex string-result-type [rgb-color-type] :native libproven
  "Format RGB as hex string.")

## ============================================================================
## SafeProbability bindings
## ============================================================================

(ffi/defbind proven-probability-create :double [:double] :native libproven
  "Create probability clamped to [0, 1].")

(ffi/defbind proven-probability-and :double [:double :double] :native libproven
  "Multiply probabilities.")

(ffi/defbind proven-probability-or-exclusive :double [:double :double] :native libproven
  "Add probabilities (mutually exclusive).")

(ffi/defbind proven-probability-not :double [:double] :native libproven
  "Complement probability.")

## ============================================================================
## SafeCalculator bindings
## ============================================================================

(ffi/defbind proven-calculator-eval float-result-type [:ptr :size] :native libproven
  "Evaluate arithmetic expression safely.")

## ============================================================================
## SafeAngle bindings
## ============================================================================

(ffi/defbind proven-angle-deg-to-rad :double [:double] :native libproven
  "Convert degrees to radians.")

(ffi/defbind proven-angle-rad-to-deg :double [:double] :native libproven
  "Convert radians to degrees.")

(ffi/defbind proven-angle-normalize-degrees :double [:double] :native libproven
  "Normalize angle to [0, 360) degrees.")

(ffi/defbind proven-angle-normalize-radians :double [:double] :native libproven
  "Normalize angle to [0, 2*pi) radians.")

## ============================================================================
## SafeUnit bindings
## ============================================================================

(ffi/defbind proven-unit-convert-length float-result-type [:double :s32 :s32]
  :native libproven
  "Convert length between units.")

(ffi/defbind proven-unit-convert-temp float-result-type [:double :s32 :s32]
  :native libproven
  "Convert temperature between units.")

## ============================================================================
## SafeHTTP bindings
## ============================================================================

(ffi/defbind proven-http-url-encode string-result-type [:ptr :size] :native libproven
  "URL-encode a string per RFC 3986.")

(ffi/defbind proven-http-url-decode string-result-type [:ptr :size] :native libproven
  "URL-decode a percent-encoded string.")

## ============================================================================
## Result extraction helpers
## ============================================================================

(defn extract-int-result
  "Extract value from IntResult tuple. Returns nil on error."
  [result]
  (when (and result (= (get result 0) proven-ok))
    (get result 1)))

(defn extract-bool-result
  "Extract value from BoolResult tuple. Returns nil on error."
  [result]
  (when (and result (= (get result 0) proven-ok))
    (get result 1)))

(defn extract-float-result
  "Extract value from FloatResult tuple. Returns nil on error."
  [result]
  (when (and result (= (get result 0) proven-ok))
    (get result 1)))

(defn extract-string-result
  "Extract string from StringResult tuple, free C memory. Returns nil on error."
  [result]
  (when (and result (= (get result 0) proven-ok) (not (nil? (get result 1))))
    (def ptr (get result 1))
    (def len (get result 2))
    (def s (ffi/read :string ptr len))
    (proven-free-string ptr)
    s))
