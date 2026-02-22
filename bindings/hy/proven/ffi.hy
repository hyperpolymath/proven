; SPDX-License-Identifier: PMPL-1.0-or-later
; Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

;; FFI declarations for libproven via ctypes.
;;
;; Loads the libproven shared library and declares all C ABI function
;; signatures used by the safe_* modules. All computation is delegated
;; to the formally verified Idris 2 + Zig implementation.
;;
;; This module MUST NOT reimplement any logic. It is a pure FFI bridge.

(import ctypes)
(import ctypes.util)
(import os)
(import pathlib [Path])


;; ---------------------------------------------------------------------------
;; Status codes
;; ---------------------------------------------------------------------------

(setv PROVEN-OK                    0)
(setv PROVEN-ERR-NULL-POINTER     -1)
(setv PROVEN-ERR-INVALID-ARGUMENT -2)
(setv PROVEN-ERR-OVERFLOW         -3)
(setv PROVEN-ERR-UNDERFLOW        -4)
(setv PROVEN-ERR-DIVISION-BY-ZERO -5)
(setv PROVEN-ERR-PARSE-FAILURE    -6)
(setv PROVEN-ERR-VALIDATION-FAILED -7)
(setv PROVEN-ERR-OUT-OF-BOUNDS    -8)
(setv PROVEN-ERR-ENCODING-ERROR   -9)
(setv PROVEN-ERR-ALLOCATION-FAILED -10)
(setv PROVEN-ERR-NOT-IMPLEMENTED  -99)


;; ---------------------------------------------------------------------------
;; Result structures
;; ---------------------------------------------------------------------------

(defclass IntResult [ctypes.Structure]
  "Result structure for integer operations."
  (setv _fields_ [#("status" ctypes.c_int32)
                   #("value" ctypes.c_int64)]))

(defclass BoolResult [ctypes.Structure]
  "Result structure for boolean operations."
  (setv _fields_ [#("status" ctypes.c_int32)
                   #("value" ctypes.c_bool)]))

(defclass StringResult [ctypes.Structure]
  "Result structure for string operations. Caller must free value via proven_free_string."
  (setv _fields_ [#("status" ctypes.c_int32)
                   #("value" ctypes.c_char_p)
                   #("length" ctypes.c_size_t)]))

(defclass FloatResult [ctypes.Structure]
  "Result structure for floating-point operations."
  (setv _fields_ [#("status" ctypes.c_int32)
                   #("value" ctypes.c_double)]))

(defclass UrlComponents [ctypes.Structure]
  "Parsed URL components."
  (setv _fields_ [#("scheme" ctypes.c_char_p)
                   #("scheme_len" ctypes.c_size_t)
                   #("host" ctypes.c_char_p)
                   #("host_len" ctypes.c_size_t)
                   #("port" ctypes.c_uint16)
                   #("has_port" ctypes.c_bool)
                   #("path" ctypes.c_char_p)
                   #("path_len" ctypes.c_size_t)
                   #("query" ctypes.c_char_p)
                   #("query_len" ctypes.c_size_t)
                   #("fragment" ctypes.c_char_p)
                   #("fragment_len" ctypes.c_size_t)]))

(defclass UrlResult [ctypes.Structure]
  "Result structure for URL parsing."
  (setv _fields_ [#("status" ctypes.c_int32)
                   #("components" UrlComponents)]))

(defclass IPv4Address [ctypes.Structure]
  "IPv4 address as 4 octets."
  (setv _fields_ [#("octets" (* ctypes.c_uint8 4))]))

(defclass IPv4Result [ctypes.Structure]
  "Result structure for IPv4 parsing."
  (setv _fields_ [#("status" ctypes.c_int32)
                   #("address" IPv4Address)]))

(defclass DateTimeStruct [ctypes.Structure]
  "DateTime components for ISO 8601."
  (setv _fields_ [#("year" ctypes.c_int32)
                   #("month" ctypes.c_uint8)
                   #("day" ctypes.c_uint8)
                   #("hour" ctypes.c_uint8)
                   #("minute" ctypes.c_uint8)
                   #("second" ctypes.c_uint8)
                   #("nanosecond" ctypes.c_uint32)
                   #("tz_offset_minutes" ctypes.c_int16)]))

(defclass DateTimeResult [ctypes.Structure]
  "Result structure for datetime parsing."
  (setv _fields_ [#("status" ctypes.c_int32)
                   #("datetime" DateTimeStruct)]))

(defclass HexDecodeResult [ctypes.Structure]
  "Result structure for hex decode with byte data."
  (setv _fields_ [#("status" ctypes.c_int32)
                   #("data" ctypes.POINTER ctypes.c_uint8)
                   #("length" ctypes.c_size_t)]))

(defclass RGBColor [ctypes.Structure]
  "RGB color (8-bit per channel)."
  (setv _fields_ [#("r" ctypes.c_uint8)
                   #("g" ctypes.c_uint8)
                   #("b" ctypes.c_uint8)]))

(defclass ColorResult [ctypes.Structure]
  "Result structure for color parsing."
  (setv _fields_ [#("status" ctypes.c_int32)
                   #("color" RGBColor)]))


;; ---------------------------------------------------------------------------
;; Library loading
;; ---------------------------------------------------------------------------

(defn find-library []
  "Locate the libproven shared library on disk."
  (setv search-paths
    [(/ (Path (os.path.dirname (os.path.abspath __file__)))
        ".." ".." ".." "ffi" "zig" "zig-out" "lib")
     (Path "/usr/local/lib")
     (Path "/usr/lib")
     (/ (Path.home) ".local" "lib")])
  (setv lib-names ["libproven.so" "libproven.dylib" "proven.dll"])
  (for [sp search-paths]
    (for [ln lib-names]
      (setv candidate (/ sp ln))
      (when (.exists candidate)
        (return (str candidate)))))
  ;; Fall back to system library finder
  (ctypes.util.find_library "proven"))


(defn setup-signatures [lib]
  "Declare ctypes function signatures for type safety on the loaded library."

  ;; -- Memory management --
  (setv lib.proven_free_string.argtypes [ctypes.c_char_p])
  (setv lib.proven_free_string.restype None)

  ;; -- Lifecycle --
  (setv lib.proven_init.argtypes [])
  (setv lib.proven_init.restype ctypes.c_int32)
  (setv lib.proven_deinit.argtypes [])
  (setv lib.proven_deinit.restype None)
  (setv lib.proven_is_initialized.argtypes [])
  (setv lib.proven_is_initialized.restype ctypes.c_bool)

  ;; -- Version --
  (setv lib.proven_ffi_abi_version.argtypes [])
  (setv lib.proven_ffi_abi_version.restype ctypes.c_uint32)
  (setv lib.proven_version_major.argtypes [])
  (setv lib.proven_version_major.restype ctypes.c_uint32)
  (setv lib.proven_version_minor.argtypes [])
  (setv lib.proven_version_minor.restype ctypes.c_uint32)
  (setv lib.proven_version_patch.argtypes [])
  (setv lib.proven_version_patch.restype ctypes.c_uint32)
  (setv lib.proven_module_count.argtypes [])
  (setv lib.proven_module_count.restype ctypes.c_uint32)

  ;; -- SafeMath --
  (setv lib.proven_math_add_checked.argtypes [ctypes.c_int64 ctypes.c_int64])
  (setv lib.proven_math_add_checked.restype IntResult)
  (setv lib.proven_math_sub_checked.argtypes [ctypes.c_int64 ctypes.c_int64])
  (setv lib.proven_math_sub_checked.restype IntResult)
  (setv lib.proven_math_mul_checked.argtypes [ctypes.c_int64 ctypes.c_int64])
  (setv lib.proven_math_mul_checked.restype IntResult)
  (setv lib.proven_math_div.argtypes [ctypes.c_int64 ctypes.c_int64])
  (setv lib.proven_math_div.restype IntResult)
  (setv lib.proven_math_mod.argtypes [ctypes.c_int64 ctypes.c_int64])
  (setv lib.proven_math_mod.restype IntResult)
  (setv lib.proven_math_abs_safe.argtypes [ctypes.c_int64])
  (setv lib.proven_math_abs_safe.restype IntResult)
  (setv lib.proven_math_clamp.argtypes [ctypes.c_int64 ctypes.c_int64 ctypes.c_int64])
  (setv lib.proven_math_clamp.restype ctypes.c_int64)
  (setv lib.proven_math_pow_checked.argtypes [ctypes.c_int64 ctypes.c_uint32])
  (setv lib.proven_math_pow_checked.restype IntResult)

  ;; -- SafeString --
  (setv lib.proven_string_is_valid_utf8.argtypes [ctypes.c_char_p ctypes.c_size_t])
  (setv lib.proven_string_is_valid_utf8.restype BoolResult)
  (setv lib.proven_string_escape_sql.argtypes [ctypes.c_char_p ctypes.c_size_t])
  (setv lib.proven_string_escape_sql.restype StringResult)
  (setv lib.proven_string_escape_html.argtypes [ctypes.c_char_p ctypes.c_size_t])
  (setv lib.proven_string_escape_html.restype StringResult)
  (setv lib.proven_string_escape_js.argtypes [ctypes.c_char_p ctypes.c_size_t])
  (setv lib.proven_string_escape_js.restype StringResult)

  ;; -- SafePath --
  (setv lib.proven_path_has_traversal.argtypes [ctypes.c_char_p ctypes.c_size_t])
  (setv lib.proven_path_has_traversal.restype BoolResult)
  (setv lib.proven_path_sanitize_filename.argtypes [ctypes.c_char_p ctypes.c_size_t])
  (setv lib.proven_path_sanitize_filename.restype StringResult)

  ;; -- SafeEmail --
  (setv lib.proven_email_is_valid.argtypes [ctypes.c_char_p ctypes.c_size_t])
  (setv lib.proven_email_is_valid.restype BoolResult)

  ;; -- SafeUrl --
  (setv lib.proven_url_parse.argtypes [ctypes.c_char_p ctypes.c_size_t])
  (setv lib.proven_url_parse.restype UrlResult)
  (setv lib.proven_url_free.argtypes [(ctypes.POINTER UrlComponents)])
  (setv lib.proven_url_free.restype None)

  ;; -- SafeCrypto --
  (setv lib.proven_crypto_constant_time_eq.argtypes
    [ctypes.c_char_p ctypes.c_size_t ctypes.c_char_p ctypes.c_size_t])
  (setv lib.proven_crypto_constant_time_eq.restype BoolResult)
  (setv lib.proven_crypto_random_bytes.argtypes [ctypes.c_char_p ctypes.c_size_t])
  (setv lib.proven_crypto_random_bytes.restype ctypes.c_int32)

  ;; -- SafeJson --
  (setv lib.proven_json_is_valid.argtypes [ctypes.c_char_p ctypes.c_size_t])
  (setv lib.proven_json_is_valid.restype BoolResult)
  (setv lib.proven_json_get_type.argtypes [ctypes.c_char_p ctypes.c_size_t])
  (setv lib.proven_json_get_type.restype ctypes.c_int32)

  ;; -- SafeFloat --
  (setv lib.proven_float_div.argtypes [ctypes.c_double ctypes.c_double])
  (setv lib.proven_float_div.restype FloatResult)
  (setv lib.proven_float_is_finite.argtypes [ctypes.c_double])
  (setv lib.proven_float_is_finite.restype ctypes.c_bool)
  (setv lib.proven_float_is_nan.argtypes [ctypes.c_double])
  (setv lib.proven_float_is_nan.restype ctypes.c_bool)
  (setv lib.proven_float_sqrt.argtypes [ctypes.c_double])
  (setv lib.proven_float_sqrt.restype FloatResult)
  (setv lib.proven_float_ln.argtypes [ctypes.c_double])
  (setv lib.proven_float_ln.restype FloatResult)

  ;; -- SafeDateTime --
  (setv lib.proven_datetime_parse.argtypes [ctypes.c_char_p ctypes.c_size_t])
  (setv lib.proven_datetime_parse.restype DateTimeResult)
  (setv lib.proven_datetime_format_iso8601.argtypes [DateTimeStruct])
  (setv lib.proven_datetime_format_iso8601.restype StringResult)
  (setv lib.proven_datetime_is_leap_year.argtypes [ctypes.c_int32])
  (setv lib.proven_datetime_is_leap_year.restype ctypes.c_bool)
  (setv lib.proven_datetime_days_in_month.argtypes [ctypes.c_int32 ctypes.c_uint8])
  (setv lib.proven_datetime_days_in_month.restype ctypes.c_uint8)

  ;; -- SafeNetwork --
  (setv lib.proven_network_parse_ipv4.argtypes [ctypes.c_char_p ctypes.c_size_t])
  (setv lib.proven_network_parse_ipv4.restype IPv4Result)
  (setv lib.proven_network_ipv4_is_private.argtypes [IPv4Address])
  (setv lib.proven_network_ipv4_is_private.restype ctypes.c_bool)
  (setv lib.proven_network_ipv4_is_loopback.argtypes [IPv4Address])
  (setv lib.proven_network_ipv4_is_loopback.restype ctypes.c_bool)

  ;; -- SafeHex --
  (setv lib.proven_hex_encode.argtypes [ctypes.c_char_p ctypes.c_size_t ctypes.c_bool])
  (setv lib.proven_hex_encode.restype StringResult)
  (setv lib.proven_hex_decode.argtypes [ctypes.c_char_p ctypes.c_size_t])
  (setv lib.proven_hex_decode.restype HexDecodeResult)
  (setv lib.proven_hex_free.argtypes [(ctypes.POINTER HexDecodeResult)])
  (setv lib.proven_hex_free.restype None)

  ;; -- SafeChecksum --
  (setv lib.proven_checksum_crc32.argtypes [ctypes.c_char_p ctypes.c_size_t])
  (setv lib.proven_checksum_crc32.restype IntResult)
  (setv lib.proven_checksum_verify_crc32.argtypes [ctypes.c_char_p ctypes.c_size_t ctypes.c_uint32])
  (setv lib.proven_checksum_verify_crc32.restype BoolResult)

  ;; -- SafeColor --
  (setv lib.proven_color_parse_hex.argtypes [ctypes.c_char_p ctypes.c_size_t])
  (setv lib.proven_color_parse_hex.restype ColorResult)
  (setv lib.proven_color_to_hex.argtypes [RGBColor])
  (setv lib.proven_color_to_hex.restype StringResult)

  ;; -- SafeCalculator --
  (setv lib.proven_calculator_eval.argtypes [ctypes.c_char_p ctypes.c_size_t])
  (setv lib.proven_calculator_eval.restype FloatResult)

  ;; -- SafeVersion --
  (setv lib.proven_version_parse.argtypes [ctypes.c_char_p ctypes.c_size_t])
  ;; Note: returns ProvenVersionResult struct, but we can treat status field
  (setv lib.proven_version_compare.argtypes
    ;; Comparing two semantic version structs is complex; we use the simpler
    ;; string-based compare if available, otherwise direct struct passing.
    ;; For now declare the base signatures.
    [ctypes.c_char_p ctypes.c_size_t ctypes.c_char_p ctypes.c_size_t])
  (setv lib.proven_version_compare.restype ctypes.c_int32)

  lib)


(setv _lib None)

(defn get-lib []
  "Return the loaded libproven, initializing on first call."
  (global _lib)
  (when (is _lib None)
    (setv path (find-library))
    (when (is path None)
      (raise (ImportError
               "Could not find libproven shared library. Build with: cd ffi/zig && zig build")))
    (setv _lib (ctypes.CDLL path))
    (setup-signatures _lib))
  _lib)


(defn ok? [status]
  "Return True if the status code indicates success."
  (= status PROVEN-OK))


(defn encode-str [s]
  "Encode a Python string to bytes for FFI, returning (ptr, length) tuple."
  (setv b (.encode s "utf-8"))
  #(b (len b)))


(defn decode-string-result [result]
  "Extract a Python string from a StringResult, freeing the C memory.
   Returns None if the result status is not OK."
  (when (!= result.status PROVEN-OK)
    (return None))
  (when (is result.value None)
    (return None))
  (setv s (.decode result.value "utf-8" :errors "replace"))
  (try
    (.proven_free_string (get-lib) result.value)
    (except [Exception]))
  s)
