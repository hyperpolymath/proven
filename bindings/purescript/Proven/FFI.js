// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
//
// FFI bridge: loads libproven via Node.js ffi-napi and exposes C functions.
// All computation delegates to Idris 2 via the Zig FFI layer.

"use strict";

const ffi = require("ffi-napi");
const ref = require("ref-napi");
const StructType = require("ref-struct-napi");

// Result types matching Zig FFI structs
const IntResult = StructType({
  status: ref.types.int32,
  value: ref.types.int64,
});

const BoolResult = StructType({
  status: ref.types.int32,
  value: ref.types.int32,
});

const FloatResult = StructType({
  status: ref.types.int32,
  value: ref.types.double,
});

const StringResult = StructType({
  status: ref.types.int32,
  value: ref.refType(ref.types.char),
  length: ref.types.size_t,
});

// Load libproven shared library
const lib = ffi.Library("libproven", {
  // SafeMath
  proven_math_add_checked: [IntResult, ["int64", "int64"]],
  proven_math_sub_checked: [IntResult, ["int64", "int64"]],
  proven_math_mul_checked: [IntResult, ["int64", "int64"]],
  proven_math_div: [IntResult, ["int64", "int64"]],
  proven_math_mod: [IntResult, ["int64", "int64"]],
  proven_math_abs_safe: [IntResult, ["int64"]],
  proven_math_clamp: ["int64", ["int64", "int64", "int64"]],

  // SafeString
  proven_string_escape_html: [StringResult, ["pointer", "size_t"]],
  proven_string_escape_sql: [StringResult, ["pointer", "size_t"]],
  proven_string_escape_js: [StringResult, ["pointer", "size_t"]],
  proven_string_is_valid_utf8: [BoolResult, ["pointer", "size_t"]],

  // SafeCrypto
  proven_crypto_constant_time_eq: [BoolResult, ["pointer", "size_t", "pointer", "size_t"]],
  proven_crypto_random_bytes: ["int32", ["pointer", "size_t"]],

  // SafeEmail
  proven_email_is_valid: [BoolResult, ["pointer", "size_t"]],

  // SafeHex
  proven_hex_encode: [StringResult, ["pointer", "size_t", "bool"]],
  proven_hex_decode: [StringResult, ["pointer", "size_t"]],

  // SafeFloat
  proven_float_div: [FloatResult, ["double", "double"]],
  proven_float_sqrt: [FloatResult, ["double"]],
  proven_float_ln: [FloatResult, ["double"]],
  proven_float_is_finite: ["bool", ["double"]],
  proven_float_is_nan: ["bool", ["double"]],

  // SafeAngle
  proven_angle_deg_to_rad: ["double", ["double"]],
  proven_angle_rad_to_deg: ["double", ["double"]],
  proven_angle_normalize_degrees: ["double", ["double"]],
  proven_angle_normalize_radians: ["double", ["double"]],

  // SafeDatetime
  proven_datetime_is_leap_year: ["bool", ["int32"]],
  proven_datetime_days_in_month: ["uint8", ["int32", "uint8"]],

  // Free
  proven_free_string: ["void", ["pointer"]],
});

// Helper: call string-returning FFI function
function callStringFFI(fn, buf, len) {
  const result = fn(buf, len);
  if (result.status === 0 && !result.value.isNull()) {
    const str = result.value.readCString(0);
    lib.proven_free_string(result.value);
    return str;
  }
  return "";
}

// Helper: call bool-returning FFI function
function callBoolFFI(fn, buf, len) {
  const result = fn(buf, len);
  return result.status === 0 && result.value !== 0;
}

// Export the library handle and helpers
exports.lib = lib;
exports.callStringFFI = callStringFFI;
exports.callBoolFFI = callBoolFFI;
exports.IntResult = IntResult;
exports.BoolResult = BoolResult;
exports.FloatResult = FloatResult;
exports.StringResult = StringResult;
