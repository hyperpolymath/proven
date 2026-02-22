// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
//
// SafeMath FFI implementation - delegates to libproven via ffi-napi.
// All computation performed in Idris 2 via the Zig FFI layer.

"use strict";

const { lib } = require("./FFI.js");

exports.safeAddImpl = function (a) {
  return function (b) {
    var r = lib.proven_math_add_checked(a, b);
    return { status: r.status, value: r.value };
  };
};

exports.safeSubImpl = function (a) {
  return function (b) {
    var r = lib.proven_math_sub_checked(a, b);
    return { status: r.status, value: r.value };
  };
};

exports.safeMulImpl = function (a) {
  return function (b) {
    var r = lib.proven_math_mul_checked(a, b);
    return { status: r.status, value: r.value };
  };
};

exports.safeDivImpl = function (a) {
  return function (b) {
    var r = lib.proven_math_div(a, b);
    return { status: r.status, value: r.value };
  };
};

exports.safeModImpl = function (a) {
  return function (b) {
    var r = lib.proven_math_mod(a, b);
    return { status: r.status, value: r.value };
  };
};

exports.clamp = function (lo) {
  return function (hi) {
    return function (value) {
      return lib.proven_math_clamp(lo, hi, value);
    };
  };
};
