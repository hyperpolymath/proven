// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
//
// SafeFloat FFI implementation - delegates to libproven via ffi-napi.
// All computation performed in Idris 2 via the Zig FFI layer.

"use strict";

const { lib } = require("./FFI.js");

exports.safeDivImpl = function (a) {
  return function (b) {
    var r = lib.proven_float_div(a, b);
    return { status: r.status, value: r.value };
  };
};

exports.safeSqrtImpl = function (x) {
  var r = lib.proven_float_sqrt(x);
  return { status: r.status, value: r.value };
};

exports.safeLnImpl = function (x) {
  var r = lib.proven_float_ln(x);
  return { status: r.status, value: r.value };
};

exports.isFiniteImpl = function (x) {
  return lib.proven_float_is_finite(x);
};

exports.isNaNImpl = function (x) {
  return lib.proven_float_is_nan(x);
};
