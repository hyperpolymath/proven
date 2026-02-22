// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
//
// SafeUnit FFI implementation - delegates to libproven via ffi-napi.
// All computation performed in Idris 2 via the Zig FFI layer.

"use strict";

const { lib } = require("./FFI.js");

// LengthUnit enum values matching Zig FFI
var LengthUnit = {
  meters: 0,
  feet: 1,
  inches: 2,
  centimeters: 3,
  millimeters: 4,
  kilometers: 5,
  miles: 6,
  yards: 7,
};

// TempUnit enum values matching Zig FFI
var TempUnit = {
  celsius: 0,
  fahrenheit: 1,
  kelvin: 2,
};

exports.convertLengthImpl = function (value) {
  return function (from) {
    return function (to) {
      var r = lib.proven_unit_convert_length(value, from, to);
      return { status: r.status, value: r.value };
    };
  };
};

exports.convertTempImpl = function (value) {
  return function (from) {
    return function (to) {
      var r = lib.proven_unit_convert_temp(value, from, to);
      return { status: r.status, value: r.value };
    };
  };
};

exports.lengthUnits = LengthUnit;
exports.tempUnits = TempUnit;
