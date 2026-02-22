// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
//
// SafeAngle FFI implementation - delegates to libproven via ffi-napi.
// All computation performed in Idris 2 via the Zig FFI layer.

"use strict";

const { lib } = require("./FFI.js");

exports.degToRadImpl = function (degrees) {
  return lib.proven_angle_deg_to_rad(degrees);
};

exports.radToDegImpl = function (radians) {
  return lib.proven_angle_rad_to_deg(radians);
};

exports.normalizeDegreesImpl = function (degrees) {
  return lib.proven_angle_normalize_degrees(degrees);
};

exports.normalizeRadiansImpl = function (radians) {
  return lib.proven_angle_normalize_radians(radians);
};
