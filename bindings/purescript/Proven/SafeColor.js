// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
//
// SafeColor FFI implementation - delegates to libproven via ffi-napi.
// All computation performed in Idris 2 via the Zig FFI layer.

"use strict";

const { lib } = require("./FFI.js");

exports.parseHexColorImpl = function (s) {
  var buf = Buffer.from(s, "utf-8");
  var r = lib.proven_color_parse_hex(buf, buf.length);
  return { status: r.status, r: r.r, g: r.g, b: r.b };
};

exports.rgbToHslImpl = function (rgb) {
  var r = lib.proven_color_rgb_to_hsl(rgb);
  return { h: r.h, s: r.s, l: r.l };
};

exports.toHexImpl = function (rgb) {
  var r = lib.proven_color_to_hex(rgb);
  if (r.status === 0 && !r.value.isNull()) {
    var str = r.value.readCString(0);
    lib.proven_free_string(r.value);
    return str;
  }
  return "";
};
