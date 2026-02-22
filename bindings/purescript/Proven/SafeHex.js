// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
//
// SafeHex FFI implementation - delegates to libproven via ffi-napi.
// All computation performed in Idris 2 via the Zig FFI layer.

"use strict";

const { lib, callStringFFI } = require("./FFI.js");

exports.hexEncodeImpl = function (buf) {
  return function (uppercase) {
    var buffer = Buffer.from(buf);
    return callStringFFI(
      function (b, l) {
        return lib.proven_hex_encode(b, l, uppercase);
      },
      buffer,
      buffer.length
    );
  };
};

exports.hexDecodeImpl = function (s) {
  var buf = Buffer.from(s, "utf-8");
  var r = lib.proven_hex_decode(buf, buf.length);
  if (r.status === 0 && r.data !== null) {
    var result = [];
    for (var i = 0; i < r.length; i++) {
      result.push(r.data[i]);
    }
    lib.proven_hex_free(r);
    return { status: 0, bytes: result };
  }
  return { status: r.status, bytes: [] };
};
