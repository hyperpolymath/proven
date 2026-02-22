// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
//
// SafeCrypto FFI implementation - delegates to libproven via ffi-napi.
// All computation performed in Idris 2 via the Zig FFI layer.

"use strict";

const ref = require("ref-napi");
const { lib } = require("./FFI.js");

exports.constantTimeCompareImpl = function (a) {
  return function (b) {
    var bufA = Buffer.from(a, "utf-8");
    var bufB = Buffer.from(b, "utf-8");
    var r = lib.proven_crypto_constant_time_eq(
      bufA,
      bufA.length,
      bufB,
      bufB.length
    );
    return r.status === 0 && r.value !== 0;
  };
};

exports.randomBytesImpl = function (len) {
  var buf = Buffer.alloc(len);
  var status = lib.proven_crypto_random_bytes(buf, len);
  if (status === 0) {
    var result = [];
    for (var i = 0; i < len; i++) {
      result.push(buf[i]);
    }
    return result;
  }
  return [];
};
