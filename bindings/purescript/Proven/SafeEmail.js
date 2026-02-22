// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
//
// SafeEmail FFI implementation - delegates to libproven via ffi-napi.
// All computation performed in Idris 2 via the Zig FFI layer.

"use strict";

const { lib, callBoolFFI } = require("./FFI.js");

exports.isValidEmailImpl = function (s) {
  var buf = Buffer.from(s, "utf-8");
  return callBoolFFI(
    lib.proven_email_is_valid.bind(lib),
    buf,
    buf.length
  );
};
