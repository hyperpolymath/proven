// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
//
// SafePath FFI implementation - delegates to libproven via ffi-napi.
// All computation performed in Idris 2 via the Zig FFI layer.

"use strict";

const { lib, callBoolFFI, callStringFFI } = require("./FFI.js");

exports.hasTraversalImpl = function (s) {
  var buf = Buffer.from(s, "utf-8");
  return callBoolFFI(
    lib.proven_path_has_traversal.bind(lib),
    buf,
    buf.length
  );
};

exports.sanitizeFilenameImpl = function (s) {
  var buf = Buffer.from(s, "utf-8");
  return callStringFFI(
    lib.proven_path_sanitize_filename.bind(lib),
    buf,
    buf.length
  );
};
