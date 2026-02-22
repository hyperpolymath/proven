// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
//
// SafeVersion FFI implementation - delegates to libproven via ffi-napi.
// All computation performed in Idris 2 via the Zig FFI layer.

"use strict";

const { lib } = require("./FFI.js");

exports.versionParseImpl = function (s) {
  var buf = Buffer.from(s, "utf-8");
  var r = lib.proven_version_parse(buf, buf.length);
  return { status: r.status, version: r };
};

exports.versionCompareImpl = function (a) {
  return function (b) {
    return lib.proven_version_compare(a, b);
  };
};
