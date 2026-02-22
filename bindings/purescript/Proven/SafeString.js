// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
//
// SafeString FFI implementation - delegates to libproven via ffi-napi.
// All computation performed in Idris 2 via the Zig FFI layer.

"use strict";

const { lib, callStringFFI, callBoolFFI } = require("./FFI.js");

exports.escapeHtmlImpl = function (s) {
  var buf = Buffer.from(s, "utf-8");
  return callStringFFI(
    lib.proven_string_escape_html.bind(lib),
    buf,
    buf.length
  );
};

exports.escapeSqlImpl = function (s) {
  var buf = Buffer.from(s, "utf-8");
  return callStringFFI(
    lib.proven_string_escape_sql.bind(lib),
    buf,
    buf.length
  );
};

exports.escapeJsImpl = function (s) {
  var buf = Buffer.from(s, "utf-8");
  return callStringFFI(
    lib.proven_string_escape_js.bind(lib),
    buf,
    buf.length
  );
};

exports.isValidUtf8Impl = function (s) {
  var buf = Buffer.from(s, "utf-8");
  return callBoolFFI(
    lib.proven_string_is_valid_utf8.bind(lib),
    buf,
    buf.length
  );
};
