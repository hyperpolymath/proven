// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
//
// SafeDateTime FFI implementation - delegates to libproven via ffi-napi.
// All computation performed in Idris 2 via the Zig FFI layer.

"use strict";

const { lib, callStringFFI } = require("./FFI.js");

exports.isLeapYearImpl = function (year) {
  return lib.proven_datetime_is_leap_year(year);
};

exports.daysInMonthImpl = function (year) {
  return function (month) {
    return lib.proven_datetime_days_in_month(year, month);
  };
};

exports.datetimeParseImpl = function (s) {
  var buf = Buffer.from(s, "utf-8");
  var r = lib.proven_datetime_parse(buf, buf.length);
  return { status: r.status, datetime: r };
};

exports.datetimeFormatIso8601Impl = function (dt) {
  var r = lib.proven_datetime_format_iso8601(dt);
  if (r.status === 0 && !r.value.isNull()) {
    var str = r.value.readCString(0);
    lib.proven_free_string(r.value);
    return str;
  }
  return "";
};
