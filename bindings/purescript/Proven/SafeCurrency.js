// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
//
// SafeCurrency FFI implementation - delegates to libproven via ffi-napi.
// All computation performed in Idris 2 via the Zig FFI layer.

"use strict";

const { lib } = require("./FFI.js");

exports.currencyParseImpl = function (s) {
  var buf = Buffer.from(s, "utf-8");
  var r = lib.proven_currency_parse(buf, buf.length);
  if (r.status === 0) {
    var code = String.fromCharCode(
      r.currency_code[0],
      r.currency_code[1],
      r.currency_code[2]
    );
    return {
      status: 0,
      amountMinor: r.amount_minor,
      currencyCode: code,
      decimalPlaces: r.decimal_places,
    };
  }
  return { status: r.status, amountMinor: 0, currencyCode: "", decimalPlaces: 0 };
};

exports.currencyFormatImpl = function (amountMinor) {
  return function (code) {
    return function (decimalPlaces) {
      var codeBytes = [
        code.charCodeAt(0),
        code.charCodeAt(1),
        code.charCodeAt(2),
      ];
      var r = lib.proven_currency_format(amountMinor, codeBytes, decimalPlaces);
      if (r.status === 0 && !r.value.isNull()) {
        var str = r.value.readCString(0);
        lib.proven_free_string(r.value);
        return str;
      }
      return "";
    };
  };
};
