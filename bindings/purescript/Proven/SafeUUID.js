// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
//
// SafeUUID FFI implementation - delegates to libproven via ffi-napi.
// All computation performed in Idris 2 via the Zig FFI layer.

"use strict";

const { lib } = require("./FFI.js");

exports.uuidV4Impl = function (unit) {
  var r = lib.proven_uuid_v4();
  if (r.status === 0) {
    var sr = lib.proven_uuid_to_string(r.uuid);
    if (sr.status === 0 && !sr.value.isNull()) {
      var str = sr.value.readCString(0);
      lib.proven_free_string(sr.value);
      return { status: 0, value: str };
    }
  }
  return { status: r.status, value: "" };
};

exports.uuidParseImpl = function (s) {
  var buf = Buffer.from(s, "utf-8");
  var r = lib.proven_uuid_parse(buf, buf.length);
  if (r.status === 0) {
    var sr = lib.proven_uuid_to_string(r.uuid);
    if (sr.status === 0 && !sr.value.isNull()) {
      var str = sr.value.readCString(0);
      lib.proven_free_string(sr.value);
      return { status: 0, value: str };
    }
  }
  return { status: r.status, value: "" };
};

exports.uuidIsNilImpl = function (s) {
  var buf = Buffer.from(s, "utf-8");
  var r = lib.proven_uuid_parse(buf, buf.length);
  if (r.status === 0) {
    return lib.proven_uuid_is_nil(r.uuid);
  }
  return false;
};

exports.uuidVersionImpl = function (s) {
  var buf = Buffer.from(s, "utf-8");
  var r = lib.proven_uuid_parse(buf, buf.length);
  if (r.status === 0) {
    return lib.proven_uuid_version(r.uuid);
  }
  return 0;
};
