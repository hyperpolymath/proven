// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
//
// SafeUrl FFI implementation - delegates to libproven via ffi-napi.
// All computation performed in Idris 2 via the Zig FFI layer.

"use strict";

const { lib } = require("./FFI.js");

function readNullTermString(ptr, len) {
  if (ptr === null || ptr.isNull()) return "";
  return ptr.readCString(0);
}

exports.parseUrlImpl = function (s) {
  var buf = Buffer.from(s, "utf-8");
  var r = lib.proven_url_parse(buf, buf.length);
  if (r.status === 0) {
    var result = {
      status: 0,
      scheme: readNullTermString(r.components.scheme, r.components.scheme_len),
      host: readNullTermString(r.components.host, r.components.host_len),
      port: r.components.has_port ? r.components.port : -1,
      path: readNullTermString(r.components.path, r.components.path_len),
      query: readNullTermString(r.components.query, r.components.query_len),
      fragment: readNullTermString(
        r.components.fragment,
        r.components.fragment_len
      ),
    };
    lib.proven_url_free(r.components);
    return result;
  }
  return {
    status: r.status,
    scheme: "",
    host: "",
    port: -1,
    path: "",
    query: "",
    fragment: "",
  };
};
