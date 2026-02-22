// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
//
// SafeNetwork FFI implementation - delegates to libproven via ffi-napi.
// All computation performed in Idris 2 via the Zig FFI layer.

"use strict";

const { lib } = require("./FFI.js");

exports.parseIpv4Impl = function (s) {
  var buf = Buffer.from(s, "utf-8");
  var r = lib.proven_network_parse_ipv4(buf, buf.length);
  if (r.status === 0) {
    return {
      status: 0,
      octets: [
        r.address.octets[0],
        r.address.octets[1],
        r.address.octets[2],
        r.address.octets[3],
      ],
    };
  }
  return { status: r.status, octets: [0, 0, 0, 0] };
};

exports.ipv4IsPrivateImpl = function (octets) {
  var addr = { octets: octets };
  return lib.proven_network_ipv4_is_private(addr);
};

exports.ipv4IsLoopbackImpl = function (octets) {
  var addr = { octets: octets };
  return lib.proven_network_ipv4_is_loopback(addr);
};
