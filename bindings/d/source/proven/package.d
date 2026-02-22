// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/**
 * Proven - FFI bindings to libproven for D.
 *
 * All computation is performed in formally verified Idris 2 code, exposed
 * via a Zig FFI bridge as a stable C ABI (libproven). This D package is a
 * thin wrapper that marshals data to and from the C interface. No safety
 * logic is reimplemented in D.
 *
 * Link against libproven.so (or libproven.a) at build time.
 *
 * Version: 0.9.0
 * Module Count: 41
 */
module proven;

pragma(lib, "proven");

/// Library version (matches libproven)
enum VERSION = "0.9.0";

/// Total number of modules in the library
enum MODULE_COUNT = 41;

// Core modules
public import proven.safe_math;
public import proven.safe_string;
public import proven.safe_path;
public import proven.safe_email;
public import proven.safe_url;
public import proven.safe_network;
public import proven.safe_crypto;
public import proven.safe_uuid;
public import proven.safe_currency;
public import proven.safe_phone;
public import proven.safe_hex;

// Data modules
public import proven.safe_json;
public import proven.safe_datetime;
public import proven.safe_float;
public import proven.safe_version;
public import proven.safe_color;
public import proven.safe_angle;
