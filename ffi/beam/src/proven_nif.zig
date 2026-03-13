// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
//
// Proven BEAM NIF - Erlang/Gleam Native Implemented Functions
//
// Wraps the proven C ABI (libproven.a) as BEAM NIFs for the Gleam bindings.
// Module name "proven_nif" matches @external(erlang, "proven_nif", ...) in Gleam.
//
// All computation is performed in Idris 2 via the proven C ABI.
// This NIF is a thin marshaling layer only.

const std = @import("std");
const proven = @import("proven");
const erl_nif = @cImport({
    @cInclude("erl_nif.h");
});

const NifEnv = ?*erl_nif.ErlNifEnv;
const NifTerm = erl_nif.ERL_NIF_TERM;
const NifArgs = [*c]const NifTerm;

// ============================================================
// NIF Helper Functions
// ============================================================

fn make_atom(env: NifEnv, name: []const u8) NifTerm {
    var atom: NifTerm = undefined;
    if (erl_nif.enif_make_existing_atom_len(env, name.ptr, name.len, &atom, erl_nif.ERL_NIF_LATIN1) != 0) {
        return atom;
    }
    return erl_nif.enif_make_atom_len(env, name.ptr, name.len);
}

fn make_ok(env: NifEnv, term: NifTerm) NifTerm {
    return erl_nif.enif_make_tuple2(env, make_atom(env, "ok"), term);
}

fn make_error(env: NifEnv, reason: []const u8) NifTerm {
    return erl_nif.enif_make_tuple2(env, make_atom(env, "error"), make_binary(env, reason));
}

fn make_error_atom(env: NifEnv, reason: []const u8) NifTerm {
    return erl_nif.enif_make_tuple2(env, make_atom(env, "error"), make_atom(env, reason));
}

fn make_binary(env: NifEnv, data: []const u8) NifTerm {
    var bin: erl_nif.ErlNifBinary = undefined;
    if (erl_nif.enif_alloc_binary(data.len, &bin) == 0) {
        return make_atom(env, "nil");
    }
    if (data.len > 0) {
        // SAFETY: bin.data is valid for bin.size bytes after enif_alloc_binary succeeds
        @memcpy(bin.data[0..data.len], data);
    }
    return erl_nif.enif_make_binary(env, &bin);
}

fn make_bool(env: NifEnv, val: bool) NifTerm {
    return if (val) make_atom(env, "true") else make_atom(env, "false");
}

fn get_binary(env: NifEnv, term: NifTerm) ?erl_nif.ErlNifBinary {
    var bin: erl_nif.ErlNifBinary = undefined;
    if (erl_nif.enif_inspect_binary(env, term, &bin) != 0) {
        return bin;
    }
    // Also try iolist
    if (erl_nif.enif_inspect_iolist_as_binary(env, term, &bin) != 0) {
        return bin;
    }
    return null;
}

fn get_int64(env: NifEnv, term: NifTerm) ?i64 {
    var val: i64 = undefined;
    if (erl_nif.enif_get_int64(env, term, &val) != 0) {
        return val;
    }
    return null;
}

fn get_uint(env: NifEnv, term: NifTerm) ?c_uint {
    var val: c_uint = undefined;
    if (erl_nif.enif_get_uint(env, term, &val) != 0) {
        return val;
    }
    return null;
}

fn get_double(env: NifEnv, term: NifTerm) ?f64 {
    var val: f64 = undefined;
    if (erl_nif.enif_get_double(env, term, &val) != 0) {
        return val;
    }
    // Try converting from int
    if (get_int64(env, term)) |i| {
        return @floatFromInt(i);
    }
    return null;
}

fn status_to_string(status: proven.ProvenStatus) []const u8 {
    return switch (status) {
        .ok => "ok",
        .err_null_pointer => "null_pointer",
        .err_invalid_argument => "invalid_argument",
        .err_overflow => "overflow",
        .err_underflow => "underflow",
        .err_division_by_zero => "division_by_zero",
        .err_parse_failure => "parse_failure",
        .err_validation_failed => "validation_failed",
        .err_out_of_bounds => "out_of_bounds",
        .err_encoding_error => "encoding_error",
        .err_allocation_failed => "allocation_failed",
        .err_not_implemented => "not_implemented",
    };
}

/// Convert IntResult to {ok, Int} | {error, String}
fn int_result_to_term(env: NifEnv, result: proven.IntResult) NifTerm {
    if (result.status == .ok) {
        return make_ok(env, erl_nif.enif_make_int64(env, result.value));
    }
    return make_error(env, status_to_string(result.status));
}

/// Convert BoolResult to {ok, Bool} | {error, String}
fn bool_result_to_term(env: NifEnv, result: proven.BoolResult) NifTerm {
    if (result.status == .ok) {
        return make_ok(env, make_bool(env, result.value));
    }
    return make_error(env, status_to_string(result.status));
}

/// Convert StringResult to {ok, String} | {error, String}, freeing the proven string
fn string_result_to_term(env: NifEnv, result: proven.StringResult) NifTerm {
    if (result.status == .ok) {
        if (result.value) |ptr| {
            const term = make_binary(env, ptr[0..result.length]);
            proven.proven_free_string(result.value);
            return make_ok(env, term);
        }
        return make_ok(env, make_binary(env, ""));
    }
    return make_error(env, status_to_string(result.status));
}

/// Convert FloatResult to {ok, Float} | {error, String}
fn float_result_to_term(env: NifEnv, result: proven.FloatResult) NifTerm {
    if (result.status == .ok) {
        return make_ok(env, erl_nif.enif_make_double(env, result.value));
    }
    return make_error(env, status_to_string(result.status));
}

fn badarg(env: NifEnv) NifTerm {
    return erl_nif.enif_make_badarg(env);
}

fn not_implemented(env: NifEnv) NifTerm {
    return make_error(env, "not_implemented");
}

// ============================================================
// Math NIF Functions (8)
// ============================================================

fn nif_math_div(env: NifEnv, _: c_int, argv: NifArgs) callconv(.c) NifTerm {
    const a = get_int64(env, argv[0]) orelse return badarg(env);
    const b = get_int64(env, argv[1]) orelse return badarg(env);
    return int_result_to_term(env, proven.proven_math_div(a, b));
}

fn nif_math_mod(env: NifEnv, _: c_int, argv: NifArgs) callconv(.c) NifTerm {
    const a = get_int64(env, argv[0]) orelse return badarg(env);
    const b = get_int64(env, argv[1]) orelse return badarg(env);
    return int_result_to_term(env, proven.proven_math_mod(a, b));
}

fn nif_math_add_checked(env: NifEnv, _: c_int, argv: NifArgs) callconv(.c) NifTerm {
    const a = get_int64(env, argv[0]) orelse return badarg(env);
    const b = get_int64(env, argv[1]) orelse return badarg(env);
    return int_result_to_term(env, proven.proven_math_add_checked(a, b));
}

fn nif_math_sub_checked(env: NifEnv, _: c_int, argv: NifArgs) callconv(.c) NifTerm {
    const a = get_int64(env, argv[0]) orelse return badarg(env);
    const b = get_int64(env, argv[1]) orelse return badarg(env);
    return int_result_to_term(env, proven.proven_math_sub_checked(a, b));
}

fn nif_math_mul_checked(env: NifEnv, _: c_int, argv: NifArgs) callconv(.c) NifTerm {
    const a = get_int64(env, argv[0]) orelse return badarg(env);
    const b = get_int64(env, argv[1]) orelse return badarg(env);
    return int_result_to_term(env, proven.proven_math_mul_checked(a, b));
}

fn nif_math_abs_safe(env: NifEnv, _: c_int, argv: NifArgs) callconv(.c) NifTerm {
    const n = get_int64(env, argv[0]) orelse return badarg(env);
    return int_result_to_term(env, proven.proven_math_abs_safe(n));
}

fn nif_math_clamp(env: NifEnv, _: c_int, argv: NifArgs) callconv(.c) NifTerm {
    const lo = get_int64(env, argv[0]) orelse return badarg(env);
    const hi = get_int64(env, argv[1]) orelse return badarg(env);
    const value = get_int64(env, argv[2]) orelse return badarg(env);
    return erl_nif.enif_make_int64(env, proven.proven_math_clamp(lo, hi, value));
}

fn nif_math_pow_checked(env: NifEnv, _: c_int, argv: NifArgs) callconv(.c) NifTerm {
    const base = get_int64(env, argv[0]) orelse return badarg(env);
    const exp = get_uint(env, argv[1]) orelse return badarg(env);
    return int_result_to_term(env, proven.proven_math_pow_checked(base, exp));
}

// ============================================================
// String NIF Functions (4)
// ============================================================

fn nif_string_is_valid_utf8(env: NifEnv, _: c_int, argv: NifArgs) callconv(.c) NifTerm {
    const bin = get_binary(env, argv[0]) orelse return badarg(env);
    return bool_result_to_term(env, proven.proven_string_is_valid_utf8(bin.data, bin.size));
}

fn nif_string_escape_sql(env: NifEnv, _: c_int, argv: NifArgs) callconv(.c) NifTerm {
    const bin = get_binary(env, argv[0]) orelse return badarg(env);
    return string_result_to_term(env, proven.proven_string_escape_sql(bin.data, bin.size));
}

fn nif_string_escape_html(env: NifEnv, _: c_int, argv: NifArgs) callconv(.c) NifTerm {
    const bin = get_binary(env, argv[0]) orelse return badarg(env);
    return string_result_to_term(env, proven.proven_string_escape_html(bin.data, bin.size));
}

fn nif_string_escape_js(env: NifEnv, _: c_int, argv: NifArgs) callconv(.c) NifTerm {
    const bin = get_binary(env, argv[0]) orelse return badarg(env);
    return string_result_to_term(env, proven.proven_string_escape_js(bin.data, bin.size));
}

// ============================================================
// Path NIF Functions (2)
// ============================================================

fn nif_path_has_traversal(env: NifEnv, _: c_int, argv: NifArgs) callconv(.c) NifTerm {
    const bin = get_binary(env, argv[0]) orelse return badarg(env);
    return bool_result_to_term(env, proven.proven_path_has_traversal(bin.data, bin.size));
}

fn nif_path_sanitize_filename(env: NifEnv, _: c_int, argv: NifArgs) callconv(.c) NifTerm {
    const bin = get_binary(env, argv[0]) orelse return badarg(env);
    return string_result_to_term(env, proven.proven_path_sanitize_filename(bin.data, bin.size));
}

// ============================================================
// JSON NIF Functions (2)
// ============================================================

fn nif_json_is_valid(env: NifEnv, _: c_int, argv: NifArgs) callconv(.c) NifTerm {
    const bin = get_binary(env, argv[0]) orelse return badarg(env);
    return bool_result_to_term(env, proven.proven_json_is_valid(bin.data, bin.size));
}

fn nif_json_get_type(env: NifEnv, _: c_int, argv: NifArgs) callconv(.c) NifTerm {
    const bin = get_binary(env, argv[0]) orelse return badarg(env);
    const json_type = proven.proven_json_get_type(bin.data, bin.size);
    const type_name: []const u8 = switch (json_type) {
        .null_ => "null",
        .bool_ => "bool",
        .number => "number",
        .string => "string",
        .array => "array",
        .object => "object",
        .invalid => return make_error(env, "invalid_json"),
    };
    return make_ok(env, make_atom(env, type_name));
}

// ============================================================
// Email NIF Functions (1)
// ============================================================

fn nif_email_is_valid(env: NifEnv, _: c_int, argv: NifArgs) callconv(.c) NifTerm {
    const bin = get_binary(env, argv[0]) orelse return badarg(env);
    return bool_result_to_term(env, proven.proven_email_is_valid(bin.data, bin.size));
}

// ============================================================
// Crypto NIF Functions (2)
// ============================================================

fn nif_crypto_constant_time_eq(env: NifEnv, _: c_int, argv: NifArgs) callconv(.c) NifTerm {
    const bin1 = get_binary(env, argv[0]) orelse return badarg(env);
    const bin2 = get_binary(env, argv[1]) orelse return badarg(env);
    return bool_result_to_term(env, proven.proven_crypto_constant_time_eq(bin1.data, bin1.size, bin2.data, bin2.size));
}

fn nif_crypto_random_bytes(env: NifEnv, _: c_int, argv: NifArgs) callconv(.c) NifTerm {
    const size_i = get_int64(env, argv[0]) orelse return badarg(env);
    if (size_i < 0 or size_i > 65536) return make_error(env, "invalid_argument");
    const size: usize = @intCast(size_i);

    var bin: erl_nif.ErlNifBinary = undefined;
    if (erl_nif.enif_alloc_binary(size, &bin) == 0) {
        return make_error(env, "allocation_failed");
    }
    const status = proven.proven_crypto_random_bytes(bin.data, size);
    if (status != .ok) {
        erl_nif.enif_release_binary(&bin);
        return make_error(env, status_to_string(status));
    }
    return make_ok(env, erl_nif.enif_make_binary(env, &bin));
}

// ============================================================
// Float NIF Functions (5)
// ============================================================

fn nif_float_div(env: NifEnv, _: c_int, argv: NifArgs) callconv(.c) NifTerm {
    const a = get_double(env, argv[0]) orelse return badarg(env);
    const b = get_double(env, argv[1]) orelse return badarg(env);
    return float_result_to_term(env, proven.proven_float_div(a, b));
}

fn nif_float_sqrt(env: NifEnv, _: c_int, argv: NifArgs) callconv(.c) NifTerm {
    const x = get_double(env, argv[0]) orelse return badarg(env);
    return float_result_to_term(env, proven.proven_float_sqrt(x));
}

fn nif_float_ln(env: NifEnv, _: c_int, argv: NifArgs) callconv(.c) NifTerm {
    const x = get_double(env, argv[0]) orelse return badarg(env);
    return float_result_to_term(env, proven.proven_float_ln(x));
}

fn nif_float_is_finite(env: NifEnv, _: c_int, argv: NifArgs) callconv(.c) NifTerm {
    const x = get_double(env, argv[0]) orelse return badarg(env);
    return make_bool(env, proven.proven_float_is_finite(x));
}

fn nif_float_is_nan(env: NifEnv, _: c_int, argv: NifArgs) callconv(.c) NifTerm {
    const x = get_double(env, argv[0]) orelse return badarg(env);
    return make_bool(env, proven.proven_float_is_nan(x));
}

// ============================================================
// Angle NIF Functions (4)
// ============================================================

fn nif_angle_deg_to_rad(env: NifEnv, _: c_int, argv: NifArgs) callconv(.c) NifTerm {
    const deg = get_double(env, argv[0]) orelse return badarg(env);
    return erl_nif.enif_make_double(env, proven.proven_angle_deg_to_rad(deg));
}

fn nif_angle_rad_to_deg(env: NifEnv, _: c_int, argv: NifArgs) callconv(.c) NifTerm {
    const rad = get_double(env, argv[0]) orelse return badarg(env);
    return erl_nif.enif_make_double(env, proven.proven_angle_rad_to_deg(rad));
}

fn nif_angle_normalize_degrees(env: NifEnv, _: c_int, argv: NifArgs) callconv(.c) NifTerm {
    const deg = get_double(env, argv[0]) orelse return badarg(env);
    return erl_nif.enif_make_double(env, proven.proven_angle_normalize_degrees(deg));
}

fn nif_angle_normalize_radians(env: NifEnv, _: c_int, argv: NifArgs) callconv(.c) NifTerm {
    const rad = get_double(env, argv[0]) orelse return badarg(env);
    return erl_nif.enif_make_double(env, proven.proven_angle_normalize_radians(rad));
}

// ============================================================
// Color NIF Functions (3)
// ============================================================

fn nif_color_parse_hex(env: NifEnv, _: c_int, argv: NifArgs) callconv(.c) NifTerm {
    const bin = get_binary(env, argv[0]) orelse return badarg(env);
    const result = proven.proven_color_parse_hex(bin.data, bin.size);
    if (result.status != .ok) return make_error(env, status_to_string(result.status));
    // Return {ok, {r, g, b}}
    return make_ok(env, erl_nif.enif_make_tuple3(
        env,
        erl_nif.enif_make_uint(env, result.color.r),
        erl_nif.enif_make_uint(env, result.color.g),
        erl_nif.enif_make_uint(env, result.color.b),
    ));
}

fn nif_color_rgb_to_hsl(env: NifEnv, _: c_int, argv: NifArgs) callconv(.c) NifTerm {
    var r_val: c_uint = undefined;
    var g_val: c_uint = undefined;
    var b_val: c_uint = undefined;
    if (erl_nif.enif_get_uint(env, argv[0], &r_val) == 0) return badarg(env);
    if (erl_nif.enif_get_uint(env, argv[1], &g_val) == 0) return badarg(env);
    if (erl_nif.enif_get_uint(env, argv[2], &b_val) == 0) return badarg(env);
    const rgb = proven.RGBColor{ .r = @intCast(r_val), .g = @intCast(g_val), .b = @intCast(b_val) };
    const hsl = proven.proven_color_rgb_to_hsl(rgb);
    return make_ok(env, erl_nif.enif_make_tuple3(
        env,
        erl_nif.enif_make_double(env, hsl.h),
        erl_nif.enif_make_double(env, hsl.s),
        erl_nif.enif_make_double(env, hsl.l),
    ));
}

fn nif_color_to_hex(env: NifEnv, _: c_int, argv: NifArgs) callconv(.c) NifTerm {
    var r_val: c_uint = undefined;
    var g_val: c_uint = undefined;
    var b_val: c_uint = undefined;
    if (erl_nif.enif_get_uint(env, argv[0], &r_val) == 0) return badarg(env);
    if (erl_nif.enif_get_uint(env, argv[1], &g_val) == 0) return badarg(env);
    if (erl_nif.enif_get_uint(env, argv[2], &b_val) == 0) return badarg(env);
    const rgb = proven.RGBColor{ .r = @intCast(r_val), .g = @intCast(g_val), .b = @intCast(b_val) };
    return string_result_to_term(env, proven.proven_color_to_hex(rgb));
}

// ============================================================
// Network NIF Functions (3)
// ============================================================

fn nif_network_parse_ipv4(env: NifEnv, _: c_int, argv: NifArgs) callconv(.c) NifTerm {
    const bin = get_binary(env, argv[0]) orelse return badarg(env);
    const result = proven.proven_network_parse_ipv4(bin.data, bin.size);
    if (result.status != .ok) return make_error(env, status_to_string(result.status));
    // Return {ok, {a, b, c, d}}
    return make_ok(env, erl_nif.enif_make_tuple4(
        env,
        erl_nif.enif_make_uint(env, result.address.octets[0]),
        erl_nif.enif_make_uint(env, result.address.octets[1]),
        erl_nif.enif_make_uint(env, result.address.octets[2]),
        erl_nif.enif_make_uint(env, result.address.octets[3]),
    ));
}

fn nif_network_ipv4_is_private(env: NifEnv, _: c_int, argv: NifArgs) callconv(.c) NifTerm {
    var a: c_uint = undefined;
    var b: c_uint = undefined;
    var c: c_uint = undefined;
    var d: c_uint = undefined;
    if (erl_nif.enif_get_uint(env, argv[0], &a) == 0) return badarg(env);
    if (erl_nif.enif_get_uint(env, argv[1], &b) == 0) return badarg(env);
    if (erl_nif.enif_get_uint(env, argv[2], &c) == 0) return badarg(env);
    if (erl_nif.enif_get_uint(env, argv[3], &d) == 0) return badarg(env);
    const addr = proven.IPv4Address{ .octets = .{ @intCast(a), @intCast(b), @intCast(c), @intCast(d) } };
    return make_bool(env, proven.proven_network_ipv4_is_private(addr));
}

fn nif_network_ipv4_is_loopback(env: NifEnv, _: c_int, argv: NifArgs) callconv(.c) NifTerm {
    var a: c_uint = undefined;
    var b: c_uint = undefined;
    var c: c_uint = undefined;
    var d: c_uint = undefined;
    if (erl_nif.enif_get_uint(env, argv[0], &a) == 0) return badarg(env);
    if (erl_nif.enif_get_uint(env, argv[1], &b) == 0) return badarg(env);
    if (erl_nif.enif_get_uint(env, argv[2], &c) == 0) return badarg(env);
    if (erl_nif.enif_get_uint(env, argv[3], &d) == 0) return badarg(env);
    const addr = proven.IPv4Address{ .octets = .{ @intCast(a), @intCast(b), @intCast(c), @intCast(d) } };
    return make_bool(env, proven.proven_network_ipv4_is_loopback(addr));
}

// ============================================================
// URL NIF Functions (1)
// ============================================================

fn nif_url_parse(env: NifEnv, _: c_int, argv: NifArgs) callconv(.c) NifTerm {
    const bin = get_binary(env, argv[0]) orelse return badarg(env);
    var result = proven.proven_url_parse(bin.data, bin.size);
    if (result.status != .ok) return make_error(env, status_to_string(result.status));
    // Build UrlComponents tuple: {scheme, host, port, path, query, fragment}
    // Each optional field is {some, Value} or none
    const scheme = optional_string_term(env, result.components.scheme, result.components.scheme_len);
    const host = optional_string_term(env, result.components.host, result.components.host_len);
    const port = if (result.components.port >= 0)
        erl_nif.enif_make_tuple2(env, make_atom(env, "some"), erl_nif.enif_make_int(env, result.components.port))
    else
        make_atom(env, "none");
    const path = make_binary(env, if (result.components.path) |p| p[0..result.components.path_len] else "");
    const query = optional_string_term(env, result.components.query, result.components.query_len);
    const fragment = optional_string_term(env, result.components.fragment, result.components.fragment_len);

    proven.proven_url_free(&result.components);

    return make_ok(env, erl_nif.enif_make_tuple6(env, scheme, host, port, path, query, fragment));
}

fn optional_string_term(env: NifEnv, ptr: ?[*]const u8, len: usize) NifTerm {
    if (ptr) |p| {
        if (len > 0) {
            return erl_nif.enif_make_tuple2(env, make_atom(env, "some"), make_binary(env, p[0..len]));
        }
    }
    return make_atom(env, "none");
}

// ============================================================
// DateTime NIF Functions (4)
// ============================================================

fn nif_datetime_parse(env: NifEnv, _: c_int, argv: NifArgs) callconv(.c) NifTerm {
    const bin = get_binary(env, argv[0]) orelse return badarg(env);
    const result = proven.proven_datetime_parse(bin.data, bin.size);
    if (result.status != .ok) return make_error(env, status_to_string(result.status));
    // Return {ok, {year, month, day, hour, minute, second, offset_minutes}}
    return make_ok(env, erl_nif.enif_make_tuple(env, 7, erl_nif.enif_make_int(env, result.datetime.year), erl_nif.enif_make_uint(env, result.datetime.month), erl_nif.enif_make_uint(env, result.datetime.day), erl_nif.enif_make_uint(env, result.datetime.hour), erl_nif.enif_make_uint(env, result.datetime.minute), erl_nif.enif_make_uint(env, result.datetime.second), erl_nif.enif_make_int(env, result.datetime.tz_offset_minutes)));
}

fn nif_datetime_format_iso8601(env: NifEnv, _: c_int, argv: NifArgs) callconv(.c) NifTerm {
    var year: c_int = undefined;
    var month: c_uint = undefined;
    var day: c_uint = undefined;
    var hour: c_uint = undefined;
    var minute: c_uint = undefined;
    var second: c_uint = undefined;
    var offset: c_int = undefined;
    if (erl_nif.enif_get_int(env, argv[0], &year) == 0) return badarg(env);
    if (erl_nif.enif_get_uint(env, argv[1], &month) == 0) return badarg(env);
    if (erl_nif.enif_get_uint(env, argv[2], &day) == 0) return badarg(env);
    if (erl_nif.enif_get_uint(env, argv[3], &hour) == 0) return badarg(env);
    if (erl_nif.enif_get_uint(env, argv[4], &minute) == 0) return badarg(env);
    if (erl_nif.enif_get_uint(env, argv[5], &second) == 0) return badarg(env);
    if (erl_nif.enif_get_int(env, argv[6], &offset) == 0) return badarg(env);
    const dt = proven.DateTime{
        .year = @intCast(year),
        .month = @intCast(month),
        .day = @intCast(day),
        .hour = @intCast(hour),
        .minute = @intCast(minute),
        .second = @intCast(second),
        .nanosecond = 0,
        .tz_offset_minutes = @intCast(offset),
    };
    return string_result_to_term(env, proven.proven_datetime_format_iso8601(dt));
}

fn nif_datetime_is_leap_year(env: NifEnv, _: c_int, argv: NifArgs) callconv(.c) NifTerm {
    var year: c_int = undefined;
    if (erl_nif.enif_get_int(env, argv[0], &year) == 0) return badarg(env);
    return make_bool(env, proven.proven_datetime_is_leap_year(@intCast(year)));
}

fn nif_datetime_days_in_month(env: NifEnv, _: c_int, argv: NifArgs) callconv(.c) NifTerm {
    var year: c_int = undefined;
    var month: c_uint = undefined;
    if (erl_nif.enif_get_int(env, argv[0], &year) == 0) return badarg(env);
    if (erl_nif.enif_get_uint(env, argv[1], &month) == 0) return badarg(env);
    return erl_nif.enif_make_uint(env, proven.proven_datetime_days_in_month(@intCast(year), @intCast(month)));
}

// ============================================================
// Hex NIF Functions (8)
// ============================================================

fn nif_hex_encode(env: NifEnv, _: c_int, argv: NifArgs) callconv(.c) NifTerm {
    const bin = get_binary(env, argv[0]) orelse return badarg(env);
    return string_result_to_term(env, proven.proven_hex_encode(bin.data, bin.size, false));
}

fn nif_hex_encode_upper(env: NifEnv, _: c_int, argv: NifArgs) callconv(.c) NifTerm {
    const bin = get_binary(env, argv[0]) orelse return badarg(env);
    return string_result_to_term(env, proven.proven_hex_encode(bin.data, bin.size, true));
}

fn nif_hex_decode(env: NifEnv, _: c_int, argv: NifArgs) callconv(.c) NifTerm {
    const input = get_binary(env, argv[0]) orelse return badarg(env);
    var result = proven.proven_hex_decode(input.data, input.size);
    if (result.status != .ok) return make_error(env, status_to_string(result.status));
    if (result.data) |data| {
        const term = make_binary(env, data[0..result.length]);
        proven.proven_hex_free(&result);
        return make_ok(env, term);
    }
    return make_ok(env, make_binary(env, ""));
}

fn nif_hex_is_valid(env: NifEnv, _: c_int, argv: NifArgs) callconv(.c) NifTerm {
    // Derived: try decode, check status
    const bin = get_binary(env, argv[0]) orelse return badarg(env);
    var result = proven.proven_hex_decode(bin.data, bin.size);
    const valid = result.status == .ok;
    if (valid) proven.proven_hex_free(&result);
    return make_bool(env, valid);
}

fn nif_hex_is_valid_bytes(env: NifEnv, _: c_int, argv: NifArgs) callconv(.c) NifTerm {
    // Same as hex_is_valid for BitArray (same representation on BEAM)
    return nif_hex_is_valid(env, 1, argv);
}

fn nif_hex_constant_time_eq(env: NifEnv, _: c_int, argv: NifArgs) callconv(.c) NifTerm {
    const bin1 = get_binary(env, argv[0]) orelse return badarg(env);
    const bin2 = get_binary(env, argv[1]) orelse return badarg(env);
    // Compare hex strings directly using crypto constant_time_eq
    const result = proven.proven_crypto_constant_time_eq(bin1.data, bin1.size, bin2.data, bin2.size);
    if (result.status == .ok) return make_bool(env, result.value);
    return make_bool(env, false);
}

fn nif_hex_to_int(env: NifEnv, _: c_int, argv: NifArgs) callconv(.c) NifTerm {
    // Decode hex, interpret bytes as big-endian integer
    const bin = get_binary(env, argv[0]) orelse return badarg(env);
    var result = proven.proven_hex_decode(bin.data, bin.size);
    if (result.status != .ok) return make_error(env, status_to_string(result.status));
    if (result.data) |data| {
        var val: i64 = 0;
        const len = @min(result.length, 8); // Max 8 bytes for i64
        for (data[0..len]) |byte| {
            val = (val << 8) | @as(i64, byte);
        }
        proven.proven_hex_free(&result);
        return make_ok(env, erl_nif.enif_make_int64(env, val));
    }
    return make_ok(env, erl_nif.enif_make_int64(env, 0));
}

fn nif_hex_from_int(env: NifEnv, _: c_int, argv: NifArgs) callconv(.c) NifTerm {
    const val = get_int64(env, argv[0]) orelse return badarg(env);
    const width = get_int64(env, argv[1]) orelse return badarg(env);
    // Format as hex string with padding
    var buf: [20]u8 = undefined;
    const uval: u64 = @bitCast(val);
    const w: usize = @intCast(@min(@max(width, 1), 16));
    const fmt = std.fmt.bufPrint(&buf, "{x:0>[1]}", .{ uval, w }) catch return make_error(env, "format_error");
    return make_ok(env, make_binary(env, fmt));
}

// ============================================================
// UUID NIF Functions (9)
// ============================================================

fn nif_uuid_v4_generate(env: NifEnv, _: c_int, _: NifArgs) callconv(.c) NifTerm {
    const result = proven.proven_uuid_v4();
    if (result.status != .ok) return make_error(env, status_to_string(result.status));
    return make_ok(env, make_binary(env, &result.uuid.bytes));
}

fn nif_uuid_parse(env: NifEnv, _: c_int, argv: NifArgs) callconv(.c) NifTerm {
    const bin = get_binary(env, argv[0]) orelse return badarg(env);
    const result = proven.proven_uuid_parse(bin.data, bin.size);
    if (result.status != .ok) return make_error(env, status_to_string(result.status));
    return make_ok(env, make_binary(env, &result.uuid.bytes));
}

fn nif_uuid_to_string(env: NifEnv, _: c_int, argv: NifArgs) callconv(.c) NifTerm {
    const bin = get_binary(env, argv[0]) orelse return badarg(env);
    if (bin.size != 16) return make_error(env, "invalid_uuid_length");
    var uuid: proven.UUID = undefined;
    @memcpy(&uuid.bytes, bin.data[0..16]);
    return string_result_to_term(env, proven.proven_uuid_to_string(uuid));
}

fn nif_uuid_to_urn(env: NifEnv, _: c_int, argv: NifArgs) callconv(.c) NifTerm {
    // Derived: "urn:uuid:" ++ uuid_to_string
    const bin = get_binary(env, argv[0]) orelse return badarg(env);
    if (bin.size != 16) return make_error(env, "invalid_uuid_length");
    var uuid: proven.UUID = undefined;
    @memcpy(&uuid.bytes, bin.data[0..16]);
    const result = proven.proven_uuid_to_string(uuid);
    if (result.status != .ok) return make_error(env, status_to_string(result.status));
    if (result.value) |ptr| {
        const uuid_str = ptr[0..result.length];
        // Build "urn:uuid:<uuid>"
        var urn_buf: [45]u8 = undefined; // "urn:uuid:" (9) + 36 chars
        @memcpy(urn_buf[0..9], "urn:uuid:");
        const copy_len = @min(uuid_str.len, 36);
        @memcpy(urn_buf[9..][0..copy_len], uuid_str[0..copy_len]);
        proven.proven_free_string(result.value);
        return make_ok(env, make_binary(env, urn_buf[0 .. 9 + copy_len]));
    }
    return make_error(env, "format_error");
}

fn nif_uuid_is_nil(env: NifEnv, _: c_int, argv: NifArgs) callconv(.c) NifTerm {
    const bin = get_binary(env, argv[0]) orelse return badarg(env);
    if (bin.size != 16) return make_bool(env, false);
    var uuid: proven.UUID = undefined;
    @memcpy(&uuid.bytes, bin.data[0..16]);
    return make_bool(env, proven.proven_uuid_is_nil(uuid));
}

fn nif_uuid_is_valid(env: NifEnv, _: c_int, argv: NifArgs) callconv(.c) NifTerm {
    // Derived: try parse, check status
    const bin = get_binary(env, argv[0]) orelse return badarg(env);
    const result = proven.proven_uuid_parse(bin.data, bin.size);
    return make_bool(env, result.status == .ok);
}

fn nif_uuid_get_version(env: NifEnv, _: c_int, argv: NifArgs) callconv(.c) NifTerm {
    const bin = get_binary(env, argv[0]) orelse return badarg(env);
    if (bin.size != 16) return make_error(env, "invalid_uuid_length");
    var uuid: proven.UUID = undefined;
    @memcpy(&uuid.bytes, bin.data[0..16]);
    const ver = proven.proven_uuid_version(uuid);
    const ver_atom: []const u8 = switch (ver) {
        1 => "v1",
        2 => "v2",
        3 => "v3",
        4 => "v4",
        5 => "v5",
        else => return make_error(env, "unknown_version"),
    };
    return make_ok(env, make_atom(env, ver_atom));
}

fn nif_uuid_get_variant(env: NifEnv, _: c_int, argv: NifArgs) callconv(.c) NifTerm {
    // Derived: extract variant from byte 8
    const bin = get_binary(env, argv[0]) orelse return badarg(env);
    if (bin.size != 16) return make_error(env, "invalid_uuid_length");
    const byte8 = bin.data[8];
    const variant: []const u8 = if (byte8 & 0x80 == 0)
        "ncs"
    else if (byte8 & 0xC0 == 0x80)
        "rfc4122"
    else if (byte8 & 0xE0 == 0xC0)
        "microsoft"
    else
        "future";
    return make_ok(env, make_atom(env, variant));
}

fn nif_uuid_equals(env: NifEnv, _: c_int, argv: NifArgs) callconv(.c) NifTerm {
    const bin1 = get_binary(env, argv[0]) orelse return badarg(env);
    const bin2 = get_binary(env, argv[1]) orelse return badarg(env);
    if (bin1.size != 16 or bin2.size != 16) return make_bool(env, false);
    return make_bool(env, std.mem.eql(u8, bin1.data[0..16], bin2.data[0..16]));
}

// ============================================================
// Version NIF Functions (2)
// ============================================================

fn nif_version_parse(env: NifEnv, _: c_int, argv: NifArgs) callconv(.c) NifTerm {
    const bin = get_binary(env, argv[0]) orelse return badarg(env);
    var result = proven.proven_version_parse(bin.data, bin.size);
    if (result.status != .ok) return make_error(env, status_to_string(result.status));
    // Return {ok, {major, minor, patch, prerelease, build_meta}}
    const pre = optional_string_term(env, result.version.prerelease, result.version.prerelease_len);
    const term = erl_nif.enif_make_tuple(env, 4, erl_nif.enif_make_uint(env, result.version.major), erl_nif.enif_make_uint(env, result.version.minor), erl_nif.enif_make_uint(env, result.version.patch), pre);
    proven.proven_version_free(&result.version);
    return make_ok(env, term);
}

fn nif_version_compare(env: NifEnv, _: c_int, argv: NifArgs) callconv(.c) NifTerm {
    // Gleam passes 6 args: major_a, minor_a, patch_a, major_b, minor_b, patch_b
    var maj_a: c_uint = undefined;
    var min_a: c_uint = undefined;
    var pat_a: c_uint = undefined;
    var maj_b: c_uint = undefined;
    var min_b: c_uint = undefined;
    var pat_b: c_uint = undefined;
    if (erl_nif.enif_get_uint(env, argv[0], &maj_a) == 0) return badarg(env);
    if (erl_nif.enif_get_uint(env, argv[1], &min_a) == 0) return badarg(env);
    if (erl_nif.enif_get_uint(env, argv[2], &pat_a) == 0) return badarg(env);
    if (erl_nif.enif_get_uint(env, argv[3], &maj_b) == 0) return badarg(env);
    if (erl_nif.enif_get_uint(env, argv[4], &min_b) == 0) return badarg(env);
    if (erl_nif.enif_get_uint(env, argv[5], &pat_b) == 0) return badarg(env);
    const a = proven.SemanticVersion{
        .major = maj_a,
        .minor = min_a,
        .patch = pat_a,
        .prerelease = null,
        .prerelease_len = 0,
    };
    const b_ver = proven.SemanticVersion{
        .major = maj_b,
        .minor = min_b,
        .patch = pat_b,
        .prerelease = null,
        .prerelease_len = 0,
    };
    return erl_nif.enif_make_int(env, proven.proven_version_compare(a, b_ver));
}

// ============================================================
// Unit NIF Functions (2)
// ============================================================

fn nif_unit_convert_length(env: NifEnv, _: c_int, argv: NifArgs) callconv(.c) NifTerm {
    const value = get_double(env, argv[0]) orelse return badarg(env);
    const from = get_uint(env, argv[1]) orelse return badarg(env);
    const to = get_uint(env, argv[2]) orelse return badarg(env);
    return float_result_to_term(env, proven.proven_unit_convert_length(value, @enumFromInt(from), @enumFromInt(to)));
}

fn nif_unit_convert_temp(env: NifEnv, _: c_int, argv: NifArgs) callconv(.c) NifTerm {
    const value = get_double(env, argv[0]) orelse return badarg(env);
    const from = get_uint(env, argv[1]) orelse return badarg(env);
    const to = get_uint(env, argv[2]) orelse return badarg(env);
    return float_result_to_term(env, proven.proven_unit_convert_temp(value, @enumFromInt(from), @enumFromInt(to)));
}

// ============================================================
// Currency NIF Functions (12) — 2 backed by C ABI, 10 stubs
// ============================================================

fn nif_currency_parse_code(env: NifEnv, _: c_int, argv: NifArgs) callconv(.c) NifTerm {
    const bin = get_binary(env, argv[0]) orelse return badarg(env);
    const result = proven.proven_currency_parse(bin.data, bin.size);
    if (result.status != .ok) return make_error(env, status_to_string(result.status));
    return make_ok(env, make_binary(env, &result.currency_code));
}

fn nif_currency_is_valid_code(env: NifEnv, _: c_int, argv: NifArgs) callconv(.c) NifTerm {
    const bin = get_binary(env, argv[0]) orelse return badarg(env);
    const result = proven.proven_currency_parse(bin.data, bin.size);
    return make_bool(env, result.status == .ok);
}

fn nif_currency_get_decimals(env: NifEnv, _: c_int, _: NifArgs) callconv(.c) NifTerm {
    return not_implemented(env);
}
fn nif_currency_get_symbol(env: NifEnv, _: c_int, _: NifArgs) callconv(.c) NifTerm {
    return not_implemented(env);
}
fn nif_currency_get_name(env: NifEnv, _: c_int, _: NifArgs) callconv(.c) NifTerm {
    return not_implemented(env);
}
fn nif_money_from_major(env: NifEnv, _: c_int, _: NifArgs) callconv(.c) NifTerm {
    return not_implemented(env);
}
fn nif_money_from_minor(env: NifEnv, _: c_int, _: NifArgs) callconv(.c) NifTerm {
    return not_implemented(env);
}
fn nif_money_add(env: NifEnv, _: c_int, _: NifArgs) callconv(.c) NifTerm {
    return not_implemented(env);
}
fn nif_money_sub(env: NifEnv, _: c_int, _: NifArgs) callconv(.c) NifTerm {
    return not_implemented(env);
}
fn nif_money_mul(env: NifEnv, _: c_int, _: NifArgs) callconv(.c) NifTerm {
    return not_implemented(env);
}
fn nif_money_div(env: NifEnv, _: c_int, _: NifArgs) callconv(.c) NifTerm {
    return not_implemented(env);
}
fn nif_money_format(env: NifEnv, _: c_int, _: NifArgs) callconv(.c) NifTerm {
    return not_implemented(env);
}

// ============================================================
// Phone NIF Functions (4) — 2 backed by C ABI, 2 derived
// ============================================================

fn nif_phone_parse(env: NifEnv, _: c_int, argv: NifArgs) callconv(.c) NifTerm {
    const bin = get_binary(env, argv[0]) orelse return badarg(env);
    const result = proven.proven_phone_parse(bin.data, bin.size);
    if (result.status != .ok) return make_error(env, status_to_string(result.status));
    // Return {ok, {country_code, national_number}}
    return make_ok(env, erl_nif.enif_make_tuple2(
        env,
        erl_nif.enif_make_uint(env, result.country_code),
        erl_nif.enif_make_uint64(env, result.national_number),
    ));
}

fn nif_phone_is_valid(env: NifEnv, _: c_int, argv: NifArgs) callconv(.c) NifTerm {
    const bin = get_binary(env, argv[0]) orelse return badarg(env);
    const result = proven.proven_phone_parse(bin.data, bin.size);
    return make_bool(env, result.status == .ok and result.is_valid);
}

fn nif_phone_format_e164(env: NifEnv, _: c_int, argv: NifArgs) callconv(.c) NifTerm {
    const cc = get_uint(env, argv[0]) orelse return badarg(env);
    var nn: u64 = undefined;
    if (erl_nif.enif_get_uint64(env, argv[1], &nn) == 0) return badarg(env);
    return string_result_to_term(env, proven.proven_phone_format_e164(@intCast(cc), nn));
}

fn nif_phone_format_international(env: NifEnv, _: c_int, _: NifArgs) callconv(.c) NifTerm {
    return not_implemented(env);
}

// ============================================================
// NIF Function Table
// ============================================================

var nif_funcs = [_]erl_nif.ErlNifFunc{
    // Math (8)
    .{ .name = "math_div", .arity = 2, .fptr = nif_math_div, .flags = 0 },
    .{ .name = "math_mod", .arity = 2, .fptr = nif_math_mod, .flags = 0 },
    .{ .name = "math_add_checked", .arity = 2, .fptr = nif_math_add_checked, .flags = 0 },
    .{ .name = "math_sub_checked", .arity = 2, .fptr = nif_math_sub_checked, .flags = 0 },
    .{ .name = "math_mul_checked", .arity = 2, .fptr = nif_math_mul_checked, .flags = 0 },
    .{ .name = "math_abs_safe", .arity = 1, .fptr = nif_math_abs_safe, .flags = 0 },
    .{ .name = "math_clamp", .arity = 3, .fptr = nif_math_clamp, .flags = 0 },
    .{ .name = "math_pow_checked", .arity = 2, .fptr = nif_math_pow_checked, .flags = 0 },
    // String (4)
    .{ .name = "string_is_valid_utf8", .arity = 1, .fptr = nif_string_is_valid_utf8, .flags = 0 },
    .{ .name = "string_escape_sql", .arity = 1, .fptr = nif_string_escape_sql, .flags = 0 },
    .{ .name = "string_escape_html", .arity = 1, .fptr = nif_string_escape_html, .flags = 0 },
    .{ .name = "string_escape_js", .arity = 1, .fptr = nif_string_escape_js, .flags = 0 },
    // Path (2)
    .{ .name = "path_has_traversal", .arity = 1, .fptr = nif_path_has_traversal, .flags = 0 },
    .{ .name = "path_sanitize_filename", .arity = 1, .fptr = nif_path_sanitize_filename, .flags = 0 },
    // JSON (2)
    .{ .name = "json_is_valid", .arity = 1, .fptr = nif_json_is_valid, .flags = 0 },
    .{ .name = "json_get_type", .arity = 1, .fptr = nif_json_get_type, .flags = 0 },
    // Email (1)
    .{ .name = "email_is_valid", .arity = 1, .fptr = nif_email_is_valid, .flags = 0 },
    // Crypto (2)
    .{ .name = "crypto_constant_time_eq", .arity = 2, .fptr = nif_crypto_constant_time_eq, .flags = 0 },
    .{ .name = "crypto_random_bytes", .arity = 1, .fptr = nif_crypto_random_bytes, .flags = 0 },
    // Float (5)
    .{ .name = "float_div", .arity = 2, .fptr = nif_float_div, .flags = 0 },
    .{ .name = "float_sqrt", .arity = 1, .fptr = nif_float_sqrt, .flags = 0 },
    .{ .name = "float_ln", .arity = 1, .fptr = nif_float_ln, .flags = 0 },
    .{ .name = "float_is_finite", .arity = 1, .fptr = nif_float_is_finite, .flags = 0 },
    .{ .name = "float_is_nan", .arity = 1, .fptr = nif_float_is_nan, .flags = 0 },
    // Angle (4)
    .{ .name = "angle_deg_to_rad", .arity = 1, .fptr = nif_angle_deg_to_rad, .flags = 0 },
    .{ .name = "angle_rad_to_deg", .arity = 1, .fptr = nif_angle_rad_to_deg, .flags = 0 },
    .{ .name = "angle_normalize_degrees", .arity = 1, .fptr = nif_angle_normalize_degrees, .flags = 0 },
    .{ .name = "angle_normalize_radians", .arity = 1, .fptr = nif_angle_normalize_radians, .flags = 0 },
    // Color (3)
    .{ .name = "color_parse_hex", .arity = 1, .fptr = nif_color_parse_hex, .flags = 0 },
    .{ .name = "color_rgb_to_hsl", .arity = 3, .fptr = nif_color_rgb_to_hsl, .flags = 0 },
    .{ .name = "color_to_hex", .arity = 3, .fptr = nif_color_to_hex, .flags = 0 },
    // Network (3)
    .{ .name = "network_parse_ipv4", .arity = 1, .fptr = nif_network_parse_ipv4, .flags = 0 },
    .{ .name = "network_ipv4_is_private", .arity = 4, .fptr = nif_network_ipv4_is_private, .flags = 0 },
    .{ .name = "network_ipv4_is_loopback", .arity = 4, .fptr = nif_network_ipv4_is_loopback, .flags = 0 },
    // URL (1)
    .{ .name = "url_parse", .arity = 1, .fptr = nif_url_parse, .flags = 0 },
    // DateTime (4)
    .{ .name = "datetime_parse", .arity = 1, .fptr = nif_datetime_parse, .flags = 0 },
    .{ .name = "datetime_format_iso8601", .arity = 7, .fptr = nif_datetime_format_iso8601, .flags = 0 },
    .{ .name = "datetime_is_leap_year", .arity = 1, .fptr = nif_datetime_is_leap_year, .flags = 0 },
    .{ .name = "datetime_days_in_month", .arity = 2, .fptr = nif_datetime_days_in_month, .flags = 0 },
    // Hex (8)
    .{ .name = "hex_encode", .arity = 1, .fptr = nif_hex_encode, .flags = 0 },
    .{ .name = "hex_encode_upper", .arity = 1, .fptr = nif_hex_encode_upper, .flags = 0 },
    .{ .name = "hex_decode", .arity = 1, .fptr = nif_hex_decode, .flags = 0 },
    .{ .name = "hex_is_valid", .arity = 1, .fptr = nif_hex_is_valid, .flags = 0 },
    .{ .name = "hex_is_valid_bytes", .arity = 1, .fptr = nif_hex_is_valid_bytes, .flags = 0 },
    .{ .name = "hex_constant_time_eq", .arity = 2, .fptr = nif_hex_constant_time_eq, .flags = 0 },
    .{ .name = "hex_to_int", .arity = 1, .fptr = nif_hex_to_int, .flags = 0 },
    .{ .name = "hex_from_int", .arity = 2, .fptr = nif_hex_from_int, .flags = 0 },
    // UUID (9)
    .{ .name = "uuid_v4_generate", .arity = 0, .fptr = nif_uuid_v4_generate, .flags = 0 },
    .{ .name = "uuid_parse", .arity = 1, .fptr = nif_uuid_parse, .flags = 0 },
    .{ .name = "uuid_to_string", .arity = 1, .fptr = nif_uuid_to_string, .flags = 0 },
    .{ .name = "uuid_to_urn", .arity = 1, .fptr = nif_uuid_to_urn, .flags = 0 },
    .{ .name = "uuid_is_nil", .arity = 1, .fptr = nif_uuid_is_nil, .flags = 0 },
    .{ .name = "uuid_is_valid", .arity = 1, .fptr = nif_uuid_is_valid, .flags = 0 },
    .{ .name = "uuid_get_version", .arity = 1, .fptr = nif_uuid_get_version, .flags = 0 },
    .{ .name = "uuid_get_variant", .arity = 1, .fptr = nif_uuid_get_variant, .flags = 0 },
    .{ .name = "uuid_equals", .arity = 2, .fptr = nif_uuid_equals, .flags = 0 },
    // Version (2)
    .{ .name = "version_parse", .arity = 1, .fptr = nif_version_parse, .flags = 0 },
    .{ .name = "version_compare", .arity = 6, .fptr = nif_version_compare, .flags = 0 },
    // Unit (2)
    .{ .name = "unit_convert_length", .arity = 3, .fptr = nif_unit_convert_length, .flags = 0 },
    .{ .name = "unit_convert_temp", .arity = 3, .fptr = nif_unit_convert_temp, .flags = 0 },
    // Currency (12) — mostly stubs
    .{ .name = "currency_parse_code", .arity = 1, .fptr = nif_currency_parse_code, .flags = 0 },
    .{ .name = "currency_is_valid_code", .arity = 1, .fptr = nif_currency_is_valid_code, .flags = 0 },
    .{ .name = "currency_get_decimals", .arity = 1, .fptr = nif_currency_get_decimals, .flags = 0 },
    .{ .name = "currency_get_symbol", .arity = 1, .fptr = nif_currency_get_symbol, .flags = 0 },
    .{ .name = "currency_get_name", .arity = 1, .fptr = nif_currency_get_name, .flags = 0 },
    .{ .name = "money_from_major", .arity = 2, .fptr = nif_money_from_major, .flags = 0 },
    .{ .name = "money_from_minor", .arity = 2, .fptr = nif_money_from_minor, .flags = 0 },
    .{ .name = "money_add", .arity = 4, .fptr = nif_money_add, .flags = 0 },
    .{ .name = "money_sub", .arity = 4, .fptr = nif_money_sub, .flags = 0 },
    .{ .name = "money_mul", .arity = 3, .fptr = nif_money_mul, .flags = 0 },
    .{ .name = "money_div", .arity = 3, .fptr = nif_money_div, .flags = 0 },
    .{ .name = "money_format", .arity = 2, .fptr = nif_money_format, .flags = 0 },
    // Phone (4)
    .{ .name = "phone_parse", .arity = 1, .fptr = nif_phone_parse, .flags = 0 },
    .{ .name = "phone_is_valid", .arity = 1, .fptr = nif_phone_is_valid, .flags = 0 },
    .{ .name = "phone_format_e164", .arity = 2, .fptr = nif_phone_format_e164, .flags = 0 },
    .{ .name = "phone_format_international", .arity = 2, .fptr = nif_phone_format_international, .flags = 0 },
};

// ============================================================
// NIF Lifecycle
// ============================================================

fn load(_: NifEnv, _: [*c]?*anyopaque, _: NifTerm) callconv(.c) c_int {
    const status = proven.proven_init();
    if (status != 0) return -1;
    return 0;
}

fn unload(_: NifEnv, _: ?*anyopaque) callconv(.c) void {
    proven.proven_deinit();
}

// ============================================================
// NIF Entry Point
// ============================================================

pub export const nif_init = erl_nif.ErlNifEntry{
    .major = erl_nif.ERL_NIF_MAJOR_VERSION,
    .minor = erl_nif.ERL_NIF_MINOR_VERSION,
    .name = "proven_nif",
    .num_of_funcs = nif_funcs.len,
    .funcs = &nif_funcs,
    .load = load,
    .reload = null,
    .upgrade = null,
    .unload = unload,
    .vm_variant = "beam.vanilla",
    .options = 0,
    .sizeof_ErlNifResourceTypeInit = @sizeOf(erl_nif.ErlNifResourceTypeInit),
    .min_erts = "erts-13.0",
};
