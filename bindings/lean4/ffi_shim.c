/* SPDX-License-Identifier: PMPL-1.0-or-later */
/* Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk> */

/**
 * @file ffi_shim.c
 * @brief C shim bridging Lean 4 managed types to libproven C ABI.
 *
 * Lean 4 represents ByteArray, String, etc. as managed objects with reference
 * counting. This shim extracts raw pointers from Lean objects and calls the
 * corresponding proven_* C functions, then wraps results back into Lean
 * objects.
 *
 * This file does NOT reimplement any logic -- it is purely a marshaling layer.
 */

#include <lean/lean.h>
#include <string.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>

/* Forward declarations for libproven functions we call */
typedef struct { int32_t status; int64_t value; } ProvenIntResult;
typedef struct { int32_t status; bool value; } ProvenBoolResult;
typedef struct { int32_t status; char* value; size_t length; } ProvenStringResult;
typedef struct { int32_t status; double value; } ProvenFloatResult;

typedef struct { uint8_t octets[4]; } ProvenIPv4Address;
typedef struct { int32_t status; ProvenIPv4Address address; } ProvenIPv4Result;

typedef struct {
    int32_t  year;
    uint8_t  month;
    uint8_t  day;
    uint8_t  hour;
    uint8_t  minute;
    uint8_t  second;
    uint32_t nanosecond;
    int16_t  tz_offset_minutes;
} ProvenDateTime;

typedef struct {
    int32_t       status;
    ProvenDateTime datetime;
} ProvenDateTimeResult;

typedef struct {
    char*    scheme;       size_t scheme_len;
    char*    host;         size_t host_len;
    uint16_t port;         bool   has_port;
    char*    path;         size_t path_len;
    char*    query;        size_t query_len;
    char*    fragment;     size_t fragment_len;
} ProvenUrlComponents;

typedef struct {
    int32_t             status;
    ProvenUrlComponents components;
} ProvenUrlResult;

typedef struct {
    int32_t  status;
    uint8_t* data;
    size_t   length;
} ProvenHexDecodeResult;

/* libproven functions */
extern ProvenBoolResult proven_string_is_valid_utf8(const uint8_t* ptr, size_t len);
extern ProvenStringResult proven_string_escape_sql(const uint8_t* ptr, size_t len);
extern ProvenStringResult proven_string_escape_html(const uint8_t* ptr, size_t len);
extern ProvenStringResult proven_string_escape_js(const uint8_t* ptr, size_t len);
extern ProvenBoolResult proven_path_has_traversal(const uint8_t* ptr, size_t len);
extern ProvenStringResult proven_path_sanitize_filename(const uint8_t* ptr, size_t len);
extern ProvenBoolResult proven_email_is_valid(const uint8_t* ptr, size_t len);
extern ProvenUrlResult proven_url_parse(const uint8_t* ptr, size_t len);
extern void proven_url_free(ProvenUrlComponents* components);
extern ProvenIPv4Result proven_network_parse_ipv4(const uint8_t* ptr, size_t len);
extern bool proven_network_ipv4_is_private(ProvenIPv4Address addr);
extern bool proven_network_ipv4_is_loopback(ProvenIPv4Address addr);
extern ProvenBoolResult proven_crypto_constant_time_eq(
    const uint8_t* ptr1, size_t len1,
    const uint8_t* ptr2, size_t len2);
extern int32_t proven_crypto_random_bytes(uint8_t* ptr, size_t len);
extern ProvenBoolResult proven_json_is_valid(const uint8_t* ptr, size_t len);
extern int32_t proven_json_get_type(const uint8_t* ptr, size_t len);
extern ProvenDateTimeResult proven_datetime_parse(const uint8_t* ptr, size_t len);
extern ProvenStringResult proven_datetime_format_iso8601(ProvenDateTime dt);
extern ProvenStringResult proven_hex_encode(const uint8_t* ptr, size_t len, bool uppercase);
extern ProvenHexDecodeResult proven_hex_decode(const uint8_t* ptr, size_t len);
extern void proven_hex_free(ProvenHexDecodeResult* result);
extern ProvenIntResult proven_checksum_crc32(const uint8_t* ptr, size_t len);
extern ProvenBoolResult proven_checksum_verify_crc32(const uint8_t* ptr, size_t len, uint32_t expected);
extern void proven_free_string(char* ptr);

/* ============================================================================
 * Helper: create a Lean BoolResult (status : Int32, value : Bool)
 * Lean structure layout: constructor tag + fields
 * ============================================================================ */

static lean_obj_res mk_bool_result(int32_t status, bool value) {
    lean_object* obj = lean_alloc_ctor(0, 0, sizeof(int32_t) + sizeof(uint8_t));
    lean_ctor_set_int32(obj, 0, status);
    lean_ctor_set_uint8(obj, sizeof(int32_t), value ? 1 : 0);
    return obj;
}

static lean_obj_res mk_int_result(int32_t status, int64_t value) {
    lean_object* obj = lean_alloc_ctor(0, 0, sizeof(int32_t) + sizeof(int64_t));
    /* Align int64 to 8 bytes: offset after int32 padded to 8 */
    lean_ctor_set_int32(obj, 0, status);
    lean_ctor_set_int64(obj, sizeof(int64_t), value);
    return obj;
}

static lean_obj_res mk_string_result_raw(int32_t status, size_t ptr, size_t len) {
    lean_object* obj = lean_alloc_ctor(0, 0, sizeof(int32_t) + 2 * sizeof(size_t));
    lean_ctor_set_int32(obj, 0, status);
    lean_ctor_set_usize(obj, sizeof(size_t), ptr);
    lean_ctor_set_usize(obj, 2 * sizeof(size_t), len);
    return obj;
}

/* ============================================================================
 * String marshaling: convert StringResultRaw -> Option String
 * ============================================================================ */

LEAN_EXPORT lean_obj_res lean_proven_marshal_string_result(
    lean_obj_arg raw_obj, lean_obj_arg /* w */
) {
    int32_t status = lean_ctor_get_int32(raw_obj, 0);
    size_t ptr_val = lean_ctor_get_usize(raw_obj, sizeof(size_t));
    size_t len_val = lean_ctor_get_usize(raw_obj, 2 * sizeof(size_t));
    lean_dec_ref(raw_obj);

    if (status != 0 || ptr_val == 0) {
        /* Return none */
        return lean_io_result_mk_ok(lean_box(0));
    }

    const char* cstr = (const char*)ptr_val;
    /* Create Lean string from C bytes */
    lean_object* lean_str = lean_mk_string_from_bytes(cstr, len_val);

    /* Free the C-allocated string */
    proven_free_string((char*)ptr_val);

    /* Return some lean_str */
    lean_object* some_obj = lean_alloc_ctor(1, 1, 0);
    lean_ctor_set(some_obj, 0, lean_str);
    return lean_io_result_mk_ok(some_obj);
}

/* ============================================================================
 * SafeString shims
 * ============================================================================ */

LEAN_EXPORT lean_obj_res lean_proven_string_is_valid_utf8(
    b_lean_obj_arg data, lean_obj_arg /* w */
) {
    size_t len = lean_sarray_size(data);
    const uint8_t* ptr = lean_sarray_cptr(data);
    ProvenBoolResult r = proven_string_is_valid_utf8(ptr, len);
    return lean_io_result_mk_ok(mk_bool_result(r.status, r.value));
}

LEAN_EXPORT lean_obj_res lean_proven_string_escape_sql(
    b_lean_obj_arg data, lean_obj_arg /* w */
) {
    size_t len = lean_sarray_size(data);
    const uint8_t* ptr = lean_sarray_cptr(data);
    ProvenStringResult r = proven_string_escape_sql(ptr, len);
    return lean_io_result_mk_ok(
        mk_string_result_raw(r.status, (size_t)r.value, r.length));
}

LEAN_EXPORT lean_obj_res lean_proven_string_escape_html(
    b_lean_obj_arg data, lean_obj_arg /* w */
) {
    size_t len = lean_sarray_size(data);
    const uint8_t* ptr = lean_sarray_cptr(data);
    ProvenStringResult r = proven_string_escape_html(ptr, len);
    return lean_io_result_mk_ok(
        mk_string_result_raw(r.status, (size_t)r.value, r.length));
}

LEAN_EXPORT lean_obj_res lean_proven_string_escape_js(
    b_lean_obj_arg data, lean_obj_arg /* w */
) {
    size_t len = lean_sarray_size(data);
    const uint8_t* ptr = lean_sarray_cptr(data);
    ProvenStringResult r = proven_string_escape_js(ptr, len);
    return lean_io_result_mk_ok(
        mk_string_result_raw(r.status, (size_t)r.value, r.length));
}

/* ============================================================================
 * SafePath shims
 * ============================================================================ */

LEAN_EXPORT lean_obj_res lean_proven_path_has_traversal(
    b_lean_obj_arg data, lean_obj_arg /* w */
) {
    size_t len = lean_sarray_size(data);
    const uint8_t* ptr = lean_sarray_cptr(data);
    ProvenBoolResult r = proven_path_has_traversal(ptr, len);
    return lean_io_result_mk_ok(mk_bool_result(r.status, r.value));
}

LEAN_EXPORT lean_obj_res lean_proven_path_sanitize_filename(
    b_lean_obj_arg data, lean_obj_arg /* w */
) {
    size_t len = lean_sarray_size(data);
    const uint8_t* ptr = lean_sarray_cptr(data);
    ProvenStringResult r = proven_path_sanitize_filename(ptr, len);
    return lean_io_result_mk_ok(
        mk_string_result_raw(r.status, (size_t)r.value, r.length));
}

/* ============================================================================
 * SafeEmail shims
 * ============================================================================ */

LEAN_EXPORT lean_obj_res lean_proven_email_is_valid(
    b_lean_obj_arg data, lean_obj_arg /* w */
) {
    size_t len = lean_sarray_size(data);
    const uint8_t* ptr = lean_sarray_cptr(data);
    ProvenBoolResult r = proven_email_is_valid(ptr, len);
    return lean_io_result_mk_ok(mk_bool_result(r.status, r.value));
}

/* ============================================================================
 * SafeUrl shims
 * ============================================================================ */

LEAN_EXPORT lean_obj_res lean_proven_url_parse(
    b_lean_obj_arg data, lean_obj_arg /* w */
) {
    size_t len = lean_sarray_size(data);
    const uint8_t* ptr = lean_sarray_cptr(data);
    ProvenUrlResult r = proven_url_parse(ptr, len);

    /* Build UrlComponentsRaw struct for Lean:
     * Fields: schemePtr, schemeLen, hostPtr, hostLen, port, hasPort,
     *         pathPtr, pathLen, queryPtr, queryLen, fragmentPtr, fragmentLen
     * Then wrap in UrlResultRaw: status + components
     */
    size_t scalar_size = sizeof(int32_t)       /* status */
                       + 12 * sizeof(size_t)    /* 6 ptr/len pairs */
                       + sizeof(uint16_t)       /* port */
                       + sizeof(uint8_t);       /* hasPort */

    lean_object* comps = lean_alloc_ctor(0, 0, 12 * sizeof(size_t) + sizeof(uint16_t) + sizeof(uint8_t));
    size_t off = 0;
    lean_ctor_set_usize(comps, off, (size_t)r.components.scheme);       off += sizeof(size_t);
    lean_ctor_set_usize(comps, off, r.components.scheme_len);           off += sizeof(size_t);
    lean_ctor_set_usize(comps, off, (size_t)r.components.host);         off += sizeof(size_t);
    lean_ctor_set_usize(comps, off, r.components.host_len);             off += sizeof(size_t);
    lean_ctor_set_uint16(comps, off, r.components.port);                off += sizeof(uint16_t);
    lean_ctor_set_uint8(comps, off, r.components.has_port ? 1 : 0);     off += sizeof(uint8_t);
    /* Pad to size_t alignment */
    off = ((off + sizeof(size_t) - 1) / sizeof(size_t)) * sizeof(size_t);
    lean_ctor_set_usize(comps, off, (size_t)r.components.path);         off += sizeof(size_t);
    lean_ctor_set_usize(comps, off, r.components.path_len);             off += sizeof(size_t);
    lean_ctor_set_usize(comps, off, (size_t)r.components.query);        off += sizeof(size_t);
    lean_ctor_set_usize(comps, off, r.components.query_len);            off += sizeof(size_t);
    lean_ctor_set_usize(comps, off, (size_t)r.components.fragment);     off += sizeof(size_t);
    lean_ctor_set_usize(comps, off, r.components.fragment_len);         off += sizeof(size_t);

    /* Wrap: UrlResultRaw = { status, components } */
    lean_object* result = lean_alloc_ctor(0, 1, sizeof(int32_t));
    lean_ctor_set(result, 0, comps);
    lean_ctor_set_int32(result, lean_ctor_num_objs(result) * sizeof(void*), r.status);

    return lean_io_result_mk_ok(result);
}

LEAN_EXPORT lean_obj_res lean_proven_url_free(
    lean_obj_arg comps_obj, lean_obj_arg /* w */
) {
    /* Extract pointers from the Lean UrlComponentsRaw and free them */
    size_t off = 0;
    size_t scheme_ptr = lean_ctor_get_usize(comps_obj, off); off += sizeof(size_t);
    off += sizeof(size_t); /* skip schemeLen */
    size_t host_ptr = lean_ctor_get_usize(comps_obj, off); off += sizeof(size_t);
    off += sizeof(size_t); /* skip hostLen */
    off += sizeof(uint16_t); /* skip port */
    off += sizeof(uint8_t);  /* skip hasPort */
    off = ((off + sizeof(size_t) - 1) / sizeof(size_t)) * sizeof(size_t);
    size_t path_ptr = lean_ctor_get_usize(comps_obj, off); off += sizeof(size_t);
    off += sizeof(size_t); /* skip pathLen */
    size_t query_ptr = lean_ctor_get_usize(comps_obj, off); off += sizeof(size_t);
    off += sizeof(size_t); /* skip queryLen */
    size_t frag_ptr = lean_ctor_get_usize(comps_obj, off);

    ProvenUrlComponents c;
    c.scheme = (char*)scheme_ptr;
    c.host = (char*)host_ptr;
    c.path = (char*)path_ptr;
    c.query = (char*)query_ptr;
    c.fragment = (char*)frag_ptr;
    proven_url_free(&c);

    lean_dec_ref(comps_obj);
    return lean_io_result_mk_ok(lean_box(0));
}

/* ============================================================================
 * SafeNetwork shims
 * ============================================================================ */

LEAN_EXPORT lean_obj_res lean_proven_network_parse_ipv4(
    b_lean_obj_arg data, lean_obj_arg /* w */
) {
    size_t len = lean_sarray_size(data);
    const uint8_t* ptr = lean_sarray_cptr(data);
    ProvenIPv4Result r = proven_network_parse_ipv4(ptr, len);

    /* IPv4Result = { status : Int32, address : IPv4Address }
     * IPv4Address = { a : UInt8, b : UInt8, c : UInt8, d : UInt8 }
     */
    lean_object* addr = lean_alloc_ctor(0, 0, 4);
    lean_ctor_set_uint8(addr, 0, r.address.octets[0]);
    lean_ctor_set_uint8(addr, 1, r.address.octets[1]);
    lean_ctor_set_uint8(addr, 2, r.address.octets[2]);
    lean_ctor_set_uint8(addr, 3, r.address.octets[3]);

    lean_object* result = lean_alloc_ctor(0, 1, sizeof(int32_t));
    lean_ctor_set(result, 0, addr);
    lean_ctor_set_int32(result, lean_ctor_num_objs(result) * sizeof(void*), r.status);

    return lean_io_result_mk_ok(result);
}

LEAN_EXPORT lean_obj_res lean_proven_network_ipv4_is_private(
    lean_obj_arg addr_obj, lean_obj_arg /* w */
) {
    ProvenIPv4Address addr;
    addr.octets[0] = lean_ctor_get_uint8(addr_obj, 0);
    addr.octets[1] = lean_ctor_get_uint8(addr_obj, 1);
    addr.octets[2] = lean_ctor_get_uint8(addr_obj, 2);
    addr.octets[3] = lean_ctor_get_uint8(addr_obj, 3);
    lean_dec_ref(addr_obj);

    bool result = proven_network_ipv4_is_private(addr);
    return lean_io_result_mk_ok(lean_box(result ? 1 : 0));
}

LEAN_EXPORT lean_obj_res lean_proven_network_ipv4_is_loopback(
    lean_obj_arg addr_obj, lean_obj_arg /* w */
) {
    ProvenIPv4Address addr;
    addr.octets[0] = lean_ctor_get_uint8(addr_obj, 0);
    addr.octets[1] = lean_ctor_get_uint8(addr_obj, 1);
    addr.octets[2] = lean_ctor_get_uint8(addr_obj, 2);
    addr.octets[3] = lean_ctor_get_uint8(addr_obj, 3);
    lean_dec_ref(addr_obj);

    bool result = proven_network_ipv4_is_loopback(addr);
    return lean_io_result_mk_ok(lean_box(result ? 1 : 0));
}

/* ============================================================================
 * SafeCrypto shims
 * ============================================================================ */

LEAN_EXPORT lean_obj_res lean_proven_crypto_constant_time_eq(
    b_lean_obj_arg a, b_lean_obj_arg b, lean_obj_arg /* w */
) {
    size_t a_len = lean_sarray_size(a);
    const uint8_t* a_ptr = lean_sarray_cptr(a);
    size_t b_len = lean_sarray_size(b);
    const uint8_t* b_ptr = lean_sarray_cptr(b);
    ProvenBoolResult r = proven_crypto_constant_time_eq(a_ptr, a_len, b_ptr, b_len);
    return lean_io_result_mk_ok(mk_bool_result(r.status, r.value));
}

LEAN_EXPORT lean_obj_res lean_proven_crypto_random_bytes(
    size_t len, lean_obj_arg /* w */
) {
    lean_object* arr = lean_alloc_sarray(1, len, len);
    uint8_t* ptr = lean_sarray_cptr(arr);
    int32_t status = proven_crypto_random_bytes(ptr, len);
    if (status != 0) {
        /* Zero-fill on error for safety */
        memset(ptr, 0, len);
    }
    return lean_io_result_mk_ok(arr);
}

/* ============================================================================
 * SafeJson shims
 * ============================================================================ */

LEAN_EXPORT lean_obj_res lean_proven_json_is_valid(
    b_lean_obj_arg data, lean_obj_arg /* w */
) {
    size_t len = lean_sarray_size(data);
    const uint8_t* ptr = lean_sarray_cptr(data);
    ProvenBoolResult r = proven_json_is_valid(ptr, len);
    return lean_io_result_mk_ok(mk_bool_result(r.status, r.value));
}

LEAN_EXPORT lean_obj_res lean_proven_json_get_type(
    b_lean_obj_arg data, lean_obj_arg /* w */
) {
    size_t len = lean_sarray_size(data);
    const uint8_t* ptr = lean_sarray_cptr(data);
    /* proven_json_get_type returns a ProvenJsonType enum (int32_t) */
    int32_t result = (int32_t)proven_json_get_type(ptr, len);
    return lean_io_result_mk_ok(lean_box(result));
}

/* ============================================================================
 * SafeDateTime shims
 * ============================================================================ */

LEAN_EXPORT lean_obj_res lean_proven_datetime_parse(
    b_lean_obj_arg data, lean_obj_arg /* w */
) {
    size_t len = lean_sarray_size(data);
    const uint8_t* ptr = lean_sarray_cptr(data);
    ProvenDateTimeResult r = proven_datetime_parse(ptr, len);

    /* DateTime = { year:Int32, month:UInt8, day:UInt8, hour:UInt8,
     *              minute:UInt8, second:UInt8, nanosecond:UInt32,
     *              tzOffsetMinutes:Int16 } */
    size_t dt_scalar_size = sizeof(int32_t) + 5 * sizeof(uint8_t) +
                            sizeof(uint32_t) + sizeof(int16_t);
    lean_object* dt = lean_alloc_ctor(0, 0, dt_scalar_size);
    size_t off = 0;
    lean_ctor_set_int32(dt, off, r.datetime.year);      off += sizeof(int32_t);
    lean_ctor_set_uint8(dt, off, r.datetime.month);     off += sizeof(uint8_t);
    lean_ctor_set_uint8(dt, off, r.datetime.day);       off += sizeof(uint8_t);
    lean_ctor_set_uint8(dt, off, r.datetime.hour);      off += sizeof(uint8_t);
    lean_ctor_set_uint8(dt, off, r.datetime.minute);    off += sizeof(uint8_t);
    lean_ctor_set_uint8(dt, off, r.datetime.second);    off += sizeof(uint8_t);
    /* Pad to uint32 alignment */
    off = ((off + 3) / 4) * 4;
    lean_ctor_set_uint32(dt, off, r.datetime.nanosecond); off += sizeof(uint32_t);
    lean_ctor_set_uint16(dt, off, (uint16_t)r.datetime.tz_offset_minutes);

    /* DateTimeResult = { status : Int32, datetime : DateTime } */
    lean_object* result = lean_alloc_ctor(0, 1, sizeof(int32_t));
    lean_ctor_set(result, 0, dt);
    lean_ctor_set_int32(result, lean_ctor_num_objs(result) * sizeof(void*), r.status);

    return lean_io_result_mk_ok(result);
}

LEAN_EXPORT lean_obj_res lean_proven_datetime_format_iso8601(
    lean_obj_arg dt_obj, lean_obj_arg /* w */
) {
    /* Extract DateTime fields from Lean object */
    ProvenDateTime dt;
    size_t off = 0;
    dt.year   = lean_ctor_get_int32(dt_obj, off);  off += sizeof(int32_t);
    dt.month  = lean_ctor_get_uint8(dt_obj, off);  off += sizeof(uint8_t);
    dt.day    = lean_ctor_get_uint8(dt_obj, off);  off += sizeof(uint8_t);
    dt.hour   = lean_ctor_get_uint8(dt_obj, off);  off += sizeof(uint8_t);
    dt.minute = lean_ctor_get_uint8(dt_obj, off);  off += sizeof(uint8_t);
    dt.second = lean_ctor_get_uint8(dt_obj, off);  off += sizeof(uint8_t);
    off = ((off + 3) / 4) * 4;
    dt.nanosecond = lean_ctor_get_uint32(dt_obj, off);      off += sizeof(uint32_t);
    dt.tz_offset_minutes = (int16_t)lean_ctor_get_uint16(dt_obj, off);
    lean_dec_ref(dt_obj);

    ProvenStringResult r = proven_datetime_format_iso8601(dt);
    return lean_io_result_mk_ok(
        mk_string_result_raw(r.status, (size_t)r.value, r.length));
}

/* ============================================================================
 * SafeHex shims
 * ============================================================================ */

LEAN_EXPORT lean_obj_res lean_proven_hex_encode(
    b_lean_obj_arg data, uint8_t uppercase, lean_obj_arg /* w */
) {
    size_t len = lean_sarray_size(data);
    const uint8_t* ptr = lean_sarray_cptr(data);
    ProvenStringResult r = proven_hex_encode(ptr, len, uppercase != 0);
    return lean_io_result_mk_ok(
        mk_string_result_raw(r.status, (size_t)r.value, r.length));
}

LEAN_EXPORT lean_obj_res lean_proven_hex_decode(
    b_lean_obj_arg data, lean_obj_arg /* w */
) {
    size_t len = lean_sarray_size(data);
    const uint8_t* ptr = lean_sarray_cptr(data);
    ProvenHexDecodeResult r = proven_hex_decode(ptr, len);

    /* Marshal as StringResultRaw so the Lean side can use marshalStringResult */
    lean_obj_res result = mk_string_result_raw(r.status, (size_t)r.data, r.length);

    /* Note: We transfer ownership of r.data to the marshal function, which
     * will call proven_free_string on it. The hex_decode result uses the
     * same allocator so proven_free_string is correct here. */
    return lean_io_result_mk_ok(result);
}

/* ============================================================================
 * SafeChecksum shims
 * ============================================================================ */

LEAN_EXPORT lean_obj_res lean_proven_checksum_crc32(
    b_lean_obj_arg data, lean_obj_arg /* w */
) {
    size_t len = lean_sarray_size(data);
    const uint8_t* ptr = lean_sarray_cptr(data);
    ProvenIntResult r = proven_checksum_crc32(ptr, len);
    return lean_io_result_mk_ok(mk_int_result(r.status, r.value));
}

LEAN_EXPORT lean_obj_res lean_proven_checksum_verify_crc32(
    b_lean_obj_arg data, uint32_t expected, lean_obj_arg /* w */
) {
    size_t len = lean_sarray_size(data);
    const uint8_t* ptr = lean_sarray_cptr(data);
    ProvenBoolResult r = proven_checksum_verify_crc32(ptr, len, expected);
    return lean_io_result_mk_ok(mk_bool_result(r.status, r.value));
}
