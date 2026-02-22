/* SPDX-License-Identifier: PMPL-1.0-or-later */
/* Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk> */
/*
 * proven_wren.c - Wren foreign method implementations for libproven.
 *
 * This C module registers Wren foreign methods that call libproven.
 * All computation is performed in verified Idris 2 code via the Zig FFI
 * layer. This file is a thin bridge; it does NOT reimplement any logic.
 *
 * Compile: cc -shared -o proven_wren.so proven_wren.c -lproven -lwren
 */

#include <string.h>
#include <stdlib.h>
#include <proven.h>
#include <wren.h>

/* ========================================================================
 * Lifecycle
 * ======================================================================== */

/**
 * Proven.init() -> Bool
 * Initialize the Proven runtime. Returns true on success.
 */
static void provenInit(WrenVM* vm)
{
    int32_t status = proven_init();
    wrenSetSlotBool(vm, 0, status == PROVEN_OK);
}

/**
 * Proven.deinit()
 * Cleanup the Proven runtime.
 */
static void provenDeinit(WrenVM* vm)
{
    proven_deinit();
}

/**
 * Proven.isInitialized -> Bool
 * Check if the runtime is initialized.
 */
static void provenIsInitialized(WrenVM* vm)
{
    wrenSetSlotBool(vm, 0, proven_is_initialized());
}

/**
 * Proven.version -> String
 * Get the library version string.
 */
static void provenVersion(WrenVM* vm)
{
    char buf[32];
    snprintf(buf, sizeof(buf), "%u.%u.%u",
             proven_version_major(),
             proven_version_minor(),
             proven_version_patch());
    wrenSetSlotString(vm, 0, buf);
}

/**
 * Proven.abiVersion -> Num
 * Get the FFI ABI version.
 */
static void provenAbiVersion(WrenVM* vm)
{
    wrenSetSlotDouble(vm, 0, (double)proven_ffi_abi_version());
}

/**
 * Proven.moduleCount -> Num
 * Get the total module count.
 */
static void provenModuleCount(WrenVM* vm)
{
    wrenSetSlotDouble(vm, 0, (double)proven_module_count());
}

/* ========================================================================
 * SafeMath
 * ======================================================================== */

/**
 * SafeMath.addChecked(a, b) -> Num or null
 * Checked addition with overflow detection via libproven.
 */
static void safeMathAddChecked(WrenVM* vm)
{
    int64_t a = (int64_t)wrenGetSlotDouble(vm, 1);
    int64_t b = (int64_t)wrenGetSlotDouble(vm, 2);
    ProvenIntResult r = proven_math_add_checked(a, b);
    if (r.status != PROVEN_OK) {
        wrenSetSlotNull(vm, 0);
    } else {
        wrenSetSlotDouble(vm, 0, (double)r.value);
    }
}

/**
 * SafeMath.subChecked(a, b) -> Num or null
 * Checked subtraction with underflow detection via libproven.
 */
static void safeMathSubChecked(WrenVM* vm)
{
    int64_t a = (int64_t)wrenGetSlotDouble(vm, 1);
    int64_t b = (int64_t)wrenGetSlotDouble(vm, 2);
    ProvenIntResult r = proven_math_sub_checked(a, b);
    if (r.status != PROVEN_OK) {
        wrenSetSlotNull(vm, 0);
    } else {
        wrenSetSlotDouble(vm, 0, (double)r.value);
    }
}

/**
 * SafeMath.mulChecked(a, b) -> Num or null
 * Checked multiplication with overflow detection via libproven.
 */
static void safeMathMulChecked(WrenVM* vm)
{
    int64_t a = (int64_t)wrenGetSlotDouble(vm, 1);
    int64_t b = (int64_t)wrenGetSlotDouble(vm, 2);
    ProvenIntResult r = proven_math_mul_checked(a, b);
    if (r.status != PROVEN_OK) {
        wrenSetSlotNull(vm, 0);
    } else {
        wrenSetSlotDouble(vm, 0, (double)r.value);
    }
}

/**
 * SafeMath.div(a, b) -> Num or null
 * Safe integer division via libproven.
 */
static void safeMathDiv(WrenVM* vm)
{
    int64_t a = (int64_t)wrenGetSlotDouble(vm, 1);
    int64_t b = (int64_t)wrenGetSlotDouble(vm, 2);
    ProvenIntResult r = proven_math_div(a, b);
    if (r.status != PROVEN_OK) {
        wrenSetSlotNull(vm, 0);
    } else {
        wrenSetSlotDouble(vm, 0, (double)r.value);
    }
}

/**
 * SafeMath.mod(a, b) -> Num or null
 * Safe modulo via libproven.
 */
static void safeMathMod(WrenVM* vm)
{
    int64_t a = (int64_t)wrenGetSlotDouble(vm, 1);
    int64_t b = (int64_t)wrenGetSlotDouble(vm, 2);
    ProvenIntResult r = proven_math_mod(a, b);
    if (r.status != PROVEN_OK) {
        wrenSetSlotNull(vm, 0);
    } else {
        wrenSetSlotDouble(vm, 0, (double)r.value);
    }
}

/**
 * SafeMath.absSafe(n) -> Num or null
 * Safe absolute value via libproven.
 */
static void safeMathAbsSafe(WrenVM* vm)
{
    int64_t n = (int64_t)wrenGetSlotDouble(vm, 1);
    ProvenIntResult r = proven_math_abs_safe(n);
    if (r.status != PROVEN_OK) {
        wrenSetSlotNull(vm, 0);
    } else {
        wrenSetSlotDouble(vm, 0, (double)r.value);
    }
}

/**
 * SafeMath.clamp(lo, hi, value) -> Num
 * Clamp value to [lo, hi] via libproven.
 */
static void safeMathClamp(WrenVM* vm)
{
    int64_t lo = (int64_t)wrenGetSlotDouble(vm, 1);
    int64_t hi = (int64_t)wrenGetSlotDouble(vm, 2);
    int64_t value = (int64_t)wrenGetSlotDouble(vm, 3);
    wrenSetSlotDouble(vm, 0, (double)proven_math_clamp(lo, hi, value));
}

/**
 * SafeMath.powChecked(base, exp) -> Num or null
 * Checked exponentiation via libproven.
 */
static void safeMathPowChecked(WrenVM* vm)
{
    int64_t base = (int64_t)wrenGetSlotDouble(vm, 1);
    uint32_t exp = (uint32_t)wrenGetSlotDouble(vm, 2);
    ProvenIntResult r = proven_math_pow_checked(base, exp);
    if (r.status != PROVEN_OK) {
        wrenSetSlotNull(vm, 0);
    } else {
        wrenSetSlotDouble(vm, 0, (double)r.value);
    }
}

/* ========================================================================
 * SafeString
 * ======================================================================== */

/**
 * SafeString.isValidUtf8(s) -> Bool or null
 * Check if string is valid UTF-8 via libproven.
 */
static void safeStringIsValidUtf8(WrenVM* vm)
{
    const char* s = wrenGetSlotString(vm, 1);
    size_t len = strlen(s);
    ProvenBoolResult r = proven_string_is_valid_utf8((const uint8_t*)s, len);
    if (r.status != PROVEN_OK) {
        wrenSetSlotNull(vm, 0);
    } else {
        wrenSetSlotBool(vm, 0, r.value);
    }
}

/**
 * SafeString.escapeHtml(s) -> String or null
 * Escape string for HTML (XSS prevention) via libproven.
 */
static void safeStringEscapeHtml(WrenVM* vm)
{
    const char* s = wrenGetSlotString(vm, 1);
    size_t len = strlen(s);
    ProvenStringResult r = proven_string_escape_html((const uint8_t*)s, len);
    if (r.status != PROVEN_OK || r.value == NULL) {
        wrenSetSlotNull(vm, 0);
    } else {
        wrenSetSlotBytes(vm, 0, r.value, r.length);
        proven_free_string(r.value);
    }
}

/**
 * SafeString.escapeSql(s) -> String or null
 * Escape string for SQL via libproven.
 */
static void safeStringEscapeSql(WrenVM* vm)
{
    const char* s = wrenGetSlotString(vm, 1);
    size_t len = strlen(s);
    ProvenStringResult r = proven_string_escape_sql((const uint8_t*)s, len);
    if (r.status != PROVEN_OK || r.value == NULL) {
        wrenSetSlotNull(vm, 0);
    } else {
        wrenSetSlotBytes(vm, 0, r.value, r.length);
        proven_free_string(r.value);
    }
}

/**
 * SafeString.escapeJs(s) -> String or null
 * Escape string for JavaScript via libproven.
 */
static void safeStringEscapeJs(WrenVM* vm)
{
    const char* s = wrenGetSlotString(vm, 1);
    size_t len = strlen(s);
    ProvenStringResult r = proven_string_escape_js((const uint8_t*)s, len);
    if (r.status != PROVEN_OK || r.value == NULL) {
        wrenSetSlotNull(vm, 0);
    } else {
        wrenSetSlotBytes(vm, 0, r.value, r.length);
        proven_free_string(r.value);
    }
}

/* ========================================================================
 * SafeEmail
 * ======================================================================== */

/**
 * SafeEmail.isValid(s) -> Bool or null
 * Validate email address via libproven.
 */
static void safeEmailIsValid(WrenVM* vm)
{
    const char* s = wrenGetSlotString(vm, 1);
    size_t len = strlen(s);
    ProvenBoolResult r = proven_email_is_valid((const uint8_t*)s, len);
    if (r.status != PROVEN_OK) {
        wrenSetSlotNull(vm, 0);
    } else {
        wrenSetSlotBool(vm, 0, r.value);
    }
}

/* ========================================================================
 * SafeUrl
 * ======================================================================== */

/**
 * SafeUrl.parse(s) -> List or null
 * Parse URL into [scheme, host, port, path, query, fragment] via libproven.
 * Port is -1 if not specified.
 */
static void safeUrlParse(WrenVM* vm)
{
    const char* s = wrenGetSlotString(vm, 1);
    size_t len = strlen(s);
    ProvenUrlResult r = proven_url_parse((const uint8_t*)s, len);
    if (r.status != PROVEN_OK) {
        wrenSetSlotNull(vm, 0);
        return;
    }

    wrenEnsureSlots(vm, 8);
    wrenSetSlotNewList(vm, 0);

    /* scheme */
    if (r.components.scheme && r.components.scheme_len > 0) {
        wrenSetSlotBytes(vm, 1, r.components.scheme, r.components.scheme_len);
    } else {
        wrenSetSlotString(vm, 1, "");
    }
    wrenInsertInList(vm, 0, -1, 1);

    /* host */
    if (r.components.host && r.components.host_len > 0) {
        wrenSetSlotBytes(vm, 1, r.components.host, r.components.host_len);
    } else {
        wrenSetSlotString(vm, 1, "");
    }
    wrenInsertInList(vm, 0, -1, 1);

    /* port */
    if (r.components.has_port) {
        wrenSetSlotDouble(vm, 1, (double)r.components.port);
    } else {
        wrenSetSlotNull(vm, 1);
    }
    wrenInsertInList(vm, 0, -1, 1);

    /* path */
    if (r.components.path && r.components.path_len > 0) {
        wrenSetSlotBytes(vm, 1, r.components.path, r.components.path_len);
    } else {
        wrenSetSlotString(vm, 1, "");
    }
    wrenInsertInList(vm, 0, -1, 1);

    /* query */
    if (r.components.query && r.components.query_len > 0) {
        wrenSetSlotBytes(vm, 1, r.components.query, r.components.query_len);
    } else {
        wrenSetSlotString(vm, 1, "");
    }
    wrenInsertInList(vm, 0, -1, 1);

    /* fragment */
    if (r.components.fragment && r.components.fragment_len > 0) {
        wrenSetSlotBytes(vm, 1, r.components.fragment, r.components.fragment_len);
    } else {
        wrenSetSlotString(vm, 1, "");
    }
    wrenInsertInList(vm, 0, -1, 1);

    proven_url_free(&r.components);
}

/* ========================================================================
 * SafeCrypto
 * ======================================================================== */

/**
 * SafeCrypto.constantTimeEq(a, b) -> Bool or null
 * Constant-time comparison via libproven.
 */
static void safeCryptoConstantTimeEq(WrenVM* vm)
{
    int aLen, bLen;
    const char* a = wrenGetSlotBytes(vm, 1, &aLen);
    const char* b = wrenGetSlotBytes(vm, 2, &bLen);
    ProvenBoolResult r = proven_crypto_constant_time_eq(
        (const uint8_t*)a, (size_t)aLen,
        (const uint8_t*)b, (size_t)bLen);
    if (r.status != PROVEN_OK) {
        wrenSetSlotNull(vm, 0);
    } else {
        wrenSetSlotBool(vm, 0, r.value);
    }
}

/* ========================================================================
 * SafeJson
 * ======================================================================== */

/**
 * SafeJson.isValid(s) -> Bool or null
 * Check if string is valid JSON via libproven.
 */
static void safeJsonIsValid(WrenVM* vm)
{
    const char* s = wrenGetSlotString(vm, 1);
    size_t len = strlen(s);
    ProvenBoolResult r = proven_json_is_valid((const uint8_t*)s, len);
    if (r.status != PROVEN_OK) {
        wrenSetSlotNull(vm, 0);
    } else {
        wrenSetSlotBool(vm, 0, r.value);
    }
}

/**
 * SafeJson.getType(s) -> String
 * Get JSON root value type as string via libproven.
 */
static void safeJsonGetType(WrenVM* vm)
{
    const char* s = wrenGetSlotString(vm, 1);
    size_t len = strlen(s);
    ProvenJsonType t = proven_json_get_type((const uint8_t*)s, len);
    switch (t) {
        case PROVEN_JSON_NULL:    wrenSetSlotString(vm, 0, "null");    break;
        case PROVEN_JSON_BOOL:    wrenSetSlotString(vm, 0, "boolean"); break;
        case PROVEN_JSON_NUMBER:  wrenSetSlotString(vm, 0, "number");  break;
        case PROVEN_JSON_STRING:  wrenSetSlotString(vm, 0, "string");  break;
        case PROVEN_JSON_ARRAY:   wrenSetSlotString(vm, 0, "array");   break;
        case PROVEN_JSON_OBJECT:  wrenSetSlotString(vm, 0, "object");  break;
        default:                  wrenSetSlotString(vm, 0, "invalid"); break;
    }
}

/* ========================================================================
 * SafePath (via SafeString module for Wren)
 * ======================================================================== */

/**
 * SafeString.hasTraversal(s) -> Bool or null
 * Check for directory traversal via libproven.
 */
static void safeStringHasTraversal(WrenVM* vm)
{
    const char* s = wrenGetSlotString(vm, 1);
    size_t len = strlen(s);
    ProvenBoolResult r = proven_path_has_traversal((const uint8_t*)s, len);
    if (r.status != PROVEN_OK) {
        wrenSetSlotNull(vm, 0);
    } else {
        wrenSetSlotBool(vm, 0, r.value);
    }
}

/**
 * SafeString.sanitizeFilename(s) -> String or null
 * Sanitize a filename via libproven.
 */
static void safeStringSanitizeFilename(WrenVM* vm)
{
    const char* s = wrenGetSlotString(vm, 1);
    size_t len = strlen(s);
    ProvenStringResult r = proven_path_sanitize_filename((const uint8_t*)s, len);
    if (r.status != PROVEN_OK || r.value == NULL) {
        wrenSetSlotNull(vm, 0);
    } else {
        wrenSetSlotBytes(vm, 0, r.value, r.length);
        proven_free_string(r.value);
    }
}

/* ========================================================================
 * Foreign method binding function
 * ======================================================================== */

/**
 * Bind foreign methods for the "proven" module.
 * Called by the Wren VM to resolve foreign method implementations.
 */
WrenForeignMethodFn provenBindForeignMethod(
    WrenVM* vm,
    const char* module,
    const char* className,
    bool isStatic,
    const char* signature)
{
    if (strcmp(module, "proven") != 0) return NULL;

    /* Proven lifecycle */
    if (strcmp(className, "Proven") == 0 && isStatic) {
        if (strcmp(signature, "init()") == 0)            return provenInit;
        if (strcmp(signature, "deinit()") == 0)          return provenDeinit;
        if (strcmp(signature, "isInitialized") == 0)     return provenIsInitialized;
        if (strcmp(signature, "version") == 0)           return provenVersion;
        if (strcmp(signature, "abiVersion") == 0)        return provenAbiVersion;
        if (strcmp(signature, "moduleCount") == 0)       return provenModuleCount;
    }

    /* SafeMath */
    if (strcmp(className, "SafeMath") == 0 && isStatic) {
        if (strcmp(signature, "addChecked(_,_)") == 0)   return safeMathAddChecked;
        if (strcmp(signature, "subChecked(_,_)") == 0)   return safeMathSubChecked;
        if (strcmp(signature, "mulChecked(_,_)") == 0)   return safeMathMulChecked;
        if (strcmp(signature, "div(_,_)") == 0)          return safeMathDiv;
        if (strcmp(signature, "mod(_,_)") == 0)          return safeMathMod;
        if (strcmp(signature, "absSafe(_)") == 0)        return safeMathAbsSafe;
        if (strcmp(signature, "clamp(_,_,_)") == 0)      return safeMathClamp;
        if (strcmp(signature, "powChecked(_,_)") == 0)   return safeMathPowChecked;
    }

    /* SafeString */
    if (strcmp(className, "SafeString") == 0 && isStatic) {
        if (strcmp(signature, "isValidUtf8(_)") == 0)      return safeStringIsValidUtf8;
        if (strcmp(signature, "escapeHtml(_)") == 0)        return safeStringEscapeHtml;
        if (strcmp(signature, "escapeSql(_)") == 0)         return safeStringEscapeSql;
        if (strcmp(signature, "escapeJs(_)") == 0)          return safeStringEscapeJs;
        if (strcmp(signature, "hasTraversal(_)") == 0)      return safeStringHasTraversal;
        if (strcmp(signature, "sanitizeFilename(_)") == 0)  return safeStringSanitizeFilename;
    }

    /* SafeEmail */
    if (strcmp(className, "SafeEmail") == 0 && isStatic) {
        if (strcmp(signature, "isValid(_)") == 0)         return safeEmailIsValid;
    }

    /* SafeUrl */
    if (strcmp(className, "SafeUrl") == 0 && isStatic) {
        if (strcmp(signature, "parse(_)") == 0)           return safeUrlParse;
    }

    /* SafeCrypto */
    if (strcmp(className, "SafeCrypto") == 0 && isStatic) {
        if (strcmp(signature, "constantTimeEq(_,_)") == 0) return safeCryptoConstantTimeEq;
    }

    /* SafeJson */
    if (strcmp(className, "SafeJson") == 0 && isStatic) {
        if (strcmp(signature, "isValid(_)") == 0)         return safeJsonIsValid;
        if (strcmp(signature, "getType(_)") == 0)         return safeJsonGetType;
    }

    return NULL;
}
