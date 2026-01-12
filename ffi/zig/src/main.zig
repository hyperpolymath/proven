// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

//! Proven FFI - Stable C ABI for verified safety functions
//!
//! This module provides FFI exports for all Proven modules, allowing
//! safe arithmetic, string handling, JSON parsing, and more from any
//! language that can call C functions.
//!
//! Built on idris2-zig-ffi for Idris 2 runtime integration.

const std = @import("std");
const ffi = @import("idris2_zig_ffi");

// Re-export idris2-zig-ffi types for Zig callers
pub const IdrisString = ffi.idris_rts.IdrisString;
pub const IdrisValue = ffi.idris_rts.IdrisValue;
pub const types = ffi.types;
pub const memory = ffi.memory;

// ============================================================================
// Result Types
// ============================================================================

/// Result status codes
pub const ProvenStatus = enum(i32) {
    ok = 0,
    err_null_pointer = -1,
    err_invalid_argument = -2,
    err_overflow = -3,
    err_underflow = -4,
    err_division_by_zero = -5,
    err_parse_failure = -6,
    err_validation_failed = -7,
    err_out_of_bounds = -8,
    err_encoding_error = -9,
    err_allocation_failed = -10,
    err_not_implemented = -99,
};

/// Result for integer operations
pub const IntResult = extern struct {
    status: ProvenStatus,
    value: i64,
};

/// Result for boolean operations
pub const BoolResult = extern struct {
    status: ProvenStatus,
    value: bool,
};

/// Result for string operations (caller must free the string)
pub const StringResult = extern struct {
    status: ProvenStatus,
    value: ?[*:0]u8,
    length: usize,
};

// ============================================================================
// Allocator for FFI strings
// ============================================================================

var gpa = std.heap.GeneralPurposeAllocator(.{}){};
const allocator = gpa.allocator();

/// Free a string allocated by proven functions
export fn proven_free_string(ptr: ?[*:0]u8) void {
    if (ptr) |p| {
        // Find length by scanning for null terminator
        var len: usize = 0;
        while (p[len] != 0) : (len += 1) {}
        allocator.free(p[0 .. len + 1]);
    }
}

// ============================================================================
// SafeMath - Arithmetic operations that cannot crash
// ============================================================================

/// Safe division: returns status and result
/// Returns err_division_by_zero if denominator is 0
export fn proven_math_div(numerator: i64, denominator: i64) IntResult {
    if (denominator == 0) {
        return .{ .status = .err_division_by_zero, .value = 0 };
    }
    // Handle MIN_INT / -1 overflow case
    if (numerator == std.math.minInt(i64) and denominator == -1) {
        return .{ .status = .err_overflow, .value = 0 };
    }
    return .{ .status = .ok, .value = @divTrunc(numerator, denominator) };
}

/// Safe modulo: returns status and result
export fn proven_math_mod(numerator: i64, denominator: i64) IntResult {
    if (denominator == 0) {
        return .{ .status = .err_division_by_zero, .value = 0 };
    }
    return .{ .status = .ok, .value = @mod(numerator, denominator) };
}

/// Checked addition: returns err_overflow if result would overflow
export fn proven_math_add_checked(a: i64, b: i64) IntResult {
    const result = @addWithOverflow(a, b);
    if (result[1] != 0) {
        return .{ .status = .err_overflow, .value = 0 };
    }
    return .{ .status = .ok, .value = result[0] };
}

/// Checked subtraction: returns err_underflow if result would underflow
export fn proven_math_sub_checked(a: i64, b: i64) IntResult {
    const result = @subWithOverflow(a, b);
    if (result[1] != 0) {
        return .{ .status = .err_underflow, .value = 0 };
    }
    return .{ .status = .ok, .value = result[0] };
}

/// Checked multiplication: returns err_overflow if result would overflow
export fn proven_math_mul_checked(a: i64, b: i64) IntResult {
    const result = @mulWithOverflow(a, b);
    if (result[1] != 0) {
        return .{ .status = .err_overflow, .value = 0 };
    }
    return .{ .status = .ok, .value = result[0] };
}

/// Safe absolute value: handles MIN_INT correctly
export fn proven_math_abs_safe(n: i64) IntResult {
    if (n == std.math.minInt(i64)) {
        return .{ .status = .err_overflow, .value = 0 };
    }
    return .{ .status = .ok, .value = if (n < 0) -n else n };
}

/// Clamp value to range [lo, hi]
export fn proven_math_clamp(lo: i64, hi: i64, value: i64) i64 {
    if (value < lo) return lo;
    if (value > hi) return hi;
    return value;
}

/// Integer power with overflow checking
export fn proven_math_pow_checked(base: i64, exp: u32) IntResult {
    if (exp == 0) return .{ .status = .ok, .value = 1 };
    if (base == 0) return .{ .status = .ok, .value = 0 };
    if (base == 1) return .{ .status = .ok, .value = 1 };
    if (base == -1) return .{ .status = .ok, .value = if (exp % 2 == 0) 1 else -1 };

    var result: i64 = 1;
    var b = base;
    var e = exp;

    while (e > 0) {
        if (e % 2 == 1) {
            const mul_result = @mulWithOverflow(result, b);
            if (mul_result[1] != 0) {
                return .{ .status = .err_overflow, .value = 0 };
            }
            result = mul_result[0];
        }
        e /= 2;
        if (e > 0) {
            const mul_result = @mulWithOverflow(b, b);
            if (mul_result[1] != 0) {
                return .{ .status = .err_overflow, .value = 0 };
            }
            b = mul_result[0];
        }
    }

    return .{ .status = .ok, .value = result };
}

// ============================================================================
// SafeString - Text operations that handle encoding safely
// ============================================================================

/// Check if a byte sequence is valid UTF-8
export fn proven_string_is_valid_utf8(ptr: ?[*]const u8, len: usize) BoolResult {
    if (ptr == null) {
        return .{ .status = .err_null_pointer, .value = false };
    }
    const slice = ptr.?[0..len];
    const valid = std.unicode.utf8ValidateSlice(slice);
    return .{ .status = .ok, .value = valid };
}

/// Escape string for SQL (single quotes)
export fn proven_string_escape_sql(ptr: ?[*]const u8, len: usize) StringResult {
    if (ptr == null) {
        return .{ .status = .err_null_pointer, .value = null, .length = 0 };
    }
    const input = ptr.?[0..len];

    // Count single quotes to determine output size
    var quote_count: usize = 0;
    for (input) |c| {
        if (c == '\'') quote_count += 1;
    }

    const output_len = len + quote_count;
    const output = allocator.allocSentinel(u8, output_len, 0) catch {
        return .{ .status = .err_allocation_failed, .value = null, .length = 0 };
    };

    var j: usize = 0;
    for (input) |c| {
        if (c == '\'') {
            output[j] = '\'';
            j += 1;
            output[j] = '\'';
        } else {
            output[j] = c;
        }
        j += 1;
    }

    return .{ .status = .ok, .value = output.ptr, .length = output_len };
}

/// Escape string for HTML (< > & " ')
export fn proven_string_escape_html(ptr: ?[*]const u8, len: usize) StringResult {
    if (ptr == null) {
        return .{ .status = .err_null_pointer, .value = null, .length = 0 };
    }
    const input = ptr.?[0..len];

    // Calculate output size
    var output_len: usize = 0;
    for (input) |c| {
        output_len += switch (c) {
            '<' => 4, // &lt;
            '>' => 4, // &gt;
            '&' => 5, // &amp;
            '"' => 6, // &quot;
            '\'' => 5, // &#39;
            else => 1,
        };
    }

    const output = allocator.allocSentinel(u8, output_len, 0) catch {
        return .{ .status = .err_allocation_failed, .value = null, .length = 0 };
    };

    var j: usize = 0;
    for (input) |c| {
        switch (c) {
            '<' => {
                @memcpy(output[j..][0..4], "&lt;");
                j += 4;
            },
            '>' => {
                @memcpy(output[j..][0..4], "&gt;");
                j += 4;
            },
            '&' => {
                @memcpy(output[j..][0..5], "&amp;");
                j += 5;
            },
            '"' => {
                @memcpy(output[j..][0..6], "&quot;");
                j += 6;
            },
            '\'' => {
                @memcpy(output[j..][0..5], "&#39;");
                j += 5;
            },
            else => {
                output[j] = c;
                j += 1;
            },
        }
    }

    return .{ .status = .ok, .value = output.ptr, .length = output_len };
}

/// Escape string for JavaScript (within quotes)
export fn proven_string_escape_js(ptr: ?[*]const u8, len: usize) StringResult {
    if (ptr == null) {
        return .{ .status = .err_null_pointer, .value = null, .length = 0 };
    }
    const input = ptr.?[0..len];

    // Calculate output size
    var output_len: usize = 0;
    for (input) |c| {
        output_len += switch (c) {
            '\\', '"', '\'' => 2,
            '\n' => 2, // \n
            '\r' => 2, // \r
            '\t' => 2, // \t
            else => 1,
        };
    }

    const output = allocator.allocSentinel(u8, output_len, 0) catch {
        return .{ .status = .err_allocation_failed, .value = null, .length = 0 };
    };

    var j: usize = 0;
    for (input) |c| {
        switch (c) {
            '\\' => {
                output[j] = '\\';
                output[j + 1] = '\\';
                j += 2;
            },
            '"' => {
                output[j] = '\\';
                output[j + 1] = '"';
                j += 2;
            },
            '\'' => {
                output[j] = '\\';
                output[j + 1] = '\'';
                j += 2;
            },
            '\n' => {
                output[j] = '\\';
                output[j + 1] = 'n';
                j += 2;
            },
            '\r' => {
                output[j] = '\\';
                output[j + 1] = 'r';
                j += 2;
            },
            '\t' => {
                output[j] = '\\';
                output[j + 1] = 't';
                j += 2;
            },
            else => {
                output[j] = c;
                j += 1;
            },
        }
    }

    return .{ .status = .ok, .value = output.ptr, .length = output_len };
}

// ============================================================================
// SafePath - Filesystem operations that prevent traversal attacks
// ============================================================================

/// Check if a path attempts directory traversal
export fn proven_path_has_traversal(ptr: ?[*]const u8, len: usize) BoolResult {
    if (ptr == null) {
        return .{ .status = .err_null_pointer, .value = false };
    }
    const path = ptr.?[0..len];

    // Check for .. sequences
    var i: usize = 0;
    while (i < path.len) {
        if (i + 1 < path.len and path[i] == '.' and path[i + 1] == '.') {
            // Check if this is actually a traversal (not part of filename)
            const before_ok = i == 0 or path[i - 1] == '/' or path[i - 1] == '\\';
            const after_ok = i + 2 >= path.len or path[i + 2] == '/' or path[i + 2] == '\\';
            if (before_ok and after_ok) {
                return .{ .status = .ok, .value = true };
            }
        }
        i += 1;
    }

    return .{ .status = .ok, .value = false };
}

/// Sanitize a filename (remove dangerous characters)
export fn proven_path_sanitize_filename(ptr: ?[*]const u8, len: usize) StringResult {
    if (ptr == null) {
        return .{ .status = .err_null_pointer, .value = null, .length = 0 };
    }
    const input = ptr.?[0..len];

    // Count safe characters
    var safe_count: usize = 0;
    for (input) |c| {
        if (isSafeFilenameChar(c)) safe_count += 1;
    }

    if (safe_count == 0) {
        return .{ .status = .err_validation_failed, .value = null, .length = 0 };
    }

    const output = allocator.allocSentinel(u8, safe_count, 0) catch {
        return .{ .status = .err_allocation_failed, .value = null, .length = 0 };
    };

    var j: usize = 0;
    for (input) |c| {
        if (isSafeFilenameChar(c)) {
            output[j] = c;
            j += 1;
        }
    }

    return .{ .status = .ok, .value = output.ptr, .length = safe_count };
}

fn isSafeFilenameChar(c: u8) bool {
    return switch (c) {
        'a'...'z', 'A'...'Z', '0'...'9', '.', '-', '_' => true,
        else => false,
    };
}

// ============================================================================
// SafeCrypto - Cryptographic primitives
// ============================================================================

/// Constant-time byte comparison (timing-safe)
export fn proven_crypto_constant_time_eq(
    ptr1: ?[*]const u8,
    len1: usize,
    ptr2: ?[*]const u8,
    len2: usize,
) BoolResult {
    if (ptr1 == null or ptr2 == null) {
        return .{ .status = .err_null_pointer, .value = false };
    }
    if (len1 != len2) {
        return .{ .status = .ok, .value = false };
    }

    const a = ptr1.?[0..len1];
    const b = ptr2.?[0..len2];

    var diff: u8 = 0;
    for (a, b) |x, y| {
        diff |= x ^ y;
    }

    return .{ .status = .ok, .value = diff == 0 };
}

/// Fill buffer with cryptographically secure random bytes
export fn proven_crypto_random_bytes(ptr: ?[*]u8, len: usize) ProvenStatus {
    if (ptr == null) {
        return .err_null_pointer;
    }

    std.crypto.random.bytes(ptr.?[0..len]);
    return .ok;
}

// ============================================================================
// SafeUrl - URL parsing and validation
// ============================================================================

/// URL components structure
pub const UrlComponents = extern struct {
    scheme: ?[*:0]u8,
    scheme_len: usize,
    host: ?[*:0]u8,
    host_len: usize,
    port: u16,
    has_port: bool,
    path: ?[*:0]u8,
    path_len: usize,
    query: ?[*:0]u8,
    query_len: usize,
    fragment: ?[*:0]u8,
    fragment_len: usize,
};

/// Result for URL parsing
pub const UrlResult = extern struct {
    status: ProvenStatus,
    components: UrlComponents,
};

/// Parse a URL into components
export fn proven_url_parse(ptr: ?[*]const u8, len: usize) UrlResult {
    const empty_components = UrlComponents{
        .scheme = null,
        .scheme_len = 0,
        .host = null,
        .host_len = 0,
        .port = 0,
        .has_port = false,
        .path = null,
        .path_len = 0,
        .query = null,
        .query_len = 0,
        .fragment = null,
        .fragment_len = 0,
    };

    if (ptr == null) {
        return .{ .status = .err_null_pointer, .components = empty_components };
    }

    const url_str = ptr.?[0..len];
    const uri = std.Uri.parse(url_str) catch {
        return .{ .status = .err_parse_failure, .components = empty_components };
    };

    var components = empty_components;

    // Copy scheme
    if (uri.scheme.len > 0) {
        const scheme = allocator.allocSentinel(u8, uri.scheme.len, 0) catch {
            return .{ .status = .err_allocation_failed, .components = empty_components };
        };
        @memcpy(scheme, uri.scheme);
        components.scheme = scheme.ptr;
        components.scheme_len = uri.scheme.len;
    }

    // Helper to get raw string from Uri.Component
    const getComponentStr = struct {
        fn get(component: std.Uri.Component) []const u8 {
            return switch (component) {
                .raw => |raw| raw,
                .percent_encoded => |encoded| encoded,
            };
        }
    }.get;

    // Copy host
    if (uri.host) |host| {
        const host_raw = getComponentStr(host);
        const host_str = allocator.allocSentinel(u8, host_raw.len, 0) catch {
            return .{ .status = .err_allocation_failed, .components = empty_components };
        };
        @memcpy(host_str, host_raw);
        components.host = host_str.ptr;
        components.host_len = host_raw.len;
    }

    // Port
    if (uri.port) |port| {
        components.port = port;
        components.has_port = true;
    }

    // Path
    const path_raw = getComponentStr(uri.path);
    if (path_raw.len > 0) {
        const path = allocator.allocSentinel(u8, path_raw.len, 0) catch {
            return .{ .status = .err_allocation_failed, .components = empty_components };
        };
        @memcpy(path, path_raw);
        components.path = path.ptr;
        components.path_len = path_raw.len;
    }

    // Query
    if (uri.query) |query| {
        const query_raw = getComponentStr(query);
        const query_str = allocator.allocSentinel(u8, query_raw.len, 0) catch {
            return .{ .status = .err_allocation_failed, .components = empty_components };
        };
        @memcpy(query_str, query_raw);
        components.query = query_str.ptr;
        components.query_len = query_raw.len;
    }

    // Fragment
    if (uri.fragment) |fragment| {
        const frag_raw = getComponentStr(fragment);
        const frag_str = allocator.allocSentinel(u8, frag_raw.len, 0) catch {
            return .{ .status = .err_allocation_failed, .components = empty_components };
        };
        @memcpy(frag_str, frag_raw);
        components.fragment = frag_str.ptr;
        components.fragment_len = frag_raw.len;
    }

    return .{ .status = .ok, .components = components };
}

/// Free URL components
export fn proven_url_free(components: *UrlComponents) void {
    if (components.scheme) |s| {
        allocator.free(s[0 .. components.scheme_len + 1]);
    }
    if (components.host) |h| {
        allocator.free(h[0 .. components.host_len + 1]);
    }
    if (components.path) |p| {
        allocator.free(p[0 .. components.path_len + 1]);
    }
    if (components.query) |q| {
        allocator.free(q[0 .. components.query_len + 1]);
    }
    if (components.fragment) |f| {
        allocator.free(f[0 .. components.fragment_len + 1]);
    }
    components.* = .{
        .scheme = null,
        .scheme_len = 0,
        .host = null,
        .host_len = 0,
        .port = 0,
        .has_port = false,
        .path = null,
        .path_len = 0,
        .query = null,
        .query_len = 0,
        .fragment = null,
        .fragment_len = 0,
    };
}

// ============================================================================
// SafeEmail - Email validation
// ============================================================================

/// Validate email address (RFC 5321 simplified)
export fn proven_email_is_valid(ptr: ?[*]const u8, len: usize) BoolResult {
    if (ptr == null) {
        return .{ .status = .err_null_pointer, .value = false };
    }
    if (len == 0 or len > 254) {
        return .{ .status = .ok, .value = false };
    }

    const email = ptr.?[0..len];

    // Find @ symbol
    var at_pos: ?usize = null;
    for (email, 0..) |c, i| {
        if (c == '@') {
            if (at_pos != null) {
                // Multiple @ symbols
                return .{ .status = .ok, .value = false };
            }
            at_pos = i;
        }
    }

    const at = at_pos orelse return .{ .status = .ok, .value = false };

    // Local part validation
    if (at == 0 or at > 64) {
        return .{ .status = .ok, .value = false };
    }

    // Domain part validation
    const domain_len = len - at - 1;
    if (domain_len == 0 or domain_len > 253) {
        return .{ .status = .ok, .value = false };
    }

    // Check for at least one dot in domain
    const domain = email[at + 1 ..];
    var has_dot = false;
    for (domain) |c| {
        if (c == '.') has_dot = true;
    }

    // localhost is valid per RFC
    if (!has_dot and !std.mem.eql(u8, domain, "localhost")) {
        return .{ .status = .ok, .value = false };
    }

    return .{ .status = .ok, .value = true };
}

// ============================================================================
// SafeNetwork - IP address parsing
// ============================================================================

/// IPv4 address structure
pub const IPv4Address = extern struct {
    octets: [4]u8,
};

/// Result for IPv4 parsing
pub const IPv4Result = extern struct {
    status: ProvenStatus,
    address: IPv4Address,
};

/// Parse IPv4 address
export fn proven_network_parse_ipv4(ptr: ?[*]const u8, len: usize) IPv4Result {
    const empty = IPv4Address{ .octets = .{ 0, 0, 0, 0 } };

    if (ptr == null) {
        return .{ .status = .err_null_pointer, .address = empty };
    }

    const str = ptr.?[0..len];

    var octets: [4]u8 = .{ 0, 0, 0, 0 };
    var octet_idx: usize = 0;
    var current_value: u16 = 0;
    var digits_in_current: usize = 0;

    for (str) |c| {
        if (c >= '0' and c <= '9') {
            current_value = current_value * 10 + (c - '0');
            digits_in_current += 1;
            if (current_value > 255 or digits_in_current > 3) {
                return .{ .status = .err_parse_failure, .address = empty };
            }
        } else if (c == '.') {
            if (digits_in_current == 0 or octet_idx >= 3) {
                return .{ .status = .err_parse_failure, .address = empty };
            }
            octets[octet_idx] = @intCast(current_value);
            octet_idx += 1;
            current_value = 0;
            digits_in_current = 0;
        } else {
            return .{ .status = .err_parse_failure, .address = empty };
        }
    }

    // Handle last octet
    if (digits_in_current == 0 or octet_idx != 3) {
        return .{ .status = .err_parse_failure, .address = empty };
    }
    octets[3] = @intCast(current_value);

    return .{ .status = .ok, .address = .{ .octets = octets } };
}

/// Check if IPv4 is private (RFC 1918)
export fn proven_network_ipv4_is_private(addr: IPv4Address) bool {
    const a = addr.octets[0];
    const b = addr.octets[1];

    // 10.0.0.0/8
    if (a == 10) return true;
    // 172.16.0.0/12
    if (a == 172 and b >= 16 and b <= 31) return true;
    // 192.168.0.0/16
    if (a == 192 and b == 168) return true;

    return false;
}

/// Check if IPv4 is loopback (127.0.0.0/8)
export fn proven_network_ipv4_is_loopback(addr: IPv4Address) bool {
    return addr.octets[0] == 127;
}

// ============================================================================
// Runtime Initialization (using idris2-zig-ffi)
// ============================================================================

var runtime_initialized: bool = false;

/// Initialize the Proven runtime (includes Idris 2 runtime)
export fn proven_init() i32 {
    if (runtime_initialized) {
        return @intFromEnum(ProvenStatus.ok);
    }

    ffi.init() catch {
        return @intFromEnum(ProvenStatus.err_allocation_failed);
    };

    runtime_initialized = true;
    return @intFromEnum(ProvenStatus.ok);
}

/// Cleanup the Proven runtime
export fn proven_deinit() void {
    if (runtime_initialized) {
        ffi.deinit();
        runtime_initialized = false;
    }
}

/// Check if runtime is initialized
export fn proven_is_initialized() bool {
    return runtime_initialized;
}

/// Get idris2-zig-ffi ABI version for compatibility checking
export fn proven_ffi_abi_version() u32 {
    return ffi.ABI_VERSION;
}

// ============================================================================
// Version info
// ============================================================================

export fn proven_version_major() u32 {
    return 0;
}

export fn proven_version_minor() u32 {
    return 3;
}

export fn proven_version_patch() u32 {
    return 0;
}

// ============================================================================
// Type Bridge: Convert between Proven and idris2-zig-ffi types
// ============================================================================

/// Convert ProvenStatus to C ABI error code
pub fn statusToCError(status: ProvenStatus) u32 {
    return switch (status) {
        .ok => c_abi.ErrorCode.OK,
        .err_null_pointer => c_abi.ErrorCode.INVALID_POINTER,
        .err_invalid_argument => c_abi.ErrorCode.INVALID_ARGUMENT,
        .err_overflow => c_abi.ErrorCode.OVERFLOW,
        .err_underflow => c_abi.ErrorCode.UNDERFLOW,
        .err_division_by_zero => c_abi.ErrorCode.DIVISION_BY_ZERO,
        .err_parse_failure => c_abi.ErrorCode.PARSE_ERROR,
        .err_validation_failed => c_abi.ErrorCode.INVALID_INPUT,
        .err_out_of_bounds => c_abi.ErrorCode.INVALID_ARGUMENT,
        .err_encoding_error => c_abi.ErrorCode.PARSE_ERROR,
        .err_allocation_failed => c_abi.ErrorCode.OUT_OF_MEMORY,
        .err_not_implemented => c_abi.ErrorCode.UNKNOWN,
    };
}

/// Convert IntResult to C ABI CResult
pub fn intResultToCResult(result: IntResult) c_abi.CResult {
    if (result.status == .ok) {
        return c_abi.CResult.ok(.{ .int = result.value });
    } else {
        return c_abi.CResult.err(statusToCError(result.status), "");
    }
}

/// Convert BoolResult to C ABI CResult
pub fn boolResultToCResult(result: BoolResult) c_abi.CResult {
    if (result.status == .ok) {
        return c_abi.CResult.ok(.{ .boolean = result.value });
    } else {
        return c_abi.CResult.err(statusToCError(result.status), "");
    }
}

// ============================================================================
// Tests
// ============================================================================

test "math_div" {
    const r1 = proven_math_div(10, 2);
    try std.testing.expectEqual(ProvenStatus.ok, r1.status);
    try std.testing.expectEqual(@as(i64, 5), r1.value);

    const r2 = proven_math_div(10, 0);
    try std.testing.expectEqual(ProvenStatus.err_division_by_zero, r2.status);
}

test "math_add_checked" {
    const r1 = proven_math_add_checked(5, 3);
    try std.testing.expectEqual(ProvenStatus.ok, r1.status);
    try std.testing.expectEqual(@as(i64, 8), r1.value);

    const max = std.math.maxInt(i64);
    const r2 = proven_math_add_checked(max, 1);
    try std.testing.expectEqual(ProvenStatus.err_overflow, r2.status);
}

test "string_is_valid_utf8" {
    const valid = "Hello, World!";
    const r1 = proven_string_is_valid_utf8(valid.ptr, valid.len);
    try std.testing.expectEqual(ProvenStatus.ok, r1.status);
    try std.testing.expect(r1.value);

    const invalid = [_]u8{ 0xFF, 0xFE };
    const r2 = proven_string_is_valid_utf8(&invalid, invalid.len);
    try std.testing.expectEqual(ProvenStatus.ok, r2.status);
    try std.testing.expect(!r2.value);
}

test "path_has_traversal" {
    const safe = "path/to/file.txt";
    const r1 = proven_path_has_traversal(safe.ptr, safe.len);
    try std.testing.expect(!r1.value);

    const unsafe = "../../../etc/passwd";
    const r2 = proven_path_has_traversal(unsafe.ptr, unsafe.len);
    try std.testing.expect(r2.value);
}

test "email_is_valid" {
    const valid = "user@example.com";
    const r1 = proven_email_is_valid(valid.ptr, valid.len);
    try std.testing.expect(r1.value);

    const invalid = "not-an-email";
    const r2 = proven_email_is_valid(invalid.ptr, invalid.len);
    try std.testing.expect(!r2.value);
}

test "network_parse_ipv4" {
    const ip = "192.168.1.1";
    const r = proven_network_parse_ipv4(ip.ptr, ip.len);
    try std.testing.expectEqual(ProvenStatus.ok, r.status);
    try std.testing.expectEqual(@as(u8, 192), r.address.octets[0]);
    try std.testing.expectEqual(@as(u8, 168), r.address.octets[1]);
    try std.testing.expectEqual(@as(u8, 1), r.address.octets[2]);
    try std.testing.expectEqual(@as(u8, 1), r.address.octets[3]);
    try std.testing.expect(proven_network_ipv4_is_private(r.address));
}
