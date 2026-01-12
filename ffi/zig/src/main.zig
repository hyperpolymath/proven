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
// SafeHeader - HTTP header operations that prevent CRLF injection
// ============================================================================

/// Header validation result
pub const HeaderResult = extern struct {
    status: ProvenStatus,
    value: ?[*:0]u8,
    length: usize,
};

/// Check for CRLF injection characters in header value
export fn proven_header_has_crlf(ptr: ?[*]const u8, len: usize) BoolResult {
    if (ptr == null) {
        return .{ .status = .err_null_pointer, .value = false };
    }
    const input = ptr.?[0..len];
    for (input) |c| {
        if (c == '\r' or c == '\n') {
            return .{ .status = .ok, .value = true };
        }
    }
    return .{ .status = .ok, .value = false };
}

/// Check if header name is valid token per RFC 7230
export fn proven_header_is_valid_name(ptr: ?[*]const u8, len: usize) BoolResult {
    if (ptr == null) {
        return .{ .status = .err_null_pointer, .value = false };
    }
    if (len == 0 or len > 256) {
        return .{ .status = .ok, .value = false };
    }
    const input = ptr.?[0..len];
    for (input) |c| {
        if (!isValidTokenChar(c)) {
            return .{ .status = .ok, .value = false };
        }
    }
    return .{ .status = .ok, .value = true };
}

fn isValidTokenChar(c: u8) bool {
    // Token chars per RFC 7230: !#$%&'*+-.^_`|~ plus alphanumeric
    if (c < 33 or c > 126) return false;
    return switch (c) {
        '(', ')', '<', '>', '@', ',', ';', ':', '\\', '"', '/', '[', ']', '?', '=' => false,
        else => true,
    };
}

/// Check if header name is in dangerous headers list
export fn proven_header_is_dangerous(ptr: ?[*]const u8, len: usize) BoolResult {
    if (ptr == null) {
        return .{ .status = .err_null_pointer, .value = false };
    }
    const input = ptr.?[0..len];

    const dangerous = [_][]const u8{
        "proxy-authorization",
        "proxy-authenticate",
        "proxy-connection",
        "transfer-encoding",
        "content-length",
        "host",
        "connection",
        "keep-alive",
        "upgrade",
        "te",
        "trailer",
    };

    // Convert to lowercase for comparison
    var lower_buf: [256]u8 = undefined;
    if (len > lower_buf.len) {
        return .{ .status = .ok, .value = false };
    }
    for (input, 0..) |c, i| {
        lower_buf[i] = if (c >= 'A' and c <= 'Z') c + 32 else c;
    }
    const lower = lower_buf[0..len];

    for (dangerous) |d| {
        if (std.mem.eql(u8, lower, d)) {
            return .{ .status = .ok, .value = true };
        }
    }
    return .{ .status = .ok, .value = false };
}

/// Create validated header string "Name: Value"
export fn proven_header_render(
    name_ptr: ?[*]const u8,
    name_len: usize,
    value_ptr: ?[*]const u8,
    value_len: usize,
) StringResult {
    if (name_ptr == null or value_ptr == null) {
        return .{ .status = .err_null_pointer, .value = null, .length = 0 };
    }

    const name = name_ptr.?[0..name_len];
    const value = value_ptr.?[0..value_len];

    // Validate name
    if (name_len == 0 or name_len > 256) {
        return .{ .status = .err_validation_failed, .value = null, .length = 0 };
    }
    for (name) |c| {
        if (!isValidTokenChar(c)) {
            return .{ .status = .err_validation_failed, .value = null, .length = 0 };
        }
    }

    // Check for CRLF in value
    for (value) |c| {
        if (c == '\r' or c == '\n') {
            return .{ .status = .err_validation_failed, .value = null, .length = 0 };
        }
    }

    // Value size limit
    if (value_len > 8192) {
        return .{ .status = .err_validation_failed, .value = null, .length = 0 };
    }

    // Build "Name: Value"
    const output_len = name_len + 2 + value_len; // ": "
    const output = allocator.allocSentinel(u8, output_len, 0) catch {
        return .{ .status = .err_allocation_failed, .value = null, .length = 0 };
    };

    @memcpy(output[0..name_len], name);
    output[name_len] = ':';
    output[name_len + 1] = ' ';
    @memcpy(output[name_len + 2 ..][0..value_len], value);

    return .{ .status = .ok, .value = output.ptr, .length = output_len };
}

/// Build Content-Security-Policy header value from directives
export fn proven_header_build_csp(directives_json: ?[*]const u8, _json_len: usize) StringResult {
    _ = _json_len; // Will be used when JSON parsing is implemented
    if (directives_json == null) {
        return .{ .status = .err_null_pointer, .value = null, .length = 0 };
    }
    // For now, return not implemented - full CSP requires JSON parsing
    // In production, this calls the Idris 2 RefC function
    return .{ .status = .err_not_implemented, .value = null, .length = 0 };
}

/// Build HSTS header value
export fn proven_header_build_hsts(max_age: i64, include_subdomains: bool, preload: bool) StringResult {
    if (max_age < 0) {
        return .{ .status = .err_invalid_argument, .value = null, .length = 0 };
    }

    // Build "max-age=N; includeSubDomains; preload"
    var buf: [128]u8 = undefined;
    var len: usize = 0;

    const prefix = "max-age=";
    @memcpy(buf[0..prefix.len], prefix);
    len = prefix.len;

    // Convert max_age to string
    var tmp: [20]u8 = undefined;
    var num = @as(u64, @intCast(max_age));
    var num_len: usize = 0;
    if (num == 0) {
        tmp[0] = '0';
        num_len = 1;
    } else {
        while (num > 0) : (num_len += 1) {
            tmp[num_len] = @intCast((num % 10) + '0');
            num /= 10;
        }
        // Reverse
        var i: usize = 0;
        while (i < num_len / 2) : (i += 1) {
            const t = tmp[i];
            tmp[i] = tmp[num_len - 1 - i];
            tmp[num_len - 1 - i] = t;
        }
    }
    @memcpy(buf[len..][0..num_len], tmp[0..num_len]);
    len += num_len;

    if (include_subdomains) {
        const sub = "; includeSubDomains";
        @memcpy(buf[len..][0..sub.len], sub);
        len += sub.len;
    }

    if (preload) {
        const pre = "; preload";
        @memcpy(buf[len..][0..pre.len], pre);
        len += pre.len;
    }

    const output = allocator.allocSentinel(u8, len, 0) catch {
        return .{ .status = .err_allocation_failed, .value = null, .length = 0 };
    };
    @memcpy(output[0..len], buf[0..len]);

    return .{ .status = .ok, .value = output.ptr, .length = len };
}

// ============================================================================
// SafeCookie - HTTP cookie operations that prevent injection attacks
// ============================================================================

/// SameSite attribute values
pub const SameSite = enum(i32) {
    strict = 0,
    lax = 1,
    none = 2,
};

/// Cookie attributes structure
pub const CookieAttributes = extern struct {
    domain: ?[*:0]const u8,
    domain_len: usize,
    path: ?[*:0]const u8,
    path_len: usize,
    max_age: i64, // -1 means not set
    secure: bool,
    http_only: bool,
    same_site: SameSite,
    partitioned: bool,
};

/// Check for cookie injection characters (semicolon, CR, LF)
export fn proven_cookie_has_injection(ptr: ?[*]const u8, len: usize) BoolResult {
    if (ptr == null) {
        return .{ .status = .err_null_pointer, .value = false };
    }
    const input = ptr.?[0..len];
    for (input) |c| {
        if (c == ';' or c == '\r' or c == '\n') {
            return .{ .status = .ok, .value = true };
        }
    }
    return .{ .status = .ok, .value = false };
}

/// Validate cookie name
export fn proven_cookie_validate_name(ptr: ?[*]const u8, len: usize) BoolResult {
    if (ptr == null) {
        return .{ .status = .err_null_pointer, .value = false };
    }
    if (len == 0 or len > 256) {
        return .{ .status = .ok, .value = false };
    }
    const input = ptr.?[0..len];
    for (input) |c| {
        if (c == ';' or c == '\r' or c == '\n') {
            return .{ .status = .ok, .value = false };
        }
    }
    return .{ .status = .ok, .value = true };
}

/// Validate cookie value
export fn proven_cookie_validate_value(ptr: ?[*]const u8, len: usize) BoolResult {
    if (ptr == null) {
        return .{ .status = .err_null_pointer, .value = false };
    }
    if (len > 4096) {
        return .{ .status = .ok, .value = false };
    }
    const input = ptr.?[0..len];
    for (input) |c| {
        if (c == ';' or c == '\r' or c == '\n') {
            return .{ .status = .ok, .value = false };
        }
    }
    return .{ .status = .ok, .value = true };
}

/// Get cookie prefix type (0=none, 1=__Secure-, 2=__Host-)
export fn proven_cookie_get_prefix(ptr: ?[*]const u8, len: usize) IntResult {
    if (ptr == null) {
        return .{ .status = .err_null_pointer, .value = 0 };
    }
    const input = ptr.?[0..len];

    if (len >= 7 and std.mem.startsWith(u8, input, "__Host-")) {
        return .{ .status = .ok, .value = 2 };
    }
    if (len >= 9 and std.mem.startsWith(u8, input, "__Secure-")) {
        return .{ .status = .ok, .value = 1 };
    }
    return .{ .status = .ok, .value = 0 };
}

/// Build Set-Cookie header value
export fn proven_cookie_build_set_cookie(
    name_ptr: ?[*]const u8,
    name_len: usize,
    value_ptr: ?[*]const u8,
    value_len: usize,
    attrs: CookieAttributes,
) StringResult {
    if (name_ptr == null or value_ptr == null) {
        return .{ .status = .err_null_pointer, .value = null, .length = 0 };
    }

    const name = name_ptr.?[0..name_len];
    const value = value_ptr.?[0..value_len];

    // Validate name and value
    if (name_len == 0 or name_len > 256 or value_len > 4096) {
        return .{ .status = .err_validation_failed, .value = null, .length = 0 };
    }
    for (name) |c| {
        if (c == ';' or c == '\r' or c == '\n') {
            return .{ .status = .err_validation_failed, .value = null, .length = 0 };
        }
    }
    for (value) |c| {
        if (c == ';' or c == '\r' or c == '\n') {
            return .{ .status = .err_validation_failed, .value = null, .length = 0 };
        }
    }

    // Build cookie string
    var buf: [8192]u8 = undefined;
    var pos: usize = 0;

    // name=value
    @memcpy(buf[pos..][0..name_len], name);
    pos += name_len;
    buf[pos] = '=';
    pos += 1;
    @memcpy(buf[pos..][0..value_len], value);
    pos += value_len;

    // Domain
    if (attrs.domain) |d| {
        const dom = d[0..attrs.domain_len];
        @memcpy(buf[pos..][0..9], "; Domain=");
        pos += 9;
        @memcpy(buf[pos..][0..dom.len], dom);
        pos += dom.len;
    }

    // Path
    if (attrs.path) |p| {
        const pth = p[0..attrs.path_len];
        @memcpy(buf[pos..][0..7], "; Path=");
        pos += 7;
        @memcpy(buf[pos..][0..pth.len], pth);
        pos += pth.len;
    }

    // Max-Age
    if (attrs.max_age >= 0) {
        @memcpy(buf[pos..][0..10], "; Max-Age=");
        pos += 10;
        // Convert to string
        var tmp: [20]u8 = undefined;
        var num = @as(u64, @intCast(attrs.max_age));
        var num_len: usize = 0;
        if (num == 0) {
            tmp[0] = '0';
            num_len = 1;
        } else {
            while (num > 0) : (num_len += 1) {
                tmp[num_len] = @intCast((num % 10) + '0');
                num /= 10;
            }
            var i: usize = 0;
            while (i < num_len / 2) : (i += 1) {
                const t = tmp[i];
                tmp[i] = tmp[num_len - 1 - i];
                tmp[num_len - 1 - i] = t;
            }
        }
        @memcpy(buf[pos..][0..num_len], tmp[0..num_len]);
        pos += num_len;
    }

    // Secure
    if (attrs.secure) {
        @memcpy(buf[pos..][0..8], "; Secure");
        pos += 8;
    }

    // HttpOnly
    if (attrs.http_only) {
        @memcpy(buf[pos..][0..10], "; HttpOnly");
        pos += 10;
    }

    // SameSite
    switch (attrs.same_site) {
        .strict => {
            @memcpy(buf[pos..][0..16], "; SameSite=Strict");
            pos += 16;
        },
        .lax => {
            @memcpy(buf[pos..][0..13], "; SameSite=Lax");
            pos += 13;
        },
        .none => {
            @memcpy(buf[pos..][0..14], "; SameSite=None");
            pos += 14;
        },
    }

    // Partitioned
    if (attrs.partitioned) {
        @memcpy(buf[pos..][0..13], "; Partitioned");
        pos += 13;
    }

    const output = allocator.allocSentinel(u8, pos, 0) catch {
        return .{ .status = .err_allocation_failed, .value = null, .length = 0 };
    };
    @memcpy(output[0..pos], buf[0..pos]);

    return .{ .status = .ok, .value = output.ptr, .length = pos };
}

/// Build delete cookie header value
export fn proven_cookie_build_delete(name_ptr: ?[*]const u8, name_len: usize) StringResult {
    if (name_ptr == null) {
        return .{ .status = .err_null_pointer, .value = null, .length = 0 };
    }
    const name = name_ptr.?[0..name_len];

    // Validate name
    if (name_len == 0 or name_len > 256) {
        return .{ .status = .err_validation_failed, .value = null, .length = 0 };
    }
    for (name) |c| {
        if (c == ';' or c == '\r' or c == '\n') {
            return .{ .status = .err_validation_failed, .value = null, .length = 0 };
        }
    }

    const suffix = "=; Max-Age=0; Path=/; Expires=Thu, 01 Jan 1970 00:00:00 GMT";
    const output_len = name_len + suffix.len;

    const output = allocator.allocSentinel(u8, output_len, 0) catch {
        return .{ .status = .err_allocation_failed, .value = null, .length = 0 };
    };

    @memcpy(output[0..name_len], name);
    @memcpy(output[name_len..][0..suffix.len], suffix);

    return .{ .status = .ok, .value = output.ptr, .length = output_len };
}

// ============================================================================
// SafeContentType - Content-Type operations preventing MIME sniffing
// ============================================================================

/// Media type category
pub const MediaCategory = enum(i32) {
    text = 0,
    image = 1,
    audio = 2,
    video = 3,
    application = 4,
    multipart = 5,
    message = 6,
    font = 7,
    model = 8,
    custom = 9,
};

/// Charset encoding
pub const Charset = enum(i32) {
    utf8 = 0,
    utf16le = 1,
    utf16be = 2,
    iso8859_1 = 3,
    ascii = 4,
    windows1252 = 5,
    other = 6,
};

/// Content type parse result
pub const ContentTypeResult = extern struct {
    status: ProvenStatus,
    media_type: ?[*:0]u8,
    media_type_len: usize,
    subtype: ?[*:0]u8,
    subtype_len: usize,
    suffix: ?[*:0]u8,
    suffix_len: usize,
    category: MediaCategory,
    charset: Charset,
    has_charset: bool,
};

/// Parse Content-Type header
export fn proven_content_type_parse(ptr: ?[*]const u8, len: usize) ContentTypeResult {
    const empty = ContentTypeResult{
        .status = .err_parse_failure,
        .media_type = null,
        .media_type_len = 0,
        .subtype = null,
        .subtype_len = 0,
        .suffix = null,
        .suffix_len = 0,
        .category = .custom,
        .charset = .utf8,
        .has_charset = false,
    };

    if (ptr == null) {
        return ContentTypeResult{ .status = .err_null_pointer, .media_type = null, .media_type_len = 0, .subtype = null, .subtype_len = 0, .suffix = null, .suffix_len = 0, .category = .custom, .charset = .utf8, .has_charset = false };
    }
    if (len == 0 or len > 1024) {
        return empty;
    }

    const input = ptr.?[0..len];

    // Find semicolon (parameter separator)
    var media_end: usize = len;
    for (input, 0..) |c, i| {
        if (c == ';') {
            media_end = i;
            break;
        }
    }

    // Find slash in media type
    const media_part = input[0..media_end];
    var slash_pos: ?usize = null;
    for (media_part, 0..) |c, i| {
        if (c == '/') {
            slash_pos = i;
            break;
        }
    }

    const sp = slash_pos orelse return empty;
    if (sp == 0 or sp >= media_end - 1) {
        return empty;
    }

    // Extract type and subtype
    const type_str = media_part[0..sp];
    const subtype_full = media_part[sp + 1 ..];

    // Check for suffix (+json, +xml, etc.)
    var subtype_str = subtype_full;
    var suffix_str: ?[]const u8 = null;
    for (subtype_full, 0..) |c, i| {
        if (c == '+' and i > 0 and i < subtype_full.len - 1) {
            subtype_str = subtype_full[0..i];
            suffix_str = subtype_full[i + 1 ..];
            break;
        }
    }

    // Determine category
    const category = blk: {
        if (std.mem.eql(u8, type_str, "text")) break :blk MediaCategory.text;
        if (std.mem.eql(u8, type_str, "image")) break :blk MediaCategory.image;
        if (std.mem.eql(u8, type_str, "audio")) break :blk MediaCategory.audio;
        if (std.mem.eql(u8, type_str, "video")) break :blk MediaCategory.video;
        if (std.mem.eql(u8, type_str, "application")) break :blk MediaCategory.application;
        if (std.mem.eql(u8, type_str, "multipart")) break :blk MediaCategory.multipart;
        if (std.mem.eql(u8, type_str, "message")) break :blk MediaCategory.message;
        if (std.mem.eql(u8, type_str, "font")) break :blk MediaCategory.font;
        if (std.mem.eql(u8, type_str, "model")) break :blk MediaCategory.model;
        break :blk MediaCategory.custom;
    };

    // Parse charset from parameters
    var charset = Charset.utf8;
    var has_charset = false;
    if (media_end < len) {
        // Look for charset= in parameters
        const params = input[media_end + 1 ..];
        var i: usize = 0;
        while (i < params.len) {
            // Skip whitespace
            while (i < params.len and (params[i] == ' ' or params[i] == '\t')) : (i += 1) {}
            if (i >= params.len) break;

            // Check for charset=
            if (i + 8 <= params.len) {
                var lower_buf: [8]u8 = undefined;
                for (params[i..][0..8], 0..) |c, j| {
                    lower_buf[j] = if (c >= 'A' and c <= 'Z') c + 32 else c;
                }
                if (std.mem.eql(u8, &lower_buf, "charset=")) {
                    i += 8;
                    // Find end of value
                    var val_start = i;
                    // Skip optional quote
                    if (i < params.len and params[i] == '"') {
                        val_start = i + 1;
                        i += 1;
                        while (i < params.len and params[i] != '"') : (i += 1) {}
                    } else {
                        while (i < params.len and params[i] != ';' and params[i] != ' ') : (i += 1) {}
                    }
                    const val = params[val_start..i];
                    has_charset = true;

                    // Parse charset value
                    var cs_lower: [16]u8 = undefined;
                    const cs_len = @min(val.len, 16);
                    for (val[0..cs_len], 0..) |c, j| {
                        cs_lower[j] = if (c >= 'A' and c <= 'Z') c + 32 else c;
                    }
                    const cs_slice = cs_lower[0..cs_len];

                    if (std.mem.eql(u8, cs_slice, "utf-8") or std.mem.eql(u8, cs_slice, "utf8")) {
                        charset = .utf8;
                    } else if (std.mem.eql(u8, cs_slice, "utf-16le")) {
                        charset = .utf16le;
                    } else if (std.mem.eql(u8, cs_slice, "utf-16be")) {
                        charset = .utf16be;
                    } else if (std.mem.eql(u8, cs_slice, "iso-8859-1") or std.mem.eql(u8, cs_slice, "latin1")) {
                        charset = .iso8859_1;
                    } else if (std.mem.eql(u8, cs_slice, "us-ascii") or std.mem.eql(u8, cs_slice, "ascii")) {
                        charset = .ascii;
                    } else if (std.mem.eql(u8, cs_slice, "windows-1252") or std.mem.eql(u8, cs_slice, "cp1252")) {
                        charset = .windows1252;
                    } else {
                        charset = .other;
                    }
                    break;
                }
            }

            // Skip to next parameter
            while (i < params.len and params[i] != ';') : (i += 1) {}
            if (i < params.len) i += 1;
        }
    }

    // Allocate output strings
    const type_out = allocator.allocSentinel(u8, type_str.len, 0) catch {
        return ContentTypeResult{ .status = .err_allocation_failed, .media_type = null, .media_type_len = 0, .subtype = null, .subtype_len = 0, .suffix = null, .suffix_len = 0, .category = .custom, .charset = .utf8, .has_charset = false };
    };
    @memcpy(type_out[0..type_str.len], type_str);

    const subtype_out = allocator.allocSentinel(u8, subtype_str.len, 0) catch {
        allocator.free(type_out[0 .. type_str.len + 1]);
        return ContentTypeResult{ .status = .err_allocation_failed, .media_type = null, .media_type_len = 0, .subtype = null, .subtype_len = 0, .suffix = null, .suffix_len = 0, .category = .custom, .charset = .utf8, .has_charset = false };
    };
    @memcpy(subtype_out[0..subtype_str.len], subtype_str);

    var suffix_out: ?[*:0]u8 = null;
    var suffix_out_len: usize = 0;
    if (suffix_str) |suf| {
        const s = allocator.allocSentinel(u8, suf.len, 0) catch {
            allocator.free(type_out[0 .. type_str.len + 1]);
            allocator.free(subtype_out[0 .. subtype_str.len + 1]);
            return ContentTypeResult{ .status = .err_allocation_failed, .media_type = null, .media_type_len = 0, .subtype = null, .subtype_len = 0, .suffix = null, .suffix_len = 0, .category = .custom, .charset = .utf8, .has_charset = false };
        };
        @memcpy(s[0..suf.len], suf);
        suffix_out = s.ptr;
        suffix_out_len = suf.len;
    }

    return ContentTypeResult{
        .status = .ok,
        .media_type = type_out.ptr,
        .media_type_len = type_str.len,
        .subtype = subtype_out.ptr,
        .subtype_len = subtype_str.len,
        .suffix = suffix_out,
        .suffix_len = suffix_out_len,
        .category = category,
        .charset = charset,
        .has_charset = has_charset,
    };
}

/// Free content type result
export fn proven_content_type_free(result: *ContentTypeResult) void {
    if (result.media_type) |m| {
        allocator.free(m[0 .. result.media_type_len + 1]);
    }
    if (result.subtype) |s| {
        allocator.free(s[0 .. result.subtype_len + 1]);
    }
    if (result.suffix) |f| {
        allocator.free(f[0 .. result.suffix_len + 1]);
    }
    result.* = .{
        .status = .ok,
        .media_type = null,
        .media_type_len = 0,
        .subtype = null,
        .subtype_len = 0,
        .suffix = null,
        .suffix_len = 0,
        .category = .custom,
        .charset = .utf8,
        .has_charset = false,
    };
}

/// Check if content type can be sniffed to something dangerous
export fn proven_content_type_can_sniff_dangerous(ptr: ?[*]const u8, len: usize) BoolResult {
    if (ptr == null) {
        return .{ .status = .err_null_pointer, .value = false };
    }
    const input = ptr.?[0..len];

    // Convert to lowercase
    var lower_buf: [64]u8 = undefined;
    const check_len = @min(len, lower_buf.len);
    for (input[0..check_len], 0..) |c, i| {
        lower_buf[i] = if (c >= 'A' and c <= 'Z') c + 32 else c;
    }
    const lower = lower_buf[0..check_len];

    const dangerous = [_][]const u8{
        "text/plain",
        "application/octet-stream",
        "application/x-unknown",
        "unknown/unknown",
    };

    for (dangerous) |d| {
        if (std.mem.eql(u8, lower, d)) {
            return .{ .status = .ok, .value = true };
        }
    }
    return .{ .status = .ok, .value = false };
}

/// Render content type to string
export fn proven_content_type_render(
    type_ptr: ?[*]const u8,
    type_len: usize,
    subtype_ptr: ?[*]const u8,
    subtype_len: usize,
    suffix_ptr: ?[*]const u8,
    suffix_len: usize,
    charset: Charset,
    has_charset: bool,
) StringResult {
    if (type_ptr == null or subtype_ptr == null) {
        return .{ .status = .err_null_pointer, .value = null, .length = 0 };
    }

    const type_str = type_ptr.?[0..type_len];
    const subtype_str = subtype_ptr.?[0..subtype_len];

    var buf: [256]u8 = undefined;
    var pos: usize = 0;

    // type/subtype
    @memcpy(buf[pos..][0..type_len], type_str);
    pos += type_len;
    buf[pos] = '/';
    pos += 1;
    @memcpy(buf[pos..][0..subtype_len], subtype_str);
    pos += subtype_len;

    // +suffix
    if (suffix_ptr != null and suffix_len > 0) {
        const suffix_str = suffix_ptr.?[0..suffix_len];
        buf[pos] = '+';
        pos += 1;
        @memcpy(buf[pos..][0..suffix_len], suffix_str);
        pos += suffix_len;
    }

    // ; charset=X
    if (has_charset) {
        const charset_str = switch (charset) {
            .utf8 => "; charset=utf-8",
            .utf16le => "; charset=utf-16le",
            .utf16be => "; charset=utf-16be",
            .iso8859_1 => "; charset=iso-8859-1",
            .ascii => "; charset=us-ascii",
            .windows1252 => "; charset=windows-1252",
            .other => "",
        };
        if (charset_str.len > 0) {
            @memcpy(buf[pos..][0..charset_str.len], charset_str);
            pos += charset_str.len;
        }
    }

    const output = allocator.allocSentinel(u8, pos, 0) catch {
        return .{ .status = .err_allocation_failed, .value = null, .length = 0 };
    };
    @memcpy(output[0..pos], buf[0..pos]);

    return .{ .status = .ok, .value = output.ptr, .length = pos };
}

/// Check if content type is JSON
export fn proven_content_type_is_json(subtype_ptr: ?[*]const u8, subtype_len: usize, suffix_ptr: ?[*]const u8, suffix_len: usize) BoolResult {
    if (subtype_ptr == null) {
        return .{ .status = .err_null_pointer, .value = false };
    }
    const subtype = subtype_ptr.?[0..subtype_len];
    if (std.mem.eql(u8, subtype, "json")) {
        return .{ .status = .ok, .value = true };
    }
    if (suffix_ptr != null and suffix_len > 0) {
        const suffix = suffix_ptr.?[0..suffix_len];
        if (std.mem.eql(u8, suffix, "json")) {
            return .{ .status = .ok, .value = true };
        }
    }
    return .{ .status = .ok, .value = false };
}

/// Check if content type is XML
export fn proven_content_type_is_xml(subtype_ptr: ?[*]const u8, subtype_len: usize, suffix_ptr: ?[*]const u8, suffix_len: usize) BoolResult {
    if (subtype_ptr == null) {
        return .{ .status = .err_null_pointer, .value = false };
    }
    const subtype = subtype_ptr.?[0..subtype_len];
    if (std.mem.eql(u8, subtype, "xml")) {
        return .{ .status = .ok, .value = true };
    }
    if (suffix_ptr != null and suffix_len > 0) {
        const suffix = suffix_ptr.?[0..suffix_len];
        if (std.mem.eql(u8, suffix, "xml")) {
            return .{ .status = .ok, .value = true };
        }
    }
    return .{ .status = .ok, .value = false };
}

// ============================================================================
// Version info
// ============================================================================

export fn proven_version_major() u32 {
    return 0;
}

export fn proven_version_minor() u32 {
    return 8;
}

export fn proven_version_patch() u32 {
    return 0;
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

test "header_has_crlf" {
    const safe = "application/json";
    const r1 = proven_header_has_crlf(safe.ptr, safe.len);
    try std.testing.expect(!r1.value);

    const unsafe = "text\r\nX-Injected: evil";
    const r2 = proven_header_has_crlf(unsafe.ptr, unsafe.len);
    try std.testing.expect(r2.value);
}

test "header_is_valid_name" {
    const valid = "Content-Type";
    const r1 = proven_header_is_valid_name(valid.ptr, valid.len);
    try std.testing.expect(r1.value);

    const invalid = "Content:Type";
    const r2 = proven_header_is_valid_name(invalid.ptr, invalid.len);
    try std.testing.expect(!r2.value);
}

test "header_is_dangerous" {
    const dangerous = "Host";
    const r1 = proven_header_is_dangerous(dangerous.ptr, dangerous.len);
    try std.testing.expect(r1.value);

    const safe = "X-Custom-Header";
    const r2 = proven_header_is_dangerous(safe.ptr, safe.len);
    try std.testing.expect(!r2.value);
}

test "header_build_hsts" {
    const r = proven_header_build_hsts(31536000, true, true);
    try std.testing.expectEqual(ProvenStatus.ok, r.status);
    defer proven_free_string(r.value);

    const expected = "max-age=31536000; includeSubDomains; preload";
    try std.testing.expectEqualStrings(expected, r.value.?[0..r.length]);
}

test "cookie_has_injection" {
    const safe = "session_token";
    const r1 = proven_cookie_has_injection(safe.ptr, safe.len);
    try std.testing.expect(!r1.value);

    const unsafe = "value;injected=true";
    const r2 = proven_cookie_has_injection(unsafe.ptr, unsafe.len);
    try std.testing.expect(r2.value);
}

test "cookie_get_prefix" {
    const host = "__Host-session";
    const r1 = proven_cookie_get_prefix(host.ptr, host.len);
    try std.testing.expectEqual(@as(i64, 2), r1.value);

    const secure = "__Secure-token";
    const r2 = proven_cookie_get_prefix(secure.ptr, secure.len);
    try std.testing.expectEqual(@as(i64, 1), r2.value);

    const normal = "regular";
    const r3 = proven_cookie_get_prefix(normal.ptr, normal.len);
    try std.testing.expectEqual(@as(i64, 0), r3.value);
}

test "content_type_parse" {
    const ct = "application/json; charset=utf-8";
    const r = proven_content_type_parse(ct.ptr, ct.len);
    try std.testing.expectEqual(ProvenStatus.ok, r.status);
    defer {
        var result = r;
        proven_content_type_free(&result);
    }

    try std.testing.expectEqualStrings("application", r.media_type.?[0..r.media_type_len]);
    try std.testing.expectEqualStrings("json", r.subtype.?[0..r.subtype_len]);
    try std.testing.expectEqual(MediaCategory.application, r.category);
    try std.testing.expect(r.has_charset);
    try std.testing.expectEqual(Charset.utf8, r.charset);
}

test "content_type_can_sniff_dangerous" {
    const dangerous = "text/plain";
    const r1 = proven_content_type_can_sniff_dangerous(dangerous.ptr, dangerous.len);
    try std.testing.expect(r1.value);

    const safe = "application/json";
    const r2 = proven_content_type_can_sniff_dangerous(safe.ptr, safe.len);
    try std.testing.expect(!r2.value);
}
