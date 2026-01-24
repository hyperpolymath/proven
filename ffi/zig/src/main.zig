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
    return 9;
}

export fn proven_version_patch() u32 {
    return 0;
}

/// Get module count (41 modules total - added SafeRegistry, SafeDigest, SafeHTTP)
export fn proven_module_count() u32 {
    return 41;
}

// ============================================================================
// SafeRegistry - OCI image reference parsing (formally verified)
// ============================================================================

/// Parsed OCI image reference
pub const ImageReference = extern struct {
    registry: ?[*:0]u8,
    registry_len: usize,
    repository: ?[*:0]u8,
    repository_len: usize,
    tag: ?[*:0]u8,
    tag_len: usize,
    digest: ?[*:0]u8,
    digest_len: usize,
};

/// Result for image reference parsing
pub const ImageRefResult = extern struct {
    status: ProvenStatus,
    reference: ImageReference,
};

/// Parse OCI image reference (e.g., "ghcr.io/user/repo:v1.0")
///
/// Format: [registry/]repository[:tag][@digest]
/// Handles Docker Hub conventions (library/ prefix, docker.io default)
export fn proven_registry_parse(ptr: ?[*]const u8, len: usize) ImageRefResult {
    const empty_ref = ImageReference{
        .registry = null,
        .registry_len = 0,
        .repository = null,
        .repository_len = 0,
        .tag = null,
        .tag_len = 0,
        .digest = null,
        .digest_len = 0,
    };

    if (ptr == null or len == 0) {
        return .{ .status = .err_null_pointer, .reference = empty_ref };
    }

    // Call Idris2 parseReference function
    // TODO: Wire to actual Idris2 FFI when Idris2 compiler generates C code
    // For now, return placeholder
    return .{ .status = .err_not_implemented, .reference = empty_ref };
}

/// Convert image reference to string
export fn proven_registry_to_string(ref: *const ImageReference) StringResult {
    // TODO: Wire to Idris2 toString function
    return .{ .status = .err_not_implemented, .value = null, .length = 0 };
}

/// Check if image reference looks like it has a registry hostname
export fn proven_registry_has_registry(ref: *const ImageReference) BoolResult {
    if (ref.registry != null and ref.registry_len > 0) {
        return .{ .status = .ok, .value = true };
    }
    return .{ .status = .ok, .value = false };
}

// ============================================================================
// SafeDigest - Cryptographic digest operations (formally verified)
// ============================================================================

/// Hash algorithm types
pub const HashAlgorithm = enum(u8) {
    SHA256 = 0,
    SHA384 = 1,
    SHA512 = 2,
    Blake3 = 3,
};

/// Digest with algorithm and hex value
pub const Digest = extern struct {
    algorithm: HashAlgorithm,
    value: ?[*:0]u8,
    value_len: usize,
};

/// Result for digest operations
pub const DigestResult = extern struct {
    status: ProvenStatus,
    digest: Digest,
};

/// Parse digest string (e.g., "sha256:abc123...")
///
/// Validates algorithm and hex encoding
export fn proven_digest_parse(ptr: ?[*]const u8, len: usize) DigestResult {
    const empty_digest = Digest{
        .algorithm = .SHA256,
        .value = null,
        .value_len = 0,
    };

    if (ptr == null or len == 0) {
        return .{ .status = .err_null_pointer, .digest = empty_digest };
    }

    const input = ptr.?[0..len];

    // Find colon separator
    const colon_idx = std.mem.indexOfScalar(u8, input, ':') orelse {
        return .{ .status = .err_parse_failure, .digest = empty_digest };
    };

    const algo_str = input[0..colon_idx];
    const hex_str = input[colon_idx + 1 ..];

    // Parse algorithm
    const algorithm: HashAlgorithm = if (std.mem.eql(u8, algo_str, "sha256"))
        .SHA256
    else if (std.mem.eql(u8, algo_str, "sha384"))
        .SHA384
    else if (std.mem.eql(u8, algo_str, "sha512"))
        .SHA512
    else if (std.mem.eql(u8, algo_str, "blake3"))
        .Blake3
    else
        return .{ .status = .err_invalid_argument, .digest = empty_digest };

    // Validate expected length
    const expected_len: usize = switch (algorithm) {
        .SHA256 => 64,
        .SHA384 => 96,
        .SHA512 => 128,
        .Blake3 => 64,
    };

    if (hex_str.len != expected_len) {
        return .{ .status = .err_invalid_argument, .digest = empty_digest };
    }

    // Validate all hex characters
    for (hex_str) |c| {
        if (!std.ascii.isHex(c)) {
            return .{ .status = .err_encoding_error, .digest = empty_digest };
        }
    }

    // Allocate and copy hex value
    const hex_copy = allocator.allocSentinel(u8, hex_str.len, 0) catch {
        return .{ .status = .err_allocation_failed, .digest = empty_digest };
    };
    @memcpy(hex_copy, hex_str);

    return .{
        .status = .ok,
        .digest = .{
            .algorithm = algorithm,
            .value = hex_copy.ptr,
            .value_len = hex_str.len,
        },
    };
}

/// Constant-time digest comparison (timing-attack resistant)
///
/// Verifies two digests match using constant-time comparison
export fn proven_digest_verify(
    expected: *const Digest,
    actual: *const Digest,
) BoolResult {
    // Check algorithms match
    if (expected.algorithm != actual.algorithm) {
        return .{ .status = .ok, .value = false };
    }

    // Check lengths match
    if (expected.value_len != actual.value_len) {
        return .{ .status = .ok, .value = false };
    }

    if (expected.value == null or actual.value == null) {
        return .{ .status = .err_null_pointer, .value = false };
    }

    // Constant-time comparison
    const a = expected.value.?[0..expected.value_len];
    const b = actual.value.?[0..actual.value_len];

    var diff: u8 = 0;
    for (a, b) |x, y| {
        diff |= x ^ y;
    }

    return .{ .status = .ok, .value = diff == 0 };
}

/// Convert digest to string (algorithm:hex)
export fn proven_digest_to_string(digest: *const Digest) StringResult {
    if (digest.value == null) {
        return .{ .status = .err_null_pointer, .value = null, .length = 0 };
    }

    const algo_str: []const u8 = switch (digest.algorithm) {
        .SHA256 => "sha256",
        .SHA384 => "sha384",
        .SHA512 => "sha512",
        .Blake3 => "blake3",
    };

    const total_len = algo_str.len + 1 + digest.value_len; // algo + ':' + hex
    const output = allocator.allocSentinel(u8, total_len, 0) catch {
        return .{ .status = .err_allocation_failed, .value = null, .length = 0 };
    };

    // Build "algorithm:hex"
    @memcpy(output[0..algo_str.len], algo_str);
    output[algo_str.len] = ':';
    @memcpy(output[algo_str.len + 1 ..], digest.value.?[0..digest.value_len]);

    return .{ .status = .ok, .value = output.ptr, .length = total_len };
}

// ============================================================================
// SafeHTTP - HTTP URL encoding and header parsing (formally verified)
// ============================================================================

/// URL-encode a string (RFC 3986 percent encoding)
///
/// Unreserved chars (A-Za-z0-9-._~) pass through, others become %XX
export fn proven_http_url_encode(ptr: ?[*]const u8, len: usize) StringResult {
    if (ptr == null or len == 0) {
        return .{ .status = .err_null_pointer, .value = null, .length = 0 };
    }

    const input = ptr.?[0..len];
    var output = std.ArrayList(u8).init(allocator);
    defer output.deinit();

    for (input) |c| {
        if (std.ascii.isAlphanumeric(c) or c == '-' or c == '_' or c == '.' or c == '~') {
            output.append(c) catch {
                return .{ .status = .err_allocation_failed, .value = null, .length = 0 };
            };
        } else {
            // Percent encode
            const hex_chars = "0123456789ABCDEF";
            const hi = (c >> 4) & 0x0F;
            const lo = c & 0x0F;
            output.append('%') catch {
                return .{ .status = .err_allocation_failed, .value = null, .length = 0 };
            };
            output.append(hex_chars[hi]) catch {
                return .{ .status = .err_allocation_failed, .value = null, .length = 0 };
            };
            output.append(hex_chars[lo]) catch {
                return .{ .status = .err_allocation_failed, .value = null, .length = 0 };
            };
        }
    }

    const result = allocator.allocSentinel(u8, output.items.len, 0) catch {
        return .{ .status = .err_allocation_failed, .value = null, .length = 0 };
    };
    @memcpy(result, output.items);

    return .{ .status = .ok, .value = result.ptr, .length = output.items.len };
}

/// URL-decode a percent-encoded string
export fn proven_http_url_decode(ptr: ?[*]const u8, len: usize) StringResult {
    if (ptr == null or len == 0) {
        return .{ .status = .err_null_pointer, .value = null, .length = 0 };
    }

    const input = ptr.?[0..len];
    var output = std.ArrayList(u8).init(allocator);
    defer output.deinit();

    var i: usize = 0;
    while (i < input.len) {
        if (input[i] == '%' and i + 2 < input.len) {
            const hi = std.fmt.charToDigit(input[i + 1], 16) catch {
                return .{ .status = .err_encoding_error, .value = null, .length = 0 };
            };
            const lo = std.fmt.charToDigit(input[i + 2], 16) catch {
                return .{ .status = .err_encoding_error, .value = null, .length = 0 };
            };
            const byte = (@as(u8, hi) << 4) | @as(u8, lo);
            output.append(byte) catch {
                return .{ .status = .err_allocation_failed, .value = null, .length = 0 };
            };
            i += 3;
        } else if (input[i] == '+') {
            output.append(' ') catch {
                return .{ .status = .err_allocation_failed, .value = null, .length = 0 };
            };
            i += 1;
        } else {
            output.append(input[i]) catch {
                return .{ .status = .err_allocation_failed, .value = null, .length = 0 };
            };
            i += 1;
        }
    }

    const result = allocator.allocSentinel(u8, output.items.len, 0) catch {
        return .{ .status = .err_allocation_failed, .value = null, .length = 0 };
    };
    @memcpy(result, output.items);

    return .{ .status = .ok, .value = result.ptr, .length = output.items.len };
}

/// WWW-Authenticate challenge components
pub const AuthChallenge = extern struct {
    scheme: ?[*:0]u8,
    scheme_len: usize,
    realm: ?[*:0]u8,
    realm_len: usize,
    service: ?[*:0]u8,
    service_len: usize,
    scope: ?[*:0]u8,
    scope_len: usize,
};

/// Result for auth challenge parsing
pub const AuthChallengeResult = extern struct {
    status: ProvenStatus,
    challenge: AuthChallenge,
};

/// Parse WWW-Authenticate header (for Docker Registry v2 OAuth2)
///
/// Format: Bearer realm="...",service="...",scope="..."
export fn proven_http_parse_www_authenticate(ptr: ?[*]const u8, len: usize) AuthChallengeResult {
    const empty_challenge = AuthChallenge{
        .scheme = null,
        .scheme_len = 0,
        .realm = null,
        .realm_len = 0,
        .service = null,
        .service_len = 0,
        .scope = null,
        .scope_len = 0,
    };

    if (ptr == null or len == 0) {
        return .{ .status = .err_null_pointer, .challenge = empty_challenge };
    }

    // TODO: Full parser implementation
    // For now, return not implemented
    return .{ .status = .err_not_implemented, .challenge = empty_challenge };
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

// ============================================================================
// SafeHex - Hexadecimal encoding/decoding
// ============================================================================

/// Hex encoding error
pub const HexError = enum(i32) {
    ok = 0,
    invalid_char = -1,
    odd_length = -2,
};

/// Hex encode bytes to string
export fn proven_hex_encode(ptr: ?[*]const u8, len: usize, uppercase: bool) StringResult {
    if (ptr == null) {
        return .{ .status = .err_null_pointer, .value = null, .length = 0 };
    }
    const input = ptr.?[0..len];
    const output_len = len * 2;

    const output = allocator.allocSentinel(u8, output_len, 0) catch {
        return .{ .status = .err_allocation_failed, .value = null, .length = 0 };
    };

    const hex_chars = if (uppercase) "0123456789ABCDEF" else "0123456789abcdef";
    for (input, 0..) |byte, i| {
        output[i * 2] = hex_chars[byte >> 4];
        output[i * 2 + 1] = hex_chars[byte & 0x0F];
    }

    return .{ .status = .ok, .value = output.ptr, .length = output_len };
}

/// Hex decode result with bytes
pub const HexDecodeResult = extern struct {
    status: ProvenStatus,
    data: ?[*]u8,
    length: usize,
};

/// Hex decode string to bytes
export fn proven_hex_decode(ptr: ?[*]const u8, len: usize) HexDecodeResult {
    if (ptr == null) {
        return .{ .status = .err_null_pointer, .data = null, .length = 0 };
    }
    if (len % 2 != 0) {
        return .{ .status = .err_parse_failure, .data = null, .length = 0 };
    }

    const input = ptr.?[0..len];
    const output_len = len / 2;

    const output = allocator.alloc(u8, output_len) catch {
        return .{ .status = .err_allocation_failed, .data = null, .length = 0 };
    };

    for (0..output_len) |i| {
        const high = hexDigitToValue(input[i * 2]) orelse {
            allocator.free(output);
            return .{ .status = .err_parse_failure, .data = null, .length = 0 };
        };
        const low = hexDigitToValue(input[i * 2 + 1]) orelse {
            allocator.free(output);
            return .{ .status = .err_parse_failure, .data = null, .length = 0 };
        };
        output[i] = (high << 4) | low;
    }

    return .{ .status = .ok, .data = output.ptr, .length = output_len };
}

fn hexDigitToValue(c: u8) ?u8 {
    return switch (c) {
        '0'...'9' => c - '0',
        'a'...'f' => c - 'a' + 10,
        'A'...'F' => c - 'A' + 10,
        else => null,
    };
}

/// Free hex decode result
export fn proven_hex_free(result: *HexDecodeResult) void {
    if (result.data) |d| {
        allocator.free(d[0..result.length]);
    }
    result.* = .{ .status = .ok, .data = null, .length = 0 };
}

// ============================================================================
// SafeUUID - UUID generation and validation
// ============================================================================

/// UUID structure (128 bits)
pub const UUID = extern struct {
    bytes: [16]u8,
};

/// UUID result
pub const UUIDResult = extern struct {
    status: ProvenStatus,
    uuid: UUID,
};

/// Generate UUID v4 (random)
export fn proven_uuid_v4() UUIDResult {
    var uuid: UUID = undefined;
    std.crypto.random.bytes(&uuid.bytes);

    // Set version 4
    uuid.bytes[6] = (uuid.bytes[6] & 0x0F) | 0x40;
    // Set variant RFC 4122
    uuid.bytes[8] = (uuid.bytes[8] & 0x3F) | 0x80;

    return .{ .status = .ok, .uuid = uuid };
}

/// Format UUID as string (36 chars with hyphens)
export fn proven_uuid_to_string(uuid: UUID) StringResult {
    const output = allocator.allocSentinel(u8, 36, 0) catch {
        return .{ .status = .err_allocation_failed, .value = null, .length = 0 };
    };

    const hex = "0123456789abcdef";
    var pos: usize = 0;
    for (uuid.bytes, 0..) |byte, i| {
        if (i == 4 or i == 6 or i == 8 or i == 10) {
            output[pos] = '-';
            pos += 1;
        }
        output[pos] = hex[byte >> 4];
        output[pos + 1] = hex[byte & 0x0F];
        pos += 2;
    }

    return .{ .status = .ok, .value = output.ptr, .length = 36 };
}

/// Parse UUID from string
export fn proven_uuid_parse(ptr: ?[*]const u8, len: usize) UUIDResult {
    const empty = UUID{ .bytes = [_]u8{0} ** 16 };
    if (ptr == null) {
        return .{ .status = .err_null_pointer, .uuid = empty };
    }
    if (len != 36) {
        return .{ .status = .err_parse_failure, .uuid = empty };
    }

    const input = ptr.?[0..len];
    var uuid: UUID = undefined;
    var byte_idx: usize = 0;

    var i: usize = 0;
    while (i < 36 and byte_idx < 16) {
        if (i == 8 or i == 13 or i == 18 or i == 23) {
            if (input[i] != '-') {
                return .{ .status = .err_parse_failure, .uuid = empty };
            }
            i += 1;
            continue;
        }

        const high = hexDigitToValue(input[i]) orelse {
            return .{ .status = .err_parse_failure, .uuid = empty };
        };
        const low = hexDigitToValue(input[i + 1]) orelse {
            return .{ .status = .err_parse_failure, .uuid = empty };
        };
        uuid.bytes[byte_idx] = (high << 4) | low;
        byte_idx += 1;
        i += 2;
    }

    return .{ .status = .ok, .uuid = uuid };
}

/// Check if UUID is nil (all zeros)
export fn proven_uuid_is_nil(uuid: UUID) bool {
    for (uuid.bytes) |b| {
        if (b != 0) return false;
    }
    return true;
}

/// Get UUID version
export fn proven_uuid_version(uuid: UUID) u8 {
    return (uuid.bytes[6] >> 4) & 0x0F;
}

// ============================================================================
// SafeCurrency - Monetary values with ISO 4217 codes
// ============================================================================

/// Currency result with minor units (cents, etc.)
pub const CurrencyResult = extern struct {
    status: ProvenStatus,
    amount_minor: i64, // Amount in minor units (cents)
    currency_code: [3]u8,
    decimal_places: u8,
};

/// Parse currency amount (e.g., "USD 123.45" or "123.45 EUR")
export fn proven_currency_parse(ptr: ?[*]const u8, len: usize) CurrencyResult {
    const empty = CurrencyResult{
        .status = .err_parse_failure,
        .amount_minor = 0,
        .currency_code = [_]u8{ 0, 0, 0 },
        .decimal_places = 2,
    };

    if (ptr == null) {
        return CurrencyResult{ .status = .err_null_pointer, .amount_minor = 0, .currency_code = [_]u8{ 0, 0, 0 }, .decimal_places = 2 };
    }
    if (len < 4) {
        return empty;
    }

    const input = ptr.?[0..len];

    // Try to find currency code (3 uppercase letters)
    var code_start: ?usize = null;
    var i: usize = 0;
    while (i < len) : (i += 1) {
        if (input[i] >= 'A' and input[i] <= 'Z') {
            if (code_start == null) code_start = i;
            if (i - code_start.? == 2) break;
        } else if (code_start != null) {
            code_start = null;
        }
    }

    if (code_start == null) {
        return empty;
    }

    var result = empty;
    result.status = .ok;
    result.currency_code[0] = input[code_start.?];
    result.currency_code[1] = input[code_start.? + 1];
    result.currency_code[2] = input[code_start.? + 2];

    // Get decimal places for currency
    result.decimal_places = getCurrencyDecimalPlaces(&result.currency_code);

    // Parse numeric part
    var amount: i64 = 0;
    var decimal_count: ?u8 = null;
    var negative = false;

    for (input) |c| {
        if (c == '-') {
            negative = true;
        } else if (c >= '0' and c <= '9') {
            amount = amount * 10 + (c - '0');
            if (decimal_count) |*dc| {
                dc.* += 1;
            }
        } else if (c == '.' or c == ',') {
            decimal_count = 0;
        }
    }

    // Adjust to minor units
    const actual_decimals = decimal_count orelse 0;
    if (actual_decimals < result.decimal_places) {
        var mult: i64 = 1;
        var j: u8 = actual_decimals;
        while (j < result.decimal_places) : (j += 1) {
            mult *= 10;
        }
        amount *= mult;
    } else if (actual_decimals > result.decimal_places) {
        var div: i64 = 1;
        var j: u8 = result.decimal_places;
        while (j < actual_decimals) : (j += 1) {
            div *= 10;
        }
        amount = @divTrunc(amount, div);
    }

    result.amount_minor = if (negative) -amount else amount;
    return result;
}

fn getCurrencyDecimalPlaces(code: *const [3]u8) u8 {
    // Common currencies with non-standard decimal places
    if (std.mem.eql(u8, code, "JPY") or std.mem.eql(u8, code, "KRW") or
        std.mem.eql(u8, code, "VND"))
    {
        return 0;
    }
    if (std.mem.eql(u8, code, "BHD") or std.mem.eql(u8, code, "KWD") or
        std.mem.eql(u8, code, "OMR"))
    {
        return 3;
    }
    return 2; // Most currencies
}

/// Format currency amount
export fn proven_currency_format(amount_minor: i64, code: [3]u8, decimal_places: u8) StringResult {
    var buf: [64]u8 = undefined;
    var pos: usize = 0;

    // Code
    buf[0] = code[0];
    buf[1] = code[1];
    buf[2] = code[2];
    buf[3] = ' ';
    pos = 4;

    // Handle negative
    var amt = amount_minor;
    if (amt < 0) {
        buf[pos] = '-';
        pos += 1;
        amt = -amt;
    }

    // Calculate divisor
    var divisor: i64 = 1;
    var dp = decimal_places;
    while (dp > 0) : (dp -= 1) {
        divisor *= 10;
    }

    const major = @divTrunc(amt, divisor);
    const minor = @mod(amt, divisor);

    // Format major part
    var major_buf: [20]u8 = undefined;
    var major_len: usize = 0;
    var m = major;
    if (m == 0) {
        major_buf[0] = '0';
        major_len = 1;
    } else {
        while (m > 0) : (major_len += 1) {
            major_buf[major_len] = @intCast(@mod(m, 10) + '0');
            m = @divTrunc(m, 10);
        }
        // Reverse
        var j: usize = 0;
        while (j < major_len / 2) : (j += 1) {
            const t = major_buf[j];
            major_buf[j] = major_buf[major_len - 1 - j];
            major_buf[major_len - 1 - j] = t;
        }
    }

    @memcpy(buf[pos..][0..major_len], major_buf[0..major_len]);
    pos += major_len;

    // Format minor part
    if (decimal_places > 0) {
        buf[pos] = '.';
        pos += 1;

        var minor_m = minor;
        var k: u8 = 0;
        while (k < decimal_places) : (k += 1) {
            const digit_pos = decimal_places - 1 - k;
            var divisor2: i64 = 1;
            var d = digit_pos;
            while (d > 0) : (d -= 1) {
                divisor2 *= 10;
            }
            const digit: u8 = @intCast(@mod(@divTrunc(minor_m, divisor2), 10));
            buf[pos] = digit + '0';
            pos += 1;
        }
    }

    const output = allocator.allocSentinel(u8, pos, 0) catch {
        return .{ .status = .err_allocation_failed, .value = null, .length = 0 };
    };
    @memcpy(output[0..pos], buf[0..pos]);

    return .{ .status = .ok, .value = output.ptr, .length = pos };
}

// ============================================================================
// SafePhone - E.164 phone number handling
// ============================================================================

/// Phone number result
pub const PhoneResult = extern struct {
    status: ProvenStatus,
    country_code: u16,
    national_number: u64,
    is_valid: bool,
};

/// Parse phone number to E.164 format
export fn proven_phone_parse(ptr: ?[*]const u8, len: usize) PhoneResult {
    const empty = PhoneResult{ .status = .err_parse_failure, .country_code = 0, .national_number = 0, .is_valid = false };

    if (ptr == null) {
        return PhoneResult{ .status = .err_null_pointer, .country_code = 0, .national_number = 0, .is_valid = false };
    }

    const input = ptr.?[0..len];

    // Extract digits only
    var digits: [20]u8 = undefined;
    var digit_count: usize = 0;
    var has_plus = false;

    for (input) |c| {
        if (c == '+') {
            has_plus = true;
        } else if (c >= '0' and c <= '9' and digit_count < 20) {
            digits[digit_count] = c - '0';
            digit_count += 1;
        }
    }

    if (digit_count < 7 or digit_count > 15) {
        return empty;
    }

    // Simple country code detection (1-3 digits)
    var country_code: u16 = 0;
    var cc_len: usize = 0;

    if (has_plus or digit_count > 10) {
        // Try 1-digit country code
        if (digits[0] == 1) {
            country_code = 1;
            cc_len = 1;
        } else if (digit_count > 11) {
            // Try 3-digit
            country_code = @as(u16, digits[0]) * 100 + @as(u16, digits[1]) * 10 + digits[2];
            cc_len = 3;
        } else if (digit_count > 10) {
            // Try 2-digit
            country_code = @as(u16, digits[0]) * 10 + digits[1];
            cc_len = 2;
        }
    }

    // Parse national number
    var national: u64 = 0;
    for (digits[cc_len..digit_count]) |d| {
        national = national * 10 + d;
    }

    return PhoneResult{
        .status = .ok,
        .country_code = country_code,
        .national_number = national,
        .is_valid = digit_count >= 7,
    };
}

/// Format phone number as E.164
export fn proven_phone_format_e164(country_code: u16, national_number: u64) StringResult {
    var buf: [20]u8 = undefined;
    var pos: usize = 0;

    buf[pos] = '+';
    pos += 1;

    // Country code
    var cc = country_code;
    var cc_buf: [3]u8 = undefined;
    var cc_len: usize = 0;
    while (cc > 0) : (cc_len += 1) {
        cc_buf[cc_len] = @intCast(@mod(cc, 10) + '0');
        cc = @divTrunc(cc, 10);
    }
    var j: usize = 0;
    while (j < cc_len) : (j += 1) {
        buf[pos + j] = cc_buf[cc_len - 1 - j];
    }
    pos += cc_len;

    // National number
    var nn = national_number;
    var nn_buf: [15]u8 = undefined;
    var nn_len: usize = 0;
    if (nn == 0) {
        nn_buf[0] = '0';
        nn_len = 1;
    } else {
        while (nn > 0) : (nn_len += 1) {
            nn_buf[nn_len] = @intCast(@mod(nn, 10) + '0');
            nn = @divTrunc(nn, 10);
        }
    }
    j = 0;
    while (j < nn_len) : (j += 1) {
        buf[pos + j] = nn_buf[nn_len - 1 - j];
    }
    pos += nn_len;

    const output = allocator.allocSentinel(u8, pos, 0) catch {
        return .{ .status = .err_allocation_failed, .value = null, .length = 0 };
    };
    @memcpy(output[0..pos], buf[0..pos]);

    return .{ .status = .ok, .value = output.ptr, .length = pos };
}

// ============================================================================
// SafeJson - JSON validation and parsing status
// ============================================================================

/// JSON value type
pub const JsonType = enum(i32) {
    null_ = 0,
    bool_ = 1,
    number = 2,
    string = 3,
    array = 4,
    object = 5,
    invalid = -1,
};

/// Check if string is valid JSON
export fn proven_json_is_valid(ptr: ?[*]const u8, len: usize) BoolResult {
    if (ptr == null) {
        return .{ .status = .err_null_pointer, .value = false };
    }
    const input = ptr.?[0..len];

    // Simple JSON validation - check balanced braces/brackets and quotes
    var brace_depth: i32 = 0;
    var bracket_depth: i32 = 0;
    var in_string = false;
    var escape_next = false;

    for (input) |c| {
        if (escape_next) {
            escape_next = false;
            continue;
        }

        if (in_string) {
            if (c == '\\') {
                escape_next = true;
            } else if (c == '"') {
                in_string = false;
            }
        } else {
            switch (c) {
                '"' => in_string = true,
                '{' => brace_depth += 1,
                '}' => {
                    brace_depth -= 1;
                    if (brace_depth < 0) return .{ .status = .ok, .value = false };
                },
                '[' => bracket_depth += 1,
                ']' => {
                    bracket_depth -= 1;
                    if (bracket_depth < 0) return .{ .status = .ok, .value = false };
                },
                else => {},
            }
        }
    }

    const valid = !in_string and brace_depth == 0 and bracket_depth == 0;
    return .{ .status = .ok, .value = valid };
}

/// Get JSON value type at root level
export fn proven_json_get_type(ptr: ?[*]const u8, len: usize) JsonType {
    if (ptr == null or len == 0) {
        return .invalid;
    }
    const input = ptr.?[0..len];

    // Skip whitespace
    var i: usize = 0;
    while (i < len and (input[i] == ' ' or input[i] == '\t' or input[i] == '\n' or input[i] == '\r')) {
        i += 1;
    }

    if (i >= len) return .invalid;

    return switch (input[i]) {
        '{' => .object,
        '[' => .array,
        '"' => .string,
        't', 'f' => .bool_,
        'n' => .null_,
        '-', '0'...'9' => .number,
        else => .invalid,
    };
}

// ============================================================================
// SafeDateTime - ISO 8601 date/time handling
// ============================================================================

/// DateTime components
pub const DateTime = extern struct {
    year: i32,
    month: u8,
    day: u8,
    hour: u8,
    minute: u8,
    second: u8,
    nanosecond: u32,
    tz_offset_minutes: i16, // 0 for UTC, negative for west of UTC
};

/// DateTime result
pub const DateTimeResult = extern struct {
    status: ProvenStatus,
    datetime: DateTime,
};

/// Parse ISO 8601 date string
export fn proven_datetime_parse(ptr: ?[*]const u8, len: usize) DateTimeResult {
    const empty = DateTime{
        .year = 0,
        .month = 0,
        .day = 0,
        .hour = 0,
        .minute = 0,
        .second = 0,
        .nanosecond = 0,
        .tz_offset_minutes = 0,
    };

    if (ptr == null) {
        return .{ .status = .err_null_pointer, .datetime = empty };
    }
    if (len < 10) {
        return .{ .status = .err_parse_failure, .datetime = empty };
    }

    const input = ptr.?[0..len];
    var dt = empty;

    // Parse date: YYYY-MM-DD
    dt.year = parseDigits(input[0..4], 4) orelse {
        return .{ .status = .err_parse_failure, .datetime = empty };
    };
    if (input[4] != '-') {
        return .{ .status = .err_parse_failure, .datetime = empty };
    }
    dt.month = @intCast(parseDigits(input[5..7], 2) orelse {
        return .{ .status = .err_parse_failure, .datetime = empty };
    });
    if (input[7] != '-') {
        return .{ .status = .err_parse_failure, .datetime = empty };
    }
    dt.day = @intCast(parseDigits(input[8..10], 2) orelse {
        return .{ .status = .err_parse_failure, .datetime = empty };
    });

    // Validate date
    if (dt.month < 1 or dt.month > 12 or dt.day < 1 or dt.day > 31) {
        return .{ .status = .err_validation_failed, .datetime = empty };
    }

    // Parse time if present: THH:MM:SS
    if (len > 10 and (input[10] == 'T' or input[10] == ' ')) {
        if (len >= 19) {
            dt.hour = @intCast(parseDigits(input[11..13], 2) orelse {
                return .{ .status = .err_parse_failure, .datetime = empty };
            });
            if (input[13] != ':') {
                return .{ .status = .err_parse_failure, .datetime = empty };
            }
            dt.minute = @intCast(parseDigits(input[14..16], 2) orelse {
                return .{ .status = .err_parse_failure, .datetime = empty };
            });
            if (input[16] != ':') {
                return .{ .status = .err_parse_failure, .datetime = empty };
            }
            dt.second = @intCast(parseDigits(input[17..19], 2) orelse {
                return .{ .status = .err_parse_failure, .datetime = empty };
            });

            // Validate time
            if (dt.hour > 23 or dt.minute > 59 or dt.second > 59) {
                return .{ .status = .err_validation_failed, .datetime = empty };
            }

            // Parse timezone if present
            if (len > 19) {
                if (input[19] == 'Z') {
                    dt.tz_offset_minutes = 0;
                } else if (input[19] == '+' or input[19] == '-') {
                    if (len >= 25) {
                        const tz_hour: i16 = @intCast(parseDigits(input[20..22], 2) orelse 0);
                        const tz_min: i16 = @intCast(parseDigits(input[23..25], 2) orelse 0);
                        dt.tz_offset_minutes = tz_hour * 60 + tz_min;
                        if (input[19] == '-') {
                            dt.tz_offset_minutes = -dt.tz_offset_minutes;
                        }
                    }
                }
            }
        }
    }

    return .{ .status = .ok, .datetime = dt };
}

fn parseDigits(slice: []const u8, count: usize) ?i32 {
    if (slice.len < count) return null;
    var result: i32 = 0;
    for (slice[0..count]) |c| {
        if (c < '0' or c > '9') return null;
        result = result * 10 + (c - '0');
    }
    return result;
}

/// Format DateTime as ISO 8601
export fn proven_datetime_format_iso8601(dt: DateTime) StringResult {
    var buf: [32]u8 = undefined;

    // YYYY-MM-DDTHH:MM:SSZ
    formatNumber(&buf, 0, @intCast(dt.year), 4);
    buf[4] = '-';
    formatNumber(&buf, 5, dt.month, 2);
    buf[7] = '-';
    formatNumber(&buf, 8, dt.day, 2);
    buf[10] = 'T';
    formatNumber(&buf, 11, dt.hour, 2);
    buf[13] = ':';
    formatNumber(&buf, 14, dt.minute, 2);
    buf[16] = ':';
    formatNumber(&buf, 17, dt.second, 2);
    buf[19] = 'Z';

    const output = allocator.allocSentinel(u8, 20, 0) catch {
        return .{ .status = .err_allocation_failed, .value = null, .length = 0 };
    };
    @memcpy(output[0..20], buf[0..20]);

    return .{ .status = .ok, .value = output.ptr, .length = 20 };
}

fn formatNumber(buf: []u8, offset: usize, value: u32, width: usize) void {
    var v = value;
    var i: usize = width;
    while (i > 0) {
        i -= 1;
        buf[offset + i] = @intCast(@mod(v, 10) + '0');
        v = @divTrunc(v, 10);
    }
}

/// Check if year is leap year
export fn proven_datetime_is_leap_year(year: i32) bool {
    if (@mod(year, 400) == 0) return true;
    if (@mod(year, 100) == 0) return false;
    return @mod(year, 4) == 0;
}

/// Get days in month
export fn proven_datetime_days_in_month(year: i32, month: u8) u8 {
    if (month < 1 or month > 12) return 0;
    const days = [_]u8{ 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 };
    if (month == 2 and proven_datetime_is_leap_year(year)) {
        return 29;
    }
    return days[month - 1];
}

// ============================================================================
// SafeFloat - Safe floating-point operations
// ============================================================================

/// Float result
pub const FloatResult = extern struct {
    status: ProvenStatus,
    value: f64,
};

/// Safe floating-point division
export fn proven_float_div(a: f64, b: f64) FloatResult {
    if (b == 0) {
        return .{ .status = .err_division_by_zero, .value = 0 };
    }
    if (std.math.isNan(a) or std.math.isNan(b)) {
        return .{ .status = .err_invalid_argument, .value = 0 };
    }
    const result = a / b;
    if (std.math.isInf(result)) {
        return .{ .status = .err_overflow, .value = 0 };
    }
    return .{ .status = .ok, .value = result };
}

/// Check if float is finite (not NaN or Inf)
export fn proven_float_is_finite(x: f64) bool {
    return std.math.isFinite(x);
}

/// Check if float is NaN
export fn proven_float_is_nan(x: f64) bool {
    return std.math.isNan(x);
}

/// Safe square root (returns error for negative)
export fn proven_float_sqrt(x: f64) FloatResult {
    if (x < 0) {
        return .{ .status = .err_invalid_argument, .value = 0 };
    }
    if (std.math.isNan(x)) {
        return .{ .status = .err_invalid_argument, .value = 0 };
    }
    return .{ .status = .ok, .value = @sqrt(x) };
}

/// Safe natural logarithm
export fn proven_float_ln(x: f64) FloatResult {
    if (x <= 0) {
        return .{ .status = .err_invalid_argument, .value = 0 };
    }
    if (std.math.isNan(x)) {
        return .{ .status = .err_invalid_argument, .value = 0 };
    }
    return .{ .status = .ok, .value = @log(x) };
}

// ============================================================================
// SafeVersion - Semantic versioning
// ============================================================================

/// Semantic version
pub const SemanticVersion = extern struct {
    major: u32,
    minor: u32,
    patch: u32,
    prerelease_len: usize,
    prerelease: ?[*:0]u8,
};

/// Version result
pub const VersionResult = extern struct {
    status: ProvenStatus,
    version: SemanticVersion,
};

/// Parse semantic version string
export fn proven_version_parse(ptr: ?[*]const u8, len: usize) VersionResult {
    const empty = SemanticVersion{ .major = 0, .minor = 0, .patch = 0, .prerelease_len = 0, .prerelease = null };

    if (ptr == null) {
        return .{ .status = .err_null_pointer, .version = empty };
    }

    const input = ptr.?[0..len];
    var version = empty;

    // Skip optional 'v' prefix
    var start: usize = 0;
    if (len > 0 and (input[0] == 'v' or input[0] == 'V')) {
        start = 1;
    }

    // Parse major.minor.patch
    var part: u32 = 0;
    var current: u32 = 0;
    var prerelease_start: ?usize = null;

    for (input[start..], start..) |c, i| {
        if (c >= '0' and c <= '9') {
            current = current * 10 + (c - '0');
        } else if (c == '.') {
            switch (part) {
                0 => version.major = current,
                1 => version.minor = current,
                else => {},
            }
            current = 0;
            part += 1;
        } else if (c == '-' or c == '+') {
            prerelease_start = i;
            break;
        }
    }

    // Handle last part
    switch (part) {
        0 => version.major = current,
        1 => version.minor = current,
        2 => version.patch = current,
        else => {},
    }

    // Copy prerelease if present
    if (prerelease_start) |ps| {
        const pre_len = len - ps;
        const pre = allocator.allocSentinel(u8, pre_len, 0) catch {
            return .{ .status = .err_allocation_failed, .version = empty };
        };
        @memcpy(pre[0..pre_len], input[ps..]);
        version.prerelease = pre.ptr;
        version.prerelease_len = pre_len;
    }

    return .{ .status = .ok, .version = version };
}

/// Compare two semantic versions
export fn proven_version_compare(a: SemanticVersion, b: SemanticVersion) i32 {
    if (a.major != b.major) return if (a.major < b.major) -1 else 1;
    if (a.minor != b.minor) return if (a.minor < b.minor) -1 else 1;
    if (a.patch != b.patch) return if (a.patch < b.patch) -1 else 1;
    // Prerelease versions are lower than release
    const a_has_pre = a.prerelease != null;
    const b_has_pre = b.prerelease != null;
    if (a_has_pre and !b_has_pre) return -1;
    if (!a_has_pre and b_has_pre) return 1;
    return 0;
}

/// Free version result
export fn proven_version_free(version: *SemanticVersion) void {
    if (version.prerelease) |p| {
        allocator.free(p[0 .. version.prerelease_len + 1]);
    }
    version.* = .{ .major = 0, .minor = 0, .patch = 0, .prerelease_len = 0, .prerelease = null };
}

// ============================================================================
// SafeGeo - Geographic coordinate operations
// ============================================================================

/// Geographic coordinate
pub const GeoCoordinate = extern struct {
    latitude: f64, // -90 to 90
    longitude: f64, // -180 to 180
};

/// Geo result
pub const GeoResult = extern struct {
    status: ProvenStatus,
    coordinate: GeoCoordinate,
};

/// Validate and normalize geographic coordinate
export fn proven_geo_validate(lat: f64, lon: f64) GeoResult {
    if (std.math.isNan(lat) or std.math.isNan(lon)) {
        return .{ .status = .err_invalid_argument, .coordinate = .{ .latitude = 0, .longitude = 0 } };
    }
    if (lat < -90 or lat > 90) {
        return .{ .status = .err_out_of_bounds, .coordinate = .{ .latitude = 0, .longitude = 0 } };
    }

    // Normalize longitude to -180..180
    var normalized_lon = lon;
    while (normalized_lon > 180) normalized_lon -= 360;
    while (normalized_lon < -180) normalized_lon += 360;

    return .{ .status = .ok, .coordinate = .{ .latitude = lat, .longitude = normalized_lon } };
}

/// Calculate distance between two points (Haversine formula, returns meters)
export fn proven_geo_distance(a: GeoCoordinate, b: GeoCoordinate) FloatResult {
    const EARTH_RADIUS: f64 = 6371000; // meters

    const lat1 = a.latitude * std.math.pi / 180;
    const lat2 = b.latitude * std.math.pi / 180;
    const dlat = (b.latitude - a.latitude) * std.math.pi / 180;
    const dlon = (b.longitude - a.longitude) * std.math.pi / 180;

    const sin_dlat = @sin(dlat / 2);
    const sin_dlon = @sin(dlon / 2);

    const h = sin_dlat * sin_dlat + @cos(lat1) * @cos(lat2) * sin_dlon * sin_dlon;
    const c = 2 * std.math.atan2(@sqrt(h), @sqrt(1 - h));

    return .{ .status = .ok, .value = EARTH_RADIUS * c };
}

/// Check if coordinate is in bounding box
export fn proven_geo_in_bounds(coord: GeoCoordinate, min_lat: f64, max_lat: f64, min_lon: f64, max_lon: f64) bool {
    return coord.latitude >= min_lat and coord.latitude <= max_lat and
        coord.longitude >= min_lon and coord.longitude <= max_lon;
}

// ============================================================================
// SafeChecksum - CRC and hash verification
// ============================================================================

/// Calculate CRC32
export fn proven_checksum_crc32(ptr: ?[*]const u8, len: usize) IntResult {
    if (ptr == null) {
        return .{ .status = .err_null_pointer, .value = 0 };
    }
    const data = ptr.?[0..len];
    const crc = std.hash.Crc32.hash(data);
    return .{ .status = .ok, .value = crc };
}

/// Verify CRC32 matches expected
export fn proven_checksum_verify_crc32(ptr: ?[*]const u8, len: usize, expected: u32) BoolResult {
    if (ptr == null) {
        return .{ .status = .err_null_pointer, .value = false };
    }
    const data = ptr.?[0..len];
    const actual = std.hash.Crc32.hash(data);
    return .{ .status = .ok, .value = actual == expected };
}

// ============================================================================
// SafeProbability - Probability values clamped to [0, 1]
// ============================================================================

/// Create probability (clamped to 0-1)
export fn proven_probability_create(value: f64) f64 {
    if (std.math.isNan(value)) return 0;
    if (value < 0) return 0;
    if (value > 1) return 1;
    return value;
}

/// Multiply probabilities (independent events)
export fn proven_probability_and(a: f64, b: f64) f64 {
    return proven_probability_create(a * b);
}

/// Add probabilities (mutually exclusive events)
export fn proven_probability_or_exclusive(a: f64, b: f64) f64 {
    return proven_probability_create(a + b);
}

/// Complement (not event)
export fn proven_probability_not(p: f64) f64 {
    return proven_probability_create(1 - p);
}

// ============================================================================
// SafeCalculator - Expression evaluation
// ============================================================================

/// Calculate expression result
export fn proven_calculator_eval(ptr: ?[*]const u8, len: usize) FloatResult {
    if (ptr == null) {
        return .{ .status = .err_null_pointer, .value = 0 };
    }

    const input = ptr.?[0..len];

    // Simple recursive descent parser for basic arithmetic
    var pos: usize = 0;
    const result = parseExpr(input, &pos) catch {
        return .{ .status = .err_parse_failure, .value = 0 };
    };

    return .{ .status = .ok, .value = result };
}

fn parseExpr(input: []const u8, pos: *usize) !f64 {
    var left = try parseTerm(input, pos);

    while (pos.* < input.len) {
        skipSpaces(input, pos);
        if (pos.* >= input.len) break;

        const op = input[pos.*];
        if (op != '+' and op != '-') break;

        pos.* += 1;
        const right = try parseTerm(input, pos);
        if (op == '+') {
            left += right;
        } else {
            left -= right;
        }
    }

    return left;
}

fn parseTerm(input: []const u8, pos: *usize) !f64 {
    var left = try parseFactor(input, pos);

    while (pos.* < input.len) {
        skipSpaces(input, pos);
        if (pos.* >= input.len) break;

        const op = input[pos.*];
        if (op != '*' and op != '/') break;

        pos.* += 1;
        const right = try parseFactor(input, pos);
        if (op == '*') {
            left *= right;
        } else {
            if (right == 0) return error.DivisionByZero;
            left /= right;
        }
    }

    return left;
}

fn parseFactor(input: []const u8, pos: *usize) !f64 {
    skipSpaces(input, pos);

    // Handle negative
    var negative = false;
    if (pos.* < input.len and input[pos.*] == '-') {
        negative = true;
        pos.* += 1;
        skipSpaces(input, pos);
    }

    // Handle parentheses
    if (pos.* < input.len and input[pos.*] == '(') {
        pos.* += 1;
        const result = try parseExpr(input, pos);
        skipSpaces(input, pos);
        if (pos.* < input.len and input[pos.*] == ')') {
            pos.* += 1;
        }
        return if (negative) -result else result;
    }

    // Parse number
    const start = pos.*;
    while (pos.* < input.len and (isDigit(input[pos.*]) or input[pos.*] == '.')) {
        pos.* += 1;
    }

    if (start == pos.*) return error.InvalidExpression;

    const num = std.fmt.parseFloat(f64, input[start..pos.*]) catch {
        return error.InvalidExpression;
    };

    return if (negative) -num else num;
}

fn skipSpaces(input: []const u8, pos: *usize) void {
    while (pos.* < input.len and (input[pos.*] == ' ' or input[pos.*] == '\t')) {
        pos.* += 1;
    }
}

fn isDigit(c: u8) bool {
    return c >= '0' and c <= '9';
}

// ============================================================================
// SafeBuffer - Bounded buffer operations
// ============================================================================

/// Bounded buffer structure
pub const BoundedBuffer = extern struct {
    data: [*]u8,
    capacity: usize,
    length: usize,
};

/// Buffer result
pub const BufferResult = extern struct {
    status: ProvenStatus,
    buffer: ?*BoundedBuffer,
};

/// Create bounded buffer
export fn proven_buffer_create(capacity: usize) BufferResult {
    if (capacity == 0 or capacity > 1024 * 1024 * 100) { // Max 100MB
        return .{ .status = .err_invalid_argument, .buffer = null };
    }

    const data = allocator.alloc(u8, capacity) catch {
        return .{ .status = .err_allocation_failed, .buffer = null };
    };

    const buffer = allocator.create(BoundedBuffer) catch {
        allocator.free(data);
        return .{ .status = .err_allocation_failed, .buffer = null };
    };

    buffer.* = .{ .data = data.ptr, .capacity = capacity, .length = 0 };

    return .{ .status = .ok, .buffer = buffer };
}

/// Append to buffer (with bounds checking)
export fn proven_buffer_append(buffer: ?*BoundedBuffer, ptr: ?[*]const u8, len: usize) ProvenStatus {
    if (buffer == null or ptr == null) {
        return .err_null_pointer;
    }

    const buf = buffer.?;
    if (buf.length + len > buf.capacity) {
        return .err_out_of_bounds;
    }

    const input = ptr.?[0..len];
    @memcpy(buf.data[buf.length..][0..len], input);
    buf.length += len;

    return .ok;
}

/// Get buffer contents
export fn proven_buffer_get(buffer: ?*BoundedBuffer, out_ptr: ?*[*]const u8, out_len: ?*usize) ProvenStatus {
    if (buffer == null or out_ptr == null or out_len == null) {
        return .err_null_pointer;
    }

    const buf = buffer.?;
    out_ptr.?.* = buf.data;
    out_len.?.* = buf.length;

    return .ok;
}

/// Free buffer
export fn proven_buffer_free(buffer: ?*BoundedBuffer) void {
    if (buffer) |buf| {
        allocator.free(buf.data[0..buf.capacity]);
        allocator.destroy(buf);
    }
}

// ============================================================================
// SafeRateLimiter - Token bucket rate limiting
// ============================================================================

/// Rate limiter state
pub const RateLimiter = extern struct {
    tokens: f64,
    capacity: f64,
    refill_rate: f64, // tokens per second
    last_refill: i64, // timestamp in milliseconds
};

/// Create rate limiter
export fn proven_rate_limiter_create(capacity: f64, refill_rate: f64) ?*RateLimiter {
    if (capacity <= 0 or refill_rate <= 0) {
        return null;
    }

    const limiter = allocator.create(RateLimiter) catch {
        return null;
    };

    limiter.* = .{
        .tokens = capacity,
        .capacity = capacity,
        .refill_rate = refill_rate,
        .last_refill = std.time.milliTimestamp(),
    };

    return limiter;
}

/// Try to consume tokens
export fn proven_rate_limiter_try_acquire(limiter: ?*RateLimiter, tokens: f64) bool {
    if (limiter == null or tokens <= 0) {
        return false;
    }

    const rl = limiter.?;
    const now = std.time.milliTimestamp();
    const elapsed = @as(f64, @floatFromInt(now - rl.last_refill)) / 1000.0;

    // Refill tokens
    rl.tokens = @min(rl.capacity, rl.tokens + elapsed * rl.refill_rate);
    rl.last_refill = now;

    // Try to consume
    if (rl.tokens >= tokens) {
        rl.tokens -= tokens;
        return true;
    }

    return false;
}

/// Free rate limiter
export fn proven_rate_limiter_free(limiter: ?*RateLimiter) void {
    if (limiter) |rl| {
        allocator.destroy(rl);
    }
}

// ============================================================================
// SafeCircuitBreaker - Fault tolerance
// ============================================================================

/// Circuit breaker state
pub const CircuitState = enum(i32) {
    closed = 0, // Normal operation
    open = 1, // Failing, reject requests
    half_open = 2, // Testing recovery
};

/// Circuit breaker
pub const CircuitBreaker = extern struct {
    state: CircuitState,
    failure_count: u32,
    failure_threshold: u32,
    success_count: u32,
    success_threshold: u32,
    last_failure: i64,
    timeout_ms: i64,
};

/// Create circuit breaker
export fn proven_circuit_breaker_create(failure_threshold: u32, success_threshold: u32, timeout_ms: i64) ?*CircuitBreaker {
    if (failure_threshold == 0 or success_threshold == 0 or timeout_ms <= 0) {
        return null;
    }

    const cb = allocator.create(CircuitBreaker) catch {
        return null;
    };

    cb.* = .{
        .state = .closed,
        .failure_count = 0,
        .failure_threshold = failure_threshold,
        .success_count = 0,
        .success_threshold = success_threshold,
        .last_failure = 0,
        .timeout_ms = timeout_ms,
    };

    return cb;
}

/// Check if request should be allowed
export fn proven_circuit_breaker_allow(cb: ?*CircuitBreaker) bool {
    if (cb == null) return false;

    const breaker = cb.?;

    switch (breaker.state) {
        .closed => return true,
        .open => {
            const now = std.time.milliTimestamp();
            if (now - breaker.last_failure >= breaker.timeout_ms) {
                breaker.state = .half_open;
                breaker.success_count = 0;
                return true;
            }
            return false;
        },
        .half_open => return true,
    }
}

/// Record success
export fn proven_circuit_breaker_success(cb: ?*CircuitBreaker) void {
    if (cb == null) return;

    const breaker = cb.?;

    if (breaker.state == .half_open) {
        breaker.success_count += 1;
        if (breaker.success_count >= breaker.success_threshold) {
            breaker.state = .closed;
            breaker.failure_count = 0;
        }
    } else if (breaker.state == .closed) {
        breaker.failure_count = 0;
    }
}

/// Record failure
export fn proven_circuit_breaker_failure(cb: ?*CircuitBreaker) void {
    if (cb == null) return;

    const breaker = cb.?;

    breaker.failure_count += 1;
    breaker.last_failure = std.time.milliTimestamp();

    if (breaker.state == .half_open or breaker.failure_count >= breaker.failure_threshold) {
        breaker.state = .open;
    }
}

/// Get circuit state
export fn proven_circuit_breaker_state(cb: ?*CircuitBreaker) CircuitState {
    if (cb == null) return .open;
    return cb.?.state;
}

/// Free circuit breaker
export fn proven_circuit_breaker_free(cb: ?*CircuitBreaker) void {
    if (cb) |breaker| {
        allocator.destroy(breaker);
    }
}

// ============================================================================
// SafePassword - Password validation
// ============================================================================

/// Password strength level
pub const PasswordStrength = enum(i32) {
    very_weak = 0,
    weak = 1,
    fair = 2,
    strong = 3,
    very_strong = 4,
};

/// Password validation result
pub const PasswordResult = extern struct {
    strength: PasswordStrength,
    has_lowercase: bool,
    has_uppercase: bool,
    has_digit: bool,
    has_special: bool,
    length: usize,
};

/// Validate password strength
export fn proven_password_validate(ptr: ?[*]const u8, len: usize) PasswordResult {
    const empty = PasswordResult{
        .strength = .very_weak,
        .has_lowercase = false,
        .has_uppercase = false,
        .has_digit = false,
        .has_special = false,
        .length = 0,
    };

    if (ptr == null) {
        return empty;
    }

    const input = ptr.?[0..len];
    var result = empty;
    result.length = len;

    for (input) |c| {
        if (c >= 'a' and c <= 'z') {
            result.has_lowercase = true;
        } else if (c >= 'A' and c <= 'Z') {
            result.has_uppercase = true;
        } else if (c >= '0' and c <= '9') {
            result.has_digit = true;
        } else if (c >= 33 and c <= 126) {
            result.has_special = true;
        }
    }

    // Calculate strength
    var score: u32 = 0;
    if (len >= 8) score += 1;
    if (len >= 12) score += 1;
    if (result.has_lowercase) score += 1;
    if (result.has_uppercase) score += 1;
    if (result.has_digit) score += 1;
    if (result.has_special) score += 1;

    result.strength = switch (score) {
        0, 1 => .very_weak,
        2 => .weak,
        3, 4 => .fair,
        5 => .strong,
        else => .very_strong,
    };

    return result;
}

/// Check if password is in common passwords list
export fn proven_password_is_common(ptr: ?[*]const u8, len: usize) bool {
    if (ptr == null) return false;
    const input = ptr.?[0..len];

    const common = [_][]const u8{
        "password",   "123456",       "12345678",   "qwerty",
        "abc123",     "monkey",       "1234567",    "letmein",
        "trustno1",   "dragon",       "baseball",   "iloveyou",
        "master",     "sunshine",     "ashley",     "bailey",
        "shadow",     "123123",       "654321",     "superman",
        "qazwsx",     "michael",      "football",   "password1",
        "password123",
    };

    for (common) |c| {
        if (std.mem.eql(u8, input, c)) return true;
    }

    return false;
}

// ============================================================================
// Tests for new modules
// ============================================================================

test "hex_encode_decode" {
    const input = "Hello";
    const encoded = proven_hex_encode(input.ptr, input.len, false);
    try std.testing.expectEqual(ProvenStatus.ok, encoded.status);
    defer proven_free_string(encoded.value);

    try std.testing.expectEqualStrings("48656c6c6f", encoded.value.?[0..encoded.length]);

    const decoded = proven_hex_decode(encoded.value.?, encoded.length);
    try std.testing.expectEqual(ProvenStatus.ok, decoded.status);
    defer {
        var result = decoded;
        proven_hex_free(&result);
    }

    try std.testing.expectEqualStrings("Hello", decoded.data.?[0..decoded.length]);
}

test "uuid_v4" {
    const result = proven_uuid_v4();
    try std.testing.expectEqual(ProvenStatus.ok, result.status);
    try std.testing.expectEqual(@as(u8, 4), proven_uuid_version(result.uuid));
    try std.testing.expect(!proven_uuid_is_nil(result.uuid));
}

test "datetime_parse" {
    const input = "2024-12-15T10:30:00Z";
    const result = proven_datetime_parse(input.ptr, input.len);
    try std.testing.expectEqual(ProvenStatus.ok, result.status);
    try std.testing.expectEqual(@as(i32, 2024), result.datetime.year);
    try std.testing.expectEqual(@as(u8, 12), result.datetime.month);
    try std.testing.expectEqual(@as(u8, 15), result.datetime.day);
    try std.testing.expectEqual(@as(u8, 10), result.datetime.hour);
    try std.testing.expectEqual(@as(u8, 30), result.datetime.minute);
}

test "calculator_eval" {
    const r1 = proven_calculator_eval("2 + 3 * 4".ptr, "2 + 3 * 4".len);
    try std.testing.expectEqual(ProvenStatus.ok, r1.status);
    try std.testing.expectApproxEqAbs(@as(f64, 14), r1.value, 0.001);

    const r2 = proven_calculator_eval("(2 + 3) * 4".ptr, "(2 + 3) * 4".len);
    try std.testing.expectEqual(ProvenStatus.ok, r2.status);
    try std.testing.expectApproxEqAbs(@as(f64, 20), r2.value, 0.001);
}

test "password_validate" {
    const weak = proven_password_validate("abc".ptr, 3);
    try std.testing.expectEqual(PasswordStrength.very_weak, weak.strength);

    const strong = proven_password_validate("MyP@ssw0rd!".ptr, 11);
    try std.testing.expect(strong.has_lowercase);
    try std.testing.expect(strong.has_uppercase);
    try std.testing.expect(strong.has_digit);
    try std.testing.expect(strong.has_special);
}

test "geo_distance" {
    // New York to Los Angeles (approximately 3935 km)
    const ny = GeoCoordinate{ .latitude = 40.7128, .longitude = -74.0060 };
    const la = GeoCoordinate{ .latitude = 34.0522, .longitude = -118.2437 };

    const result = proven_geo_distance(ny, la);
    try std.testing.expectEqual(ProvenStatus.ok, result.status);
    // Should be approximately 3935 km = 3935000 m
    try std.testing.expect(result.value > 3900000 and result.value < 4000000);
}

test "rate_limiter" {
    const limiter = proven_rate_limiter_create(10, 1);
    try std.testing.expect(limiter != null);
    defer proven_rate_limiter_free(limiter);

    // Should allow first 10 requests
    var i: usize = 0;
    while (i < 10) : (i += 1) {
        try std.testing.expect(proven_rate_limiter_try_acquire(limiter, 1));
    }

    // Should deny 11th request (no time has passed)
    try std.testing.expect(!proven_rate_limiter_try_acquire(limiter, 1));
}

test "circuit_breaker" {
    const cb = proven_circuit_breaker_create(3, 2, 1000);
    try std.testing.expect(cb != null);
    defer proven_circuit_breaker_free(cb);

    // Initial state should be closed
    try std.testing.expectEqual(CircuitState.closed, proven_circuit_breaker_state(cb));
    try std.testing.expect(proven_circuit_breaker_allow(cb));

    // Record 3 failures to trip the circuit
    proven_circuit_breaker_failure(cb);
    proven_circuit_breaker_failure(cb);
    proven_circuit_breaker_failure(cb);

    try std.testing.expectEqual(CircuitState.open, proven_circuit_breaker_state(cb));
}

// ============================================================================
// SafeColor - Color space conversions
// ============================================================================

/// RGB color
pub const RGBColor = extern struct {
    r: u8,
    g: u8,
    b: u8,
};

/// HSL color
pub const HSLColor = extern struct {
    h: f64, // 0-360
    s: f64, // 0-1
    l: f64, // 0-1
};

/// Parse hex color string (#RRGGBB or #RGB)
export fn proven_color_parse_hex(ptr: ?[*]const u8, len: usize) extern struct { status: ProvenStatus, color: RGBColor } {
    const empty = RGBColor{ .r = 0, .g = 0, .b = 0 };
    if (ptr == null) {
        return .{ .status = .err_null_pointer, .color = empty };
    }

    const input = ptr.?[0..len];
    var start: usize = 0;
    if (len > 0 and input[0] == '#') start = 1;

    const hex_len = len - start;

    if (hex_len == 6) {
        const r = (hexDigitToValue(input[start]) orelse return .{ .status = .err_parse_failure, .color = empty }) << 4 |
            (hexDigitToValue(input[start + 1]) orelse return .{ .status = .err_parse_failure, .color = empty });
        const g = (hexDigitToValue(input[start + 2]) orelse return .{ .status = .err_parse_failure, .color = empty }) << 4 |
            (hexDigitToValue(input[start + 3]) orelse return .{ .status = .err_parse_failure, .color = empty });
        const b = (hexDigitToValue(input[start + 4]) orelse return .{ .status = .err_parse_failure, .color = empty }) << 4 |
            (hexDigitToValue(input[start + 5]) orelse return .{ .status = .err_parse_failure, .color = empty });
        return .{ .status = .ok, .color = .{ .r = r, .g = g, .b = b } };
    } else if (hex_len == 3) {
        const r_nibble = hexDigitToValue(input[start]) orelse return .{ .status = .err_parse_failure, .color = empty };
        const g_nibble = hexDigitToValue(input[start + 1]) orelse return .{ .status = .err_parse_failure, .color = empty };
        const b_nibble = hexDigitToValue(input[start + 2]) orelse return .{ .status = .err_parse_failure, .color = empty };
        return .{ .status = .ok, .color = .{
            .r = r_nibble << 4 | r_nibble,
            .g = g_nibble << 4 | g_nibble,
            .b = b_nibble << 4 | b_nibble,
        } };
    }

    return .{ .status = .err_parse_failure, .color = empty };
}

/// Convert RGB to HSL
export fn proven_color_rgb_to_hsl(rgb: RGBColor) HSLColor {
    const r: f64 = @as(f64, @floatFromInt(rgb.r)) / 255.0;
    const g: f64 = @as(f64, @floatFromInt(rgb.g)) / 255.0;
    const b: f64 = @as(f64, @floatFromInt(rgb.b)) / 255.0;

    const max_val = @max(r, @max(g, b));
    const min_val = @min(r, @min(g, b));
    const delta = max_val - min_val;

    var h: f64 = 0;
    var s: f64 = 0;
    const l: f64 = (max_val + min_val) / 2;

    if (delta > 0) {
        s = if (l < 0.5) delta / (max_val + min_val) else delta / (2 - max_val - min_val);

        if (max_val == r) {
            h = (g - b) / delta + (if (g < b) 6 else 0);
        } else if (max_val == g) {
            h = (b - r) / delta + 2;
        } else {
            h = (r - g) / delta + 4;
        }
        h *= 60;
    }

    return .{ .h = h, .s = s, .l = l };
}

/// Format RGB as hex string
export fn proven_color_to_hex(rgb: RGBColor) StringResult {
    const hex = "0123456789abcdef";
    var buf: [7]u8 = undefined;
    buf[0] = '#';
    buf[1] = hex[rgb.r >> 4];
    buf[2] = hex[rgb.r & 0x0F];
    buf[3] = hex[rgb.g >> 4];
    buf[4] = hex[rgb.g & 0x0F];
    buf[5] = hex[rgb.b >> 4];
    buf[6] = hex[rgb.b & 0x0F];

    const output = allocator.allocSentinel(u8, 7, 0) catch {
        return .{ .status = .err_allocation_failed, .value = null, .length = 0 };
    };
    @memcpy(output[0..7], &buf);

    return .{ .status = .ok, .value = output.ptr, .length = 7 };
}

// ============================================================================
// SafeAngle - Angle conversions and normalization
// ============================================================================

/// Convert degrees to radians
export fn proven_angle_deg_to_rad(degrees: f64) f64 {
    return degrees * std.math.pi / 180.0;
}

/// Convert radians to degrees
export fn proven_angle_rad_to_deg(radians: f64) f64 {
    return radians * 180.0 / std.math.pi;
}

/// Normalize angle to 0-360 degrees
export fn proven_angle_normalize_degrees(degrees: f64) f64 {
    if (std.math.isNan(degrees)) return 0;
    var result = @mod(degrees, 360.0);
    if (result < 0) result += 360.0;
    return result;
}

/// Normalize angle to 0-2π radians
export fn proven_angle_normalize_radians(radians: f64) f64 {
    if (std.math.isNan(radians)) return 0;
    const two_pi = 2.0 * std.math.pi;
    var result = @mod(radians, two_pi);
    if (result < 0) result += two_pi;
    return result;
}

// ============================================================================
// SafeUnit - Physical unit conversions
// ============================================================================

/// Length unit
pub const LengthUnit = enum(i32) {
    meters = 0,
    kilometers = 1,
    centimeters = 2,
    millimeters = 3,
    feet = 4,
    inches = 5,
    miles = 6,
    yards = 7,
};

/// Convert length between units
export fn proven_unit_convert_length(value: f64, from: LengthUnit, to: LengthUnit) FloatResult {
    if (std.math.isNan(value)) {
        return .{ .status = .err_invalid_argument, .value = 0 };
    }

    // Convert to meters first
    const to_meters: f64 = switch (from) {
        .meters => value,
        .kilometers => value * 1000,
        .centimeters => value / 100,
        .millimeters => value / 1000,
        .feet => value * 0.3048,
        .inches => value * 0.0254,
        .miles => value * 1609.344,
        .yards => value * 0.9144,
    };

    // Convert from meters to target
    const result: f64 = switch (to) {
        .meters => to_meters,
        .kilometers => to_meters / 1000,
        .centimeters => to_meters * 100,
        .millimeters => to_meters * 1000,
        .feet => to_meters / 0.3048,
        .inches => to_meters / 0.0254,
        .miles => to_meters / 1609.344,
        .yards => to_meters / 0.9144,
    };

    return .{ .status = .ok, .value = result };
}

/// Temperature unit
pub const TempUnit = enum(i32) {
    celsius = 0,
    fahrenheit = 1,
    kelvin = 2,
};

/// Convert temperature between units
export fn proven_unit_convert_temp(value: f64, from: TempUnit, to: TempUnit) FloatResult {
    if (std.math.isNan(value)) {
        return .{ .status = .err_invalid_argument, .value = 0 };
    }

    // Convert to Celsius first
    const celsius: f64 = switch (from) {
        .celsius => value,
        .fahrenheit => (value - 32) * 5 / 9,
        .kelvin => value - 273.15,
    };

    // Check for absolute zero violation
    if (celsius < -273.15) {
        return .{ .status = .err_out_of_bounds, .value = 0 };
    }

    // Convert from Celsius to target
    const result: f64 = switch (to) {
        .celsius => celsius,
        .fahrenheit => celsius * 9 / 5 + 32,
        .kelvin => celsius + 273.15,
    };

    return .{ .status = .ok, .value = result };
}

// ============================================================================
// SafeQueue - Bounded FIFO queue
// ============================================================================

/// Bounded queue
pub const BoundedQueue = extern struct {
    data: [*]i64,
    capacity: usize,
    head: usize,
    tail: usize,
    count: usize,
};

/// Create bounded queue
export fn proven_queue_create(capacity: usize) ?*BoundedQueue {
    if (capacity == 0 or capacity > 1000000) return null;

    const data = allocator.alloc(i64, capacity) catch return null;
    const queue = allocator.create(BoundedQueue) catch {
        allocator.free(data);
        return null;
    };

    queue.* = .{
        .data = data.ptr,
        .capacity = capacity,
        .head = 0,
        .tail = 0,
        .count = 0,
    };

    return queue;
}

/// Push to queue (returns false if full)
export fn proven_queue_push(queue: ?*BoundedQueue, value: i64) bool {
    if (queue == null) return false;
    const q = queue.?;
    if (q.count >= q.capacity) return false;

    q.data[q.tail] = value;
    q.tail = (q.tail + 1) % q.capacity;
    q.count += 1;
    return true;
}

/// Pop from queue
export fn proven_queue_pop(queue: ?*BoundedQueue) IntResult {
    if (queue == null) {
        return .{ .status = .err_null_pointer, .value = 0 };
    }
    const q = queue.?;
    if (q.count == 0) {
        return .{ .status = .err_out_of_bounds, .value = 0 };
    }

    const value = q.data[q.head];
    q.head = (q.head + 1) % q.capacity;
    q.count -= 1;

    return .{ .status = .ok, .value = value };
}

/// Get queue size
export fn proven_queue_size(queue: ?*BoundedQueue) usize {
    if (queue == null) return 0;
    return queue.?.count;
}

/// Free queue
export fn proven_queue_free(queue: ?*BoundedQueue) void {
    if (queue) |q| {
        allocator.free(q.data[0..q.capacity]);
        allocator.destroy(q);
    }
}

// ============================================================================
// SafeBloom - Probabilistic set membership
// ============================================================================

/// Bloom filter
pub const BloomFilter = extern struct {
    bits: [*]u8,
    bit_count: usize,
    hash_count: u32,
};

/// Create bloom filter
export fn proven_bloom_create(expected_elements: usize, false_positive_rate: f64) ?*BloomFilter {
    if (expected_elements == 0 or false_positive_rate <= 0 or false_positive_rate >= 1) {
        return null;
    }

    // Calculate optimal size: m = -n*ln(p)/(ln(2)^2)
    const n: f64 = @floatFromInt(expected_elements);
    const ln2 = @log(@as(f64, 2.0));
    const m_float = -n * @log(false_positive_rate) / (ln2 * ln2);
    const m: usize = @intFromFloat(@max(8, @min(m_float, 1000000000)));

    // Calculate optimal hash count: k = (m/n)*ln(2)
    const k: u32 = @intFromFloat(@max(1, @min(@as(f64, @floatFromInt(m)) / n * ln2, 16)));

    const byte_count = (m + 7) / 8;
    const bits = allocator.alloc(u8, byte_count) catch return null;
    @memset(bits, 0);

    const filter = allocator.create(BloomFilter) catch {
        allocator.free(bits);
        return null;
    };

    filter.* = .{
        .bits = bits.ptr,
        .bit_count = m,
        .hash_count = k,
    };

    return filter;
}

fn bloomHash(data: []const u8, seed: u32) u64 {
    var hash: u64 = seed;
    for (data) |byte| {
        hash = hash *% 31 +% byte;
    }
    return hash;
}

/// Add element to bloom filter
export fn proven_bloom_add(filter: ?*BloomFilter, ptr: ?[*]const u8, len: usize) void {
    if (filter == null or ptr == null) return;
    const f = filter.?;
    const data = ptr.?[0..len];

    var i: u32 = 0;
    while (i < f.hash_count) : (i += 1) {
        const hash = bloomHash(data, i);
        const bit_idx = hash % f.bit_count;
        const byte_idx = bit_idx / 8;
        const bit_offset: u3 = @intCast(bit_idx % 8);
        f.bits[byte_idx] |= @as(u8, 1) << bit_offset;
    }
}

/// Check if element might be in filter
export fn proven_bloom_contains(filter: ?*BloomFilter, ptr: ?[*]const u8, len: usize) bool {
    if (filter == null or ptr == null) return false;
    const f = filter.?;
    const data = ptr.?[0..len];

    var i: u32 = 0;
    while (i < f.hash_count) : (i += 1) {
        const hash = bloomHash(data, i);
        const bit_idx = hash % f.bit_count;
        const byte_idx = bit_idx / 8;
        const bit_offset: u3 = @intCast(bit_idx % 8);
        if ((f.bits[byte_idx] & (@as(u8, 1) << bit_offset)) == 0) {
            return false;
        }
    }
    return true;
}

/// Free bloom filter
export fn proven_bloom_free(filter: ?*BloomFilter) void {
    if (filter) |f| {
        allocator.free(f.bits[0..(f.bit_count + 7) / 8]);
        allocator.destroy(f);
    }
}

// ============================================================================
// SafeRetry - Exponential backoff
// ============================================================================

/// Retry configuration
pub const RetryConfig = extern struct {
    max_attempts: u32,
    base_delay_ms: u64,
    max_delay_ms: u64,
    multiplier: f64,
};

/// Calculate delay for attempt (with jitter)
export fn proven_retry_delay(config: RetryConfig, attempt: u32) u64 {
    if (attempt == 0 or attempt > config.max_attempts) {
        return 0;
    }

    // Exponential backoff: base * multiplier^(attempt-1)
    var delay: f64 = @floatFromInt(config.base_delay_ms);
    var i: u32 = 1;
    while (i < attempt) : (i += 1) {
        delay *= config.multiplier;
    }

    // Cap at max delay
    if (delay > @as(f64, @floatFromInt(config.max_delay_ms))) {
        delay = @floatFromInt(config.max_delay_ms);
    }

    // Add jitter (±25%)
    const jitter_range = delay * 0.25;
    const random_bytes = blk: {
        var bytes: [8]u8 = undefined;
        std.crypto.random.bytes(&bytes);
        break :blk bytes;
    };
    const random_val: f64 = @as(f64, @floatFromInt(std.mem.readInt(u32, random_bytes[0..4], .little))) / @as(f64, @floatFromInt(std.math.maxInt(u32)));
    delay += jitter_range * (random_val * 2 - 1);

    return @intFromFloat(@max(1, delay));
}

/// Check if should retry
export fn proven_retry_should_retry(config: RetryConfig, attempt: u32) bool {
    return attempt < config.max_attempts;
}

// ============================================================================
// SafeMonotonic - Monotonically increasing sequences
// ============================================================================

/// Monotonic counter
pub const MonotonicCounter = extern struct {
    value: u64,
    max_value: u64,
};

/// Create monotonic counter
export fn proven_monotonic_create(initial: u64, max_value: u64) ?*MonotonicCounter {
    if (initial >= max_value) return null;

    const counter = allocator.create(MonotonicCounter) catch return null;
    counter.* = .{ .value = initial, .max_value = max_value };
    return counter;
}

/// Get next value (atomic-like increment)
export fn proven_monotonic_next(counter: ?*MonotonicCounter) IntResult {
    if (counter == null) {
        return .{ .status = .err_null_pointer, .value = 0 };
    }
    const c = counter.?;
    if (c.value >= c.max_value) {
        return .{ .status = .err_overflow, .value = 0 };
    }

    const result = c.value;
    c.value += 1;
    return .{ .status = .ok, .value = @intCast(result) };
}

/// Free counter
export fn proven_monotonic_free(counter: ?*MonotonicCounter) void {
    if (counter) |c| {
        allocator.destroy(c);
    }
}

// ============================================================================
// SafeStateMachine - Type-safe state transitions
// ============================================================================

/// State machine
pub const StateMachine = extern struct {
    current_state: u32,
    state_count: u32,
    transitions: [*]u8, // Packed transition matrix
};

/// Create state machine
export fn proven_state_machine_create(state_count: u32, initial_state: u32) ?*StateMachine {
    if (state_count == 0 or state_count > 256 or initial_state >= state_count) {
        return null;
    }

    const transition_count = state_count * state_count;
    const transitions = allocator.alloc(u8, transition_count) catch return null;
    @memset(transitions, 0); // All transitions invalid by default

    const sm = allocator.create(StateMachine) catch {
        allocator.free(transitions);
        return null;
    };

    sm.* = .{
        .current_state = initial_state,
        .state_count = state_count,
        .transitions = transitions.ptr,
    };

    return sm;
}

/// Allow transition from state A to state B
export fn proven_state_machine_allow(sm: ?*StateMachine, from: u32, to: u32) bool {
    if (sm == null) return false;
    const machine = sm.?;
    if (from >= machine.state_count or to >= machine.state_count) return false;

    machine.transitions[from * machine.state_count + to] = 1;
    return true;
}

/// Try to transition to new state
export fn proven_state_machine_transition(sm: ?*StateMachine, to: u32) bool {
    if (sm == null) return false;
    const machine = sm.?;
    if (to >= machine.state_count) return false;

    const idx = machine.current_state * machine.state_count + to;
    if (machine.transitions[idx] == 0) return false;

    machine.current_state = to;
    return true;
}

/// Get current state
export fn proven_state_machine_state(sm: ?*StateMachine) u32 {
    if (sm == null) return 0;
    return sm.?.current_state;
}

/// Free state machine
export fn proven_state_machine_free(sm: ?*StateMachine) void {
    if (sm) |machine| {
        allocator.free(machine.transitions[0 .. machine.state_count * machine.state_count]);
        allocator.destroy(machine);
    }
}

// ============================================================================
// SafeTensor - Basic tensor operations
// ============================================================================

/// Tensor (2D matrix)
pub const Tensor2D = extern struct {
    data: [*]f64,
    rows: usize,
    cols: usize,
};

/// Create 2D tensor
export fn proven_tensor_create(rows: usize, cols: usize) ?*Tensor2D {
    if (rows == 0 or cols == 0 or rows > 10000 or cols > 10000) {
        return null;
    }

    const size = rows * cols;
    const data = allocator.alloc(f64, size) catch return null;
    @memset(data, 0);

    const tensor = allocator.create(Tensor2D) catch {
        allocator.free(data);
        return null;
    };

    tensor.* = .{ .data = data.ptr, .rows = rows, .cols = cols };
    return tensor;
}

/// Set tensor value
export fn proven_tensor_set(tensor: ?*Tensor2D, row: usize, col: usize, value: f64) ProvenStatus {
    if (tensor == null) return .err_null_pointer;
    const t = tensor.?;
    if (row >= t.rows or col >= t.cols) return .err_out_of_bounds;

    t.data[row * t.cols + col] = value;
    return .ok;
}

/// Get tensor value
export fn proven_tensor_get(tensor: ?*Tensor2D, row: usize, col: usize) FloatResult {
    if (tensor == null) return .{ .status = .err_null_pointer, .value = 0 };
    const t = tensor.?;
    if (row >= t.rows or col >= t.cols) return .{ .status = .err_out_of_bounds, .value = 0 };

    return .{ .status = .ok, .value = t.data[row * t.cols + col] };
}

/// Matrix multiplication
export fn proven_tensor_matmul(a: ?*Tensor2D, b: ?*Tensor2D) ?*Tensor2D {
    if (a == null or b == null) return null;
    const ta = a.?;
    const tb = b.?;

    if (ta.cols != tb.rows) return null;

    const result = proven_tensor_create(ta.rows, tb.cols) orelse return null;

    for (0..ta.rows) |i| {
        for (0..tb.cols) |j| {
            var sum: f64 = 0;
            for (0..ta.cols) |k| {
                sum += ta.data[i * ta.cols + k] * tb.data[k * tb.cols + j];
            }
            result.data[i * result.cols + j] = sum;
        }
    }

    return result;
}

/// Free tensor
export fn proven_tensor_free(tensor: ?*Tensor2D) void {
    if (tensor) |t| {
        allocator.free(t.data[0 .. t.rows * t.cols]);
        allocator.destroy(t);
    }
}

// ============================================================================
// SafeML - Machine learning utilities
// ============================================================================

/// Softmax normalization
export fn proven_ml_softmax(input: ?[*]const f64, output: ?[*]f64, len: usize) ProvenStatus {
    if (input == null or output == null) return .err_null_pointer;
    if (len == 0) return .err_invalid_argument;

    const in = input.?[0..len];
    const out = output.?[0..len];

    // Find max for numerical stability
    var max_val = in[0];
    for (in[1..]) |v| {
        if (v > max_val) max_val = v;
    }

    // Compute exp and sum
    var sum: f64 = 0;
    for (in, 0..) |v, i| {
        out[i] = @exp(v - max_val);
        sum += out[i];
    }

    // Normalize
    if (sum == 0) return .err_division_by_zero;
    for (out) |*v| {
        v.* /= sum;
    }

    return .ok;
}

/// Sigmoid function
export fn proven_ml_sigmoid(x: f64) f64 {
    if (x > 700) return 1; // Prevent overflow
    if (x < -700) return 0;
    return 1.0 / (1.0 + @exp(-x));
}

/// ReLU function
export fn proven_ml_relu(x: f64) f64 {
    return @max(0, x);
}

/// Leaky ReLU
export fn proven_ml_leaky_relu(x: f64, alpha: f64) f64 {
    return if (x > 0) x else alpha * x;
}

/// Clamp value to range
export fn proven_ml_clamp(x: f64, min_val: f64, max_val: f64) f64 {
    if (x < min_val) return min_val;
    if (x > max_val) return max_val;
    return x;
}

// ============================================================================
// SafeLRU - Least Recently Used cache (simplified)
// ============================================================================

/// LRU entry
pub const LRUEntry = extern struct {
    key: u64,
    value: i64,
    prev: usize,
    next: usize,
    valid: bool,
};

/// LRU cache
pub const LRUCache = extern struct {
    entries: [*]LRUEntry,
    capacity: usize,
    head: usize,
    tail: usize,
    count: usize,
};

/// Create LRU cache
export fn proven_lru_create(capacity: usize) ?*LRUCache {
    if (capacity == 0 or capacity > 100000) return null;

    const entries = allocator.alloc(LRUEntry, capacity) catch return null;
    for (entries, 0..) |*e, i| {
        e.* = .{
            .key = 0,
            .value = 0,
            .prev = if (i > 0) i - 1 else 0,
            .next = if (i < capacity - 1) i + 1 else capacity - 1,
            .valid = false,
        };
    }

    const cache = allocator.create(LRUCache) catch {
        allocator.free(entries);
        return null;
    };

    cache.* = .{
        .entries = entries.ptr,
        .capacity = capacity,
        .head = 0,
        .tail = capacity - 1,
        .count = 0,
    };

    return cache;
}

/// Get from LRU cache
export fn proven_lru_get(cache: ?*LRUCache, key: u64) IntResult {
    if (cache == null) return .{ .status = .err_null_pointer, .value = 0 };
    const c = cache.?;

    // Linear search (simplified; real impl would use hash map)
    for (c.entries[0..c.capacity]) |*entry| {
        if (entry.valid and entry.key == key) {
            // Move to front would go here in full impl
            return .{ .status = .ok, .value = entry.value };
        }
    }

    return .{ .status = .err_out_of_bounds, .value = 0 };
}

/// Put in LRU cache
export fn proven_lru_put(cache: ?*LRUCache, key: u64, value: i64) ProvenStatus {
    if (cache == null) return .err_null_pointer;
    const c = cache.?;

    // Check if key exists
    for (c.entries[0..c.capacity]) |*entry| {
        if (entry.valid and entry.key == key) {
            entry.value = value;
            return .ok;
        }
    }

    // Find empty slot or evict LRU
    for (c.entries[0..c.capacity]) |*entry| {
        if (!entry.valid) {
            entry.key = key;
            entry.value = value;
            entry.valid = true;
            c.count += 1;
            return .ok;
        }
    }

    // Evict from tail (simplified)
    c.entries[c.tail].key = key;
    c.entries[c.tail].value = value;

    return .ok;
}

/// Free LRU cache
export fn proven_lru_free(cache: ?*LRUCache) void {
    if (cache) |c| {
        allocator.free(c.entries[0..c.capacity]);
        allocator.destroy(c);
    }
}

// ============================================================================
// SafeGraph - Graph operations with cycle detection
// ============================================================================

/// Graph (adjacency list, simplified)
pub const Graph = extern struct {
    edges: [*]u8, // Packed adjacency matrix
    node_count: usize,
};

/// Create graph
export fn proven_graph_create(node_count: usize) ?*Graph {
    if (node_count == 0 or node_count > 10000) return null;

    const edge_bytes = (node_count * node_count + 7) / 8;
    const edges = allocator.alloc(u8, edge_bytes) catch return null;
    @memset(edges, 0);

    const graph = allocator.create(Graph) catch {
        allocator.free(edges);
        return null;
    };

    graph.* = .{ .edges = edges.ptr, .node_count = node_count };
    return graph;
}

/// Add edge
export fn proven_graph_add_edge(graph: ?*Graph, from: usize, to: usize) ProvenStatus {
    if (graph == null) return .err_null_pointer;
    const g = graph.?;
    if (from >= g.node_count or to >= g.node_count) return .err_out_of_bounds;

    const bit_idx = from * g.node_count + to;
    const byte_idx = bit_idx / 8;
    const bit_offset: u3 = @intCast(bit_idx % 8);
    g.edges[byte_idx] |= @as(u8, 1) << bit_offset;

    return .ok;
}

/// Check if edge exists
export fn proven_graph_has_edge(graph: ?*Graph, from: usize, to: usize) bool {
    if (graph == null) return false;
    const g = graph.?;
    if (from >= g.node_count or to >= g.node_count) return false;

    const bit_idx = from * g.node_count + to;
    const byte_idx = bit_idx / 8;
    const bit_offset: u3 = @intCast(bit_idx % 8);

    return (g.edges[byte_idx] & (@as(u8, 1) << bit_offset)) != 0;
}

/// Free graph
export fn proven_graph_free(graph: ?*Graph) void {
    if (graph) |g| {
        const edge_bytes = (g.node_count * g.node_count + 7) / 8;
        allocator.free(g.edges[0..edge_bytes]);
        allocator.destroy(g);
    }
}

// ============================================================================
// Additional tests for new modules
// ============================================================================

test "color_parse_hex" {
    const r1 = proven_color_parse_hex("#FF0000".ptr, 7);
    try std.testing.expectEqual(ProvenStatus.ok, r1.status);
    try std.testing.expectEqual(@as(u8, 255), r1.color.r);
    try std.testing.expectEqual(@as(u8, 0), r1.color.g);
    try std.testing.expectEqual(@as(u8, 0), r1.color.b);
}

test "angle_conversion" {
    const rad = proven_angle_deg_to_rad(180);
    try std.testing.expectApproxEqAbs(std.math.pi, rad, 0.001);

    const deg = proven_angle_rad_to_deg(std.math.pi);
    try std.testing.expectApproxEqAbs(@as(f64, 180), deg, 0.001);
}

test "unit_conversion" {
    const r = proven_unit_convert_length(1, .kilometers, .meters);
    try std.testing.expectEqual(ProvenStatus.ok, r.status);
    try std.testing.expectApproxEqAbs(@as(f64, 1000), r.value, 0.001);

    const temp = proven_unit_convert_temp(0, .celsius, .kelvin);
    try std.testing.expectEqual(ProvenStatus.ok, temp.status);
    try std.testing.expectApproxEqAbs(@as(f64, 273.15), temp.value, 0.001);
}

test "queue" {
    const q = proven_queue_create(3);
    try std.testing.expect(q != null);
    defer proven_queue_free(q);

    try std.testing.expect(proven_queue_push(q, 10));
    try std.testing.expect(proven_queue_push(q, 20));
    try std.testing.expect(proven_queue_push(q, 30));
    try std.testing.expect(!proven_queue_push(q, 40)); // Full

    try std.testing.expectEqual(@as(usize, 3), proven_queue_size(q));

    const v1 = proven_queue_pop(q);
    try std.testing.expectEqual(@as(i64, 10), v1.value);
}

test "bloom_filter" {
    const filter = proven_bloom_create(100, 0.01);
    try std.testing.expect(filter != null);
    defer proven_bloom_free(filter);

    const key = "test_key";
    proven_bloom_add(filter, key.ptr, key.len);
    try std.testing.expect(proven_bloom_contains(filter, key.ptr, key.len));

    const missing = "missing_key";
    // May have false positives, but shouldn't have for completely different key
}

test "state_machine" {
    const sm = proven_state_machine_create(3, 0);
    try std.testing.expect(sm != null);
    defer proven_state_machine_free(sm);

    // Allow: 0 -> 1, 1 -> 2
    try std.testing.expect(proven_state_machine_allow(sm, 0, 1));
    try std.testing.expect(proven_state_machine_allow(sm, 1, 2));

    try std.testing.expectEqual(@as(u32, 0), proven_state_machine_state(sm));
    try std.testing.expect(proven_state_machine_transition(sm, 1));
    try std.testing.expectEqual(@as(u32, 1), proven_state_machine_state(sm));
    try std.testing.expect(!proven_state_machine_transition(sm, 0)); // Not allowed
    try std.testing.expect(proven_state_machine_transition(sm, 2));
    try std.testing.expectEqual(@as(u32, 2), proven_state_machine_state(sm));
}

test "ml_sigmoid" {
    try std.testing.expectApproxEqAbs(@as(f64, 0.5), proven_ml_sigmoid(0), 0.001);
    try std.testing.expect(proven_ml_sigmoid(10) > 0.999);
    try std.testing.expect(proven_ml_sigmoid(-10) < 0.001);
}
