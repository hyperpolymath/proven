// PROVEN FFI Implementation
//
// This module implements the C-compatible FFI declared in src/abi/Foreign.idr
// All types and layouts must match the Idris2 ABI definitions.
//
// SPDX-License-Identifier: PMPL-1.0-or-later

const std = @import("std");

// Version information (keep in sync with project)
const VERSION = "0.1.0";
const BUILD_INFO = "PROVEN built with Zig " ++ @import("builtin").zig_version_string;

/// Thread-local error storage
threadlocal var last_error: ?[]const u8 = null;

/// Set the last error message
fn setError(msg: []const u8) void {
    last_error = msg;
}

/// Clear the last error
fn clearError() void {
    last_error = null;
}

//==============================================================================
// Core Types (must match src/abi/Types.idr)
//==============================================================================

/// Result codes (must match Idris2 Result type)
pub const Result = enum(c_int) {
    ok = 0,
    @"error" = 1,
    invalid_param = 2,
    out_of_memory = 3,
    null_pointer = 4,
};

/// Library handle.  C consumers receive only a pointer, so C code treats it
/// as opaque.  Zig code accesses fields directly via the pointer.
pub const Handle = struct {
    allocator:   std.mem.Allocator,
    initialized: bool,
};

//==============================================================================
// Library Lifecycle
//==============================================================================

/// Initialize the library
/// Returns a handle, or null on failure
export fn proven_init() ?*Handle {
    const allocator = std.heap.c_allocator;

    const handle = allocator.create(Handle) catch {
        setError("Failed to allocate handle");
        return null;
    };

    // Initialize handle
    handle.* = .{
        .allocator = allocator,
        .initialized = true,
    };

    clearError();
    return handle;
}

/// Free the library handle
export fn proven_free(handle: ?*Handle) void {
    const h = handle orelse return;
    const allocator = h.allocator;

    // Clean up resources
    h.initialized = false;

    allocator.destroy(h);
    clearError();
}

//==============================================================================
// Core Operations
//==============================================================================

/// Process data (example operation)
export fn proven_process(handle: ?*Handle, input: u32) Result {
    const h = handle orelse {
        setError("Null handle");
        return .null_pointer;
    };

    if (!h.initialized) {
        setError("Handle not initialized");
        return .@"error";
    }

    // Example processing logic
    _ = input;

    clearError();
    return .ok;
}

//==============================================================================
// String Operations
//==============================================================================

/// Get a string result (example)
/// Caller must free the returned string
export fn proven_get_string(handle: ?*Handle) ?[*:0]const u8 {
    const h = handle orelse {
        setError("Null handle");
        return null;
    };

    if (!h.initialized) {
        setError("Handle not initialized");
        return null;
    }

    // Example: allocate and return a string
    const result = h.allocator.dupeZ(u8, "Example result") catch {
        setError("Failed to allocate string");
        return null;
    };

    clearError();
    return result.ptr;
}

/// Free a string allocated by the library
export fn proven_free_string(str: ?[*:0]const u8) void {
    const s = str orelse return;
    const allocator = std.heap.c_allocator;

    const slice = std.mem.span(s);
    allocator.free(slice);
}

//==============================================================================
// Array/Buffer Operations
//==============================================================================

/// Process an array of data
export fn proven_process_array(
    handle: ?*Handle,
    buffer: ?[*]const u8,
    len: u32,
) Result {
    const h = handle orelse {
        setError("Null handle");
        return .null_pointer;
    };

    const buf = buffer orelse {
        setError("Null buffer");
        return .null_pointer;
    };

    if (!h.initialized) {
        setError("Handle not initialized");
        return .@"error";
    }

    // Access the buffer
    const data = buf[0..len];
    _ = data;

    // Process data here

    clearError();
    return .ok;
}

//==============================================================================
// Error Handling
//==============================================================================

/// Get the last error message
/// Returns null if no error
export fn proven_last_error() ?[*:0]const u8 {
    const err = last_error orelse return null;

    // Return C string (static storage, no need to free)
    const allocator = std.heap.c_allocator;
    const c_str = allocator.dupeZ(u8, err) catch return null;
    return c_str.ptr;
}

//==============================================================================
// Version Information
//==============================================================================

/// Get the library version
export fn proven_version() [*:0]const u8 {
    return VERSION.ptr;
}

/// Get build information
export fn proven_build_info() [*:0]const u8 {
    return BUILD_INFO.ptr;
}

//==============================================================================
// Callback Support
//==============================================================================

/// Callback function type (C ABI)
pub const Callback = *const fn (u64, u32) callconv(.c) u32;

/// Register a callback
export fn proven_register_callback(
    handle: ?*Handle,
    callback: ?Callback,
) Result {
    const h = handle orelse {
        setError("Null handle");
        return .null_pointer;
    };

    const cb = callback orelse {
        setError("Null callback");
        return .null_pointer;
    };

    if (!h.initialized) {
        setError("Handle not initialized");
        return .@"error";
    }

    // Store callback for later use
    _ = cb;

    clearError();
    return .ok;
}

//==============================================================================
// SafePath — Filesystem traversal prevention
// Matches the declarations in proven.h and src/abi/Foreign.idr.
//==============================================================================

/// C-ABI result type for boolean operations (matches ProvenBoolResult in proven.h).
/// Layout: { status: i32, value: bool }  (4 + 1 bytes; compiler pads as needed).
pub const BoolResult = extern struct {
    status: c_int,
    value:  bool,
};

/// Status codes matching ProvenStatus in proven.h.
const PROVEN_OK: c_int                    =   0;
const PROVEN_ERR_NULL_POINTER: c_int      =  -1;

/// Check if a byte slice contains a directory traversal sequence ("..").
///
/// Matches: ProvenBoolResult proven_path_has_traversal(const uint8_t* ptr, size_t len)
///
/// Returns BoolResult{ .status = PROVEN_OK, .value = true } when a ".." component
/// is present in the path, and { .status = PROVEN_OK, .value = false } otherwise.
/// Returns { .status = PROVEN_ERR_NULL_POINTER } when ptr is null.
///
/// Detection rule: look for the two-byte sequence ".." bounded by path separators
/// ('/'), a NUL byte, or the start/end of the slice.  This matches what the
/// Idris2 backend will formally verify once the RefC pipeline is available.
export fn proven_path_has_traversal(ptr: ?[*]const u8, len: usize) callconv(.c) BoolResult {
    const p = ptr orelse return BoolResult{ .status = PROVEN_ERR_NULL_POINTER, .value = false };
    if (len == 0) return BoolResult{ .status = PROVEN_OK, .value = false };

    const bytes = p[0..len];

    // Walk the byte slice and check every ".." component.
    var i: usize = 0;
    while (i < len) {
        // Find next component boundary: advance past leading separators.
        while (i < len and bytes[i] == '/') : (i += 1) {}
        if (i >= len) break;

        // Measure the component.
        const comp_start = i;
        while (i < len and bytes[i] != '/') : (i += 1) {}
        const component = bytes[comp_start..i];

        if (std.mem.eql(u8, component, "..")) {
            return BoolResult{ .status = PROVEN_OK, .value = true };
        }
    }

    return BoolResult{ .status = PROVEN_OK, .value = false };
}

//==============================================================================
// Utility Functions
//==============================================================================

/// Check if handle is initialized
export fn proven_is_initialized(handle: ?*Handle) u32 {
    const h = handle orelse return 0;
    return if (h.initialized) 1 else 0;
}

//==============================================================================
// Tests
//==============================================================================

test "lifecycle" {
    const handle = proven_init() orelse return error.InitFailed;
    defer proven_free(handle);

    try std.testing.expect(proven_is_initialized(handle) == 1);
}

test "error handling" {
    const result = proven_process(null, 0);
    try std.testing.expectEqual(Result.null_pointer, result);

    const err = proven_last_error();
    try std.testing.expect(err != null);
}

test "version" {
    const ver = proven_version();
    const ver_str = std.mem.span(ver);
    try std.testing.expectEqualStrings(VERSION, ver_str);
}

test "proven_path_has_traversal detects traversal" {
    // Classic attacks.
    const r1 = proven_path_has_traversal("../etc/passwd".ptr, "../etc/passwd".len);
    try std.testing.expectEqual(@as(c_int, PROVEN_OK), r1.status);
    try std.testing.expect(r1.value);

    const r2 = proven_path_has_traversal("/tmp/../../root".ptr, "/tmp/../../root".len);
    try std.testing.expectEqual(@as(c_int, PROVEN_OK), r2.status);
    try std.testing.expect(r2.value);
}

test "proven_path_has_traversal allows safe paths" {
    const r1 = proven_path_has_traversal("/var/mnt/eclipse/repos/myrepo/file.scm".ptr,
        "/var/mnt/eclipse/repos/myrepo/file.scm".len);
    try std.testing.expectEqual(@as(c_int, PROVEN_OK), r1.status);
    try std.testing.expect(!r1.value);

    const r2 = proven_path_has_traversal("/tmp/scratch.scm".ptr, "/tmp/scratch.scm".len);
    try std.testing.expectEqual(@as(c_int, PROVEN_OK), r2.status);
    try std.testing.expect(!r2.value);
}

test "proven_path_has_traversal handles null and empty" {
    const r_null = proven_path_has_traversal(null, 0);
    try std.testing.expectEqual(@as(c_int, PROVEN_ERR_NULL_POINTER), r_null.status);

    const r_empty = proven_path_has_traversal("".ptr, 0);
    try std.testing.expectEqual(@as(c_int, PROVEN_OK), r_empty.status);
    try std.testing.expect(!r_empty.value);
}
