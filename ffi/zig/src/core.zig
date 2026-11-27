// SPDX-License-Identifier: MPL-2.0
// PROVEN FFI Implementation
//
// This module implements the C-compatible FFI declared in src/abi/Foreign.idr
// All types and layouts must match the Idris2 ABI definitions.
//

const std = @import("std");
const lifecycle = @import("lifecycle");

// Version information (keep in sync with project)
const VERSION = "0.1.0";
const BUILD_INFO = "PROVEN built with Zig " ++ @import("builtin").zig_version_string;

/// Thread-local error storage
threadlocal var last_error: ?[:0]const u8 = null;

/// Set the last error message
fn setError(comptime msg: [:0]const u8) void {
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
    allocator: std.mem.Allocator,
    initialized: bool,
};

//==============================================================================
// Library Lifecycle
//==============================================================================

/// Initialize the library
/// Returns a handle, or null on failure
export fn proven_handle_create() ?*Handle {
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

/// Canonical no-argument lifecycle ABI. Policy is implemented in Idris.
export fn proven_init() callconv(.c) i32 {
    return lifecycle.init();
}
export fn proven_deinit() callconv(.c) void {
    lifecycle.deinit();
}
export fn proven_is_initialized() callconv(.c) bool {
    return lifecycle.isInitialized();
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

/// Return the library version triple
export fn proven_version_major() u32 {
    return 1;
}
export fn proven_version_minor() u32 {
    return 1;
}
export fn proven_version_patch() u32 {
    return 0;
}

/// Return the FFI ABI version
export fn proven_ffi_abi_version() u32 {
    return 2;
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

    // Borrowed static storage; callers must not free this pointer.
    return err.ptr;
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
    value: bool,
};

/// Status codes matching ProvenStatus in proven.h.
const PROVEN_OK: c_int = 0;
const PROVEN_ERR_NULL_POINTER: c_int = -1;

/// Detect exact parent components in the borrowed byte range via Idris.
/// Slash, backslash and NUL are boundaries; NUL does not truncate the range.
/// Caller supplies readable storage for len bytes for the duration of this call.
pub fn pathHasTraversal(comptime backend: type, ptr: ?[*]const u8, len: usize) BoolResult {
    const p = ptr orelse return .{ .status = PROVEN_ERR_NULL_POINTER, .value = false };
    const detected = backend.detect(p[0..len]) catch |err| {
        setError("Path backend failure");
        return .{ .status = if (@as(anyerror, err) == error.BackendUnavailable) -99 else -7, .value = false };
    };
    return .{ .status = PROVEN_OK, .value = detected };
}
//==============================================================================
// SafeHeader — HTTP header safety
// Matches the declarations in proven.h and src/abi/Foreign.idr.
//==============================================================================

/// Check if a byte slice contains CRLF injection characters ("\r\n").
///
/// Matches: ProvenBoolResult proven_header_has_crlf(const uint8_t* ptr, size_t len)
///
/// Returns success/true when either CR or LF is found by the Idris byte kernel,
/// and { .status = PROVEN_OK, .value = false } otherwise.
/// Returns { .status = PROVEN_ERR_NULL_POINTER } when ptr is null.
///
/// The exact byte range is passed to Idris; embedded NUL is data.
/// Without a linked Idris backend, returns PROVEN_ERR_NOT_IMPLEMENTED (-99).
pub fn headerHasCrlf(comptime header_backend: type, ptr: ?[*]const u8, len: usize) BoolResult {
    const p = ptr orelse return BoolResult{ .status = PROVEN_ERR_NULL_POINTER, .value = false };
    const detected = header_backend.detect(p[0..len]) catch |err| {
        setError("Header backend failure");
        return .{ .status = if (@as(anyerror, err) == error.BackendUnavailable) -99 else -7, .value = false };
    };
    return .{ .status = PROVEN_OK, .value = detected };
}

//==============================================================================
// Utility Functions
//==============================================================================

/// Check if handle is initialized
export fn proven_handle_is_initialized(handle: ?*Handle) u32 {
    const h = handle orelse return 0;
    return if (h.initialized) 1 else 0;
}

//==============================================================================
// Tests
//==============================================================================

test "lifecycle" {
    const handle = proven_handle_create() orelse return error.InitFailed;
    defer proven_free(handle);

    try std.testing.expect(proven_handle_is_initialized(handle) == 1);
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

test "canonical lifecycle uses the declared C types and fails closed" {
    const initialize: *const fn () callconv(.c) i32 = &proven_init;
    const shutdown: *const fn () callconv(.c) void = &proven_deinit;
    const initialized: *const fn () callconv(.c) bool = &proven_is_initialized;
    shutdown();
    try std.testing.expect(!initialized());
    const expected: i32 = if (lifecycle.available) 0 else -99;
    try std.testing.expectEqual(expected, initialize());
    try std.testing.expectEqual(lifecycle.available, initialized());
    try std.testing.expectEqual(expected, initialize());
    shutdown();
    shutdown();
    try std.testing.expect(!initialized());
}

test "error messages are borrowed static storage" {
    _ = proven_process(null, 0);
    const first = proven_last_error() orelse return error.MissingError;
    for (0..1024) |_| {
        const next = proven_last_error() orelse return error.MissingError;
        try std.testing.expect(first == next);
        try std.testing.expectEqualStrings("Null handle", std.mem.span(next));
    }
    clearError();
    try std.testing.expect(proven_last_error() == null);
}
