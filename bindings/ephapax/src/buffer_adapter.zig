// SPDX-License-Identifier: Apache-2.0
//! SafeBuffer adapter for Ephapax

const std = @import("std");
const types = @import("types.zig");

// External C functions from proven library
extern fn idris_proven_buffer_new(capacity: u64) ?*types.BufferHandle;
extern fn idris_proven_buffer_write(buffer: *types.BufferHandle, data: [*]const u8, data_len: u64) i32;
extern fn idris_proven_buffer_read(buffer: *types.BufferHandle, offset: u64, len: u64, out_data: [*]u8) i32;
extern fn idris_proven_buffer_capacity(buffer: *types.BufferHandle) u64;
extern fn idris_proven_buffer_size(buffer: *types.BufferHandle) u64;
extern fn idris_proven_buffer_is_full(buffer: *types.BufferHandle) bool;
extern fn idris_proven_buffer_remaining(buffer: *types.BufferHandle) u64;
extern fn idris_proven_buffer_free(buffer: *types.BufferHandle) void;

//-----------------------------------------------------------------------------
// FFI Exports for Ephapax
//-----------------------------------------------------------------------------

export fn ephapax_proven_buffer_new(capacity: u64) callconv(.C) ?*types.BufferHandle {
    return idris_proven_buffer_new(capacity);
}

export fn ephapax_proven_buffer_write(
    buffer: *types.BufferHandle,
    data: [*]const u8,
    data_len: u64,
) callconv(.C) types.Result {
    const result = idris_proven_buffer_write(buffer, data, data_len);
    return if (result == 0) types.Result.Ok else types.Result.Error;
}

export fn ephapax_proven_buffer_read(
    buffer: *types.BufferHandle,
    offset: u64,
    len: u64,
    out_data: [*]u8,
) callconv(.C) types.Result {
    const result = idris_proven_buffer_read(buffer, offset, len, out_data);
    return if (result == 0) types.Result.Ok else types.Result.Error;
}

export fn ephapax_proven_buffer_capacity(buffer: *types.BufferHandle) callconv(.C) u64 {
    return idris_proven_buffer_capacity(buffer);
}

export fn ephapax_proven_buffer_size(buffer: *types.BufferHandle) callconv(.C) u64 {
    return idris_proven_buffer_size(buffer);
}

export fn ephapax_proven_buffer_is_full(buffer: *types.BufferHandle) callconv(.C) bool {
    return idris_proven_buffer_is_full(buffer);
}

export fn ephapax_proven_buffer_remaining(buffer: *types.BufferHandle) callconv(.C) u64 {
    return idris_proven_buffer_remaining(buffer);
}

export fn ephapax_proven_buffer_free(buffer: *types.BufferHandle) callconv(.C) void {
    idris_proven_buffer_free(buffer);
}
