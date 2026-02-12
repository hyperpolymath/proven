// SPDX-License-Identifier: Apache-2.0
//! SafeResource adapter for Ephapax

const std = @import("std");
const types = @import("types.zig");

// Resource state enum (matches proven library)
pub const ResourceState = enum(c_int) {
    Unacquired = 0,
    Acquired = 1,
    Released = 2,
};

// External C functions from proven library
extern fn idris_proven_resource_new_handle(id: u64) ?*types.ResourceHandle;
extern fn idris_proven_resource_mark_acquired(handle: *types.ResourceHandle, timestamp: u64) ?*types.ResourceHandle;
extern fn idris_proven_resource_mark_released(handle: *types.ResourceHandle, timestamp: u64) ?*types.ResourceHandle;
extern fn idris_proven_resource_is_held(handle: *types.ResourceHandle) bool;
extern fn idris_proven_resource_get_state(handle: *types.ResourceHandle) u32;
extern fn idris_proven_resource_free_handle(handle: *types.ResourceHandle) void;

//-----------------------------------------------------------------------------
// FFI Exports for Ephapax
//-----------------------------------------------------------------------------

export fn ephapax_proven_resource_new_handle(id: u64) callconv(.C) ?*types.ResourceHandle {
    return idris_proven_resource_new_handle(id);
}

export fn ephapax_proven_resource_mark_acquired(
    handle: *types.ResourceHandle,
    timestamp: u64,
) callconv(.C) ?*types.ResourceHandle {
    return idris_proven_resource_mark_acquired(handle, timestamp);
}

export fn ephapax_proven_resource_mark_released(
    handle: *types.ResourceHandle,
    timestamp: u64,
) callconv(.C) ?*types.ResourceHandle {
    return idris_proven_resource_mark_released(handle, timestamp);
}

export fn ephapax_proven_resource_is_held(handle: *types.ResourceHandle) callconv(.C) bool {
    return idris_proven_resource_is_held(handle);
}

export fn ephapax_proven_resource_get_state(handle: *types.ResourceHandle) callconv(.C) ResourceState {
    const state_val = idris_proven_resource_get_state(handle);
    return @enumFromInt(state_val);
}

export fn ephapax_proven_resource_free_handle(handle: *types.ResourceHandle) callconv(.C) void {
    idris_proven_resource_free_handle(handle);
}
