// SPDX-License-Identifier: PMPL-1.0
//! Pure Zig implementation of SafeResource

const std = @import("std");

pub const ResourceState = enum(u32) {
    Unacquired = 0,
    Acquired = 1,
    Released = 2,
};

pub const ResourceHandle = struct {
    resource_id: u64,
    state: ResourceState,
    acquired_at: u64,
    released_at: u64,
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator, id: u64) !*ResourceHandle {
        const handle = try allocator.create(ResourceHandle);
        handle.* = .{
            .resource_id = id,
            .state = .Unacquired,
            .acquired_at = 0,
            .released_at = 0,
            .allocator = allocator,
        };
        return handle;
    }

    pub fn deinit(self: *ResourceHandle) void {
        const allocator = self.allocator;
        allocator.destroy(self);
    }

    pub fn markAcquired(self: *ResourceHandle, timestamp: u64) void {
        self.state = .Acquired;
        self.acquired_at = timestamp;
    }

    pub fn markReleased(self: *ResourceHandle, timestamp: u64) void {
        self.state = .Released;
        self.released_at = timestamp;
    }

    pub fn isHeld(self: *ResourceHandle) bool {
        return self.state == .Acquired;
    }
};

// C-compatible FFI exports
var gpa = std.heap.GeneralPurposeAllocator(.{}){};

pub export fn idris_proven_resource_new_handle(id: u64) ?*ResourceHandle {
    const allocator = gpa.allocator();
    return ResourceHandle.init(allocator, id) catch null;
}

pub export fn idris_proven_resource_mark_acquired(
    handle: *ResourceHandle,
    timestamp: u64,
) ?*ResourceHandle {
    handle.markAcquired(timestamp);
    return handle;
}

pub export fn idris_proven_resource_mark_released(
    handle: *ResourceHandle,
    timestamp: u64,
) ?*ResourceHandle {
    handle.markReleased(timestamp);
    return handle;
}

pub export fn idris_proven_resource_is_held(handle: *ResourceHandle) bool {
    return handle.isHeld();
}

pub export fn idris_proven_resource_get_state(handle: *ResourceHandle) u32 {
    return @intFromEnum(handle.state);
}

pub export fn idris_proven_resource_free_handle(handle: *ResourceHandle) void {
    handle.deinit();
}
