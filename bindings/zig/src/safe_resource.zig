// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

//! Safe resource handle management with RAII semantics.
//!
//! Provides type-safe resource handles that ensure proper cleanup through
//! Zig's defer mechanism. Resources are tracked and cannot be double-freed
//! or used after release.

const std = @import("std");
const Allocator = std.mem.Allocator;

/// Error types for resource operations.
pub const ResourceError = error{
    ResourceAlreadyReleased,
    ResourceNotFound,
    ResourceLimitExceeded,
    InvalidHandle,
    AllocationFailed,
    AcquisitionFailed,
};

/// Generation counter to detect stale handles.
pub const Generation = u32;

/// A type-safe handle that references a managed resource.
pub fn Handle(comptime _: type) type {
    return struct {
        index: u32,
        generation: Generation,

        const Self = @This();

        /// Sentinel value for invalid handles.
        pub const invalid = Self{
            .index = std.math.maxInt(u32),
            .generation = 0,
        };

        /// Check if this handle is valid (not the sentinel).
        pub fn isValid(self: Self) bool {
            return self.index != std.math.maxInt(u32);
        }
    };
}

/// A resource pool that manages handles to resources of type T.
pub fn ResourcePool(comptime T: type, comptime capacity: usize) type {
    return struct {
        const Self = @This();
        const HandleType = Handle(T);

        const Slot = struct {
            resource: ?T,
            generation: Generation,
            is_active: bool,
        };

        slots: [capacity]Slot,
        free_list: [capacity]u32,
        free_count: usize,
        active_count: usize,

        /// Initialize an empty resource pool.
        pub fn init() Self {
            var pool = Self{
                .slots = undefined,
                .free_list = undefined,
                .free_count = capacity,
                .active_count = 0,
            };

            // Initialize all slots as empty
            for (&pool.slots, 0..) |*slot, i| {
                slot.* = Slot{
                    .resource = null,
                    .generation = 0,
                    .is_active = false,
                };
                pool.free_list[i] = @intCast(capacity - 1 - i);
            }

            return pool;
        }

        /// Acquire a slot and store a resource.
        pub fn acquire(self: *Self, resource: T) ResourceError!HandleType {
            if (self.free_count == 0) return error.ResourceLimitExceeded;

            self.free_count -= 1;
            const index = self.free_list[self.free_count];

            var slot = &self.slots[index];
            slot.resource = resource;
            slot.is_active = true;
            self.active_count += 1;

            return HandleType{
                .index = index,
                .generation = slot.generation,
            };
        }

        /// Release a resource by handle.
        pub fn release(self: *Self, handle: HandleType) ResourceError!T {
            if (!handle.isValid()) return error.InvalidHandle;
            if (handle.index >= capacity) return error.InvalidHandle;

            var slot = &self.slots[handle.index];

            if (!slot.is_active) return error.ResourceAlreadyReleased;
            if (slot.generation != handle.generation) return error.ResourceAlreadyReleased;

            const resource = slot.resource.?;
            slot.resource = null;
            slot.is_active = false;
            slot.generation +%= 1; // Wrap on overflow

            self.free_list[self.free_count] = handle.index;
            self.free_count += 1;
            self.active_count -= 1;

            return resource;
        }

        /// Get a reference to a resource without releasing it.
        pub fn get(self: *Self, handle: HandleType) ResourceError!*T {
            if (!handle.isValid()) return error.InvalidHandle;
            if (handle.index >= capacity) return error.InvalidHandle;

            var slot = &self.slots[handle.index];

            if (!slot.is_active) return error.ResourceAlreadyReleased;
            if (slot.generation != handle.generation) return error.ResourceAlreadyReleased;

            return &slot.resource.?;
        }

        /// Get a const reference to a resource.
        pub fn getConst(self: *const Self, handle: HandleType) ResourceError!*const T {
            if (!handle.isValid()) return error.InvalidHandle;
            if (handle.index >= capacity) return error.InvalidHandle;

            const slot = &self.slots[handle.index];

            if (!slot.is_active) return error.ResourceAlreadyReleased;
            if (slot.generation != handle.generation) return error.ResourceAlreadyReleased;

            return &slot.resource.?;
        }

        /// Check if a handle is still valid.
        pub fn isActive(self: *const Self, handle: HandleType) bool {
            if (!handle.isValid()) return false;
            if (handle.index >= capacity) return false;

            const slot = &self.slots[handle.index];
            return slot.is_active and slot.generation == handle.generation;
        }

        /// Get the number of active resources.
        pub fn activeCount(self: *const Self) usize {
            return self.active_count;
        }

        /// Get remaining capacity.
        pub fn remainingCapacity(self: *const Self) usize {
            return self.free_count;
        }

        /// Clear all resources (does not call deinit on resources).
        pub fn clear(self: *Self) void {
            for (&self.slots, 0..) |*slot, i| {
                if (slot.is_active) {
                    slot.resource = null;
                    slot.is_active = false;
                    slot.generation +%= 1;
                }
                self.free_list[i] = @intCast(capacity - 1 - i);
            }
            self.free_count = capacity;
            self.active_count = 0;
        }
    };
}

/// A scoped guard that automatically releases a resource when it goes out of scope.
pub fn ScopedResource(comptime T: type, comptime PoolType: type) type {
    return struct {
        const Self = @This();

        pool: *PoolType,
        handle: Handle(T),

        /// Create a scoped guard for an existing handle.
        pub fn init(pool: *PoolType, handle: Handle(T)) Self {
            return Self{
                .pool = pool,
                .handle = handle,
            };
        }

        /// Get the underlying resource.
        pub fn get(self: *Self) ResourceError!*T {
            return self.pool.get(self.handle);
        }

        /// Get const reference to the underlying resource.
        pub fn getConst(self: *const Self) ResourceError!*const T {
            return self.pool.getConst(self.handle);
        }

        /// Release the resource (called automatically by defer).
        pub fn deinit(self: *Self) void {
            _ = self.pool.release(self.handle) catch {};
        }

        /// Explicitly release and return the resource.
        pub fn release(self: *Self) ResourceError!T {
            const resource = try self.pool.release(self.handle);
            self.handle = Handle(T).invalid;
            return resource;
        }

        /// Check if still valid.
        pub fn isValid(self: *const Self) bool {
            return self.pool.isActive(self.handle);
        }
    };
}

/// A reference-counted resource wrapper.
pub fn RefCounted(comptime T: type) type {
    return struct {
        const Self = @This();

        value: T,
        ref_count: u32,

        /// Create a new reference-counted resource.
        pub fn init(value: T) Self {
            return Self{
                .value = value,
                .ref_count = 1,
            };
        }

        /// Increment reference count.
        pub fn retain(self: *Self) void {
            self.ref_count += 1;
        }

        /// Decrement reference count, returns true if resource should be freed.
        pub fn release(self: *Self) bool {
            if (self.ref_count == 0) return true;
            self.ref_count -= 1;
            return self.ref_count == 0;
        }

        /// Get the current reference count.
        pub fn count(self: *const Self) u32 {
            return self.ref_count;
        }

        /// Check if this is the only reference.
        pub fn isUnique(self: *const Self) bool {
            return self.ref_count == 1;
        }
    };
}

/// A unique resource that cannot be copied (move-only semantics).
pub fn UniqueResource(comptime T: type) type {
    return struct {
        const Self = @This();

        value: ?T,

        /// Create a unique resource.
        pub fn init(value: T) Self {
            return Self{ .value = value };
        }

        /// Create an empty unique resource.
        pub fn empty() Self {
            return Self{ .value = null };
        }

        /// Check if the resource is present.
        pub fn hasValue(self: *const Self) bool {
            return self.value != null;
        }

        /// Get a reference to the value.
        pub fn get(self: *Self) ResourceError!*T {
            if (self.value) |*v| {
                return v;
            }
            return error.ResourceAlreadyReleased;
        }

        /// Get a const reference to the value.
        pub fn getConst(self: *const Self) ResourceError!*const T {
            if (self.value) |*v| {
                return v;
            }
            return error.ResourceAlreadyReleased;
        }

        /// Take ownership of the value, leaving this empty.
        pub fn take(self: *Self) ResourceError!T {
            if (self.value) |v| {
                self.value = null;
                return v;
            }
            return error.ResourceAlreadyReleased;
        }

        /// Replace the value, returning the old one.
        pub fn replace(self: *Self, new_value: T) ?T {
            const old = self.value;
            self.value = new_value;
            return old;
        }

        /// Clear the value.
        pub fn clear(self: *Self) void {
            self.value = null;
        }
    };
}

/// Lease-based resource that expires after a deadline.
pub fn LeasedResource(comptime T: type) type {
    return struct {
        const Self = @This();

        value: T,
        lease_start: u64,
        lease_duration: u64,

        /// Create a leased resource.
        pub fn init(value: T, current_time: u64, duration: u64) Self {
            return Self{
                .value = value,
                .lease_start = current_time,
                .lease_duration = duration,
            };
        }

        /// Check if the lease has expired.
        pub fn isExpired(self: *const Self, current_time: u64) bool {
            return current_time >= self.lease_start + self.lease_duration;
        }

        /// Get remaining lease time.
        pub fn remainingTime(self: *const Self, current_time: u64) u64 {
            const expiry = self.lease_start + self.lease_duration;
            if (current_time >= expiry) return 0;
            return expiry - current_time;
        }

        /// Renew the lease.
        pub fn renew(self: *Self, current_time: u64, new_duration: u64) void {
            self.lease_start = current_time;
            self.lease_duration = new_duration;
        }

        /// Get the value if lease is valid.
        pub fn getValue(self: *Self, current_time: u64) ResourceError!*T {
            if (self.isExpired(current_time)) return error.ResourceAlreadyReleased;
            return &self.value;
        }
    };
}

test "ResourcePool basic operations" {
    var pool = ResourcePool(u32, 4).init();

    const h1 = try pool.acquire(100);
    const h2 = try pool.acquire(200);

    try std.testing.expectEqual(@as(usize, 2), pool.activeCount());
    try std.testing.expectEqual(@as(u32, 100), (try pool.get(h1)).*);
    try std.testing.expectEqual(@as(u32, 200), (try pool.get(h2)).*);

    const released = try pool.release(h1);
    try std.testing.expectEqual(@as(u32, 100), released);
    try std.testing.expectEqual(@as(usize, 1), pool.activeCount());
}

test "ResourcePool generation tracking" {
    var pool = ResourcePool(u32, 2).init();

    const h1 = try pool.acquire(42);
    _ = try pool.release(h1);

    // Acquire a new resource in the same slot
    const h2 = try pool.acquire(99);

    // Old handle should be invalid
    try std.testing.expectError(error.ResourceAlreadyReleased, pool.get(h1));

    // New handle should work
    try std.testing.expectEqual(@as(u32, 99), (try pool.get(h2)).*);
}

test "ResourcePool capacity limit" {
    var pool = ResourcePool(u32, 2).init();

    _ = try pool.acquire(1);
    _ = try pool.acquire(2);
    try std.testing.expectError(error.ResourceLimitExceeded, pool.acquire(3));
}

test "ScopedResource auto cleanup" {
    var pool = ResourcePool(u32, 4).init();
    const handle = try pool.acquire(42);

    {
        var scoped = ScopedResource(u32, @TypeOf(pool)).init(&pool, handle);
        defer scoped.deinit();

        try std.testing.expectEqual(@as(u32, 42), (try scoped.get()).*);
    }

    // After scope exit, resource should be released
    try std.testing.expectError(error.ResourceAlreadyReleased, pool.get(handle));
}

test "RefCounted" {
    var rc = RefCounted(u32).init(42);
    try std.testing.expectEqual(@as(u32, 1), rc.count());

    rc.retain();
    try std.testing.expectEqual(@as(u32, 2), rc.count());

    try std.testing.expect(!rc.release());
    try std.testing.expectEqual(@as(u32, 1), rc.count());

    try std.testing.expect(rc.release());
}

test "UniqueResource" {
    var unique = UniqueResource(u32).init(42);
    try std.testing.expect(unique.hasValue());

    const value = try unique.take();
    try std.testing.expectEqual(@as(u32, 42), value);
    try std.testing.expect(!unique.hasValue());

    try std.testing.expectError(error.ResourceAlreadyReleased, unique.take());
}

test "LeasedResource" {
    var leased = LeasedResource(u32).init(42, 100, 50);

    try std.testing.expect(!leased.isExpired(120));
    try std.testing.expectEqual(@as(u64, 30), leased.remainingTime(120));

    try std.testing.expect(leased.isExpired(150));
    try std.testing.expectEqual(@as(u64, 0), leased.remainingTime(150));

    try std.testing.expectError(error.ResourceAlreadyReleased, leased.getValue(200));

    leased.renew(200, 100);
    try std.testing.expect(!leased.isExpired(250));
}
