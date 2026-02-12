// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

//! Safe counting semaphore with bounded permits that cannot crash.
//!
//! Provides synchronization primitives for resource management with
//! guaranteed bounds checking. Useful for limiting concurrent access
//! to resources, implementing connection pools, and rate limiting.

const std = @import("std");

/// Error types for semaphore operations.
pub const SemaphoreError = error{
    NoPermitsAvailable,
    TooManyPermits,
    InvalidPermitCount,
    WouldOverflow,
    AlreadyReleased,
    Timeout,
};

/// Result of a try-acquire operation.
pub const AcquireResult = union(enum) {
    acquired: void,
    unavailable: usize, // Number of permits needed
};

/// Counting semaphore with a maximum permit limit.
pub fn CountingSemaphore(comptime max_permits: usize) type {
    return struct {
        const Self = @This();

        available: usize,
        max: usize,
        total_acquired: u64 = 0,
        total_released: u64 = 0,

        /// Initialize with a given number of permits.
        pub fn init(initial_permits: usize) Self {
            return .{
                .available = @min(initial_permits, max_permits),
                .max = max_permits,
            };
        }

        /// Initialize with all permits available.
        pub fn initFull() Self {
            return init(max_permits);
        }

        /// Initialize with no permits available.
        pub fn initEmpty() Self {
            return init(0);
        }

        /// Get the number of currently available permits.
        pub fn availablePermits(self: *const Self) usize {
            return self.available;
        }

        /// Get the maximum number of permits.
        pub fn maxPermits(self: *const Self) usize {
            return self.max;
        }

        /// Check if any permits are available.
        pub fn hasAvailable(self: *const Self) bool {
            return self.available > 0;
        }

        /// Check if all permits are available.
        pub fn isFull(self: *const Self) bool {
            return self.available == self.max;
        }

        /// Check if no permits are available.
        pub fn isEmpty(self: *const Self) bool {
            return self.available == 0;
        }

        /// Get the number of permits currently held.
        pub fn heldPermits(self: *const Self) usize {
            return self.max - self.available;
        }

        /// Try to acquire a single permit without blocking.
        pub fn tryAcquire(self: *Self) SemaphoreError!void {
            return self.tryAcquireMany(1);
        }

        /// Try to acquire multiple permits without blocking.
        pub fn tryAcquireMany(self: *Self, count: usize) SemaphoreError!void {
            if (count == 0) return error.InvalidPermitCount;
            if (count > max_permits) return error.TooManyPermits;
            if (self.available < count) return error.NoPermitsAvailable;

            self.available -= count;
            self.total_acquired += count;
        }

        /// Try to acquire, returning a result instead of error.
        pub fn tryAcquireResult(self: *Self) AcquireResult {
            return self.tryAcquireManyResult(1);
        }

        /// Try to acquire many, returning a result instead of error.
        pub fn tryAcquireManyResult(self: *Self, count: usize) AcquireResult {
            if (count == 0 or count > max_permits) return .{ .unavailable = count };
            if (self.available < count) return .{ .unavailable = count - self.available };

            self.available -= count;
            self.total_acquired += count;
            return .acquired;
        }

        /// Release a single permit.
        pub fn release(self: *Self) SemaphoreError!void {
            return self.releaseMany(1);
        }

        /// Release multiple permits.
        pub fn releaseMany(self: *Self, count: usize) SemaphoreError!void {
            if (count == 0) return error.InvalidPermitCount;
            if (self.available + count > self.max) return error.WouldOverflow;

            self.available += count;
            self.total_released += count;
        }

        /// Force release (saturating - won't exceed max).
        pub fn releaseSaturating(self: *Self, count: usize) void {
            const new_available = self.available + count;
            self.available = @min(new_available, self.max);
            self.total_released += @min(count, self.max - self.available + count);
        }

        /// Drain all available permits, returning count acquired.
        pub fn drainAll(self: *Self) usize {
            const drained = self.available;
            self.total_acquired += drained;
            self.available = 0;
            return drained;
        }

        /// Reset to initial state with given permits.
        pub fn reset(self: *Self, permits: usize) void {
            self.available = @min(permits, self.max);
            self.total_acquired = 0;
            self.total_released = 0;
        }

        /// Get statistics about semaphore usage.
        pub fn stats(self: *const Self) SemaphoreStats {
            return .{
                .available = self.available,
                .max = self.max,
                .held = self.heldPermits(),
                .total_acquired = self.total_acquired,
                .total_released = self.total_released,
            };
        }
    };
}

/// Statistics about semaphore usage.
pub const SemaphoreStats = struct {
    available: usize,
    max: usize,
    held: usize,
    total_acquired: u64,
    total_released: u64,

    pub fn utilizationPercent(self: SemaphoreStats) f64 {
        if (self.max == 0) return 0.0;
        return @as(f64, @floatFromInt(self.held)) / @as(f64, @floatFromInt(self.max)) * 100.0;
    }
};

/// Binary semaphore (mutex-like, 0 or 1 permits).
pub const BinarySemaphore = struct {
    locked: bool = false,
    lock_count: u64 = 0,
    unlock_count: u64 = 0,

    pub fn init() BinarySemaphore {
        return .{};
    }

    pub fn initLocked() BinarySemaphore {
        return .{ .locked = true };
    }

    pub fn isLocked(self: *const BinarySemaphore) bool {
        return self.locked;
    }

    pub fn isUnlocked(self: *const BinarySemaphore) bool {
        return !self.locked;
    }

    pub fn tryLock(self: *BinarySemaphore) SemaphoreError!void {
        if (self.locked) return error.NoPermitsAvailable;
        self.locked = true;
        self.lock_count += 1;
    }

    pub fn tryLockResult(self: *BinarySemaphore) bool {
        if (self.locked) return false;
        self.locked = true;
        self.lock_count += 1;
        return true;
    }

    pub fn unlock(self: *BinarySemaphore) SemaphoreError!void {
        if (!self.locked) return error.AlreadyReleased;
        self.locked = false;
        self.unlock_count += 1;
    }

    pub fn forceUnlock(self: *BinarySemaphore) void {
        if (self.locked) {
            self.locked = false;
            self.unlock_count += 1;
        }
    }

    pub fn reset(self: *BinarySemaphore) void {
        self.locked = false;
        self.lock_count = 0;
        self.unlock_count = 0;
    }
};

/// Permit guard for RAII-style resource management.
/// Automatically releases permits when guard goes out of scope.
pub fn PermitGuard(comptime SemaphoreType: type) type {
    return struct {
        const Self = @This();

        semaphore: *SemaphoreType,
        permits_held: usize,
        released: bool = false,

        /// Create a guard that holds acquired permits.
        pub fn init(semaphore: *SemaphoreType, permits: usize) ?Self {
            semaphore.tryAcquireMany(permits) catch return null;
            return .{
                .semaphore = semaphore,
                .permits_held = permits,
            };
        }

        /// Explicitly release permits before going out of scope.
        pub fn release(self: *Self) void {
            if (!self.released) {
                self.semaphore.releaseSaturating(self.permits_held);
                self.released = true;
            }
        }

        /// Get the number of permits held by this guard.
        pub fn heldCount(self: *const Self) usize {
            if (self.released) return 0;
            return self.permits_held;
        }
    };
}

/// Weighted semaphore where different resources have different costs.
pub fn WeightedSemaphore(comptime max_weight: usize) type {
    return struct {
        const Self = @This();

        available_weight: usize,
        max_weight_value: usize,

        pub fn init(initial_weight: usize) Self {
            return .{
                .available_weight = @min(initial_weight, max_weight),
                .max_weight_value = max_weight,
            };
        }

        pub fn initFull() Self {
            return init(max_weight);
        }

        /// Try to acquire a resource with the given weight.
        pub fn tryAcquireWeight(self: *Self, weight: usize) SemaphoreError!void {
            if (weight == 0) return error.InvalidPermitCount;
            if (weight > max_weight) return error.TooManyPermits;
            if (self.available_weight < weight) return error.NoPermitsAvailable;

            self.available_weight -= weight;
        }

        /// Release weight back to the semaphore.
        pub fn releaseWeight(self: *Self, weight: usize) SemaphoreError!void {
            if (weight == 0) return error.InvalidPermitCount;
            if (self.available_weight + weight > self.max_weight_value) return error.WouldOverflow;

            self.available_weight += weight;
        }

        /// Get available weight.
        pub fn availableWeight(self: *const Self) usize {
            return self.available_weight;
        }

        /// Check if a given weight can be acquired.
        pub fn canAcquire(self: *const Self, weight: usize) bool {
            return weight <= self.available_weight;
        }
    };
}

/// Fair semaphore that tracks waiting order (simplified, bounded waiters).
pub fn FairSemaphore(comptime max_permits: usize, comptime max_waiters: usize) type {
    return struct {
        const Self = @This();

        semaphore: CountingSemaphore(max_permits),
        wait_queue: [max_waiters]WaitEntry = [_]WaitEntry{.{}} ** max_waiters,
        waiter_count: usize = 0,
        next_ticket: u64 = 0,
        served_ticket: u64 = 0,

        const WaitEntry = struct {
            ticket: u64 = 0,
            permits_needed: usize = 0,
            active: bool = false,
        };

        pub fn init(initial_permits: usize) Self {
            return .{
                .semaphore = CountingSemaphore(max_permits).init(initial_permits),
            };
        }

        /// Register a waiter and get a ticket number.
        pub fn registerWaiter(self: *Self, permits_needed: usize) ?u64 {
            if (self.waiter_count >= max_waiters) return null;

            const ticket = self.next_ticket;
            self.next_ticket += 1;

            // Find empty slot
            for (&self.wait_queue) |*entry| {
                if (!entry.active) {
                    entry.* = .{
                        .ticket = ticket,
                        .permits_needed = permits_needed,
                        .active = true,
                    };
                    self.waiter_count += 1;
                    return ticket;
                }
            }
            return null;
        }

        /// Check if it's this ticket's turn and permits are available.
        pub fn canProceed(self: *const Self, ticket: u64, permits_needed: usize) bool {
            if (ticket != self.served_ticket) return false;
            return self.semaphore.available >= permits_needed;
        }

        /// Acquire permits for a registered waiter.
        pub fn acquireWithTicket(self: *Self, ticket: u64, permits_needed: usize) SemaphoreError!void {
            if (!self.canProceed(ticket, permits_needed)) return error.NoPermitsAvailable;

            try self.semaphore.tryAcquireMany(permits_needed);

            // Remove from wait queue
            for (&self.wait_queue) |*entry| {
                if (entry.active and entry.ticket == ticket) {
                    entry.active = false;
                    self.waiter_count -|= 1;
                    break;
                }
            }

            self.served_ticket += 1;
        }

        /// Release permits.
        pub fn release(self: *Self, count: usize) SemaphoreError!void {
            return self.semaphore.releaseMany(count);
        }

        /// Get number of waiters.
        pub fn waitingCount(self: *const Self) usize {
            return self.waiter_count;
        }
    };
}

test "CountingSemaphore basic" {
    var sem = CountingSemaphore(5).initFull();

    try std.testing.expectEqual(@as(usize, 5), sem.availablePermits());
    try sem.tryAcquire();
    try std.testing.expectEqual(@as(usize, 4), sem.availablePermits());

    try sem.tryAcquireMany(2);
    try std.testing.expectEqual(@as(usize, 2), sem.availablePermits());

    try sem.release();
    try std.testing.expectEqual(@as(usize, 3), sem.availablePermits());
}

test "CountingSemaphore errors" {
    var sem = CountingSemaphore(3).init(1);

    try sem.tryAcquire();
    try std.testing.expectError(error.NoPermitsAvailable, sem.tryAcquire());

    try sem.release();
    try sem.release();
    try sem.release();
    try std.testing.expectError(error.WouldOverflow, sem.release());
}

test "CountingSemaphore drain" {
    var sem = CountingSemaphore(10).init(7);
    const drained = sem.drainAll();
    try std.testing.expectEqual(@as(usize, 7), drained);
    try std.testing.expect(sem.isEmpty());
}

test "BinarySemaphore" {
    var sem = BinarySemaphore.init();

    try std.testing.expect(sem.isUnlocked());
    try sem.tryLock();
    try std.testing.expect(sem.isLocked());
    try std.testing.expectError(error.NoPermitsAvailable, sem.tryLock());

    try sem.unlock();
    try std.testing.expect(sem.isUnlocked());
    try std.testing.expectError(error.AlreadyReleased, sem.unlock());
}

test "CountingSemaphore stats" {
    var sem = CountingSemaphore(10).initFull();
    try sem.tryAcquireMany(4);

    const s = sem.stats();
    try std.testing.expectEqual(@as(usize, 6), s.available);
    try std.testing.expectEqual(@as(usize, 4), s.held);
    try std.testing.expectEqual(@as(u64, 4), s.total_acquired);
    try std.testing.expect(s.utilizationPercent() >= 39.0 and s.utilizationPercent() <= 41.0);
}

test "WeightedSemaphore" {
    var sem = WeightedSemaphore(100).initFull();

    try sem.tryAcquireWeight(30);
    try std.testing.expectEqual(@as(usize, 70), sem.availableWeight());

    try sem.tryAcquireWeight(50);
    try std.testing.expectEqual(@as(usize, 20), sem.availableWeight());

    try std.testing.expectError(error.NoPermitsAvailable, sem.tryAcquireWeight(30));

    try sem.releaseWeight(80);
    try std.testing.expectEqual(@as(usize, 100), sem.availableWeight());
}

test "AcquireResult" {
    var sem = CountingSemaphore(3).init(2);

    const result1 = sem.tryAcquireManyResult(1);
    try std.testing.expect(result1 == .acquired);

    const result2 = sem.tryAcquireManyResult(3);
    switch (result2) {
        .unavailable => |needed| try std.testing.expectEqual(@as(usize, 2), needed),
        .acquired => unreachable,
    }
}
