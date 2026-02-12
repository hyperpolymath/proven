// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

//! Safe transaction state management that cannot crash.
//!
//! Provides ACID-style transaction semantics with begin/commit/rollback
//! operations. Tracks transaction state, supports nested transactions
//! via savepoints, and ensures consistent state transitions.

const std = @import("std");

/// Get current Unix timestamp in seconds.
fn getTimestamp() i64 {
    const ts = std.posix.clock_gettime(.REALTIME) catch return 0;
    return ts.sec;
}

/// Error types for transaction operations.
pub const TransactionError = error{
    NotInTransaction,
    AlreadyInTransaction,
    SavepointNotFound,
    MaxSavepointsReached,
    InvalidState,
    RollbackRequired,
    CommitFailed,
};

/// Transaction isolation levels (for documentation/metadata purposes).
pub const IsolationLevel = enum {
    read_uncommitted,
    read_committed,
    repeatable_read,
    serializable,

    /// Get string representation.
    pub fn toString(self: IsolationLevel) []const u8 {
        return switch (self) {
            .read_uncommitted => "READ UNCOMMITTED",
            .read_committed => "READ COMMITTED",
            .repeatable_read => "REPEATABLE READ",
            .serializable => "SERIALIZABLE",
        };
    }
};

/// Transaction state enumeration.
pub const TransactionState = enum {
    idle,
    active,
    committed,
    rolled_back,
    failed,

    /// Check if transaction is in a terminal state.
    pub fn isTerminal(self: TransactionState) bool {
        return self == .committed or self == .rolled_back or self == .failed;
    }

    /// Check if transaction can accept operations.
    pub fn canOperate(self: TransactionState) bool {
        return self == .active;
    }
};

/// Maximum number of savepoints per transaction.
pub const MAX_SAVEPOINTS = 32;

/// Maximum number of operations to track in history.
pub const MAX_OPERATIONS = 256;

/// A recorded operation for rollback purposes.
pub const Operation = struct {
    op_type: OperationType,
    key: []const u8,
    old_value: ?[]const u8,
    new_value: ?[]const u8,
    timestamp: i64,
};

/// Types of operations that can be recorded.
pub const OperationType = enum {
    insert,
    update,
    delete,
    savepoint,
};

/// A savepoint marker within a transaction.
pub const Savepoint = struct {
    name: []const u8,
    operation_index: usize,
    created_at: i64,
};

/// Transaction manager with bounded operation tracking.
pub fn TransactionManager(comptime max_ops: usize) type {
    return struct {
        const Self = @This();

        state: TransactionState = .idle,
        isolation_level: IsolationLevel = .read_committed,
        operations: [max_ops]?Operation = [_]?Operation{null} ** max_ops,
        operation_count: usize = 0,
        savepoints: [MAX_SAVEPOINTS]?Savepoint = [_]?Savepoint{null} ** MAX_SAVEPOINTS,
        savepoint_count: usize = 0,
        started_at: ?i64 = null,
        transaction_id: u64 = 0,
        next_id: u64 = 1,

        /// Initialize a new transaction manager.
        pub fn init() Self {
            return .{};
        }

        /// Begin a new transaction.
        pub fn begin(self: *Self) TransactionError!void {
            return self.beginWithIsolation(.read_committed);
        }

        /// Begin a transaction with specific isolation level.
        pub fn beginWithIsolation(self: *Self, level: IsolationLevel) TransactionError!void {
            if (self.state == .active) return error.AlreadyInTransaction;
            if (self.state == .failed) return error.RollbackRequired;

            self.state = .active;
            self.isolation_level = level;
            self.operation_count = 0;
            self.savepoint_count = 0;
            self.started_at = getTimestamp();
            self.transaction_id = self.next_id;
            self.next_id += 1;

            // Clear operations
            for (&self.operations) |*op| {
                op.* = null;
            }
            for (&self.savepoints) |*sp| {
                sp.* = null;
            }
        }

        /// Commit the current transaction.
        pub fn commit(self: *Self) TransactionError!void {
            if (self.state != .active) return error.NotInTransaction;

            self.state = .committed;
            self.started_at = null;
        }

        /// Rollback the current transaction.
        pub fn rollback(self: *Self) TransactionError!void {
            if (self.state != .active and self.state != .failed) {
                return error.NotInTransaction;
            }

            self.state = .rolled_back;
            self.started_at = null;
        }

        /// Create a savepoint.
        pub fn savepoint(self: *Self, name: []const u8) TransactionError!void {
            if (self.state != .active) return error.NotInTransaction;
            if (self.savepoint_count >= MAX_SAVEPOINTS) return error.MaxSavepointsReached;

            self.savepoints[self.savepoint_count] = .{
                .name = name,
                .operation_index = self.operation_count,
                .created_at = getTimestamp(),
            };
            self.savepoint_count += 1;
        }

        /// Rollback to a savepoint.
        pub fn rollbackToSavepoint(self: *Self, name: []const u8) TransactionError!void {
            if (self.state != .active) return error.NotInTransaction;

            // Find the savepoint
            var found_idx: ?usize = null;
            for (self.savepoints[0..self.savepoint_count], 0..) |sp_opt, i| {
                if (sp_opt) |sp| {
                    if (std.mem.eql(u8, sp.name, name)) {
                        found_idx = i;
                        break;
                    }
                }
            }

            if (found_idx) |idx| {
                const sp = self.savepoints[idx].?;
                // Discard operations after savepoint
                self.operation_count = sp.operation_index;
                // Remove this and later savepoints
                self.savepoint_count = idx;
            } else {
                return error.SavepointNotFound;
            }
        }

        /// Release a savepoint (keep changes but remove marker).
        pub fn releaseSavepoint(self: *Self, name: []const u8) TransactionError!void {
            if (self.state != .active) return error.NotInTransaction;

            // Find and remove the savepoint
            var found_idx: ?usize = null;
            for (self.savepoints[0..self.savepoint_count], 0..) |sp_opt, i| {
                if (sp_opt) |sp| {
                    if (std.mem.eql(u8, sp.name, name)) {
                        found_idx = i;
                        break;
                    }
                }
            }

            if (found_idx) |idx| {
                // Shift remaining savepoints down
                var i = idx;
                while (i < self.savepoint_count - 1) : (i += 1) {
                    self.savepoints[i] = self.savepoints[i + 1];
                }
                self.savepoints[self.savepoint_count - 1] = null;
                self.savepoint_count -= 1;
            } else {
                return error.SavepointNotFound;
            }
        }

        /// Record an operation (for rollback tracking).
        pub fn recordOperation(self: *Self, op_type: OperationType, key: []const u8, old_value: ?[]const u8, new_value: ?[]const u8) TransactionError!void {
            if (self.state != .active) return error.NotInTransaction;
            if (self.operation_count >= max_ops) {
                self.state = .failed;
                return error.InvalidState;
            }

            self.operations[self.operation_count] = .{
                .op_type = op_type,
                .key = key,
                .old_value = old_value,
                .new_value = new_value,
                .timestamp = getTimestamp(),
            };
            self.operation_count += 1;
        }

        /// Get current transaction state.
        pub fn getState(self: *const Self) TransactionState {
            return self.state;
        }

        /// Check if currently in an active transaction.
        pub fn isActive(self: *const Self) bool {
            return self.state == .active;
        }

        /// Get current transaction ID (0 if not in transaction).
        pub fn getId(self: *const Self) u64 {
            if (self.state == .active) {
                return self.transaction_id;
            }
            return 0;
        }

        /// Get number of recorded operations.
        pub fn getOperationCount(self: *const Self) usize {
            return self.operation_count;
        }

        /// Get number of active savepoints.
        pub fn getSavepointCount(self: *const Self) usize {
            return self.savepoint_count;
        }

        /// Get isolation level.
        pub fn getIsolationLevel(self: *const Self) IsolationLevel {
            return self.isolation_level;
        }

        /// Mark transaction as failed (requires rollback).
        pub fn fail(self: *Self) void {
            if (self.state == .active) {
                self.state = .failed;
            }
        }

        /// Reset to idle state (after commit/rollback).
        pub fn reset(self: *Self) void {
            self.state = .idle;
            self.operation_count = 0;
            self.savepoint_count = 0;
            self.started_at = null;
        }
    };
}

/// Simple transaction guard for RAII-style transaction management.
pub fn TransactionGuard(comptime TxnManager: type) type {
    return struct {
        const Self = @This();

        manager: *TxnManager,
        committed: bool = false,

        /// Create a guard and begin transaction.
        pub fn init(manager: *TxnManager) TransactionError!Self {
            try manager.begin();
            return .{ .manager = manager };
        }

        /// Commit the transaction.
        pub fn commit(self: *Self) TransactionError!void {
            try self.manager.commit();
            self.committed = true;
        }

        /// Explicitly rollback.
        pub fn rollback(self: *Self) TransactionError!void {
            try self.manager.rollback();
            self.committed = true; // Prevent double rollback in deinit
        }

        /// Auto-rollback on scope exit if not committed.
        pub fn deinit(self: *Self) void {
            if (!self.committed and self.manager.isActive()) {
                self.manager.rollback() catch {};
            }
        }
    };
}

/// Default transaction manager type.
pub const DefaultTransactionManager = TransactionManager(MAX_OPERATIONS);

test "basic transaction lifecycle" {
    var txn = DefaultTransactionManager.init();

    try std.testing.expectEqual(TransactionState.idle, txn.getState());

    try txn.begin();
    try std.testing.expect(txn.isActive());
    try std.testing.expect(txn.getId() > 0);

    try txn.commit();
    try std.testing.expectEqual(TransactionState.committed, txn.getState());
}

test "rollback transaction" {
    var txn = DefaultTransactionManager.init();

    try txn.begin();
    try txn.recordOperation(.insert, "key1", null, "value1");
    try std.testing.expectEqual(@as(usize, 1), txn.getOperationCount());

    try txn.rollback();
    try std.testing.expectEqual(TransactionState.rolled_back, txn.getState());
}

test "savepoints" {
    var txn = DefaultTransactionManager.init();

    try txn.begin();
    try txn.recordOperation(.insert, "key1", null, "value1");

    try txn.savepoint("sp1");
    try txn.recordOperation(.insert, "key2", null, "value2");
    try std.testing.expectEqual(@as(usize, 2), txn.getOperationCount());

    try txn.rollbackToSavepoint("sp1");
    try std.testing.expectEqual(@as(usize, 1), txn.getOperationCount());

    try txn.commit();
}

test "transaction error states" {
    var txn = DefaultTransactionManager.init();

    // Cannot commit without active transaction
    try std.testing.expectError(error.NotInTransaction, txn.commit());

    // Cannot begin twice
    try txn.begin();
    try std.testing.expectError(error.AlreadyInTransaction, txn.begin());

    try txn.commit();
}

test "isolation levels" {
    var txn = DefaultTransactionManager.init();

    try txn.beginWithIsolation(.serializable);
    try std.testing.expectEqual(IsolationLevel.serializable, txn.getIsolationLevel());
    try std.testing.expectEqualStrings("SERIALIZABLE", txn.getIsolationLevel().toString());

    try txn.commit();
}

test "transaction guard auto-rollback" {
    var txn = DefaultTransactionManager.init();

    {
        var guard = try TransactionGuard(DefaultTransactionManager).init(&txn);
        defer guard.deinit();
        // No commit - should auto-rollback
    }

    try std.testing.expectEqual(TransactionState.rolled_back, txn.getState());
}
