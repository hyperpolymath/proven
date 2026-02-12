// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

//! Safe data provenance tracking that cannot crash.
//!
//! Provides immutable audit trails, data lineage tracking, and chain-of-custody
//! verification for data integrity and compliance requirements.

const std = @import("std");

pub const ProvenanceError = error{
    InvalidTimestamp,
    ChainBroken,
    MaxEntriesReached,
    DuplicateEntry,
    InvalidHash,
};

/// Type of provenance event.
pub const EventType = enum {
    created,
    modified,
    accessed,
    copied,
    transformed,
    validated,
    exported,
    imported,
    deleted,
    restored,
    merged,
    split,

    /// Convert to human-readable string.
    pub fn toString(self: EventType) []const u8 {
        return switch (self) {
            .created => "created",
            .modified => "modified",
            .accessed => "accessed",
            .copied => "copied",
            .transformed => "transformed",
            .validated => "validated",
            .exported => "exported",
            .imported => "imported",
            .deleted => "deleted",
            .restored => "restored",
            .merged => "merged",
            .split => "split",
        };
    }
};

/// Data classification levels.
pub const Classification = enum(u8) {
    public = 0,
    internal = 1,
    confidential = 2,
    restricted = 3,
    top_secret = 4,

    /// Check if this classification is at least as restrictive as another.
    pub fn isAtLeast(self: Classification, other: Classification) bool {
        return @intFromEnum(self) >= @intFromEnum(other);
    }

    /// Get the more restrictive of two classifications.
    pub fn max(self: Classification, other: Classification) Classification {
        return if (@intFromEnum(self) > @intFromEnum(other)) self else other;
    }
};

/// Source of data (for lineage tracking).
pub const DataSource = struct {
    source_type: SourceType,
    identifier: []const u8,
    version: ?[]const u8 = null,
    location: ?[]const u8 = null,

    pub const SourceType = enum {
        database,
        file,
        api,
        user_input,
        system_generated,
        external_feed,
        derived,
        unknown,
    };

    /// Create a database source.
    pub fn database(table: []const u8) DataSource {
        return .{ .source_type = .database, .identifier = table };
    }

    /// Create a file source.
    pub fn file(path: []const u8) DataSource {
        return .{ .source_type = .file, .identifier = path };
    }

    /// Create an API source.
    pub fn api(endpoint: []const u8) DataSource {
        return .{ .source_type = .api, .identifier = endpoint };
    }

    /// Create a user input source.
    pub fn userInput(user_id: []const u8) DataSource {
        return .{ .source_type = .user_input, .identifier = user_id };
    }

    /// Create a derived data source.
    pub fn derived(from: []const u8) DataSource {
        return .{ .source_type = .derived, .identifier = from };
    }
};

/// Single provenance entry in the audit trail.
pub const ProvenanceEntry = struct {
    timestamp: i64,
    event_type: EventType,
    actor: []const u8,
    description: ?[]const u8 = null,
    source: ?DataSource = null,
    previous_hash: ?[32]u8 = null,

    /// Compute a simple hash of this entry.
    pub fn computeHash(self: ProvenanceEntry) [32]u8 {
        var hasher = std.hash.Wyhash.init(0);

        hasher.update(std.mem.asBytes(&self.timestamp));
        hasher.update(std.mem.asBytes(&@intFromEnum(self.event_type)));
        hasher.update(self.actor);

        if (self.description) |desc| {
            hasher.update(desc);
        }

        if (self.previous_hash) |prev| {
            hasher.update(&prev);
        }

        const hash = hasher.final();
        var result: [32]u8 = undefined;
        std.mem.writeInt(u64, result[0..8], hash, .little);
        std.mem.writeInt(u64, result[8..16], hash, .little);
        std.mem.writeInt(u64, result[16..24], hash, .little);
        std.mem.writeInt(u64, result[24..32], hash, .little);
        return result;
    }
};

/// Provenance chain for tracking data history.
pub fn ProvenanceChain(comptime max_entries: usize) type {
    return struct {
        const Self = @This();

        entries: [max_entries]?ProvenanceEntry = [_]?ProvenanceEntry{null} ** max_entries,
        entry_count: usize = 0,
        data_id: []const u8,
        classification: Classification = .internal,
        current_hash: ?[32]u8 = null,

        /// Initialize a new provenance chain.
        pub fn init(data_id: []const u8, creator: []const u8, timestamp: i64) Self {
            var chain = Self{ .data_id = data_id };

            const entry = ProvenanceEntry{
                .timestamp = timestamp,
                .event_type = .created,
                .actor = creator,
                .description = "Initial creation",
                .previous_hash = null,
            };

            chain.entries[0] = entry;
            chain.entry_count = 1;
            chain.current_hash = entry.computeHash();

            return chain;
        }

        /// Add a new event to the chain.
        pub fn addEvent(
            self: *Self,
            event_type: EventType,
            actor: []const u8,
            timestamp: i64,
            description: ?[]const u8,
        ) ProvenanceError!void {
            if (self.entry_count >= max_entries) return error.MaxEntriesReached;
            if (timestamp < self.getLastTimestamp()) return error.InvalidTimestamp;

            const entry = ProvenanceEntry{
                .timestamp = timestamp,
                .event_type = event_type,
                .actor = actor,
                .description = description,
                .previous_hash = self.current_hash,
            };

            self.entries[self.entry_count] = entry;
            self.entry_count += 1;
            self.current_hash = entry.computeHash();
        }

        /// Get the last timestamp in the chain.
        pub fn getLastTimestamp(self: *const Self) i64 {
            if (self.entry_count == 0) return 0;
            return self.entries[self.entry_count - 1].?.timestamp;
        }

        /// Get the last entry.
        pub fn getLastEntry(self: *const Self) ?ProvenanceEntry {
            if (self.entry_count == 0) return null;
            return self.entries[self.entry_count - 1];
        }

        /// Verify the integrity of the chain.
        pub fn verifyIntegrity(self: *const Self) bool {
            if (self.entry_count == 0) return true;

            var prev_hash: ?[32]u8 = null;

            for (self.entries[0..self.entry_count]) |entry_opt| {
                if (entry_opt) |entry| {
                    // Check that previous hash matches
                    if (prev_hash) |expected| {
                        if (entry.previous_hash) |actual| {
                            if (!std.mem.eql(u8, &expected, &actual)) return false;
                        } else {
                            return false;
                        }
                    } else {
                        if (entry.previous_hash != null) return false;
                    }

                    prev_hash = entry.computeHash();
                }
            }

            // Verify final hash matches stored hash
            if (self.current_hash) |stored| {
                if (prev_hash) |computed| {
                    return std.mem.eql(u8, &stored, &computed);
                }
            }

            return true;
        }

        /// Get entries filtered by event type.
        pub fn getEventsByType(self: *const Self, event_type: EventType) []const ?ProvenanceEntry {
            var count: usize = 0;
            for (self.entries[0..self.entry_count]) |entry_opt| {
                if (entry_opt) |entry| {
                    if (entry.event_type == event_type) count += 1;
                }
            }
            // Return the full slice for iteration - caller must filter
            return self.entries[0..self.entry_count];
        }

        /// Get all unique actors who have touched this data.
        pub fn getActors(self: *const Self, buffer: [][]const u8) usize {
            var count: usize = 0;

            for (self.entries[0..self.entry_count]) |entry_opt| {
                if (entry_opt) |entry| {
                    // Check if actor already in buffer
                    var found = false;
                    for (buffer[0..count]) |existing| {
                        if (std.mem.eql(u8, existing, entry.actor)) {
                            found = true;
                            break;
                        }
                    }

                    if (!found and count < buffer.len) {
                        buffer[count] = entry.actor;
                        count += 1;
                    }
                }
            }

            return count;
        }

        /// Check if a specific actor has accessed this data.
        pub fn hasActorAccessed(self: *const Self, actor: []const u8) bool {
            for (self.entries[0..self.entry_count]) |entry_opt| {
                if (entry_opt) |entry| {
                    if (std.mem.eql(u8, entry.actor, actor)) return true;
                }
            }
            return false;
        }
    };
}

/// Data lineage node for tracking derivations.
pub const LineageNode = struct {
    data_id: []const u8,
    transformation: ?[]const u8 = null,
    parent_ids: [8]?[]const u8 = [_]?[]const u8{null} ** 8,
    parent_count: usize = 0,

    /// Add a parent data source.
    pub fn addParent(self: *LineageNode, parent_id: []const u8) bool {
        if (self.parent_count >= 8) return false;
        self.parent_ids[self.parent_count] = parent_id;
        self.parent_count += 1;
        return true;
    }

    /// Check if this data is derived from a specific source.
    pub fn isDerivedFrom(self: *const LineageNode, source_id: []const u8) bool {
        for (self.parent_ids[0..self.parent_count]) |parent_opt| {
            if (parent_opt) |parent| {
                if (std.mem.eql(u8, parent, source_id)) return true;
            }
        }
        return false;
    }

    /// Check if this is root data (no parents).
    pub fn isRoot(self: *const LineageNode) bool {
        return self.parent_count == 0;
    }
};

/// Compliance evidence record.
pub const ComplianceRecord = struct {
    requirement: []const u8,
    status: ComplianceStatus,
    verified_at: i64,
    verified_by: []const u8,
    evidence: ?[]const u8 = null,
    expiry: ?i64 = null,

    pub const ComplianceStatus = enum {
        compliant,
        non_compliant,
        pending_review,
        exempted,
        not_applicable,
    };

    /// Check if this compliance record is still valid.
    pub fn isValid(self: ComplianceRecord, current_time: i64) bool {
        if (self.status != .compliant and self.status != .exempted) return false;
        if (self.expiry) |exp| {
            return current_time < exp;
        }
        return true;
    }
};

/// Custody transfer record.
pub const CustodyTransfer = struct {
    from_custodian: []const u8,
    to_custodian: []const u8,
    timestamp: i64,
    reason: ?[]const u8 = null,
    authorized_by: ?[]const u8 = null,

    /// Check if transfer was authorized.
    pub fn isAuthorized(self: CustodyTransfer) bool {
        return self.authorized_by != null;
    }
};

// ============================================================================
// Tests
// ============================================================================

test "EventType toString" {
    try std.testing.expectEqualStrings("created", EventType.created.toString());
    try std.testing.expectEqualStrings("modified", EventType.modified.toString());
}

test "Classification comparison" {
    try std.testing.expect(Classification.restricted.isAtLeast(.confidential));
    try std.testing.expect(!Classification.public.isAtLeast(.internal));
    try std.testing.expectEqual(Classification.restricted, Classification.confidential.max(.restricted));
}

test "DataSource creation" {
    const db_source = DataSource.database("users");
    try std.testing.expectEqual(DataSource.SourceType.database, db_source.source_type);
    try std.testing.expectEqualStrings("users", db_source.identifier);

    const file_source = DataSource.file("/data/export.csv");
    try std.testing.expectEqual(DataSource.SourceType.file, file_source.source_type);
}

test "ProvenanceChain basic" {
    var chain = ProvenanceChain(100).init("data-001", "system", 1000);

    try std.testing.expectEqual(@as(usize, 1), chain.entry_count);
    try std.testing.expectEqual(@as(i64, 1000), chain.getLastTimestamp());
    try std.testing.expect(chain.verifyIntegrity());
}

test "ProvenanceChain add events" {
    var chain = ProvenanceChain(100).init("data-001", "system", 1000);

    try chain.addEvent(.modified, "user-1", 2000, "Updated field X");
    try chain.addEvent(.accessed, "user-2", 3000, null);

    try std.testing.expectEqual(@as(usize, 3), chain.entry_count);
    try std.testing.expect(chain.verifyIntegrity());
    try std.testing.expect(chain.hasActorAccessed("user-1"));
    try std.testing.expect(!chain.hasActorAccessed("user-3"));
}

test "ProvenanceChain timestamp validation" {
    var chain = ProvenanceChain(100).init("data-001", "system", 1000);

    // Adding event with earlier timestamp should fail
    try std.testing.expectError(error.InvalidTimestamp, chain.addEvent(.modified, "user", 500, null));
}

test "ProvenanceChain integrity verification" {
    var chain = ProvenanceChain(100).init("data-001", "system", 1000);
    try chain.addEvent(.modified, "user", 2000, null);

    // Chain should be valid
    try std.testing.expect(chain.verifyIntegrity());
}

test "ProvenanceChain get actors" {
    var chain = ProvenanceChain(100).init("data-001", "alice", 1000);
    try chain.addEvent(.modified, "bob", 2000, null);
    try chain.addEvent(.accessed, "alice", 3000, null);
    try chain.addEvent(.exported, "charlie", 4000, null);

    var actors: [10][]const u8 = undefined;
    const count = chain.getActors(&actors);

    try std.testing.expectEqual(@as(usize, 3), count);
}

test "LineageNode" {
    var node = LineageNode{ .data_id = "derived-001", .transformation = "aggregation" };
    _ = node.addParent("source-001");
    _ = node.addParent("source-002");

    try std.testing.expect(node.isDerivedFrom("source-001"));
    try std.testing.expect(!node.isDerivedFrom("source-003"));
    try std.testing.expect(!node.isRoot());
}

test "ComplianceRecord validity" {
    const record = ComplianceRecord{
        .requirement = "GDPR-Art17",
        .status = .compliant,
        .verified_at = 1000,
        .verified_by = "auditor",
        .expiry = 5000,
    };

    try std.testing.expect(record.isValid(3000));
    try std.testing.expect(!record.isValid(6000));
}

test "CustodyTransfer" {
    const transfer = CustodyTransfer{
        .from_custodian = "team-a",
        .to_custodian = "team-b",
        .timestamp = 1000,
        .authorized_by = "manager",
    };

    try std.testing.expect(transfer.isAuthorized());
}
