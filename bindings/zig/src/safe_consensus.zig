// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

//! Safe distributed consensus primitives for quorum calculations.
//!
//! Provides algorithms for computing quorum thresholds in distributed systems,
//! including simple majority, Byzantine fault tolerance, and Raft consensus.
//! All operations are checked for overflow and invalid inputs.

const std = @import("std");

/// Error types for consensus operations.
pub const ConsensusError = error{
    /// Cluster size must be at least 1
    InvalidClusterSize,
    /// Byzantine tolerance exceeds maximum possible for cluster size
    ExcessiveFaultTolerance,
    /// Arithmetic overflow in calculation
    Overflow,
    /// Vote count exceeds cluster size
    InvalidVoteCount,
    /// Required votes not achievable
    UnachievableQuorum,
};

/// Result of a quorum check.
pub const QuorumResult = struct {
    /// Whether quorum was achieved
    achieved: bool,
    /// Votes required for quorum
    required_votes: u64,
    /// Current vote count
    current_votes: u64,
    /// Additional votes needed (0 if achieved)
    votes_needed: u64,
};

/// Byzantine fault tolerance configuration.
pub const BftConfig = struct {
    /// Maximum faulty nodes tolerated
    faulty_nodes: u64,
    /// Total cluster size
    cluster_size: u64,

    /// Check if configuration is valid (3f + 1 <= n)
    pub fn isValid(self: BftConfig) bool {
        const required = std.math.mul(u64, self.faulty_nodes, 3) catch return false;
        const min_nodes = std.math.add(u64, required, 1) catch return false;
        return self.cluster_size >= min_nodes;
    }
};

/// Calculate simple majority quorum (n/2 + 1).
///
/// Returns the minimum number of nodes required for a simple majority.
pub fn simpleMajority(cluster_size: u64) ConsensusError!u64 {
    if (cluster_size == 0) return error.InvalidClusterSize;

    const half = cluster_size / 2;
    return std.math.add(u64, half, 1) catch error.Overflow;
}

/// Calculate quorum for Raft consensus (majority of nodes).
///
/// Raft requires a majority to elect a leader and commit entries.
pub fn raftQuorum(cluster_size: u64) ConsensusError!u64 {
    return simpleMajority(cluster_size);
}

/// Calculate quorum for Byzantine fault tolerant consensus.
///
/// BFT systems require 2f + 1 nodes to agree, where f is the maximum
/// number of faulty nodes. The cluster must have at least 3f + 1 nodes.
pub fn byzantineQuorum(cluster_size: u64, max_faulty_nodes: u64) ConsensusError!u64 {
    if (cluster_size == 0) return error.InvalidClusterSize;

    // Validate BFT constraint: n >= 3f + 1
    const min_cluster = std.math.mul(u64, max_faulty_nodes, 3) catch return error.Overflow;
    const min_required = std.math.add(u64, min_cluster, 1) catch return error.Overflow;

    if (cluster_size < min_required) {
        return error.ExcessiveFaultTolerance;
    }

    // Quorum is 2f + 1
    const double_faulty = std.math.mul(u64, max_faulty_nodes, 2) catch return error.Overflow;
    return std.math.add(u64, double_faulty, 1) catch error.Overflow;
}

/// Calculate maximum Byzantine faults tolerable for a given cluster size.
///
/// For n nodes, we can tolerate f = (n - 1) / 3 Byzantine faults.
pub fn maxByzantineFaults(cluster_size: u64) ConsensusError!u64 {
    if (cluster_size == 0) return error.InvalidClusterSize;

    const adjusted = std.math.sub(u64, cluster_size, 1) catch return 0;
    return adjusted / 3;
}

/// Check if a vote count achieves simple majority quorum.
pub fn checkSimpleMajority(cluster_size: u64, vote_count: u64) ConsensusError!QuorumResult {
    if (cluster_size == 0) return error.InvalidClusterSize;
    if (vote_count > cluster_size) return error.InvalidVoteCount;

    const required_votes = try simpleMajority(cluster_size);
    const achieved = vote_count >= required_votes;
    const votes_needed = if (achieved) 0 else required_votes - vote_count;

    return QuorumResult{
        .achieved = achieved,
        .required_votes = required_votes,
        .current_votes = vote_count,
        .votes_needed = votes_needed,
    };
}

/// Check if a vote count achieves Byzantine quorum.
pub fn checkByzantineQuorum(cluster_size: u64, max_faulty_nodes: u64, vote_count: u64) ConsensusError!QuorumResult {
    if (cluster_size == 0) return error.InvalidClusterSize;
    if (vote_count > cluster_size) return error.InvalidVoteCount;

    const required_votes = try byzantineQuorum(cluster_size, max_faulty_nodes);
    const achieved = vote_count >= required_votes;
    const votes_needed = if (achieved) 0 else required_votes - vote_count;

    return QuorumResult{
        .achieved = achieved,
        .required_votes = required_votes,
        .current_votes = vote_count,
        .votes_needed = votes_needed,
    };
}

/// Calculate weighted quorum threshold.
///
/// In weighted voting, nodes have different vote weights. The quorum
/// is achieved when total weight exceeds half the total weight.
pub fn weightedQuorum(total_weight: u64) ConsensusError!u64 {
    if (total_weight == 0) return error.InvalidClusterSize;

    const half = total_weight / 2;
    return std.math.add(u64, half, 1) catch error.Overflow;
}

/// Check if weighted votes achieve quorum.
pub fn checkWeightedQuorum(total_weight: u64, weighted_votes: u64) ConsensusError!QuorumResult {
    if (total_weight == 0) return error.InvalidClusterSize;
    if (weighted_votes > total_weight) return error.InvalidVoteCount;

    const required_votes = try weightedQuorum(total_weight);
    const achieved = weighted_votes >= required_votes;
    const votes_needed = if (achieved) 0 else required_votes - weighted_votes;

    return QuorumResult{
        .achieved = achieved,
        .required_votes = required_votes,
        .current_votes = weighted_votes,
        .votes_needed = votes_needed,
    };
}

/// Calculate optimal cluster size for desired fault tolerance.
///
/// Returns the minimum cluster size needed to tolerate the specified
/// number of Byzantine faults: n = 3f + 1.
pub fn optimalClusterSize(desired_fault_tolerance: u64) ConsensusError!u64 {
    const triple = std.math.mul(u64, desired_fault_tolerance, 3) catch return error.Overflow;
    return std.math.add(u64, triple, 1) catch error.Overflow;
}

/// Calculate supermajority threshold (2/3 + 1).
///
/// Used in some consensus protocols requiring more than simple majority.
pub fn superMajority(cluster_size: u64) ConsensusError!u64 {
    if (cluster_size == 0) return error.InvalidClusterSize;

    // Calculate ceil(2n/3) using integer arithmetic: (2n + 2) / 3
    const doubled = std.math.mul(u64, cluster_size, 2) catch return error.Overflow;
    const adjusted = std.math.add(u64, doubled, 2) catch return error.Overflow;
    return adjusted / 3;
}

/// Check if quorum is still achievable given current state.
///
/// Returns true if enough responsive nodes remain to potentially achieve quorum.
pub fn isQuorumAchievable(cluster_size: u64, responsive_nodes: u64, required_quorum: u64) ConsensusError!bool {
    if (cluster_size == 0) return error.InvalidClusterSize;
    if (responsive_nodes > cluster_size) return error.InvalidVoteCount;
    if (required_quorum > cluster_size) return error.UnachievableQuorum;

    return responsive_nodes >= required_quorum;
}

/// Paxos prepare phase quorum (simple majority).
pub fn paxosPrepareQuorum(cluster_size: u64) ConsensusError!u64 {
    return simpleMajority(cluster_size);
}

/// Paxos accept phase quorum (simple majority).
pub fn paxosAcceptQuorum(cluster_size: u64) ConsensusError!u64 {
    return simpleMajority(cluster_size);
}

// ============================================================================
// Tests
// ============================================================================

test "simpleMajority" {
    try std.testing.expectEqual(@as(u64, 2), try simpleMajority(3));
    try std.testing.expectEqual(@as(u64, 3), try simpleMajority(5));
    try std.testing.expectEqual(@as(u64, 4), try simpleMajority(7));
    try std.testing.expectEqual(@as(u64, 1), try simpleMajority(1));
    try std.testing.expectError(error.InvalidClusterSize, simpleMajority(0));
}

test "raftQuorum" {
    try std.testing.expectEqual(@as(u64, 2), try raftQuorum(3));
    try std.testing.expectEqual(@as(u64, 3), try raftQuorum(5));
    try std.testing.expectEqual(@as(u64, 4), try raftQuorum(7));
}

test "byzantineQuorum" {
    // 4 nodes can tolerate 1 Byzantine fault, quorum = 2*1 + 1 = 3
    try std.testing.expectEqual(@as(u64, 3), try byzantineQuorum(4, 1));
    // 7 nodes can tolerate 2 Byzantine faults, quorum = 2*2 + 1 = 5
    try std.testing.expectEqual(@as(u64, 5), try byzantineQuorum(7, 2));
    // 3 nodes is minimum for 0 faults
    try std.testing.expectEqual(@as(u64, 1), try byzantineQuorum(3, 0));
    // 3 nodes cannot tolerate 1 fault (requires 4 nodes)
    try std.testing.expectError(error.ExcessiveFaultTolerance, byzantineQuorum(3, 1));
}

test "maxByzantineFaults" {
    try std.testing.expectEqual(@as(u64, 0), try maxByzantineFaults(1));
    try std.testing.expectEqual(@as(u64, 0), try maxByzantineFaults(3));
    try std.testing.expectEqual(@as(u64, 1), try maxByzantineFaults(4));
    try std.testing.expectEqual(@as(u64, 1), try maxByzantineFaults(6));
    try std.testing.expectEqual(@as(u64, 2), try maxByzantineFaults(7));
}

test "checkSimpleMajority" {
    const result = try checkSimpleMajority(5, 3);
    try std.testing.expect(result.achieved);
    try std.testing.expectEqual(@as(u64, 3), result.required_votes);
    try std.testing.expectEqual(@as(u64, 0), result.votes_needed);

    const result2 = try checkSimpleMajority(5, 2);
    try std.testing.expect(!result2.achieved);
    try std.testing.expectEqual(@as(u64, 1), result2.votes_needed);

    try std.testing.expectError(error.InvalidVoteCount, checkSimpleMajority(5, 6));
}

test "optimalClusterSize" {
    try std.testing.expectEqual(@as(u64, 1), try optimalClusterSize(0));
    try std.testing.expectEqual(@as(u64, 4), try optimalClusterSize(1));
    try std.testing.expectEqual(@as(u64, 7), try optimalClusterSize(2));
    try std.testing.expectEqual(@as(u64, 10), try optimalClusterSize(3));
}

test "superMajority" {
    try std.testing.expectEqual(@as(u64, 1), try superMajority(1));
    try std.testing.expectEqual(@as(u64, 2), try superMajority(3));
    try std.testing.expectEqual(@as(u64, 4), try superMajority(5));
    try std.testing.expectEqual(@as(u64, 5), try superMajority(7));
}

test "weightedQuorum" {
    try std.testing.expectEqual(@as(u64, 51), try weightedQuorum(100));
    try std.testing.expectEqual(@as(u64, 6), try weightedQuorum(10));
    try std.testing.expectEqual(@as(u64, 1), try weightedQuorum(1));
}

test "BftConfig isValid" {
    const valid_config = BftConfig{ .faulty_nodes = 1, .cluster_size = 4 };
    try std.testing.expect(valid_config.isValid());

    const invalid_config = BftConfig{ .faulty_nodes = 1, .cluster_size = 3 };
    try std.testing.expect(!invalid_config.isValid());

    const edge_config = BftConfig{ .faulty_nodes = 0, .cluster_size = 1 };
    try std.testing.expect(edge_config.isValid());
}
