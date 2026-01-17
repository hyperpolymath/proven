// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

/**
 * SafeConsensus - Distributed consensus primitives for quorum calculations.
 *
 * Provides safe algorithms for computing quorum thresholds:
 * - Simple majority (n/2 + 1)
 * - Byzantine fault tolerance (2f + 1 with 3f + 1 nodes)
 * - Raft consensus quorum
 * - Weighted quorum
 * - Supermajority (2/3 + 1)
 *
 * All operations are checked for overflow and invalid inputs.
 */

// ============================================================================
// Types
// ============================================================================

/** Error types for consensus operations */
type consensusError =
  | InvalidClusterSize
  | ExcessiveFaultTolerance
  | Overflow
  | InvalidVoteCount
  | UnachievableQuorum

/** Result of a quorum check */
type quorumResult = {
  achieved: bool,
  requiredVotes: int,
  currentVotes: int,
  votesNeeded: int,
}

/** Byzantine fault tolerance configuration */
type bftConfig = {
  faultyNodes: int,
  clusterSize: int,
}

// ============================================================================
// Error Handling
// ============================================================================

/** Convert consensus error to string */
let errorToString = (err: consensusError): string => {
  switch err {
  | InvalidClusterSize => "Cluster size must be at least 1"
  | ExcessiveFaultTolerance => "Byzantine tolerance exceeds maximum possible for cluster size"
  | Overflow => "Arithmetic overflow in calculation"
  | InvalidVoteCount => "Vote count exceeds cluster size"
  | UnachievableQuorum => "Required votes not achievable"
  }
}

// ============================================================================
// Simple Majority
// ============================================================================

/** Calculate simple majority quorum (n/2 + 1) */
let simpleMajority = (clusterSize: int): result<int, consensusError> => {
  if clusterSize <= 0 {
    Error(InvalidClusterSize)
  } else {
    let half = clusterSize / 2
    let quorum = half + 1
    // Check for overflow
    if quorum < half {
      Error(Overflow)
    } else {
      Ok(quorum)
    }
  }
}

/** Check if a vote count achieves simple majority quorum */
let checkSimpleMajority = (clusterSize: int, voteCount: int): result<quorumResult, consensusError> => {
  if clusterSize <= 0 {
    Error(InvalidClusterSize)
  } else if voteCount > clusterSize {
    Error(InvalidVoteCount)
  } else if voteCount < 0 {
    Error(InvalidVoteCount)
  } else {
    switch simpleMajority(clusterSize) {
    | Error(err) => Error(err)
    | Ok(requiredVotes) => {
        let achieved = voteCount >= requiredVotes
        let votesNeeded = if achieved {
          0
        } else {
          requiredVotes - voteCount
        }
        Ok({
          achieved,
          requiredVotes,
          currentVotes: voteCount,
          votesNeeded,
        })
      }
    }
  }
}

// ============================================================================
// Raft Consensus
// ============================================================================

/** Calculate quorum for Raft consensus (majority of nodes) */
let raftQuorum = (clusterSize: int): result<int, consensusError> => {
  simpleMajority(clusterSize)
}

/** Check if a vote count achieves Raft quorum */
let checkRaftQuorum = (clusterSize: int, voteCount: int): result<quorumResult, consensusError> => {
  checkSimpleMajority(clusterSize, voteCount)
}

// ============================================================================
// Byzantine Fault Tolerance
// ============================================================================

/** Calculate maximum Byzantine faults tolerable for a given cluster size */
let maxByzantineFaults = (clusterSize: int): result<int, consensusError> => {
  if clusterSize <= 0 {
    Error(InvalidClusterSize)
  } else {
    // f = (n - 1) / 3
    Ok((clusterSize - 1) / 3)
  }
}

/** Calculate quorum for Byzantine fault tolerant consensus */
let byzantineQuorum = (clusterSize: int, maxFaultyNodes: int): result<int, consensusError> => {
  if clusterSize <= 0 {
    Error(InvalidClusterSize)
  } else if maxFaultyNodes < 0 {
    Error(InvalidVoteCount)
  } else {
    // Validate BFT constraint: n >= 3f + 1
    let minRequired = maxFaultyNodes * 3 + 1
    if clusterSize < minRequired {
      Error(ExcessiveFaultTolerance)
    } else {
      // Quorum is 2f + 1
      let quorum = maxFaultyNodes * 2 + 1
      Ok(quorum)
    }
  }
}

/** Check if a vote count achieves Byzantine quorum */
let checkByzantineQuorum = (
  clusterSize: int,
  maxFaultyNodes: int,
  voteCount: int,
): result<quorumResult, consensusError> => {
  if clusterSize <= 0 {
    Error(InvalidClusterSize)
  } else if voteCount > clusterSize {
    Error(InvalidVoteCount)
  } else if voteCount < 0 {
    Error(InvalidVoteCount)
  } else {
    switch byzantineQuorum(clusterSize, maxFaultyNodes) {
    | Error(err) => Error(err)
    | Ok(requiredVotes) => {
        let achieved = voteCount >= requiredVotes
        let votesNeeded = if achieved {
          0
        } else {
          requiredVotes - voteCount
        }
        Ok({
          achieved,
          requiredVotes,
          currentVotes: voteCount,
          votesNeeded,
        })
      }
    }
  }
}

/** Check if BFT configuration is valid (3f + 1 <= n) */
let isValidBftConfig = (config: bftConfig): bool => {
  if config.clusterSize <= 0 || config.faultyNodes < 0 {
    false
  } else {
    let minNodes = config.faultyNodes * 3 + 1
    config.clusterSize >= minNodes
  }
}

// ============================================================================
// Weighted Quorum
// ============================================================================

/** Calculate weighted quorum threshold */
let weightedQuorum = (totalWeight: int): result<int, consensusError> => {
  if totalWeight <= 0 {
    Error(InvalidClusterSize)
  } else {
    let half = totalWeight / 2
    let quorum = half + 1
    if quorum < half {
      Error(Overflow)
    } else {
      Ok(quorum)
    }
  }
}

/** Check if weighted votes achieve quorum */
let checkWeightedQuorum = (totalWeight: int, weightedVotes: int): result<quorumResult, consensusError> => {
  if totalWeight <= 0 {
    Error(InvalidClusterSize)
  } else if weightedVotes > totalWeight {
    Error(InvalidVoteCount)
  } else if weightedVotes < 0 {
    Error(InvalidVoteCount)
  } else {
    switch weightedQuorum(totalWeight) {
    | Error(err) => Error(err)
    | Ok(requiredVotes) => {
        let achieved = weightedVotes >= requiredVotes
        let votesNeeded = if achieved {
          0
        } else {
          requiredVotes - weightedVotes
        }
        Ok({
          achieved,
          requiredVotes,
          currentVotes: weightedVotes,
          votesNeeded,
        })
      }
    }
  }
}

// ============================================================================
// Supermajority
// ============================================================================

/** Calculate supermajority threshold (2/3 + 1) */
let superMajority = (clusterSize: int): result<int, consensusError> => {
  if clusterSize <= 0 {
    Error(InvalidClusterSize)
  } else {
    let doubled = clusterSize * 2
    // Check for overflow
    if doubled < clusterSize {
      Error(Overflow)
    } else {
      let twoThirds = doubled / 3
      let quorum = twoThirds + 1
      if quorum < twoThirds {
        Error(Overflow)
      } else {
        Ok(quorum)
      }
    }
  }
}

/** Check if a vote count achieves supermajority */
let checkSuperMajority = (clusterSize: int, voteCount: int): result<quorumResult, consensusError> => {
  if clusterSize <= 0 {
    Error(InvalidClusterSize)
  } else if voteCount > clusterSize {
    Error(InvalidVoteCount)
  } else if voteCount < 0 {
    Error(InvalidVoteCount)
  } else {
    switch superMajority(clusterSize) {
    | Error(err) => Error(err)
    | Ok(requiredVotes) => {
        let achieved = voteCount >= requiredVotes
        let votesNeeded = if achieved {
          0
        } else {
          requiredVotes - voteCount
        }
        Ok({
          achieved,
          requiredVotes,
          currentVotes: voteCount,
          votesNeeded,
        })
      }
    }
  }
}

// ============================================================================
// Optimal Cluster Size
// ============================================================================

/** Calculate optimal cluster size for desired fault tolerance (n = 3f + 1) */
let optimalClusterSize = (desiredFaultTolerance: int): result<int, consensusError> => {
  if desiredFaultTolerance < 0 {
    Error(InvalidVoteCount)
  } else {
    let triple = desiredFaultTolerance * 3
    // Check for overflow
    if triple / 3 != desiredFaultTolerance {
      Error(Overflow)
    } else {
      let size = triple + 1
      if size < triple {
        Error(Overflow)
      } else {
        Ok(size)
      }
    }
  }
}

// ============================================================================
// Quorum Achievability
// ============================================================================

/** Check if quorum is still achievable given current state */
let isQuorumAchievable = (
  clusterSize: int,
  responsiveNodes: int,
  requiredQuorum: int,
): result<bool, consensusError> => {
  if clusterSize <= 0 {
    Error(InvalidClusterSize)
  } else if responsiveNodes > clusterSize {
    Error(InvalidVoteCount)
  } else if responsiveNodes < 0 {
    Error(InvalidVoteCount)
  } else if requiredQuorum > clusterSize {
    Error(UnachievableQuorum)
  } else {
    Ok(responsiveNodes >= requiredQuorum)
  }
}

// ============================================================================
// Paxos Quorum
// ============================================================================

/** Paxos prepare phase quorum (simple majority) */
let paxosPrepareQuorum = (clusterSize: int): result<int, consensusError> => {
  simpleMajority(clusterSize)
}

/** Paxos accept phase quorum (simple majority) */
let paxosAcceptQuorum = (clusterSize: int): result<int, consensusError> => {
  simpleMajority(clusterSize)
}

// ============================================================================
// Utility Functions
// ============================================================================

/** Create a quorum result for display */
let makeQuorumResult = (
  ~achieved: bool,
  ~requiredVotes: int,
  ~currentVotes: int,
): quorumResult => {
  {
    achieved,
    requiredVotes,
    currentVotes,
    votesNeeded: if achieved {
      0
    } else {
      requiredVotes - currentVotes
    },
  }
}

/** Get progress as a percentage (0.0 to 1.0) */
let quorumProgress = (result: quorumResult): float => {
  if result.requiredVotes == 0 {
    1.0
  } else {
    let progress = Belt.Int.toFloat(result.currentVotes) /. Belt.Int.toFloat(result.requiredVotes)
    Js.Math.min_float(1.0, progress)
  }
}

/** Get progress as integer percentage (0 to 100) */
let quorumProgressPercent = (result: quorumResult): int => {
  Belt.Float.toInt(quorumProgress(result) *. 100.0)
}

/** Format quorum result as string */
let quorumResultToString = (result: quorumResult): string => {
  let status = if result.achieved {
    "achieved"
  } else {
    "not achieved"
  }
  `Quorum ${status}: ${Belt.Int.toString(result.currentVotes)}/${Belt.Int.toString(
      result.requiredVotes,
    )} votes (${Belt.Int.toString(result.votesNeeded)} needed)`
}
