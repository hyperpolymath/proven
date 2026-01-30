-- SPDX-License-Identifier: Apache-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell <jonathan.jewell@open.ac.uk>
||| FFI exports for SafeConsensus operations
|||
||| This module exports distributed consensus algorithm helpers to the C ABI
||| via Idris2's RefC backend. All functions are proven total.
|||
||| Return conventions:
||| - Role → Int (Follower=0, Candidate=1, Leader=2)
||| - Validation → Int (0 = invalid/false, 1 = valid/true)
||| - Count → Int (votes, nodes, log entries)
|||
||| CRITICAL: Consensus algorithms ensure agreement in distributed systems
|||           despite failures and network partitions.
|||
||| Raft Consensus:
||| - Leader election via majority vote
||| - Log replication with consistency
||| - Safety: at most one leader per term
||| - Roles: Follower (passive), Candidate (election), Leader (commands)
|||
||| Quorum:
||| - Majority: > n/2 (strictly greater than half)
||| - Prevents split-brain
||| - Required for: leader election, log commits
|||
||| Paxos:
||| - Multi-Paxos for replicated log
||| - Phase 1: Prepare → Promise
||| - Phase 2: Accept → Accepted
||| - Proposal numbers for ordering
|||
||| Logical Clocks:
||| - Lamport: Total ordering of events
||| - Vector: Partial ordering, detects concurrency
|||
||| Consensus Properties:
||| - Agreement: All nodes decide same value
||| - Validity: Decided value was proposed
||| - Termination: All correct nodes decide eventually
module Proven.FFI.SafeConsensus

import Proven.SafeConsensus
import Proven.Core
import Data.String

%default total

--------------------------------------------------------------------------------
-- FFI Result Encoding
--------------------------------------------------------------------------------

||| Encode Bool as Int
encodeBool : Bool -> Int
encodeBool False = 0
encodeBool True = 1

--------------------------------------------------------------------------------
-- Node Role Encoding
--------------------------------------------------------------------------------

export
proven_idris_consensus_role_follower : Int
proven_idris_consensus_role_follower = 0

export
proven_idris_consensus_role_candidate : Int
proven_idris_consensus_role_candidate = 1

export
proven_idris_consensus_role_leader : Int
proven_idris_consensus_role_leader = 2

export
proven_idris_consensus_is_valid_role : Int -> Int
proven_idris_consensus_is_valid_role role =
  encodeBool (role >= 0 && role <= 2)

export
proven_idris_consensus_is_leader : Int -> Int
proven_idris_consensus_is_leader role =
  encodeBool (role == 2)

export
proven_idris_consensus_is_follower : Int -> Int
proven_idris_consensus_is_follower role =
  encodeBool (role == 0)

export
proven_idris_consensus_is_candidate : Int -> Int
proven_idris_consensus_is_candidate role =
  encodeBool (role == 1)

export
proven_idris_consensus_can_vote : Int -> Int
proven_idris_consensus_can_vote role =
  encodeBool (role == 0 || role == 1)  -- Follower or Candidate

export
proven_idris_consensus_can_append : Int -> Int
proven_idris_consensus_can_append role =
  encodeBool (role == 2)  -- Leader only

--------------------------------------------------------------------------------
-- Quorum Calculations
--------------------------------------------------------------------------------

export
proven_idris_consensus_quorum_size : Int -> Int
proven_idris_consensus_quorum_size totalNodes =
  (totalNodes `div` 2) + 1

export
proven_idris_consensus_has_quorum : Int -> Int -> Int
proven_idris_consensus_has_quorum totalNodes votes =
  let needed = proven_idris_consensus_quorum_size totalNodes
  in encodeBool (votes >= needed)

export
proven_idris_consensus_votes_needed : Int -> Int -> Int
proven_idris_consensus_votes_needed totalNodes currentVotes =
  let needed = proven_idris_consensus_quorum_size totalNodes
  in if currentVotes >= needed then 0
     else needed - currentVotes

export
proven_idris_consensus_can_win_election : Int -> Int -> Int -> Int
proven_idris_consensus_can_win_election totalNodes currentVotes remainingVotes =
  let needed = proven_idris_consensus_quorum_size totalNodes
      maxPossible = currentVotes + remainingVotes
  in encodeBool (maxPossible >= needed)

export
proven_idris_consensus_majority_percent : Int -> Int -> Double
proven_idris_consensus_majority_percent votes totalNodes =
  if totalNodes == 0 then 0.0
  else cast votes / cast totalNodes * 100.0

--------------------------------------------------------------------------------
-- Term Operations
--------------------------------------------------------------------------------

export
proven_idris_consensus_term_compare : Int -> Int -> Int
proven_idris_consensus_term_compare term1 term2 =
  if term1 < term2 then (-1)
  else if term1 == term2 then 0
  else 1

export
proven_idris_consensus_term_newer : Int -> Int -> Int
proven_idris_consensus_term_newer term1 term2 =
  encodeBool (term1 > term2)

export
proven_idris_consensus_term_same : Int -> Int -> Int
proven_idris_consensus_term_same term1 term2 =
  encodeBool (term1 == term2)

export
proven_idris_consensus_term_increment : Int -> Int
proven_idris_consensus_term_increment currentTerm = currentTerm + 1

export
proven_idris_consensus_term_is_valid : Int -> Int
proven_idris_consensus_term_is_valid term =
  encodeBool (term >= 0)

--------------------------------------------------------------------------------
-- Log Operations
--------------------------------------------------------------------------------

export
proven_idris_consensus_log_index : Int -> Int
proven_idris_consensus_log_index index = index

export
proven_idris_consensus_log_length : Int -> Int
proven_idris_consensus_log_length length = length

export
proven_idris_consensus_log_is_empty : Int -> Int
proven_idris_consensus_log_is_empty length =
  encodeBool (length == 0)

export
proven_idris_consensus_log_up_to_date : Int -> Int -> Int -> Int -> Int
proven_idris_consensus_log_up_to_date candidateIdx candidateTerm myIdx myTerm =
  -- Candidate log is up-to-date if:
  -- - Candidate term > my term, OR
  -- - Same term AND candidate index >= my index
  encodeBool (candidateTerm > myTerm ||
              (candidateTerm == myTerm && candidateIdx >= myIdx))

export
proven_idris_consensus_log_matches : Int -> Int -> Int -> Int -> Int
proven_idris_consensus_log_matches prevIdx prevTerm logIdx logTerm =
  -- Log matches if entry at prevIdx has term prevTerm
  encodeBool (prevIdx == logIdx && prevTerm == logTerm)

export
proven_idris_consensus_commit_index : Int -> Int
proven_idris_consensus_commit_index index = index

export
proven_idris_consensus_can_commit : Int -> Int -> Int -> Int
proven_idris_consensus_can_commit matchedReplicas quorumSize logIndex =
  -- Can commit if replicated on quorum
  encodeBool (matchedReplicas >= quorumSize)

--------------------------------------------------------------------------------
-- Vote Granting
--------------------------------------------------------------------------------

export
proven_idris_consensus_can_grant_vote : Int -> Int -> Int -> Int
proven_idris_consensus_can_grant_vote termOk logOk notVoted =
  encodeBool (termOk == 1 && logOk == 1 && notVoted == 1)

export
proven_idris_consensus_has_voted : Int -> Int
proven_idris_consensus_has_voted votedFor =
  encodeBool (votedFor >= 0)  -- -1 = not voted, >= 0 = voted for node ID

export
proven_idris_consensus_voted_for_self : Int -> Int -> Int
proven_idris_consensus_voted_for_self nodeId votedFor =
  encodeBool (nodeId == votedFor)

export
proven_idris_consensus_vote_matches : Int -> Int -> Int
proven_idris_consensus_vote_matches votedFor candidateId =
  encodeBool (votedFor == candidateId)

--------------------------------------------------------------------------------
-- Paxos Operations
--------------------------------------------------------------------------------

export
proven_idris_paxos_proposal_number : Int -> Int
proven_idris_paxos_proposal_number number = number

export
proven_idris_paxos_can_promise : Int -> Int -> Int
proven_idris_paxos_can_promise proposalNumber promisedNumber =
  encodeBool (proposalNumber > promisedNumber)

export
proven_idris_paxos_can_accept : Int -> Int -> Int
proven_idris_paxos_can_accept proposalNumber promisedNumber =
  encodeBool (proposalNumber >= promisedNumber)

export
proven_idris_paxos_has_accepted : Int -> Int
proven_idris_paxos_has_accepted hasValue = hasValue

export
proven_idris_paxos_proposal_newer : Int -> Int -> Int
proven_idris_paxos_proposal_newer proposal1 proposal2 =
  encodeBool (proposal1 > proposal2)

export
proven_idris_paxos_max_proposal : Int -> Int -> Int
proven_idris_paxos_max_proposal proposal1 proposal2 =
  if proposal1 > proposal2 then proposal1 else proposal2

--------------------------------------------------------------------------------
-- Lamport Clock
--------------------------------------------------------------------------------

export
proven_idris_lamport_tick : Int -> Int
proven_idris_lamport_tick clock = clock + 1

export
proven_idris_lamport_receive : Int -> Int -> Int
proven_idris_lamport_receive localClock receivedClock =
  let maxClock = if localClock > receivedClock
                   then localClock
                   else receivedClock
  in maxClock + 1

export
proven_idris_lamport_happened_before : Int -> Int -> Int
proven_idris_lamport_happened_before clock1 clock2 =
  encodeBool (clock1 < clock2)

export
proven_idris_lamport_concurrent : Int -> Int -> Int
proven_idris_lamport_concurrent clock1 clock2 =
  encodeBool (clock1 == clock2)

export
proven_idris_lamport_compare : Int -> Int -> Int
proven_idris_lamport_compare clock1 clock2 =
  if clock1 < clock2 then (-1)
  else if clock1 == clock2 then 0
  else 1

--------------------------------------------------------------------------------
-- Vector Clock Operations
--------------------------------------------------------------------------------

export
proven_idris_vector_tick : Int -> Int
proven_idris_vector_tick value = value + 1

export
proven_idris_vector_merge : Int -> Int -> Int
proven_idris_vector_merge value1 value2 =
  if value1 > value2 then value1 else value2

export
proven_idris_vector_component_less : Int -> Int -> Int
proven_idris_vector_component_less value1 value2 =
  encodeBool (value1 < value2)

export
proven_idris_vector_component_le : Int -> Int -> Int
proven_idris_vector_component_le value1 value2 =
  encodeBool (value1 <= value2)

--------------------------------------------------------------------------------
-- Consensus Safety Properties
--------------------------------------------------------------------------------

export
proven_idris_consensus_election_safety : Int -> Int
proven_idris_consensus_election_safety leadersInTerm =
  -- At most one leader per term
  encodeBool (leadersInTerm <= 1)

export
proven_idris_consensus_leader_completeness : Int -> Int -> Int
proven_idris_consensus_leader_completeness leaderHasEntry committed =
  -- If entry committed, leader has it
  encodeBool (committed == 0 || leaderHasEntry == 1)

export
proven_idris_consensus_log_matching : Int -> Int -> Int -> Int -> Int
proven_idris_consensus_log_matching idx1 term1 idx2 term2 =
  -- If logs match at index with same term, all preceding entries match
  encodeBool (idx1 == idx2 && term1 == term2)

export
proven_idris_consensus_state_machine_safety : Int -> Int
proven_idris_consensus_state_machine_safety sameCommittedEntries =
  -- All nodes apply same committed entries in same order
  encodeBool (sameCommittedEntries == 1)

--------------------------------------------------------------------------------
-- Cluster Operations
--------------------------------------------------------------------------------

export
proven_idris_consensus_cluster_size : Int -> Int
proven_idris_consensus_cluster_size size = size

export
proven_idris_consensus_is_valid_cluster_size : Int -> Int
proven_idris_consensus_is_valid_cluster_size size =
  encodeBool (size > 0 && size <= 1000 && size `mod` 2 == 1)  -- Odd number preferred

export
proven_idris_consensus_min_cluster_size : Int
proven_idris_consensus_min_cluster_size = 3

export
proven_idris_consensus_recommended_cluster_size : Int
proven_idris_consensus_recommended_cluster_size = 5

export
proven_idris_consensus_max_failures_tolerated : Int -> Int
proven_idris_consensus_max_failures_tolerated clusterSize =
  (clusterSize - 1) `div` 2

export
proven_idris_consensus_remaining_capacity : Int -> Int -> Int
proven_idris_consensus_remaining_capacity clusterSize failedNodes =
  let maxFailures = proven_idris_consensus_max_failures_tolerated clusterSize
  in if failedNodes >= maxFailures then 0
     else maxFailures - failedNodes

--------------------------------------------------------------------------------
-- Heartbeat and Timeouts
--------------------------------------------------------------------------------

export
proven_idris_consensus_heartbeat_interval : Int
proven_idris_consensus_heartbeat_interval = 100  -- 100ms

export
proven_idris_consensus_election_timeout_min : Int
proven_idris_consensus_election_timeout_min = 150  -- 150ms

export
proven_idris_consensus_election_timeout_max : Int
proven_idris_consensus_election_timeout_max = 300  -- 300ms

export
proven_idris_consensus_is_leader_alive : Int -> Int -> Int
proven_idris_consensus_is_leader_alive timeSinceHeartbeat electionTimeout =
  encodeBool (timeSinceHeartbeat < electionTimeout)

export
proven_idris_consensus_should_start_election : Int -> Int -> Int
proven_idris_consensus_should_start_election timeSinceHeartbeat electionTimeout =
  encodeBool (timeSinceHeartbeat >= electionTimeout)

--------------------------------------------------------------------------------
-- Statistics
--------------------------------------------------------------------------------

export
proven_idris_consensus_election_count : Int -> Int
proven_idris_consensus_election_count count = count

export
proven_idris_consensus_term_duration : Int -> Int -> Int
proven_idris_consensus_term_duration startTime endTime =
  if endTime >= startTime
    then endTime - startTime
    else 0

export
proven_idris_consensus_average_term_duration : Int -> Int -> Double
proven_idris_consensus_average_term_duration totalDuration termCount =
  if termCount == 0 then 0.0
  else cast totalDuration / cast termCount

export
proven_idris_consensus_replication_lag : Int -> Int -> Int
proven_idris_consensus_replication_lag leaderCommitIndex followerCommitIndex =
  if leaderCommitIndex >= followerCommitIndex
    then leaderCommitIndex - followerCommitIndex
    else 0

--------------------------------------------------------------------------------
-- Error Messages
--------------------------------------------------------------------------------

export
proven_idris_consensus_friendly_error : String -> String
proven_idris_consensus_friendly_error errorMsg =
  if isInfixOf "quorum" (toLower errorMsg) || isInfixOf "majority" (toLower errorMsg)
    then "Quorum not reached (insufficient votes or replicas)"
  else if isInfixOf "split brain" (toLower errorMsg)
    then "Split brain detected (multiple leaders in same term)"
  else if isInfixOf "log" (toLower errorMsg) && isInfixOf "mismatch" (toLower errorMsg)
    then "Log mismatch (inconsistent entries between nodes)"
  else if isInfixOf "stale" (toLower errorMsg) || isInfixOf "term" (toLower errorMsg)
    then "Stale term detected (older than current term)"
  else if isInfixOf "election" (toLower errorMsg)
    then "Election failed (no leader elected)"
  else
    "Consensus error"

export
proven_idris_consensus_role_description : Int -> String
proven_idris_consensus_role_description role =
  if role == 0 then "Follower (replicates leader's log)"
  else if role == 1 then "Candidate (requesting votes for leadership)"
  else if role == 2 then "Leader (accepts commands, replicates to followers)"
  else "Unknown role"
