-- SPDX-License-Identifier: Apache-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell <jonathan.jewell@open.ac.uk>
||| FFI exports for SafeTransaction operations
|||
||| This module exports transactional operation helpers to the C ABI
||| via Idris2's RefC backend. All functions are proven total.
|||
||| Return conventions:
||| - Status → Int (Pending=0, Committed=1, RolledBack=2, Failed=3)
||| - Isolation → Int (ReadUncommitted=0, ReadCommitted=1, RepeatableRead=2, Serializable=3)
||| - Count → Int (operations, log entries)
||| - Validation → Int (0 = invalid/false, 1 = valid/true)
|||
||| CRITICAL: ACID properties ensure data consistency:
|||           - Atomicity: All operations succeed or all fail
|||           - Consistency: Valid state before and after
|||           - Isolation: Concurrent transactions don't interfere
|||           - Durability: Committed changes persist
|||
||| Transaction Lifecycle:
||| - begin(): Create transaction (Pending state)
||| - addOp(): Add operations (SetState, ModifyState, Validate)
||| - commit(): Execute all operations atomically
|||   - Success → Committed state
|||   - Failure → Failed state (with reason)
||| - rollback(): Revert to initial state (RolledBack state)
|||
||| State Transitions:
||| - Pending → Committed: commit() succeeds
||| - Pending → Failed: commit() validation fails
||| - Pending → RolledBack: rollback() called
||| - Once Committed/Failed/RolledBack, transaction is final
|||
||| Savepoints:
||| - Named checkpoints within transaction
||| - Partial rollback to savepoint
||| - Nested transaction semantics
|||
||| Isolation Levels (SQL standard):
||| - ReadUncommitted (0): Dirty reads possible
||| - ReadCommitted (1): No dirty reads
||| - RepeatableRead (2): Consistent reads within transaction
||| - Serializable (3): Full isolation, as if sequential
|||
||| NOTE: Polymorphic transaction state managed in Zig.
|||       These helpers validate operations and track status.
module Proven.FFI.SafeTransaction

import Proven.SafeTransaction
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
-- Transaction Status Encoding
--------------------------------------------------------------------------------

export
proven_idris_tx_status_pending : Int
proven_idris_tx_status_pending = 0

export
proven_idris_tx_status_committed : Int
proven_idris_tx_status_committed = 1

export
proven_idris_tx_status_rolledback : Int
proven_idris_tx_status_rolledback = 2

export
proven_idris_tx_status_failed : Int
proven_idris_tx_status_failed = 3

export
proven_idris_tx_is_valid_status : Int -> Int
proven_idris_tx_is_valid_status status =
  encodeBool (status >= 0 && status <= 3)

export
proven_idris_tx_is_active : Int -> Int
proven_idris_tx_is_active status =
  encodeBool (status == 0)  -- Pending

export
proven_idris_tx_is_committed : Int -> Int
proven_idris_tx_is_committed status =
  encodeBool (status == 1)  -- Committed

export
proven_idris_tx_is_rolledback : Int -> Int
proven_idris_tx_is_rolledback status =
  encodeBool (status == 2)  -- RolledBack

export
proven_idris_tx_is_failed : Int -> Int
proven_idris_tx_is_failed status =
  encodeBool (status == 3)  -- Failed

export
proven_idris_tx_is_final : Int -> Int
proven_idris_tx_is_final status =
  encodeBool (status >= 1 && status <= 3)  -- Committed/RolledBack/Failed

export
proven_idris_tx_can_commit : Int -> Int
proven_idris_tx_can_commit status =
  encodeBool (status == 0)  -- Pending

export
proven_idris_tx_can_rollback : Int -> Int
proven_idris_tx_can_rollback status =
  encodeBool (status == 0)  -- Pending

--------------------------------------------------------------------------------
-- Isolation Level Encoding
--------------------------------------------------------------------------------

export
proven_idris_tx_isolation_read_uncommitted : Int
proven_idris_tx_isolation_read_uncommitted = 0

export
proven_idris_tx_isolation_read_committed : Int
proven_idris_tx_isolation_read_committed = 1

export
proven_idris_tx_isolation_repeatable_read : Int
proven_idris_tx_isolation_repeatable_read = 2

export
proven_idris_tx_isolation_serializable : Int
proven_idris_tx_isolation_serializable = 3

export
proven_idris_tx_is_valid_isolation : Int -> Int
proven_idris_tx_is_valid_isolation isolation =
  encodeBool (isolation >= 0 && isolation <= 3)

export
proven_idris_tx_isolation_allows_dirty_reads : Int -> Int
proven_idris_tx_isolation_allows_dirty_reads isolation =
  encodeBool (isolation == 0)  -- ReadUncommitted only

export
proven_idris_tx_isolation_prevents_dirty_reads : Int -> Int
proven_idris_tx_isolation_prevents_dirty_reads isolation =
  encodeBool (isolation >= 1)  -- ReadCommitted and above

export
proven_idris_tx_isolation_prevents_nonrepeatable_reads : Int -> Int
proven_idris_tx_isolation_prevents_nonrepeatable_reads isolation =
  encodeBool (isolation >= 2)  -- RepeatableRead and above

export
proven_idris_tx_isolation_prevents_phantoms : Int -> Int
proven_idris_tx_isolation_prevents_phantoms isolation =
  encodeBool (isolation == 3)  -- Serializable only

--------------------------------------------------------------------------------
-- Transaction Operations
--------------------------------------------------------------------------------

export
proven_idris_tx_begin : Int -> Int
proven_idris_tx_begin txId = txId

export
proven_idris_tx_op_count : Int -> Int
proven_idris_tx_op_count count = count

export
proven_idris_tx_log_count : Int -> Int
proven_idris_tx_log_count count = count

export
proven_idris_tx_has_operations : Int -> Int
proven_idris_tx_has_operations opCount =
  encodeBool (opCount > 0)

export
proven_idris_tx_max_operations : Int
proven_idris_tx_max_operations = 10000

export
proven_idris_tx_can_add_operation : Int -> Int -> Int
proven_idris_tx_can_add_operation opCount maxOps =
  encodeBool (opCount < maxOps)

--------------------------------------------------------------------------------
-- Savepoint Operations
--------------------------------------------------------------------------------

export
proven_idris_tx_savepoint_count : Int -> Int
proven_idris_tx_savepoint_count count = count

export
proven_idris_tx_max_savepoints : Int
proven_idris_tx_max_savepoints = 100

export
proven_idris_tx_can_create_savepoint : Int -> Int -> Int
proven_idris_tx_can_create_savepoint spCount maxSavepoints =
  encodeBool (spCount < maxSavepoints)

export
proven_idris_tx_operations_since_savepoint : Int -> Int -> Int
proven_idris_tx_operations_since_savepoint currentOpCount savepointOpCount =
  if currentOpCount >= savepointOpCount
    then currentOpCount - savepointOpCount
    else 0

export
proven_idris_tx_can_rollback_to_savepoint : Int -> Int -> Int
proven_idris_tx_can_rollback_to_savepoint status savepointOpCount =
  encodeBool (status == 0 && savepointOpCount >= 0)  -- Pending and valid savepoint

--------------------------------------------------------------------------------
-- Transaction Statistics
--------------------------------------------------------------------------------

export
proven_idris_tx_success_rate : Int -> Int -> Double
proven_idris_tx_success_rate committed total =
  if total == 0 then 0.0
  else cast committed / cast total

export
proven_idris_tx_success_rate_percent : Int -> Int -> Double
proven_idris_tx_success_rate_percent committed total =
  proven_idris_tx_success_rate committed total * 100.0

export
proven_idris_tx_rollback_rate : Int -> Int -> Double
proven_idris_tx_rollback_rate rolledBack total =
  if total == 0 then 0.0
  else cast rolledBack / cast total

export
proven_idris_tx_rollback_rate_percent : Int -> Int -> Double
proven_idris_tx_rollback_rate_percent rolledBack total =
  proven_idris_tx_rollback_rate rolledBack total * 100.0

export
proven_idris_tx_failure_rate : Int -> Int -> Double
proven_idris_tx_failure_rate failed total =
  if total == 0 then 0.0
  else cast failed / cast total

export
proven_idris_tx_failure_rate_percent : Int -> Int -> Double
proven_idris_tx_failure_rate_percent failed total =
  proven_idris_tx_failure_rate failed total * 100.0

export
proven_idris_tx_average_operations : Int -> Int -> Double
proven_idris_tx_average_operations totalOps totalTxs =
  if totalTxs == 0 then 0.0
  else cast totalOps / cast totalTxs

--------------------------------------------------------------------------------
-- Conflict Detection
--------------------------------------------------------------------------------

export
proven_idris_tx_has_conflict : Int -> Int
proven_idris_tx_has_conflict hasConflict = hasConflict

export
proven_idris_tx_conflict_count : Int -> Int
proven_idris_tx_conflict_count count = count

export
proven_idris_tx_conflict_rate : Int -> Int -> Double
proven_idris_tx_conflict_rate conflicts total =
  if total == 0 then 0.0
  else cast conflicts / cast total

export
proven_idris_tx_should_retry : Int -> Int -> Int
proven_idris_tx_should_retry status hasConflict =
  encodeBool (status == 3 && hasConflict == 1)  -- Failed due to conflict

--------------------------------------------------------------------------------
-- Validation
--------------------------------------------------------------------------------

export
proven_idris_tx_is_valid_txid : Int -> Int
proven_idris_tx_is_valid_txid txId =
  encodeBool (txId >= 0)

export
proven_idris_tx_is_valid_timestamp : Int -> Int
proven_idris_tx_is_valid_timestamp timestamp =
  encodeBool (timestamp >= 0)

export
proven_idris_tx_is_valid_operation_count : Int -> Int
proven_idris_tx_is_valid_operation_count count =
  encodeBool (count >= 0 && count <= 10000)

--------------------------------------------------------------------------------
-- ACID Checks
--------------------------------------------------------------------------------

export
proven_idris_tx_atomicity_preserved : Int -> Int -> Int
proven_idris_tx_atomicity_preserved status allOpsExecuted =
  -- Atomicity: either all ops execute or none
  encodeBool ((status == 1 && allOpsExecuted == 1) || (status == 2) || (status == 3))

export
proven_idris_tx_consistency_preserved : Int -> Int
proven_idris_tx_consistency_preserved validationPassed =
  -- Consistency: state valid before and after
  encodeBool (validationPassed == 1)

export
proven_idris_tx_isolation_preserved : Int -> Int
proven_idris_tx_isolation_preserved noInterference =
  -- Isolation: no interference from concurrent txs
  encodeBool (noInterference == 1)

export
proven_idris_tx_durability_preserved : Int -> Int
proven_idris_tx_durability_preserved persisted =
  -- Durability: committed changes persist
  encodeBool (persisted == 1)

--------------------------------------------------------------------------------
-- Concurrency Control
--------------------------------------------------------------------------------

export
proven_idris_tx_lock_acquired : Int -> Int
proven_idris_tx_lock_acquired acquired = acquired

export
proven_idris_tx_lock_released : Int -> Int
proven_idris_tx_lock_released released = released

export
proven_idris_tx_deadlock_detected : Int -> Int
proven_idris_tx_deadlock_detected hasDeadlock = hasDeadlock

export
proven_idris_tx_wait_time : Int -> Int
proven_idris_tx_wait_time milliseconds = milliseconds

--------------------------------------------------------------------------------
-- Performance Metrics
--------------------------------------------------------------------------------

export
proven_idris_tx_duration : Int -> Int -> Int
proven_idris_tx_duration startTime endTime =
  if endTime >= startTime
    then endTime - startTime
    else 0

export
proven_idris_tx_average_duration : Int -> Int -> Double
proven_idris_tx_average_duration totalDuration totalTxs =
  if totalTxs == 0 then 0.0
  else cast totalDuration / cast totalTxs

export
proven_idris_tx_throughput : Int -> Int -> Double
proven_idris_tx_throughput committed timeWindow =
  if timeWindow == 0 then 0.0
  else cast committed / cast timeWindow

--------------------------------------------------------------------------------
-- Constants
--------------------------------------------------------------------------------

export
proven_idris_tx_default_isolation : Int
proven_idris_tx_default_isolation = 1  -- ReadCommitted

export
proven_idris_tx_default_timeout : Int
proven_idris_tx_default_timeout = 30000  -- 30 seconds

export
proven_idris_tx_max_retry_attempts : Int
proven_idris_tx_max_retry_attempts = 3

export
proven_idris_tx_default_max_operations : Int
proven_idris_tx_default_max_operations = 1000

--------------------------------------------------------------------------------
-- Error Messages
--------------------------------------------------------------------------------

export
proven_idris_tx_friendly_error : String -> String
proven_idris_tx_friendly_error errorMsg =
  if isInfixOf "deadlock" (toLower errorMsg)
    then "Transaction deadlock detected (conflicting locks)"
  else if isInfixOf "conflict" (toLower errorMsg) || isInfixOf "serialization" (toLower errorMsg)
    then "Transaction conflict detected (concurrent modification)"
  else if isInfixOf "validation" (toLower errorMsg)
    then "Transaction validation failed (consistency check failed)"
  else if isInfixOf "timeout" (toLower errorMsg)
    then "Transaction timeout (took too long)"
  else if isInfixOf "rollback" (toLower errorMsg)
    then "Transaction rolled back (changes reverted)"
  else if isInfixOf "not active" (toLower errorMsg)
    then "Transaction not active (already committed or rolled back)"
  else
    "Transaction error"

export
proven_idris_tx_status_description : Int -> String
proven_idris_tx_status_description status =
  if status == 0 then "Pending (transaction active, not yet committed)"
  else if status == 1 then "Committed (all changes applied successfully)"
  else if status == 2 then "Rolled back (all changes reverted)"
  else if status == 3 then "Failed (transaction aborted due to error)"
  else "Unknown status"

export
proven_idris_tx_isolation_description : Int -> String
proven_idris_tx_isolation_description isolation =
  if isolation == 0 then "Read Uncommitted (dirty reads possible, lowest isolation)"
  else if isolation == 1 then "Read Committed (no dirty reads, default)"
  else if isolation == 2 then "Repeatable Read (consistent reads within transaction)"
  else if isolation == 3 then "Serializable (full isolation, highest consistency)"
  else "Unknown isolation level"
