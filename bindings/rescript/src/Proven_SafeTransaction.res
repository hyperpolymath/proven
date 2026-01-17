// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

/**
 * SafeTransaction - Transaction state management that cannot crash.
 *
 * Provides ACID-style transaction semantics with begin/commit/rollback
 * operations. Tracks transaction state, supports nested transactions
 * via savepoints, and ensures consistent state transitions.
 */

/** Error types for transaction operations */
type transactionError =
  | NotInTransaction
  | AlreadyInTransaction
  | SavepointNotFound
  | MaxSavepointsReached
  | InvalidState
  | RollbackRequired
  | CommitFailed

/** Transaction isolation levels */
type isolationLevel =
  | ReadUncommitted
  | ReadCommitted
  | RepeatableRead
  | Serializable

/** Transaction state enumeration */
type transactionState =
  | Idle
  | Active
  | Committed
  | RolledBack
  | Failed

/** Types of operations that can be recorded */
type operationType =
  | Insert
  | Update
  | Delete
  | SavepointOp

/** A recorded operation for rollback purposes */
type operation = {
  opType: operationType,
  key: string,
  oldValue: option<string>,
  newValue: option<string>,
  timestamp: float,
}

/** A savepoint marker within a transaction */
type savepoint = {
  name: string,
  operationIndex: int,
  createdAt: float,
}

/** Maximum number of savepoints per transaction */
let maxSavepoints = 32

/** Maximum number of operations to track in history */
let maxOperations = 256

/** Transaction manager with bounded operation tracking */
type transactionManager = {
  mutable state: transactionState,
  mutable isolationLevel: isolationLevel,
  mutable operations: array<operation>,
  mutable savepoints: array<savepoint>,
  mutable startedAt: option<float>,
  mutable transactionId: int,
  mutable nextId: int,
}

/** Get string representation of isolation level */
let isolationLevelToString = (level: isolationLevel): string => {
  switch level {
  | ReadUncommitted => "READ UNCOMMITTED"
  | ReadCommitted => "READ COMMITTED"
  | RepeatableRead => "REPEATABLE READ"
  | Serializable => "SERIALIZABLE"
  }
}

/** Check if transaction is in a terminal state */
let isTerminal = (state: transactionState): bool => {
  switch state {
  | Committed | RolledBack | Failed => true
  | Idle | Active => false
  }
}

/** Check if transaction can accept operations */
let canOperate = (state: transactionState): bool => {
  state == Active
}

/** Initialize a new transaction manager */
let createTransactionManager = (): transactionManager => {
  {
    state: Idle,
    isolationLevel: ReadCommitted,
    operations: [],
    savepoints: [],
    startedAt: None,
    transactionId: 0,
    nextId: 1,
  }
}

/** Begin a new transaction with default isolation */
let begin = (txn: transactionManager): result<unit, transactionError> => {
  if txn.state == Active {
    Error(AlreadyInTransaction)
  } else if txn.state == Failed {
    Error(RollbackRequired)
  } else {
    txn.state = Active
    txn.isolationLevel = ReadCommitted
    txn.operations = []
    txn.savepoints = []
    txn.startedAt = Some(Js.Date.now())
    txn.transactionId = txn.nextId
    txn.nextId = txn.nextId + 1
    Ok()
  }
}

/** Begin a transaction with specific isolation level */
let beginWithIsolation = (
  txn: transactionManager,
  level: isolationLevel,
): result<unit, transactionError> => {
  if txn.state == Active {
    Error(AlreadyInTransaction)
  } else if txn.state == Failed {
    Error(RollbackRequired)
  } else {
    txn.state = Active
    txn.isolationLevel = level
    txn.operations = []
    txn.savepoints = []
    txn.startedAt = Some(Js.Date.now())
    txn.transactionId = txn.nextId
    txn.nextId = txn.nextId + 1
    Ok()
  }
}

/** Commit the current transaction */
let commit = (txn: transactionManager): result<unit, transactionError> => {
  if txn.state != Active {
    Error(NotInTransaction)
  } else {
    txn.state = Committed
    txn.startedAt = None
    Ok()
  }
}

/** Rollback the current transaction */
let rollback = (txn: transactionManager): result<unit, transactionError> => {
  if txn.state != Active && txn.state != Failed {
    Error(NotInTransaction)
  } else {
    txn.state = RolledBack
    txn.startedAt = None
    Ok()
  }
}

/** Create a savepoint */
let createSavepoint = (txn: transactionManager, name: string): result<unit, transactionError> => {
  if txn.state != Active {
    Error(NotInTransaction)
  } else if Belt.Array.length(txn.savepoints) >= maxSavepoints {
    Error(MaxSavepointsReached)
  } else {
    let sp: savepoint = {
      name: name,
      operationIndex: Belt.Array.length(txn.operations),
      createdAt: Js.Date.now(),
    }
    txn.savepoints = Belt.Array.concat(txn.savepoints, [sp])
    Ok()
  }
}

/** Rollback to a savepoint */
let rollbackToSavepoint = (
  txn: transactionManager,
  name: string,
): result<unit, transactionError> => {
  if txn.state != Active {
    Error(NotInTransaction)
  } else {
    let maybeIdx = txn.savepoints->Belt.Array.getIndexBy(sp => sp.name == name)
    switch maybeIdx {
    | None => Error(SavepointNotFound)
    | Some(idx) =>
      let sp = Belt.Array.getUnsafe(txn.savepoints, idx)
      txn.operations = Belt.Array.slice(txn.operations, ~offset=0, ~len=sp.operationIndex)
      txn.savepoints = Belt.Array.slice(txn.savepoints, ~offset=0, ~len=idx)
      Ok()
    }
  }
}

/** Release a savepoint (keep changes but remove marker) */
let releaseSavepoint = (txn: transactionManager, name: string): result<unit, transactionError> => {
  if txn.state != Active {
    Error(NotInTransaction)
  } else {
    let maybeIdx = txn.savepoints->Belt.Array.getIndexBy(sp => sp.name == name)
    switch maybeIdx {
    | None => Error(SavepointNotFound)
    | Some(idx) =>
      let before = Belt.Array.slice(txn.savepoints, ~offset=0, ~len=idx)
      let after = Belt.Array.sliceToEnd(txn.savepoints, idx + 1)
      txn.savepoints = Belt.Array.concat(before, after)
      Ok()
    }
  }
}

/** Record an operation (for rollback tracking) */
let recordOperation = (
  txn: transactionManager,
  opType: operationType,
  key: string,
  oldValue: option<string>,
  newValue: option<string>,
): result<unit, transactionError> => {
  if txn.state != Active {
    Error(NotInTransaction)
  } else if Belt.Array.length(txn.operations) >= maxOperations {
    txn.state = Failed
    Error(InvalidState)
  } else {
    let op: operation = {
      opType: opType,
      key: key,
      oldValue: oldValue,
      newValue: newValue,
      timestamp: Js.Date.now(),
    }
    txn.operations = Belt.Array.concat(txn.operations, [op])
    Ok()
  }
}

/** Get current transaction state */
let getState = (txn: transactionManager): transactionState => {
  txn.state
}

/** Check if currently in an active transaction */
let isActive = (txn: transactionManager): bool => {
  txn.state == Active
}

/** Get current transaction ID (0 if not in transaction) */
let getId = (txn: transactionManager): int => {
  if txn.state == Active {
    txn.transactionId
  } else {
    0
  }
}

/** Get number of recorded operations */
let getOperationCount = (txn: transactionManager): int => {
  Belt.Array.length(txn.operations)
}

/** Get number of active savepoints */
let getSavepointCount = (txn: transactionManager): int => {
  Belt.Array.length(txn.savepoints)
}

/** Get isolation level */
let getIsolationLevel = (txn: transactionManager): isolationLevel => {
  txn.isolationLevel
}

/** Mark transaction as failed (requires rollback) */
let fail = (txn: transactionManager): unit => {
  if txn.state == Active {
    txn.state = Failed
  }
}

/** Reset to idle state (after commit/rollback) */
let resetState = (txn: transactionManager): unit => {
  txn.state = Idle
  txn.operations = []
  txn.savepoints = []
  txn.startedAt = None
}

/** Transaction guard for RAII-style transaction management */
type transactionGuard = {
  manager: transactionManager,
  mutable committed: bool,
}

/** Create a guard and begin transaction */
let createGuard = (manager: transactionManager): result<transactionGuard, transactionError> => {
  switch begin(manager) {
  | Error(e) => Error(e)
  | Ok() => Ok({manager: manager, committed: false})
  }
}

/** Commit the guarded transaction */
let commitGuard = (guard: transactionGuard): result<unit, transactionError> => {
  switch commit(guard.manager) {
  | Error(e) => Error(e)
  | Ok() =>
    guard.committed = true
    Ok()
  }
}

/** Rollback the guarded transaction */
let rollbackGuard = (guard: transactionGuard): result<unit, transactionError> => {
  switch rollback(guard.manager) {
  | Error(e) => Error(e)
  | Ok() =>
    guard.committed = true // Prevent double rollback
    Ok()
  }
}

/** Finalize guard - auto-rollback if not committed */
let finalizeGuard = (guard: transactionGuard): unit => {
  if !guard.committed && isActive(guard.manager) {
    let _ = rollback(guard.manager)
  }
}

/** Execute a function within a transaction, auto-rollback on failure */
let withTransaction = (
  manager: transactionManager,
  f: unit => result<'a, 'e>,
): result<'a, transactionError> => {
  switch begin(manager) {
  | Error(e) => Error(e)
  | Ok() =>
    switch f() {
    | Error(_) =>
      let _ = rollback(manager)
      Error(RollbackRequired)
    | Ok(result) =>
      switch commit(manager) {
      | Error(e) =>
        let _ = rollback(manager)
        Error(e)
      | Ok() => Ok(result)
      }
    }
  }
}
