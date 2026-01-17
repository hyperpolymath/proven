-- SPDX-License-Identifier: Palimpsest-MPL-1.0
||| SafeTransaction - Safe transactional operations
|||
||| This module provides ACID-like transaction semantics
||| for safe state modifications with rollback support.
module Proven.SafeTransaction

import public Proven.Core

%default total

--------------------------------------------------------------------------------
-- Transaction State
--------------------------------------------------------------------------------

||| Transaction status
public export
data TxStatus : Type where
  Pending : TxStatus
  Committed : TxStatus
  RolledBack : TxStatus
  Failed : (reason : String) -> TxStatus

public export
Eq TxStatus where
  Pending == Pending = True
  Committed == Committed = True
  RolledBack == RolledBack = True
  (Failed a) == (Failed b) = a == b
  _ == _ = False

public export
Show TxStatus where
  show Pending = "Pending"
  show Committed = "Committed"
  show RolledBack = "RolledBack"
  show (Failed r) = "Failed(" ++ r ++ ")"

--------------------------------------------------------------------------------
-- Transaction Operations
--------------------------------------------------------------------------------

||| A single operation in a transaction
public export
data TxOp state : Type where
  ||| Set a new state value
  SetState : state -> TxOp state
  ||| Modify state with a function
  ModifyState : (state -> state) -> TxOp state
  ||| Validate current state
  Validate : (state -> Bool) -> String -> TxOp state

||| Transaction log entry
public export
record TxLogEntry state where
  constructor MkLogEntry
  operation : String
  beforeState : state
  afterState : state
  timestamp : Nat

--------------------------------------------------------------------------------
-- Transaction Type
--------------------------------------------------------------------------------

||| A transaction with operations and state
public export
record Transaction state where
  constructor MkTx
  txId : Nat
  initialState : state
  currentState : state
  operations : List (TxOp state)
  log : List (TxLogEntry state)
  status : TxStatus

||| Create a new transaction
public export
begin : (txId : Nat) -> state -> Transaction state
begin tid initial = MkTx tid initial initial [] [] Pending

||| Check if transaction is active
public export
isActive : Transaction state -> Bool
isActive tx = tx.status == Pending

||| Check if transaction succeeded
public export
isCommitted : Transaction state -> Bool
isCommitted tx = tx.status == Committed

--------------------------------------------------------------------------------
-- Operations
--------------------------------------------------------------------------------

||| Add an operation to the transaction
public export
addOp : TxOp state -> Transaction state -> Transaction state
addOp op tx =
  if isActive tx
    then MkTx tx.txId tx.initialState tx.currentState (op :: tx.operations) tx.log tx.status
    else tx

||| Set state value
public export
set : state -> Transaction state -> Transaction state
set s = addOp (SetState s)

||| Modify state
public export
modify : (state -> state) -> Transaction state -> Transaction state
modify f = addOp (ModifyState f)

||| Add a validation check
public export
validate : (state -> Bool) -> String -> Transaction state -> Transaction state
validate pred msg = addOp (Validate pred msg)

--------------------------------------------------------------------------------
-- Execution
--------------------------------------------------------------------------------

||| Execute a single operation
executeOp : (timestamp : Nat) -> TxOp state -> state -> Either String (state, TxLogEntry state)
executeOp ts (SetState newState) oldState =
  Right (newState, MkLogEntry "set" oldState newState ts)
executeOp ts (ModifyState f) oldState =
  let newState = f oldState
  in Right (newState, MkLogEntry "modify" oldState newState ts)
executeOp ts (Validate pred msg) st =
  if pred st
    then Right (st, MkLogEntry ("validate: " ++ msg) st st ts)
    else Left msg

||| Execute all operations in order
executeOps : (timestamp : Nat) -> List (TxOp state) -> state -> List (TxLogEntry state) -> Either String (state, List (TxLogEntry state))
executeOps _ [] st log = Right (st, log)
executeOps ts (op :: ops) st log =
  case executeOp ts op st of
    Left err => Left err
    Right (newSt, entry) => executeOps (S ts) ops newSt (entry :: log)

||| Commit the transaction
public export
commit : (timestamp : Nat) -> Transaction state -> Transaction state
commit ts tx =
  if not (isActive tx)
    then tx
    else case executeOps ts (reverse tx.operations) tx.initialState [] of
           Left err => MkTx tx.txId tx.initialState tx.initialState tx.operations tx.log (Failed err)
           Right (finalState, newLog) => MkTx tx.txId tx.initialState finalState tx.operations newLog Committed

||| Rollback the transaction
public export
rollback : Transaction state -> Transaction state
rollback tx =
  if isActive tx
    then MkTx tx.txId tx.initialState tx.initialState tx.operations tx.log RolledBack
    else tx

--------------------------------------------------------------------------------
-- Savepoints
--------------------------------------------------------------------------------

||| A savepoint within a transaction
public export
record Savepoint state where
  constructor MkSavepoint
  name : String
  state : state
  opCount : Nat

||| Create a savepoint
public export
savepoint : String -> Transaction state -> (Savepoint state, Transaction state)
savepoint name tx =
  let sp = MkSavepoint name tx.currentState (length tx.operations)
  in (sp, tx)

||| Rollback to a savepoint
public export
rollbackTo : Savepoint state -> Transaction state -> Transaction state
rollbackTo sp tx =
  if isActive tx
    then MkTx tx.txId tx.initialState sp.state (drop (minus (length tx.operations) sp.opCount) tx.operations) tx.log Pending
    else tx

--------------------------------------------------------------------------------
-- Transaction Isolation
--------------------------------------------------------------------------------

||| Isolation level
public export
data IsolationLevel : Type where
  ReadUncommitted : IsolationLevel
  ReadCommitted : IsolationLevel
  RepeatableRead : IsolationLevel
  Serializable : IsolationLevel

public export
Eq IsolationLevel where
  ReadUncommitted == ReadUncommitted = True
  ReadCommitted == ReadCommitted = True
  RepeatableRead == RepeatableRead = True
  Serializable == Serializable = True
  _ == _ = False

public export
Show IsolationLevel where
  show ReadUncommitted = "ReadUncommitted"
  show ReadCommitted = "ReadCommitted"
  show RepeatableRead = "RepeatableRead"
  show Serializable = "Serializable"

--------------------------------------------------------------------------------
-- Statistics
--------------------------------------------------------------------------------

||| Get operation count
public export
opCount : Transaction state -> Nat
opCount tx = length tx.operations

||| Get log entry count
public export
logCount : Transaction state -> Nat
logCount tx = length tx.log

||| Get final state (if committed)
public export
finalState : Transaction state -> Maybe state
finalState tx =
  if isCommitted tx then Just tx.currentState else Nothing

public export
Show state => Show (Transaction state) where
  show tx = "Transaction(" ++ show tx.txId ++ ", " ++ show tx.status ++ 
            ", ops=" ++ show (opCount tx) ++ ")"

