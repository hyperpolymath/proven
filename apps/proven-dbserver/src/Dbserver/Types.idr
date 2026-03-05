-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
--
-- proven-dbserver: Core protocol types for database server.
--
-- All types are closed sum types with total Show instances.
-- No parsers, no full implementations -- skeleton only.

module Dbserver.Types

%default total

-- ============================================================================
-- QueryType
-- ============================================================================

||| Categories of SQL-like queries the database server can execute.
public export
data QueryType : Type where
  ||| Retrieve rows matching criteria.
  Select      : QueryType
  ||| Insert new rows.
  Insert      : QueryType
  ||| Update existing rows.
  Update      : QueryType
  ||| Delete rows.
  Delete      : QueryType
  ||| Create a new table.
  CreateTable : QueryType
  ||| Drop an existing table.
  DropTable   : QueryType
  ||| Alter an existing table schema.
  AlterTable  : QueryType
  ||| Create an index on one or more columns.
  CreateIndex : QueryType
  ||| Drop an existing index.
  DropIndex   : QueryType
  ||| Begin a new transaction.
  Begin       : QueryType
  ||| Commit the current transaction.
  Commit      : QueryType
  ||| Roll back the current transaction.
  Rollback    : QueryType

export
Show QueryType where
  show Select      = "Select"
  show Insert      = "Insert"
  show Update      = "Update"
  show Delete      = "Delete"
  show CreateTable = "CreateTable"
  show DropTable   = "DropTable"
  show AlterTable  = "AlterTable"
  show CreateIndex = "CreateIndex"
  show DropIndex   = "DropIndex"
  show Begin       = "Begin"
  show Commit      = "Commit"
  show Rollback    = "Rollback"

-- ============================================================================
-- DataType
-- ============================================================================

||| Column data types supported by the database.
public export
data DataType : Type where
  ||| Signed integer (arbitrary precision).
  Integer   : DataType
  ||| IEEE 754 double-precision floating point.
  Float     : DataType
  ||| UTF-8 text string.
  Text      : DataType
  ||| Binary large object.
  Blob      : DataType
  ||| Boolean (true/false).
  Boolean   : DataType
  ||| Timestamp with timezone.
  Timestamp : DataType
  ||| Universally Unique Identifier (RFC 4122).
  UUID      : DataType
  ||| JSON document.
  JSON      : DataType
  ||| SQL NULL (absence of a value).
  Null      : DataType

export
Show DataType where
  show Integer   = "Integer"
  show Float     = "Float"
  show Text      = "Text"
  show Blob      = "Blob"
  show Boolean   = "Boolean"
  show Timestamp = "Timestamp"
  show UUID      = "UUID"
  show JSON      = "JSON"
  show Null      = "Null"

-- ============================================================================
-- IsolationLevel
-- ============================================================================

||| Transaction isolation levels (SQL standard).
public export
data IsolationLevel : Type where
  ||| Dirty reads allowed.
  ReadUncommitted : IsolationLevel
  ||| Only committed data visible.
  ReadCommitted   : IsolationLevel
  ||| Snapshot isolation -- reads are repeatable within the transaction.
  RepeatableRead  : IsolationLevel
  ||| Full serialisability -- transactions appear to execute sequentially.
  Serializable    : IsolationLevel

export
Show IsolationLevel where
  show ReadUncommitted = "ReadUncommitted"
  show ReadCommitted   = "ReadCommitted"
  show RepeatableRead  = "RepeatableRead"
  show Serializable    = "Serializable"

-- ============================================================================
-- ErrorCode
-- ============================================================================

||| Error codes returned by database operations.
public export
data ErrorCode : Type where
  ||| Query has a syntax error.
  SyntaxError          : ErrorCode
  ||| Referenced table does not exist.
  TableNotFound        : ErrorCode
  ||| Referenced column does not exist.
  ColumnNotFound       : ErrorCode
  ||| Insert or update violates a unique constraint.
  DuplicateKey         : ErrorCode
  ||| Operation violates a foreign key or check constraint.
  ConstraintViolation  : ErrorCode
  ||| Value type does not match the column type.
  TypeMismatch         : ErrorCode
  ||| Two transactions are waiting on each other.
  DeadlockDetected     : ErrorCode
  ||| Transaction was aborted due to conflict or timeout.
  TransactionAborted   : ErrorCode
  ||| No storage space remaining.
  DiskFull             : ErrorCode
  ||| Client connection was lost.
  ConnectionLost       : ErrorCode

export
Show ErrorCode where
  show SyntaxError         = "SyntaxError"
  show TableNotFound       = "TableNotFound"
  show ColumnNotFound      = "ColumnNotFound"
  show DuplicateKey        = "DuplicateKey"
  show ConstraintViolation = "ConstraintViolation"
  show TypeMismatch        = "TypeMismatch"
  show DeadlockDetected    = "DeadlockDetected"
  show TransactionAborted  = "TransactionAborted"
  show DiskFull            = "DiskFull"
  show ConnectionLost      = "ConnectionLost"

-- ============================================================================
-- JoinType
-- ============================================================================

||| Types of table joins supported in queries.
public export
data JoinType : Type where
  ||| Inner join -- only matching rows from both tables.
  Inner      : JoinType
  ||| Left outer join -- all rows from left, matching from right.
  LeftOuter  : JoinType
  ||| Right outer join -- all rows from right, matching from left.
  RightOuter : JoinType
  ||| Full outer join -- all rows from both tables.
  FullOuter  : JoinType
  ||| Cross join -- Cartesian product of both tables.
  Cross      : JoinType

export
Show JoinType where
  show Inner      = "Inner"
  show LeftOuter  = "LeftOuter"
  show RightOuter = "RightOuter"
  show FullOuter  = "FullOuter"
  show Cross      = "Cross"
