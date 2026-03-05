-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
--
-- proven-fileserver: Core protocol types for network file server.
--
-- All types are closed sum types with total Show instances.
-- No parsers, no full implementations -- skeleton only.

module Fileserver.Types

%default total

-- ============================================================================
-- Operation
-- ============================================================================

||| File operations supported by the server.
public export
data Operation : Type where
  ||| Read file contents.
  Read   : Operation
  ||| Write data to a file.
  Write  : Operation
  ||| Create a new file or directory.
  Create : Operation
  ||| Delete a file or directory.
  Delete : Operation
  ||| Rename or move a file.
  Rename : Operation
  ||| List directory contents.
  List   : Operation
  ||| Get file metadata (size, timestamps, permissions).
  Stat   : Operation
  ||| Acquire a lock on a file.
  Lock   : Operation
  ||| Release a lock on a file.
  Unlock : Operation
  ||| Watch a file or directory for changes.
  Watch  : Operation

export
Show Operation where
  show Read   = "Read"
  show Write  = "Write"
  show Create = "Create"
  show Delete = "Delete"
  show Rename = "Rename"
  show List   = "List"
  show Stat   = "Stat"
  show Lock   = "Lock"
  show Unlock = "Unlock"
  show Watch  = "Watch"

-- ============================================================================
-- FileType
-- ============================================================================

||| Types of filesystem entries the server can represent.
public export
data FileType : Type where
  ||| Regular file containing data.
  Regular     : FileType
  ||| Directory containing other entries.
  Directory   : FileType
  ||| Symbolic link pointing to another path.
  Symlink     : FileType
  ||| Block device (disk, partition).
  BlockDevice : FileType
  ||| Character device (terminal, serial port).
  CharDevice  : FileType
  ||| Named pipe (FIFO).
  FIFO        : FileType
  ||| Unix domain socket.
  Socket      : FileType

export
Show FileType where
  show Regular     = "Regular"
  show Directory   = "Directory"
  show Symlink     = "Symlink"
  show BlockDevice = "BlockDevice"
  show CharDevice  = "CharDevice"
  show FIFO        = "FIFO"
  show Socket      = "Socket"

-- ============================================================================
-- Permission
-- ============================================================================

||| Individual POSIX permission bits.
public export
data Permission : Type where
  ||| Owner can read.
  OwnerRead     : Permission
  ||| Owner can write.
  OwnerWrite    : Permission
  ||| Owner can execute.
  OwnerExecute  : Permission
  ||| Group can read.
  GroupRead     : Permission
  ||| Group can write.
  GroupWrite    : Permission
  ||| Group can execute.
  GroupExecute  : Permission
  ||| Others can read.
  OtherRead     : Permission
  ||| Others can write.
  OtherWrite    : Permission
  ||| Others can execute.
  OtherExecute  : Permission

export
Show Permission where
  show OwnerRead     = "OwnerRead"
  show OwnerWrite    = "OwnerWrite"
  show OwnerExecute  = "OwnerExecute"
  show GroupRead     = "GroupRead"
  show GroupWrite    = "GroupWrite"
  show GroupExecute  = "GroupExecute"
  show OtherRead     = "OtherRead"
  show OtherWrite    = "OtherWrite"
  show OtherExecute  = "OtherExecute"

-- ============================================================================
-- LockType
-- ============================================================================

||| Types of file locks.
public export
data LockType : Type where
  ||| Shared (read) lock -- multiple holders allowed.
  Shared    : LockType
  ||| Exclusive (write) lock -- single holder only.
  Exclusive : LockType
  ||| Advisory lock -- not enforced by the kernel.
  Advisory  : LockType
  ||| Mandatory lock -- enforced at the kernel level.
  Mandatory : LockType

export
Show LockType where
  show Shared    = "Shared"
  show Exclusive = "Exclusive"
  show Advisory  = "Advisory"
  show Mandatory = "Mandatory"

-- ============================================================================
-- ErrorCode
-- ============================================================================

||| Error codes returned by file operations.
public export
data ErrorCode : Type where
  ||| File or directory does not exist.
  NotFound         : ErrorCode
  ||| Insufficient permissions for the requested operation.
  PermissionDenied : ErrorCode
  ||| A file or directory with that name already exists.
  AlreadyExists    : ErrorCode
  ||| Directory is not empty (cannot delete).
  NotEmpty         : ErrorCode
  ||| Target is a directory when a file was expected.
  IsDirectory      : ErrorCode
  ||| Target is not a directory when one was expected.
  NotDirectory     : ErrorCode
  ||| No space remaining on the storage device.
  NoSpace          : ErrorCode
  ||| Filesystem is mounted read-only.
  ReadOnly         : ErrorCode
  ||| File is locked by another process.
  Locked           : ErrorCode
  ||| Low-level I/O error.
  IOError          : ErrorCode

export
Show ErrorCode where
  show NotFound         = "NotFound"
  show PermissionDenied = "PermissionDenied"
  show AlreadyExists    = "AlreadyExists"
  show NotEmpty         = "NotEmpty"
  show IsDirectory      = "IsDirectory"
  show NotDirectory     = "NotDirectory"
  show NoSpace          = "NoSpace"
  show ReadOnly         = "ReadOnly"
  show Locked           = "Locked"
  show IOError          = "IOError"
