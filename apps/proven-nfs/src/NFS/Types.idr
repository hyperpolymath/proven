-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
--
-- NFS.Types: Core protocol types for NFSv4 (RFC 7530).
--
-- Defines closed sum types for NFSv4 operations (the compound procedure
-- operations from RFC 7530 Section 16), file types (RFC 7530 Section 5.8.1),
-- and status codes (NFS4ERR_* from RFC 7530 Section 13).

module NFS.Types

%default total

-- ============================================================================
-- NFSv4 operations (RFC 7530 Section 16)
-- ============================================================================

||| NFSv4 operations carried within a COMPOUND procedure.
||| Each operation is a single step in a compound request; the server
||| executes them sequentially and stops at the first failure.
public export
data Operation : Type where
  ||| Check access permissions for a file (Section 16.2).
  Access  : Operation
  ||| Close an open file (Section 16.3).
  Close   : Operation
  ||| Flush uncommitted data to stable storage (Section 16.4).
  Commit  : Operation
  ||| Create a non-regular file object (Section 16.5).
  Create  : Operation
  ||| Retrieve file attributes (Section 16.9).
  GetAttr : Operation
  ||| Create a hard link (Section 16.11).
  Link    : Operation
  ||| Create or test a byte-range lock (Section 16.12).
  Lock    : Operation
  ||| Look up a filename in a directory (Section 16.15).
  Lookup  : Operation
  ||| Open a file, creating it if necessary (Section 16.18).
  Open    : Operation
  ||| Read file data (Section 16.23).
  Read    : Operation
  ||| Read directory entries (Section 16.24).
  ReadDir : Operation
  ||| Remove a filesystem object (Section 16.27).
  Remove  : Operation
  ||| Rename a filesystem object (Section 16.29).
  Rename  : Operation
  ||| Set file attributes (Section 16.32).
  SetAttr : Operation
  ||| Write file data (Section 16.36).
  Write   : Operation

public export
Eq Operation where
  Access  == Access  = True
  Close   == Close   = True
  Commit  == Commit  = True
  Create  == Create  = True
  GetAttr == GetAttr = True
  Link    == Link    = True
  Lock    == Lock    = True
  Lookup  == Lookup  = True
  Open    == Open    = True
  Read    == Read    = True
  ReadDir == ReadDir = True
  Remove  == Remove  = True
  Rename  == Rename  = True
  SetAttr == SetAttr = True
  Write   == Write   = True
  _       == _       = False

public export
Show Operation where
  show Access  = "ACCESS"
  show Close   = "CLOSE"
  show Commit  = "COMMIT"
  show Create  = "CREATE"
  show GetAttr = "GETATTR"
  show Link    = "LINK"
  show Lock    = "LOCK"
  show Lookup  = "LOOKUP"
  show Open    = "OPEN"
  show Read    = "READ"
  show ReadDir = "READDIR"
  show Remove  = "REMOVE"
  show Rename  = "RENAME"
  show SetAttr = "SETATTR"
  show Write   = "WRITE"

-- ============================================================================
-- NFSv4 file types (RFC 7530 Section 5.8.1)
-- ============================================================================

||| NFSv4 file types from the ftype4 enum (RFC 7530 Section 5.8.1).
public export
data FileType : Type where
  ||| Regular data file (NF4REG).
  Regular     : FileType
  ||| Directory (NF4DIR).
  Directory   : FileType
  ||| Block special device (NF4BLK).
  BlockDevice : FileType
  ||| Character special device (NF4CHR).
  CharDevice  : FileType
  ||| Symbolic link (NF4LNK).
  Link        : FileType
  ||| Named socket (NF4SOCK).
  Socket      : FileType
  ||| Named pipe / FIFO (NF4FIFO).
  FIFO        : FileType

public export
Eq FileType where
  Regular     == Regular     = True
  Directory   == Directory   = True
  BlockDevice == BlockDevice = True
  CharDevice  == CharDevice  = True
  Link        == Link        = True
  Socket      == Socket      = True
  FIFO        == FIFO        = True
  _           == _           = False

public export
Show FileType where
  show Regular     = "NF4REG"
  show Directory   = "NF4DIR"
  show BlockDevice = "NF4BLK"
  show CharDevice  = "NF4CHR"
  show Link        = "NF4LNK"
  show Socket      = "NF4SOCK"
  show FIFO        = "NF4FIFO"

-- ============================================================================
-- NFSv4 status codes (RFC 7530 Section 13)
-- ============================================================================

||| Common NFS4 status codes (NFS4ERR_*) from RFC 7530 Section 13.
||| NFS4_OK indicates success; all others indicate specific failure conditions.
public export
data Status : Type where
  ||| Operation succeeded (NFS4_OK, 0).
  Ok       : Status
  ||| Permission denied (NFS4ERR_PERM, 1).
  Perm     : Status
  ||| No such file or directory (NFS4ERR_NOENT, 2).
  NoEnt    : Status
  ||| I/O error (NFS4ERR_IO, 5).
  IO       : Status
  ||| I/O error on a non-existent device (NFS4ERR_NXIO, 6).
  NxIO     : Status
  ||| Access denied by ACL or mode (NFS4ERR_ACCESS, 13).
  Access   : Status
  ||| File or directory already exists (NFS4ERR_EXIST, 17).
  Exist    : Status
  ||| Target is not a directory (NFS4ERR_NOTDIR, 20).
  NotDir   : Status
  ||| Target is a directory (NFS4ERR_ISDIR, 21).
  IsDir    : Status
  ||| File too large (NFS4ERR_FBIG, 27).
  FBig     : Status
  ||| No space left on device (NFS4ERR_NOSPC, 28).
  NoSpc    : Status
  ||| Read-only filesystem (NFS4ERR_ROFS, 30).
  ROfs     : Status
  ||| Directory is not empty (NFS4ERR_NOTEMPTY, 66).
  NotEmpty : Status
  ||| Stale filehandle (NFS4ERR_STALE, 70).
  Stale    : Status

public export
Eq Status where
  Ok       == Ok       = True
  Perm     == Perm     = True
  NoEnt    == NoEnt    = True
  IO       == IO       = True
  NxIO     == NxIO     = True
  Access   == Access   = True
  Exist    == Exist    = True
  NotDir   == NotDir   = True
  IsDir    == IsDir    = True
  FBig     == FBig     = True
  NoSpc    == NoSpc    = True
  ROfs     == ROfs     = True
  NotEmpty == NotEmpty = True
  Stale    == Stale    = True
  _        == _        = False

public export
Show Status where
  show Ok       = "NFS4_OK(0)"
  show Perm     = "NFS4ERR_PERM(1)"
  show NoEnt    = "NFS4ERR_NOENT(2)"
  show IO       = "NFS4ERR_IO(5)"
  show NxIO     = "NFS4ERR_NXIO(6)"
  show Access   = "NFS4ERR_ACCESS(13)"
  show Exist    = "NFS4ERR_EXIST(17)"
  show NotDir   = "NFS4ERR_NOTDIR(20)"
  show IsDir    = "NFS4ERR_ISDIR(21)"
  show FBig     = "NFS4ERR_FBIG(27)"
  show NoSpc    = "NFS4ERR_NOSPC(28)"
  show ROfs     = "NFS4ERR_ROFS(30)"
  show NotEmpty = "NFS4ERR_NOTEMPTY(66)"
  show Stale    = "NFS4ERR_STALE(70)"

-- ============================================================================
-- Enumerations of all constructors
-- ============================================================================

||| All NFSv4 operations.
public export
allOperations : List Operation
allOperations = [Access, Close, Commit, Create, GetAttr, Link, Lock,
                 Lookup, Open, Read, ReadDir, Remove, Rename, SetAttr, Write]

||| All NFSv4 file types.
public export
allFileTypes : List FileType
allFileTypes = [Regular, Directory, BlockDevice, CharDevice, Link, Socket, FIFO]

||| All NFSv4 status codes.
public export
allStatuses : List Status
allStatuses = [Ok, Perm, NoEnt, IO, NxIO, Access, Exist,
               NotDir, IsDir, FBig, NoSpc, ROfs, NotEmpty, Stale]
