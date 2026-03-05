-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
--
-- FTP Transfer Modes (RFC 959 Section 3.4)
--
-- Defines transfer types (ASCII/Binary), data connection modes
-- (Active/Passive), and structure types. The representation type
-- is a closed sum with no partial cases.

module FTP.Transfer

%default total

-- ============================================================================
-- Transfer types (RFC 959 Section 3.1.1)
-- ============================================================================

||| FTP representation types for file transfer.
public export
data TransferType : Type where
  ||| ASCII type — text files with CR/LF line endings.
  ASCII  : TransferType
  ||| Image (binary) type — raw byte stream, no translation.
  Binary : TransferType

public export
Eq TransferType where
  ASCII  == ASCII  = True
  Binary == Binary = True
  _      == _      = False

public export
Show TransferType where
  show ASCII  = "ASCII"
  show Binary = "Binary"

||| The TYPE command code for this transfer type.
public export
typeCode : TransferType -> String
typeCode ASCII  = "A"
typeCode Binary = "I"

||| Parse a TYPE argument into a TransferType.
public export
parseType : String -> Maybe TransferType
parseType "A" = Just ASCII
parseType "a" = Just ASCII
parseType "I" = Just Binary
parseType "i" = Just Binary
parseType _   = Nothing

-- ============================================================================
-- Data connection modes (RFC 959 Section 3.2)
-- ============================================================================

||| How the data connection is established.
public export
data DataMode : Type where
  ||| Active mode: server connects to client-specified address.
  Active  : (host : String) -> (port : Bits16) -> DataMode
  ||| Passive mode: server listens, client connects.
  Passive : (host : String) -> (port : Bits16) -> DataMode

public export
Show DataMode where
  show (Active h p)  = "Active(" ++ h ++ ":" ++ show (cast {to=Nat} p) ++ ")"
  show (Passive h p) = "Passive(" ++ h ++ ":" ++ show (cast {to=Nat} p) ++ ")"

||| Whether the server initiates the data connection.
public export
serverInitiates : DataMode -> Bool
serverInitiates (Active _ _)  = True
serverInitiates (Passive _ _) = False

-- ============================================================================
-- Transfer state
-- ============================================================================

||| State of an ongoing data transfer.
public export
data TransferState : Type where
  ||| No transfer in progress.
  Idle       : TransferState
  ||| Transfer is in progress.
  InProgress : (bytesTransferred : Nat) -> TransferState
  ||| Transfer completed successfully.
  Completed  : (totalBytes : Nat) -> TransferState
  ||| Transfer was aborted.
  Aborted    : (reason : String) -> TransferState

public export
Show TransferState where
  show Idle              = "Idle"
  show (InProgress n)    = "InProgress(" ++ show n ++ " bytes)"
  show (Completed n)     = "Completed(" ++ show n ++ " bytes)"
  show (Aborted reason)  = "Aborted(" ++ reason ++ ")"

||| Whether a new transfer can begin.
public export
canStartTransfer : TransferState -> Bool
canStartTransfer Idle          = True
canStartTransfer (Completed _) = True
canStartTransfer (Aborted _)   = True
canStartTransfer (InProgress _) = False

||| Record some bytes transferred.
public export
addBytes : TransferState -> Nat -> TransferState
addBytes (InProgress n) more = InProgress (n + more)
addBytes other          _    = other

||| Complete the current transfer.
public export
completeTransfer : TransferState -> TransferState
completeTransfer (InProgress n) = Completed n
completeTransfer other          = other

||| Abort the current transfer.
public export
abortTransfer : TransferState -> String -> TransferState
abortTransfer (InProgress _) reason = Aborted reason
abortTransfer other          _      = other
