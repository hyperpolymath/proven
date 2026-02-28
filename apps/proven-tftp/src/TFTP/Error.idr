-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
--
-- TFTP Error Codes (RFC 1350 Section 5)
--
-- TFTP defines 8 error codes (0-7) that can be sent in ERROR packets.
-- Each error code has a standard human-readable message. Custom error
-- messages can accompany any error code to provide additional detail.
-- The type system ensures only valid error codes are representable.

module TFTP.Error

%default total

-- ============================================================================
-- TFTP Error Codes (RFC 1350 Section 5)
-- ============================================================================

||| The 8 TFTP error codes as defined in RFC 1350.
public export
data TFTPError : Type where
  ||| Error 0: Not defined; see error message (if any).
  NotDefined       : TFTPError
  ||| Error 1: File not found.
  FileNotFound     : TFTPError
  ||| Error 2: Access violation.
  AccessViolation  : TFTPError
  ||| Error 3: Disk full or allocation exceeded.
  DiskFull         : TFTPError
  ||| Error 4: Illegal TFTP operation.
  IllegalOperation : TFTPError
  ||| Error 5: Unknown transfer ID (wrong source port).
  UnknownTID       : TFTPError
  ||| Error 6: File already exists.
  FileExists       : TFTPError
  ||| Error 7: No such user (used with mail mode).
  NoSuchUser       : TFTPError

public export
Eq TFTPError where
  NotDefined       == NotDefined       = True
  FileNotFound     == FileNotFound     = True
  AccessViolation  == AccessViolation  = True
  DiskFull         == DiskFull         = True
  IllegalOperation == IllegalOperation = True
  UnknownTID       == UnknownTID       = True
  FileExists       == FileExists       = True
  NoSuchUser       == NoSuchUser       = True
  _                == _                = False

public export
Show TFTPError where
  show NotDefined       = "Not Defined"
  show FileNotFound     = "File Not Found"
  show AccessViolation  = "Access Violation"
  show DiskFull         = "Disk Full"
  show IllegalOperation = "Illegal Operation"
  show UnknownTID       = "Unknown Transfer ID"
  show FileExists       = "File Already Exists"
  show NoSuchUser       = "No Such User"

-- ============================================================================
-- Numeric code conversion
-- ============================================================================

||| Convert a TFTP error to its numeric code (0-7).
public export
errorCode : TFTPError -> Bits16
errorCode NotDefined       = 0
errorCode FileNotFound     = 1
errorCode AccessViolation  = 2
errorCode DiskFull         = 3
errorCode IllegalOperation = 4
errorCode UnknownTID       = 5
errorCode FileExists       = 6
errorCode NoSuchUser       = 7

||| Decode a numeric code to a TFTP error.
||| Returns Nothing for codes outside the valid range (0-7).
public export
errorFromCode : Bits16 -> Maybe TFTPError
errorFromCode 0 = Just NotDefined
errorFromCode 1 = Just FileNotFound
errorFromCode 2 = Just AccessViolation
errorFromCode 3 = Just DiskFull
errorFromCode 4 = Just IllegalOperation
errorFromCode 5 = Just UnknownTID
errorFromCode 6 = Just FileExists
errorFromCode 7 = Just NoSuchUser
errorFromCode _ = Nothing

-- ============================================================================
-- Default error messages (RFC 1350 Section 5)
-- ============================================================================

||| The standard error message for each error code.
||| These are the default messages sent in ERROR packets when no
||| custom message is provided.
public export
defaultMessage : TFTPError -> String
defaultMessage NotDefined       = "Not defined, see error message (if any)"
defaultMessage FileNotFound     = "File not found"
defaultMessage AccessViolation  = "Access violation"
defaultMessage DiskFull         = "Disk full or allocation exceeded"
defaultMessage IllegalOperation = "Illegal TFTP operation"
defaultMessage UnknownTID       = "Unknown transfer ID"
defaultMessage FileExists       = "File already exists"
defaultMessage NoSuchUser       = "No such user"

-- ============================================================================
-- Error classification
-- ============================================================================

||| Whether the error is recoverable (the client could retry the transfer).
||| DiskFull and UnknownTID may be transient; others are typically permanent.
public export
isRecoverable : TFTPError -> Bool
isRecoverable DiskFull   = True   -- Space may become available
isRecoverable UnknownTID = True   -- Retry may succeed with correct TID
isRecoverable _          = False

||| Whether the error indicates a security-related condition.
||| AccessViolation and NoSuchUser may indicate access control issues.
public export
isSecurityError : TFTPError -> Bool
isSecurityError AccessViolation = True
isSecurityError NoSuchUser      = True
isSecurityError _               = False

||| Whether the error indicates a client-side problem (bad request).
public export
isClientError : TFTPError -> Bool
isClientError FileNotFound     = True
isClientError AccessViolation  = True
isClientError IllegalOperation = True
isClientError UnknownTID       = True
isClientError FileExists       = True
isClientError NoSuchUser       = True
isClientError _                = False

||| Whether the error indicates a server-side problem.
public export
isServerError : TFTPError -> Bool
isServerError DiskFull    = True
isServerError NotDefined  = True
isServerError _           = False

-- ============================================================================
-- Error packet construction
-- ============================================================================

||| An error with an optional custom message.
||| If the custom message is Nothing, the default message will be used.
public export
record ErrorInfo where
  constructor MkErrorInfo
  ||| The error code.
  code    : TFTPError
  ||| Custom error message (overrides default if present).
  message : Maybe String

||| Get the effective error message (custom or default).
public export
effectiveMessage : ErrorInfo -> String
effectiveMessage info =
  case info.message of
    Just msg => msg
    Nothing  => defaultMessage info.code

public export
Show ErrorInfo where
  show info = "ERROR " ++ show (cast {to=Nat} (errorCode info.code))
              ++ ": " ++ effectiveMessage info

||| Create an error info with the default message.
public export
mkError : TFTPError -> ErrorInfo
mkError err = MkErrorInfo err Nothing

||| Create an error info with a custom message.
public export
mkErrorWithMsg : TFTPError -> String -> ErrorInfo
mkErrorWithMsg err msg = MkErrorInfo err (Just msg)
