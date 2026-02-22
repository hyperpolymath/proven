// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

// Proven - Main unit re-exporting all safe wrapper modules.
//
// This is the primary entry point for the Proven Pascal bindings. It
// re-exports all Safe* wrapper units and provides lifecycle management
// (initialization and deinitialization) for the Proven runtime.
//
// All computation is performed by the formally verified Idris 2 core
// via the Zig FFI bridge exposing a stable C ABI. These bindings are
// thin wrappers that ONLY call libproven; they do NOT reimplement logic.
//
// Usage:
//   uses Proven;
//
//   begin
//     ProvenInitialize;
//     try
//       // Use SafeMath, SafeString, SafePath, etc.
//       ...
//     finally
//       ProvenFinalize;
//     end;
//   end.
//
// Compatible with Free Pascal (FPC) and Delphi.

unit Proven;

{$IFDEF FPC}
  {$MODE OBJFPC}{$H+}
{$ENDIF}

interface

uses
  LibProven,
  SafeMath,
  SafeString,
  SafePath,
  SafeEmail,
  SafeUrl,
  SafeNetwork,
  SafeCrypto,
  SafeJson,
  SafeDateTime;

const
  // Library version string.
  ProvenVersionString = '0.9.0';

type
  // Re-export status codes for error handling convenience.
  TProvenStatus = Int32;

const
  // Status code constants re-exported from LibProven.
  psOK                   = PROVEN_OK;
  psErrNullPointer       = PROVEN_ERR_NULL_POINTER;
  psErrInvalidArgument   = PROVEN_ERR_INVALID_ARGUMENT;
  psErrOverflow          = PROVEN_ERR_OVERFLOW;
  psErrUnderflow         = PROVEN_ERR_UNDERFLOW;
  psErrDivisionByZero    = PROVEN_ERR_DIVISION_BY_ZERO;
  psErrParseFailure      = PROVEN_ERR_PARSE_FAILURE;
  psErrValidationFailed  = PROVEN_ERR_VALIDATION_FAILED;
  psErrOutOfBounds       = PROVEN_ERR_OUT_OF_BOUNDS;
  psErrEncodingError     = PROVEN_ERR_ENCODING_ERROR;
  psErrAllocationFailed  = PROVEN_ERR_ALLOCATION_FAILED;
  psErrNotImplemented    = PROVEN_ERR_NOT_IMPLEMENTED;

// Initialize the Proven runtime. Must be called before using any Proven
// functions. Safe to call multiple times.
// Returns True on success, False on failure.
function ProvenInitialize: Boolean;

// Finalize and cleanup the Proven runtime.
// All Proven-allocated resources should be freed before calling this.
procedure ProvenFinalize;

// Check if the Proven runtime is currently initialized.
function ProvenIsInitialized: Boolean;

// Get the FFI ABI version for compatibility checking.
function ProvenABIVersion: UInt32;

// Get the library version components.
function ProvenMajorVersion: UInt32;
function ProvenMinorVersion: UInt32;
function ProvenPatchVersion: UInt32;

// Get the total number of modules in the library.
function ProvenModuleCount: UInt32;

// Returns a human-readable name for a status code.
function ProvenStatusName(Status: TProvenStatus): AnsiString;

implementation

function ProvenInitialize: Boolean;
begin
  Result := (proven_init = PROVEN_OK);
end;

procedure ProvenFinalize;
begin
  proven_deinit;
end;

function ProvenIsInitialized: Boolean;
begin
  Result := Boolean(proven_is_initialized);
end;

function ProvenABIVersion: UInt32;
begin
  Result := proven_ffi_abi_version;
end;

function ProvenMajorVersion: UInt32;
begin
  Result := proven_version_major;
end;

function ProvenMinorVersion: UInt32;
begin
  Result := proven_version_minor;
end;

function ProvenPatchVersion: UInt32;
begin
  Result := proven_version_patch;
end;

function ProvenModuleCount: UInt32;
begin
  Result := proven_module_count;
end;

function ProvenStatusName(Status: TProvenStatus): AnsiString;
begin
  case Status of
    PROVEN_OK:                    Result := 'OK';
    PROVEN_ERR_NULL_POINTER:      Result := 'ERR_NULL_POINTER';
    PROVEN_ERR_INVALID_ARGUMENT:  Result := 'ERR_INVALID_ARGUMENT';
    PROVEN_ERR_OVERFLOW:          Result := 'ERR_OVERFLOW';
    PROVEN_ERR_UNDERFLOW:         Result := 'ERR_UNDERFLOW';
    PROVEN_ERR_DIVISION_BY_ZERO:  Result := 'ERR_DIVISION_BY_ZERO';
    PROVEN_ERR_PARSE_FAILURE:     Result := 'ERR_PARSE_FAILURE';
    PROVEN_ERR_VALIDATION_FAILED: Result := 'ERR_VALIDATION_FAILED';
    PROVEN_ERR_OUT_OF_BOUNDS:     Result := 'ERR_OUT_OF_BOUNDS';
    PROVEN_ERR_ENCODING_ERROR:    Result := 'ERR_ENCODING_ERROR';
    PROVEN_ERR_ALLOCATION_FAILED: Result := 'ERR_ALLOCATION_FAILED';
    PROVEN_ERR_NOT_IMPLEMENTED:   Result := 'ERR_NOT_IMPLEMENTED';
  else
    Result := 'UNKNOWN_STATUS(' + AnsiString(IntToStr(Status)) + ')';
  end;
end;

end.
