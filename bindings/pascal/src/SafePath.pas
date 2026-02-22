// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

// SafePath - Safe filesystem path operations wrapper for libproven.
//
// Provides directory traversal detection and filename sanitization via the
// formally verified Idris 2 core. This unit ONLY wraps FFI calls; it does
// NOT reimplement any logic.
//
// Compatible with Free Pascal (FPC) and Delphi.

unit SafePath;

{$IFDEF FPC}
  {$MODE OBJFPC}{$H+}
{$ENDIF}

interface

uses
  LibProven;

type
  // High-level result for path boolean checks.
  TSafePathBoolResult = record
    Success: Boolean;
    Value: Boolean;
    ErrorCode: Int32;
  end;

  // High-level result for path string operations.
  TSafePathStringResult = record
    Success: Boolean;
    Value: AnsiString;
    ErrorCode: Int32;
  end;

// Check if a path contains directory traversal sequences ("..").
// Returns True in Value if traversal was detected (path is dangerous).
// Calls proven_path_has_traversal via FFI.
function HasTraversal(const Path: AnsiString): TSafePathBoolResult;

// Sanitize a filename by removing dangerous characters.
// Returns the sanitized filename.
// Calls proven_path_sanitize_filename via FFI.
function SanitizeFilename(const Filename: AnsiString): TSafePathStringResult;

implementation

function HasTraversal(const Path: AnsiString): TSafePathBoolResult;
var
  R: TProvenBoolResult;
begin
  if Length(Path) = 0 then
  begin
    Result.Success := True;
    Result.Value := False;
    Result.ErrorCode := PROVEN_OK;
    Exit;
  end;
  R := proven_path_has_traversal(PByte(PAnsiChar(Path)), NativeUInt(Length(Path)));
  Result.Success := (R.Status = PROVEN_OK);
  if Result.Success then
  begin
    Result.Value := Boolean(R.Value);
    Result.ErrorCode := PROVEN_OK;
  end
  else
  begin
    Result.Value := False;
    Result.ErrorCode := R.Status;
  end;
end;

function SanitizeFilename(const Filename: AnsiString): TSafePathStringResult;
var
  R: TProvenStringResult;
begin
  if Length(Filename) = 0 then
  begin
    Result.Success := True;
    Result.Value := '';
    Result.ErrorCode := PROVEN_OK;
    Exit;
  end;
  R := proven_path_sanitize_filename(
    PByte(PAnsiChar(Filename)), NativeUInt(Length(Filename))
  );
  Result.Success := (R.Status = PROVEN_OK);
  if Result.Success then
  begin
    SetString(Result.Value, R.Value, R.Length);
    Result.ErrorCode := PROVEN_OK;
  end
  else
  begin
    Result.Value := '';
    Result.ErrorCode := R.Status;
  end;
  if R.Value <> nil then
    proven_free_string(R.Value);
end;

end.
