// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

// SafeString - Safe string operations wrapper for libproven.
//
// Provides UTF-8 validation, SQL/HTML/JS escaping via the formally verified
// Idris 2 core. This unit ONLY wraps FFI calls; it does NOT reimplement
// any logic.
//
// Compatible with Free Pascal (FPC) and Delphi.

unit SafeString;

{$IFDEF FPC}
  {$MODE OBJFPC}{$H+}
{$ENDIF}

interface

uses
  LibProven;

type
  // High-level result for safe string operations.
  // Check Success before accessing Value.
  TSafeStringResult = record
    Success: Boolean;
    Value: AnsiString;
    ErrorCode: Int32;
  end;

  // High-level result for boolean validation operations.
  TSafeBoolResult = record
    Success: Boolean;
    Value: Boolean;
    ErrorCode: Int32;
  end;

// Check if a string contains valid UTF-8 data.
// Calls proven_string_is_valid_utf8 via FFI.
function IsValidUTF8(const S: AnsiString): TSafeBoolResult;

// Escape a string for safe SQL embedding (single-quote escaping).
// Prefer parameterized queries over string escaping.
// Calls proven_string_escape_sql via FFI.
function EscapeSQL(const S: AnsiString): TSafeStringResult;

// Escape a string for safe HTML embedding (prevents XSS).
// Calls proven_string_escape_html via FFI.
function EscapeHTML(const S: AnsiString): TSafeStringResult;

// Escape a string for safe JavaScript string literal embedding.
// Calls proven_string_escape_js via FFI.
function EscapeJS(const S: AnsiString): TSafeStringResult;

implementation

// Convert raw FFI string result to high-level Pascal result.
// Frees the C-allocated string after copying into a Pascal AnsiString.
function ToSafeStringResult(const R: TProvenStringResult): TSafeStringResult;
begin
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
  // Always free the C-allocated string if it was allocated.
  if R.Value <> nil then
    proven_free_string(R.Value);
end;

// Convert raw FFI bool result to high-level Pascal result.
function ToSafeBoolResult(const R: TProvenBoolResult): TSafeBoolResult;
begin
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

function IsValidUTF8(const S: AnsiString): TSafeBoolResult;
begin
  if Length(S) = 0 then
  begin
    // Empty strings are trivially valid UTF-8.
    Result.Success := True;
    Result.Value := True;
    Result.ErrorCode := PROVEN_OK;
    Exit;
  end;
  Result := ToSafeBoolResult(
    proven_string_is_valid_utf8(PByte(PAnsiChar(S)), NativeUInt(Length(S)))
  );
end;

function EscapeSQL(const S: AnsiString): TSafeStringResult;
begin
  if Length(S) = 0 then
  begin
    Result.Success := True;
    Result.Value := '';
    Result.ErrorCode := PROVEN_OK;
    Exit;
  end;
  Result := ToSafeStringResult(
    proven_string_escape_sql(PByte(PAnsiChar(S)), NativeUInt(Length(S)))
  );
end;

function EscapeHTML(const S: AnsiString): TSafeStringResult;
begin
  if Length(S) = 0 then
  begin
    Result.Success := True;
    Result.Value := '';
    Result.ErrorCode := PROVEN_OK;
    Exit;
  end;
  Result := ToSafeStringResult(
    proven_string_escape_html(PByte(PAnsiChar(S)), NativeUInt(Length(S)))
  );
end;

function EscapeJS(const S: AnsiString): TSafeStringResult;
begin
  if Length(S) = 0 then
  begin
    Result.Success := True;
    Result.Value := '';
    Result.ErrorCode := PROVEN_OK;
    Exit;
  end;
  Result := ToSafeStringResult(
    proven_string_escape_js(PByte(PAnsiChar(S)), NativeUInt(Length(S)))
  );
end;

end.
