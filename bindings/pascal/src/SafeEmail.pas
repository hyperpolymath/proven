// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

// SafeEmail - Safe email validation wrapper for libproven.
//
// Provides email address validation (RFC 5321 simplified) via the formally
// verified Idris 2 core. This unit ONLY wraps FFI calls; it does NOT
// reimplement any logic.
//
// Compatible with Free Pascal (FPC) and Delphi.

unit SafeEmail;

{$IFDEF FPC}
  {$MODE OBJFPC}{$H+}
{$ENDIF}

interface

uses
  LibProven;

type
  // High-level result for email validation.
  TSafeEmailResult = record
    Success: Boolean;
    IsValid: Boolean;
    ErrorCode: Int32;
  end;

// Validate an email address (RFC 5321 simplified).
// Calls proven_email_is_valid via FFI.
function ValidateEmail(const Email: AnsiString): TSafeEmailResult;

implementation

function ValidateEmail(const Email: AnsiString): TSafeEmailResult;
var
  R: TProvenBoolResult;
begin
  if Length(Email) = 0 then
  begin
    Result.Success := True;
    Result.IsValid := False;
    Result.ErrorCode := PROVEN_OK;
    Exit;
  end;
  R := proven_email_is_valid(PByte(PAnsiChar(Email)), NativeUInt(Length(Email)));
  Result.Success := (R.Status = PROVEN_OK);
  if Result.Success then
  begin
    Result.IsValid := Boolean(R.Value);
    Result.ErrorCode := PROVEN_OK;
  end
  else
  begin
    Result.IsValid := False;
    Result.ErrorCode := R.Status;
  end;
end;

end.
