// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

// SafeJson - Safe JSON validation and type detection wrapper for libproven.
//
// Provides JSON validity checking and root-level type detection via the
// formally verified Idris 2 core. This unit ONLY wraps FFI calls; it does
// NOT reimplement any logic.
//
// Compatible with Free Pascal (FPC) and Delphi.

unit SafeJson;

{$IFDEF FPC}
  {$MODE OBJFPC}{$H+}
{$ENDIF}

interface

uses
  LibProven;

type
  // JSON value type enumeration.
  TJsonValueType = (
    jvtNull    = 0,
    jvtBool    = 1,
    jvtNumber  = 2,
    jvtString  = 3,
    jvtArray   = 4,
    jvtObject  = 5,
    jvtInvalid = -1
  );

  // High-level result for JSON validation.
  TSafeJsonValidResult = record
    Success: Boolean;
    IsValid: Boolean;
    ErrorCode: Int32;
  end;

  // High-level result for JSON type detection.
  TSafeJsonTypeResult = record
    Success: Boolean;
    ValueType: TJsonValueType;
  end;

// Check if a string contains valid JSON.
// Calls proven_json_is_valid via FFI.
function IsValidJson(const Json: AnsiString): TSafeJsonValidResult;

// Get the JSON value type at the root level.
// Returns jvtInvalid if the string is not valid JSON.
// Calls proven_json_get_type via FFI.
function GetJsonType(const Json: AnsiString): TSafeJsonTypeResult;

// Convenience: Returns a human-readable name for a JSON type.
function JsonTypeName(ValueType: TJsonValueType): AnsiString;

implementation

function IsValidJson(const Json: AnsiString): TSafeJsonValidResult;
var
  R: TProvenBoolResult;
begin
  if Length(Json) = 0 then
  begin
    Result.Success := True;
    Result.IsValid := False;
    Result.ErrorCode := PROVEN_OK;
    Exit;
  end;

  R := proven_json_is_valid(PByte(PAnsiChar(Json)), NativeUInt(Length(Json)));
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

function GetJsonType(const Json: AnsiString): TSafeJsonTypeResult;
var
  TypeCode: Int32;
begin
  if Length(Json) = 0 then
  begin
    Result.Success := False;
    Result.ValueType := jvtInvalid;
    Exit;
  end;

  TypeCode := proven_json_get_type(PByte(PAnsiChar(Json)), NativeUInt(Length(Json)));

  case TypeCode of
    PROVEN_JSON_NULL:    begin Result.Success := True; Result.ValueType := jvtNull; end;
    PROVEN_JSON_BOOL:    begin Result.Success := True; Result.ValueType := jvtBool; end;
    PROVEN_JSON_NUMBER:  begin Result.Success := True; Result.ValueType := jvtNumber; end;
    PROVEN_JSON_STRING:  begin Result.Success := True; Result.ValueType := jvtString; end;
    PROVEN_JSON_ARRAY:   begin Result.Success := True; Result.ValueType := jvtArray; end;
    PROVEN_JSON_OBJECT:  begin Result.Success := True; Result.ValueType := jvtObject; end;
  else
    Result.Success := False;
    Result.ValueType := jvtInvalid;
  end;
end;

function JsonTypeName(ValueType: TJsonValueType): AnsiString;
begin
  case ValueType of
    jvtNull:    Result := 'null';
    jvtBool:    Result := 'boolean';
    jvtNumber:  Result := 'number';
    jvtString:  Result := 'string';
    jvtArray:   Result := 'array';
    jvtObject:  Result := 'object';
    jvtInvalid: Result := 'invalid';
  else
    Result := 'unknown';
  end;
end;

end.
