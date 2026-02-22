// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

// SafeDateTime - Safe ISO 8601 date/time handling wrapper for libproven.
//
// Provides ISO 8601 parsing, formatting, leap year detection, and days-in-month
// calculation via the formally verified Idris 2 core. This unit ONLY wraps
// FFI calls; it does NOT reimplement any logic.
//
// Compatible with Free Pascal (FPC) and Delphi.

unit SafeDateTime;

{$IFDEF FPC}
  {$MODE OBJFPC}{$H+}
{$ENDIF}

interface

uses
  LibProven;

type
  // DateTime components in Pascal-native form.
  TDateTimeComponents = record
    Year: Int32;
    Month: Byte;              // 1-12
    Day: Byte;                // 1-31
    Hour: Byte;               // 0-23
    Minute: Byte;             // 0-59
    Second: Byte;             // 0-59
    Nanosecond: UInt32;
    TzOffsetMinutes: Int16;   // 0 for UTC, negative for west
  end;

  // High-level result for DateTime parsing.
  TSafeDateTimeResult = record
    Success: Boolean;
    DateTime: TDateTimeComponents;
    ErrorCode: Int32;
  end;

  // High-level result for DateTime formatting.
  TSafeDateTimeStringResult = record
    Success: Boolean;
    Value: AnsiString;
    ErrorCode: Int32;
  end;

// Parse an ISO 8601 date string.
// Supports: YYYY-MM-DD, YYYY-MM-DDTHH:MM:SS, YYYY-MM-DDTHH:MM:SSZ,
// YYYY-MM-DDTHH:MM:SS+HH:MM
// Calls proven_datetime_parse via FFI.
function ParseISO8601(const DateStr: AnsiString): TSafeDateTimeResult;

// Format DateTime components as an ISO 8601 string.
// Calls proven_datetime_format_iso8601 via FFI.
function FormatISO8601(const DT: TDateTimeComponents): TSafeDateTimeStringResult;

// Check if a year is a leap year.
// Calls proven_datetime_is_leap_year via FFI.
function IsLeapYear(Year: Int32): Boolean;

// Get the number of days in a given month.
// Returns 0 if the month is invalid.
// Calls proven_datetime_days_in_month via FFI.
function DaysInMonth(Year: Int32; Month: Byte): Byte;

implementation

function ParseISO8601(const DateStr: AnsiString): TSafeDateTimeResult;
var
  R: TProvenDateTimeResult;
begin
  if Length(DateStr) = 0 then
  begin
    Result.Success := False;
    Result.ErrorCode := PROVEN_ERR_INVALID_ARGUMENT;
    FillChar(Result.DateTime, SizeOf(Result.DateTime), 0);
    Exit;
  end;

  R := proven_datetime_parse(PByte(PAnsiChar(DateStr)), NativeUInt(Length(DateStr)));
  Result.Success := (R.Status = PROVEN_OK);

  if Result.Success then
  begin
    Result.ErrorCode := PROVEN_OK;
    Result.DateTime.Year := R.DateTime.Year;
    Result.DateTime.Month := R.DateTime.Month;
    Result.DateTime.Day := R.DateTime.Day;
    Result.DateTime.Hour := R.DateTime.Hour;
    Result.DateTime.Minute := R.DateTime.Minute;
    Result.DateTime.Second := R.DateTime.Second;
    Result.DateTime.Nanosecond := R.DateTime.Nanosecond;
    Result.DateTime.TzOffsetMinutes := R.DateTime.TzOffsetMinutes;
  end
  else
  begin
    Result.ErrorCode := R.Status;
    FillChar(Result.DateTime, SizeOf(Result.DateTime), 0);
  end;
end;

function FormatISO8601(const DT: TDateTimeComponents): TSafeDateTimeStringResult;
var
  CDT: TProvenDateTime;
  R: TProvenStringResult;
begin
  // Convert Pascal record to C-compatible record.
  CDT.Year := DT.Year;
  CDT.Month := DT.Month;
  CDT.Day := DT.Day;
  CDT.Hour := DT.Hour;
  CDT.Minute := DT.Minute;
  CDT.Second := DT.Second;
  CDT.Nanosecond := DT.Nanosecond;
  CDT.TzOffsetMinutes := DT.TzOffsetMinutes;

  R := proven_datetime_format_iso8601(CDT);
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

function IsLeapYear(Year: Int32): Boolean;
begin
  Result := Boolean(proven_datetime_is_leap_year(Year));
end;

function DaysInMonth(Year: Int32; Month: Byte): Byte;
begin
  Result := proven_datetime_days_in_month(Year, Month);
end;

end.
