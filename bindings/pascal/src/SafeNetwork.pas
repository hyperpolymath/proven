// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

// SafeNetwork - Safe network operations wrapper for libproven.
//
// Provides IPv4 address parsing, private address detection, and loopback
// detection via the formally verified Idris 2 core. This unit ONLY wraps
// FFI calls; it does NOT reimplement any logic.
//
// Compatible with Free Pascal (FPC) and Delphi.

unit SafeNetwork;

{$IFDEF FPC}
  {$MODE OBJFPC}{$H+}
{$ENDIF}

interface

uses
  LibProven;

type
  // Parsed IPv4 address in Pascal-native form.
  TIPv4Address = record
    Octets: array[0..3] of Byte;
  end;

  // High-level result for IPv4 parsing.
  TSafeIPv4Result = record
    Success: Boolean;
    Address: TIPv4Address;
    ErrorCode: Int32;
  end;

// Parse an IPv4 address string (e.g., "192.168.1.1").
// Calls proven_network_parse_ipv4 via FFI.
function ParseIPv4(const IPStr: AnsiString): TSafeIPv4Result;

// Check if an IPv4 address is private (RFC 1918).
// Private ranges: 10.x.x.x, 172.16-31.x.x, 192.168.x.x.
// Calls proven_network_ipv4_is_private via FFI.
function IsPrivateIPv4(const Addr: TIPv4Address): Boolean;

// Check if an IPv4 address is loopback (127.0.0.0/8).
// Calls proven_network_ipv4_is_loopback via FFI.
function IsLoopbackIPv4(const Addr: TIPv4Address): Boolean;

// Convenience: Parse and check if an IP string is private in one call.
// Returns False if parsing fails.
function IsPrivateIPv4Str(const IPStr: AnsiString): Boolean;

// Convenience: Parse and check if an IP string is loopback in one call.
// Returns False if parsing fails.
function IsLoopbackIPv4Str(const IPStr: AnsiString): Boolean;

implementation

function ParseIPv4(const IPStr: AnsiString): TSafeIPv4Result;
var
  R: TProvenIPv4Result;
begin
  if Length(IPStr) = 0 then
  begin
    Result.Success := False;
    Result.ErrorCode := PROVEN_ERR_INVALID_ARGUMENT;
    FillChar(Result.Address, SizeOf(Result.Address), 0);
    Exit;
  end;

  R := proven_network_parse_ipv4(PByte(PAnsiChar(IPStr)), NativeUInt(Length(IPStr)));
  Result.Success := (R.Status = PROVEN_OK);

  if Result.Success then
  begin
    Result.ErrorCode := PROVEN_OK;
    Move(R.Address.Octets, Result.Address.Octets, 4);
  end
  else
  begin
    Result.ErrorCode := R.Status;
    FillChar(Result.Address, SizeOf(Result.Address), 0);
  end;
end;

function IsPrivateIPv4(const Addr: TIPv4Address): Boolean;
var
  CAddr: TProvenIPv4Address;
begin
  Move(Addr.Octets, CAddr.Octets, 4);
  Result := Boolean(proven_network_ipv4_is_private(CAddr));
end;

function IsLoopbackIPv4(const Addr: TIPv4Address): Boolean;
var
  CAddr: TProvenIPv4Address;
begin
  Move(Addr.Octets, CAddr.Octets, 4);
  Result := Boolean(proven_network_ipv4_is_loopback(CAddr));
end;

function IsPrivateIPv4Str(const IPStr: AnsiString): Boolean;
var
  ParseResult: TSafeIPv4Result;
begin
  ParseResult := ParseIPv4(IPStr);
  if ParseResult.Success then
    Result := IsPrivateIPv4(ParseResult.Address)
  else
    Result := False;
end;

function IsLoopbackIPv4Str(const IPStr: AnsiString): Boolean;
var
  ParseResult: TSafeIPv4Result;
begin
  ParseResult := ParseIPv4(IPStr);
  if ParseResult.Success then
    Result := IsLoopbackIPv4(ParseResult.Address)
  else
    Result := False;
end;

end.
