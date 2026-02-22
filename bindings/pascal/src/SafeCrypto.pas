// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

// SafeCrypto - Safe cryptographic operations wrapper for libproven.
//
// Provides constant-time comparison, secure random byte generation, hex
// encoding/decoding, and CRC32 checksum operations via the formally verified
// Idris 2 core. This unit ONLY wraps FFI calls; it does NOT reimplement
// any logic.
//
// Compatible with Free Pascal (FPC) and Delphi.

unit SafeCrypto;

{$IFDEF FPC}
  {$MODE OBJFPC}{$H+}
{$ENDIF}

interface

uses
  LibProven;

type
  // High-level result for boolean crypto operations.
  TSafeCryptoBoolResult = record
    Success: Boolean;
    Value: Boolean;
    ErrorCode: Int32;
  end;

  // High-level result for string crypto operations.
  TSafeCryptoStringResult = record
    Success: Boolean;
    Value: AnsiString;
    ErrorCode: Int32;
  end;

  // High-level result for byte array crypto operations.
  TSafeCryptoBytesResult = record
    Success: Boolean;
    Data: array of Byte;
    ErrorCode: Int32;
  end;

  // High-level result for CRC32 checksum operations.
  TSafeCRC32Result = record
    Success: Boolean;
    Value: Int64;
    ErrorCode: Int32;
  end;

// Constant-time byte comparison (timing-attack safe).
// Returns True if both byte sequences are equal.
// Calls proven_crypto_constant_time_eq via FFI.
function ConstantTimeEqual(const A, B: AnsiString): TSafeCryptoBoolResult;

// Generate cryptographically secure random bytes.
// Calls proven_crypto_random_bytes via FFI.
function RandomBytes(Count: NativeUInt): TSafeCryptoBytesResult;

// Encode bytes to hexadecimal string.
// Calls proven_hex_encode via FFI.
function HexEncode(const Data: array of Byte; Uppercase: Boolean = False): TSafeCryptoStringResult;

// Encode a string to hexadecimal.
// Calls proven_hex_encode via FFI.
function HexEncodeString(const S: AnsiString; Uppercase: Boolean = False): TSafeCryptoStringResult;

// Decode hexadecimal string to bytes.
// Calls proven_hex_decode via FFI.
function HexDecode(const HexStr: AnsiString): TSafeCryptoBytesResult;

// Calculate CRC32 checksum of data.
// Calls proven_checksum_crc32 via FFI.
function CRC32Checksum(const Data: AnsiString): TSafeCRC32Result;

// Verify CRC32 checksum matches expected value.
// Calls proven_checksum_verify_crc32 via FFI.
function VerifyCRC32(const Data: AnsiString; Expected: UInt32): TSafeCryptoBoolResult;

implementation

function ConstantTimeEqual(const A, B: AnsiString): TSafeCryptoBoolResult;
var
  R: TProvenBoolResult;
  PtrA, PtrB: PByte;
begin
  // Handle empty strings safely.
  if Length(A) = 0 then
    PtrA := nil
  else
    PtrA := PByte(PAnsiChar(A));

  if Length(B) = 0 then
    PtrB := nil
  else
    PtrB := PByte(PAnsiChar(B));

  R := proven_crypto_constant_time_eq(
    PtrA, NativeUInt(Length(A)),
    PtrB, NativeUInt(Length(B))
  );

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

function RandomBytes(Count: NativeUInt): TSafeCryptoBytesResult;
var
  Status: Int32;
begin
  if Count = 0 then
  begin
    Result.Success := True;
    SetLength(Result.Data, 0);
    Result.ErrorCode := PROVEN_OK;
    Exit;
  end;

  SetLength(Result.Data, Count);
  Status := proven_crypto_random_bytes(@Result.Data[0], Count);

  Result.Success := (Status = PROVEN_OK);
  if Result.Success then
    Result.ErrorCode := PROVEN_OK
  else
  begin
    Result.ErrorCode := Status;
    SetLength(Result.Data, 0);
  end;
end;

function HexEncode(const Data: array of Byte; Uppercase: Boolean = False): TSafeCryptoStringResult;
var
  R: TProvenStringResult;
  UCase: ByteBool;
begin
  if Length(Data) = 0 then
  begin
    Result.Success := True;
    Result.Value := '';
    Result.ErrorCode := PROVEN_OK;
    Exit;
  end;

  if Uppercase then
    UCase := ByteBool(True)
  else
    UCase := ByteBool(False);

  R := proven_hex_encode(@Data[0], NativeUInt(Length(Data)), UCase);
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

function HexEncodeString(const S: AnsiString; Uppercase: Boolean = False): TSafeCryptoStringResult;
var
  R: TProvenStringResult;
  UCase: ByteBool;
begin
  if Length(S) = 0 then
  begin
    Result.Success := True;
    Result.Value := '';
    Result.ErrorCode := PROVEN_OK;
    Exit;
  end;

  if Uppercase then
    UCase := ByteBool(True)
  else
    UCase := ByteBool(False);

  R := proven_hex_encode(PByte(PAnsiChar(S)), NativeUInt(Length(S)), UCase);
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

function HexDecode(const HexStr: AnsiString): TSafeCryptoBytesResult;
var
  R: TProvenHexDecodeResult;
begin
  if Length(HexStr) = 0 then
  begin
    Result.Success := True;
    SetLength(Result.Data, 0);
    Result.ErrorCode := PROVEN_OK;
    Exit;
  end;

  R := proven_hex_decode(PByte(PAnsiChar(HexStr)), NativeUInt(Length(HexStr)));
  Result.Success := (R.Status = PROVEN_OK);
  if Result.Success then
  begin
    SetLength(Result.Data, R.Length);
    if R.Length > 0 then
      Move(R.Data^, Result.Data[0], R.Length);
    Result.ErrorCode := PROVEN_OK;
  end
  else
  begin
    SetLength(Result.Data, 0);
    Result.ErrorCode := R.Status;
  end;
  // Free the C-allocated decode result.
  proven_hex_free(@R);
end;

function CRC32Checksum(const Data: AnsiString): TSafeCRC32Result;
var
  R: TProvenIntResult;
begin
  if Length(Data) = 0 then
  begin
    Result.Success := True;
    Result.Value := 0;
    Result.ErrorCode := PROVEN_OK;
    Exit;
  end;

  R := proven_checksum_crc32(PByte(PAnsiChar(Data)), NativeUInt(Length(Data)));
  Result.Success := (R.Status = PROVEN_OK);
  if Result.Success then
  begin
    Result.Value := R.Value;
    Result.ErrorCode := PROVEN_OK;
  end
  else
  begin
    Result.Value := 0;
    Result.ErrorCode := R.Status;
  end;
end;

function VerifyCRC32(const Data: AnsiString; Expected: UInt32): TSafeCryptoBoolResult;
var
  R: TProvenBoolResult;
begin
  if Length(Data) = 0 then
  begin
    Result.Success := True;
    Result.Value := (Expected = 0);
    Result.ErrorCode := PROVEN_OK;
    Exit;
  end;

  R := proven_checksum_verify_crc32(
    PByte(PAnsiChar(Data)), NativeUInt(Length(Data)), Expected
  );
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

end.
