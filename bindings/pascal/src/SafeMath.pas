// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

// SafeMath - Safe mathematical operations wrapper for libproven.
//
// Provides checked integer arithmetic that cannot crash. All computation
// is performed by the formally verified Idris 2 core via the C FFI.
// This unit ONLY wraps FFI calls; it does NOT reimplement any logic.
//
// Compatible with Free Pascal (FPC) and Delphi.

unit SafeMath;

{$IFDEF FPC}
  {$MODE OBJFPC}{$H+}
{$ENDIF}

interface

uses
  LibProven;

type
  // High-level result for safe integer operations.
  // Check Success before accessing Value.
  TSafeIntResult = record
    Success: Boolean;
    Value: Int64;
    ErrorCode: Int32;
  end;

// Checked addition with overflow detection.
// Calls proven_math_add_checked via FFI.
function SafeAdd(A, B: Int64): TSafeIntResult;

// Checked subtraction with underflow detection.
// Calls proven_math_sub_checked via FFI.
function SafeSub(A, B: Int64): TSafeIntResult;

// Checked multiplication with overflow detection.
// Calls proven_math_mul_checked via FFI.
function SafeMul(A, B: Int64): TSafeIntResult;

// Safe integer division. Returns error on division by zero or
// overflow (Int64.MinValue / -1).
// Calls proven_math_div via FFI.
function SafeDiv(Numerator, Denominator: Int64): TSafeIntResult;

// Safe modulo operation. Returns error on division by zero.
// Calls proven_math_mod via FFI.
function SafeMod(Numerator, Denominator: Int64): TSafeIntResult;

// Safe absolute value. Returns error for Int64.MinValue
// (cannot be represented as a positive Int64).
// Calls proven_math_abs_safe via FFI.
function SafeAbs(N: Int64): TSafeIntResult;

// Clamp value to [Lo, Hi] range.
// Calls proven_math_clamp via FFI.
function SafeClamp(Lo, Hi, Value: Int64): Int64;

// Integer exponentiation with overflow checking.
// Exponent must be non-negative.
// Calls proven_math_pow_checked via FFI.
function SafePow(Base: Int64; Exp: UInt32): TSafeIntResult;

implementation

// Convert raw FFI result to high-level Pascal result.
function ToSafeIntResult(const R: TProvenIntResult): TSafeIntResult;
begin
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

function SafeAdd(A, B: Int64): TSafeIntResult;
begin
  Result := ToSafeIntResult(proven_math_add_checked(A, B));
end;

function SafeSub(A, B: Int64): TSafeIntResult;
begin
  Result := ToSafeIntResult(proven_math_sub_checked(A, B));
end;

function SafeMul(A, B: Int64): TSafeIntResult;
begin
  Result := ToSafeIntResult(proven_math_mul_checked(A, B));
end;

function SafeDiv(Numerator, Denominator: Int64): TSafeIntResult;
begin
  Result := ToSafeIntResult(proven_math_div(Numerator, Denominator));
end;

function SafeMod(Numerator, Denominator: Int64): TSafeIntResult;
begin
  Result := ToSafeIntResult(proven_math_mod(Numerator, Denominator));
end;

function SafeAbs(N: Int64): TSafeIntResult;
begin
  Result := ToSafeIntResult(proven_math_abs_safe(N));
end;

function SafeClamp(Lo, Hi, Value: Int64): Int64;
begin
  Result := proven_math_clamp(Lo, Hi, Value);
end;

function SafePow(Base: Int64; Exp: UInt32): TSafeIntResult;
begin
  Result := ToSafeIntResult(proven_math_pow_checked(Base, Exp));
end;

end.
