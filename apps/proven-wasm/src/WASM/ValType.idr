-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
--
-- WebAssembly Value Types (WASM Spec Section 2.3.1)
--
-- Defines the 7 WASM value types (4 numeric, 1 vector, 2 reference)
-- with binary encoding, classification, size information, and
-- Eq/Show instances.  Unknown type bytes are rejected as Nothing.

module WASM.ValType

%default total

-- ============================================================================
-- Value Types (WASM Spec Section 2.3.1)
-- ============================================================================

||| WebAssembly value types.
||| These are the types that can appear in function signatures,
||| local variables, and on the operand stack.
public export
data ValType : Type where
  ||| 32-bit integer
  I32       : ValType
  ||| 64-bit integer
  I64       : ValType
  ||| 32-bit IEEE 754 floating-point
  F32       : ValType
  ||| 64-bit IEEE 754 floating-point
  F64       : ValType
  ||| 128-bit SIMD vector (WASM SIMD proposal)
  V128      : ValType
  ||| Reference to a function
  FuncRef   : ValType
  ||| Reference to an external host object
  ExternRef : ValType

public export
Eq ValType where
  I32       == I32       = True
  I64       == I64       = True
  F32       == F32       = True
  F64       == F64       = True
  V128      == V128      = True
  FuncRef   == FuncRef   = True
  ExternRef == ExternRef = True
  _         == _         = False

public export
Show ValType where
  show I32       = "i32"
  show I64       = "i64"
  show F32       = "f32"
  show F64       = "f64"
  show V128      = "v128"
  show FuncRef   = "funcref"
  show ExternRef = "externref"

-- ============================================================================
-- Binary Encoding (WASM Spec Section 5.3.1)
-- ============================================================================

||| Encode a value type to its binary representation.
||| These are the single-byte type codes used in the WASM binary format.
public export
valTypeToByte : ValType -> Bits8
valTypeToByte I32       = 0x7F
valTypeToByte I64       = 0x7E
valTypeToByte F32       = 0x7D
valTypeToByte F64       = 0x7C
valTypeToByte V128      = 0x7B
valTypeToByte FuncRef   = 0x70
valTypeToByte ExternRef = 0x6F

||| Decode a binary type byte to a value type.
||| Returns Nothing for unknown type bytes — no crash.
public export
valTypeFromByte : Bits8 -> Maybe ValType
valTypeFromByte 0x7F = Just I32
valTypeFromByte 0x7E = Just I64
valTypeFromByte 0x7D = Just F32
valTypeFromByte 0x7C = Just F64
valTypeFromByte 0x7B = Just V128
valTypeFromByte 0x70 = Just FuncRef
valTypeFromByte 0x6F = Just ExternRef
valTypeFromByte _    = Nothing

-- ============================================================================
-- Classification
-- ============================================================================

||| Check if a value type is a numeric type (i32, i64, f32, f64).
public export
isNumType : ValType -> Bool
isNumType I32 = True
isNumType I64 = True
isNumType F32 = True
isNumType F64 = True
isNumType _   = False

||| Check if a value type is a vector type (v128).
public export
isVecType : ValType -> Bool
isVecType V128 = True
isVecType _    = False

||| Check if a value type is a reference type (funcref, externref).
public export
isRefType : ValType -> Bool
isRefType FuncRef   = True
isRefType ExternRef = True
isRefType _         = False

||| Check if a value type is an integer type (i32, i64).
public export
isIntType : ValType -> Bool
isIntType I32 = True
isIntType I64 = True
isIntType _   = False

||| Check if a value type is a floating-point type (f32, f64).
public export
isFloatType : ValType -> Bool
isFloatType F32 = True
isFloatType F64 = True
isFloatType _   = False

-- ============================================================================
-- Size Information
-- ============================================================================

||| Get the size of a value type in bytes.
||| Reference types have no fixed size (represented as 0).
public export
valTypeSize : ValType -> Nat
valTypeSize I32       = 4
valTypeSize I64       = 8
valTypeSize F32       = 4
valTypeSize F64       = 8
valTypeSize V128      = 16
valTypeSize FuncRef   = 0  -- Reference, not a fixed-size value
valTypeSize ExternRef = 0  -- Reference, not a fixed-size value

||| Get the size of a value type in bits.
public export
valTypeBits : ValType -> Nat
valTypeBits t = valTypeSize t * 8

||| Get the WAT (WebAssembly Text Format) name for a value type.
public export
valTypeWat : ValType -> String
valTypeWat = show  -- WAT names match our Show instance

||| Get the default value for a numeric type as a list of zero bytes.
||| Reference types have no default (Nothing).
public export
defaultValue : ValType -> Maybe (List Bits8)
defaultValue I32       = Just (replicate 4 0x00)
defaultValue I64       = Just (replicate 8 0x00)
defaultValue F32       = Just (replicate 4 0x00)
defaultValue F64       = Just (replicate 8 0x00)
defaultValue V128      = Just (replicate 16 0x00)
defaultValue FuncRef   = Nothing
defaultValue ExternRef = Nothing
