-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
||| SafeTPU - Tensor processing unit safety
|||
||| This module provides type-safe configuration and validation for tensor
||| processing hardware. It covers TPU versioning, numerical precision
||| selection, tensor shape validation, and memory budget enforcement.
||| All operations are pure and total — no runtime exceptions.
module Proven.SafeTPU

import Data.List

%default total

--------------------------------------------------------------------------------
-- TPU Versions
--------------------------------------------------------------------------------

||| Google TPU hardware generations and variants
public export
data TPUVersion = V2 | V3 | V4 | V5 | Edge

public export
Eq TPUVersion where
  V2   == V2   = True
  V3   == V3   = True
  V4   == V4   = True
  V5   == V5   = True
  Edge == Edge = True
  _    == _    = False

public export
Show TPUVersion where
  show V2   = "TPU v2"
  show V3   = "TPU v3"
  show V4   = "TPU v4"
  show V5   = "TPU v5"
  show Edge = "TPU Edge"

--------------------------------------------------------------------------------
-- Numerical Precision
--------------------------------------------------------------------------------

||| Numerical precision formats for tensor operations
public export
data Precision = FP32 | FP16 | BF16 | INT8 | INT4

public export
Eq Precision where
  FP32 == FP32 = True
  FP16 == FP16 = True
  BF16 == BF16 = True
  INT8 == INT8 = True
  INT4 == INT4 = True
  _    == _    = False

public export
Show Precision where
  show FP32 = "FP32"
  show FP16 = "FP16"
  show BF16 = "BF16"
  show INT8 = "INT8"
  show INT4 = "INT4"

||| Ordering on precision by bit width (descending precision)
public export
Ord Precision where
  compare a b = compare (precBits a) (precBits b)
  where
    precBits : Precision -> Nat
    precBits FP32 = 32
    precBits FP16 = 16
    precBits BF16 = 16
    precBits INT8 = 8
    precBits INT4 = 4

||| Number of bits for a given precision format
public export
bitsPerElement : Precision -> Nat
bitsPerElement FP32 = 32
bitsPerElement FP16 = 16
bitsPerElement BF16 = 16
bitsPerElement INT8 = 8
bitsPerElement INT4 = 4

--------------------------------------------------------------------------------
-- Tensor Shape
--------------------------------------------------------------------------------

||| A tensor shape with a non-empty list of dimensions and an element type
||| @ dims    List of dimension sizes (must be non-empty, each > 0)
||| @ elemType Numerical precision of tensor elements
public export
record TensorShape where
  constructor MkTensorShape
  dims     : List Nat
  elemType : Precision

public export
Eq TensorShape where
  a == b = a.dims == b.dims && a.elemType == b.elemType

public export
Show TensorShape where
  show ts = "Tensor(" ++ showDims ts.dims ++ ", " ++ show ts.elemType ++ ")"
  where
    showDims : List Nat -> String
    showDims [] = "[]"
    showDims [x] = show x
    showDims (x :: xs) = show x ++ "x" ++ showDims xs

||| Total number of elements in a tensor (product of all dimensions)
public export
totalElements : TensorShape -> Nat
totalElements ts = foldl (*) 1 ts.dims

||| Total memory required in bytes (rounded up from bits)
public export
tensorBytes : TensorShape -> Nat
tensorBytes ts =
  let totalBits = totalElements ts * bitsPerElement ts.elemType
  in (totalBits + 7) `div` 8

--------------------------------------------------------------------------------
-- TPU Configuration
--------------------------------------------------------------------------------

||| TPU hardware configuration
||| @ version    TPU hardware generation
||| @ coreCount  Number of active cores (must be positive)
||| @ memoryLimit Maximum available memory in bytes
public export
record TPUConfig where
  constructor MkTPUConfig
  version     : TPUVersion
  coreCount   : Nat
  memoryLimit : Nat

public export
Show TPUConfig where
  show cfg = "TPUConfig(" ++ show cfg.version
          ++ ", cores=" ++ show cfg.coreCount
          ++ ", mem=" ++ show cfg.memoryLimit ++ "B)"

--------------------------------------------------------------------------------
-- Validation
--------------------------------------------------------------------------------

||| Validate a tensor shape: non-empty dimensions, all positive
public export
validateTensorShape : TensorShape -> Maybe TensorShape
validateTensorShape ts =
  case ts.dims of
    [] => Nothing
    ds => if all (\d => d >= 1) ds then Just ts else Nothing

||| Check whether a tensor fits within a memory budget (in bytes)
public export
fitsInMemory : TensorShape -> (budget : Nat) -> Bool
fitsInMemory ts budget = tensorBytes ts <= budget

||| Check whether a precision format is supported on a given TPU version
||| Edge TPUs only support INT8 and INT4; all others support everything
public export
isPrecisionSupported : TPUVersion -> Precision -> Bool
isPrecisionSupported Edge INT8 = True
isPrecisionSupported Edge INT4 = True
isPrecisionSupported Edge _    = False
isPrecisionSupported _    _    = True
