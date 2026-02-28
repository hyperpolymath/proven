-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
--
-- WebAssembly Type System (WASM Spec Section 2.3)
--
-- Defines function types (params -> results), table types, global
-- types (with mutability), and limit types.  These are the building
-- blocks of the WASM module type section and are used for validation.

module WASM.Types

import WASM.ValType
import WASM.Memory

%default total

-- ============================================================================
-- Function Types (WASM Spec Section 2.3.3)
-- ============================================================================

||| A WASM function type: maps parameter types to result types.
||| In WASM 1.0, a function may return at most one value.
||| In the multi-value proposal, multiple results are allowed.
public export
record FuncType where
  constructor MkFuncType
  ||| Parameter types (consumed from the stack)
  params  : List ValType
  ||| Result types (pushed onto the stack)
  results : List ValType

public export
Eq FuncType where
  a == b = a.params == b.params && a.results == b.results

public export
Show FuncType where
  show ft = "(" ++ showTypes ft.params ++ ") -> ("
            ++ showTypes ft.results ++ ")"
  where
    showTypes : List ValType -> String
    showTypes []        = ""
    showTypes [x]       = show x
    showTypes (x :: xs) = show x ++ ", " ++ showTypes xs

||| The binary encoding prefix for function types (WASM Spec Section 5.3.3).
public export
funcTypeByte : Bits8
funcTypeByte = 0x60

||| Count the number of parameters in a function type.
public export
paramCount : FuncType -> Nat
paramCount ft = length ft.params

||| Count the number of results in a function type.
public export
resultCount : FuncType -> Nat
resultCount ft = length ft.results

-- ============================================================================
-- Mutability (WASM Spec Section 2.3.4)
-- ============================================================================

||| Mutability flag for global variables.
public export
data Mutability : Type where
  ||| Global is immutable (const) — cannot be changed after initialisation
  Immutable : Mutability
  ||| Global is mutable (var) — can be changed with global.set
  Mutable   : Mutability

public export
Eq Mutability where
  Immutable == Immutable = True
  Mutable   == Mutable   = True
  _         == _         = False

public export
Show Mutability where
  show Immutable = "const"
  show Mutable   = "var"

||| Encode mutability to its binary representation.
public export
mutabilityToByte : Mutability -> Bits8
mutabilityToByte Immutable = 0x00
mutabilityToByte Mutable   = 0x01

||| Decode a mutability byte.
public export
mutabilityFromByte : Bits8 -> Maybe Mutability
mutabilityFromByte 0x00 = Just Immutable
mutabilityFromByte 0x01 = Just Mutable
mutabilityFromByte _    = Nothing

-- ============================================================================
-- Global Types (WASM Spec Section 2.3.4)
-- ============================================================================

||| The type of a global variable: a value type plus mutability.
public export
record GlobalType where
  constructor MkGlobalType
  ||| The type of value stored in the global
  valType    : ValType
  ||| Whether the global can be modified after initialisation
  mutability : Mutability

public export
Eq GlobalType where
  a == b = a.valType == b.valType && a.mutability == b.mutability

public export
Show GlobalType where
  show gt = "(" ++ show gt.mutability ++ " " ++ show gt.valType ++ ")"

-- ============================================================================
-- Table Types (WASM Spec Section 2.3.5)
-- ============================================================================

||| The type of a table: a reference type with limits.
||| Tables hold references (funcref or externref) and are used for
||| indirect function calls via `call_indirect`.
public export
record TableType where
  constructor MkTableType
  ||| Element type (must be a reference type)
  elemType : ValType
  ||| Size limits (minimum and maximum number of elements)
  limits   : MemoryLimits

public export
Show TableType where
  show tt = show tt.elemType ++ " " ++ show tt.limits

||| Validate that a table type's element type is a reference type.
public export
validateTableType : TableType -> Bool
validateTableType tt = isRefType tt.elemType

-- ============================================================================
-- Import/Export Descriptors (WASM Spec Section 2.5)
-- ============================================================================

||| The kind of an import or export.
public export
data ExternKind : Type where
  ||| Function import/export
  FuncExtern   : ExternKind
  ||| Table import/export
  TableExtern  : ExternKind
  ||| Memory import/export
  MemExtern    : ExternKind
  ||| Global import/export
  GlobalExtern : ExternKind

public export
Eq ExternKind where
  FuncExtern   == FuncExtern   = True
  TableExtern  == TableExtern  = True
  MemExtern    == MemExtern    = True
  GlobalExtern == GlobalExtern = True
  _            == _            = False

public export
Show ExternKind where
  show FuncExtern   = "func"
  show TableExtern  = "table"
  show MemExtern    = "memory"
  show GlobalExtern = "global"

||| Encode an extern kind to its binary representation.
public export
externKindToByte : ExternKind -> Bits8
externKindToByte FuncExtern   = 0x00
externKindToByte TableExtern  = 0x01
externKindToByte MemExtern    = 0x02
externKindToByte GlobalExtern = 0x03

||| Decode an extern kind from a byte.
public export
externKindFromByte : Bits8 -> Maybe ExternKind
externKindFromByte 0x00 = Just FuncExtern
externKindFromByte 0x01 = Just TableExtern
externKindFromByte 0x02 = Just MemExtern
externKindFromByte 0x03 = Just GlobalExtern
externKindFromByte _    = Nothing

-- ============================================================================
-- Export Record
-- ============================================================================

||| An export declaration: a name and the kind/index of the exported item.
public export
record Export where
  constructor MkExport
  ||| Export name (must be valid UTF-8)
  name      : String
  ||| Kind of the exported item
  kind      : ExternKind
  ||| Index into the corresponding index space
  index     : Nat

public export
Show Export where
  show e = "export \"" ++ e.name ++ "\" " ++ show e.kind
           ++ " " ++ show e.index

||| Validate that an export name is non-empty.
public export
validateExportName : String -> Bool
validateExportName "" = False
validateExportName _  = True
