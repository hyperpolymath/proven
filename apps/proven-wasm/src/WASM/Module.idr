-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
--
-- WebAssembly Module (WASM Spec Section 2.5)
--
-- Defines the WASM module structure with type section, function section,
-- memory section, export section, and code section.  Includes validation
-- for export names and module well-formedness.

module WASM.Module

import WASM.ValType
import WASM.Instruction
import WASM.Memory
import WASM.Types

%default total

-- ============================================================================
-- Function Definition
-- ============================================================================

||| A WASM function definition: type index, local declarations, and body.
public export
record FuncDef where
  constructor MkFuncDef
  ||| Index into the type section (identifies the function's signature)
  typeIdx : Nat
  ||| Local variable declarations (types of additional locals)
  locals  : List ValType
  ||| Function body (list of instructions)
  body    : List Instruction

public export
Show FuncDef where
  show fd = "func type=" ++ show fd.typeIdx
            ++ " locals=" ++ show (length fd.locals)
            ++ " body=" ++ show (length fd.body) ++ " instrs"

-- ============================================================================
-- Module Record (WASM Spec Section 2.5)
-- ============================================================================

||| A complete WebAssembly module with all sections.
||| This is the in-memory representation before binary encoding.
public export
record WASMModule where
  constructor MkModule
  ||| Type section: function type signatures
  types     : List FuncType
  ||| Function section: function definitions
  funcs     : List FuncDef
  ||| Memory section: linear memory configurations
  memories  : List MemoryLimits
  ||| Export section: exported functions, memories, tables, globals
  exports   : List Export
  ||| Global section: global variable definitions
  globals   : List (GlobalType, List Instruction)
  ||| Table section: table definitions
  tables    : List TableType

public export
Show WASMModule where
  show m = "WASMModule(types=" ++ show (length m.types)
           ++ " funcs=" ++ show (length m.funcs)
           ++ " memories=" ++ show (length m.memories)
           ++ " exports=" ++ show (length m.exports)
           ++ " globals=" ++ show (length m.globals)
           ++ " tables=" ++ show (length m.tables) ++ ")"

||| Create an empty WASM module with no sections.
public export
emptyModule : WASMModule
emptyModule = MkModule
  { types    = []
  , funcs    = []
  , memories = []
  , exports  = []
  , globals  = []
  , tables   = []
  }

-- ============================================================================
-- Module Building
-- ============================================================================

||| Add a function type to the module's type section.
||| Returns the index of the newly added type.
public export
addType : FuncType -> WASMModule -> (WASMModule, Nat)
addType ft m =
  let idx = length m.types
  in ({ types $= (++ [ft]) } m, idx)

||| Add a function definition to the module.
||| Returns the index of the newly added function.
public export
addFunc : FuncDef -> WASMModule -> (WASMModule, Nat)
addFunc fd m =
  let idx = length m.funcs
  in ({ funcs $= (++ [fd]) } m, idx)

||| Add a memory to the module.
public export
addMemory : MemoryLimits -> WASMModule -> WASMModule
addMemory ml m = { memories $= (++ [ml]) } m

||| Add an export to the module.
public export
addExport : Export -> WASMModule -> WASMModule
addExport e m = { exports $= (++ [e]) } m

||| Add a global variable to the module.
public export
addGlobal : GlobalType -> List Instruction -> WASMModule -> WASMModule
addGlobal gt init m = { globals $= (++ [(gt, init)]) } m

-- ============================================================================
-- Module Validation (WASM Spec Section 3)
-- ============================================================================

||| Module validation errors.
public export
data ModuleError : Type where
  ||| Export name is empty
  EmptyExportName     : ModuleError
  ||| Duplicate export name
  DuplicateExport     : (name : String) -> ModuleError
  ||| Function references a non-existent type index
  InvalidTypeIndex    : (funcIdx : Nat) -> (typeIdx : Nat) -> ModuleError
  ||| Too many functions (exceeds implementation limit)
  TooManyFunctions    : (count : Nat) -> ModuleError
  ||| Multiple memories (WASM 1.0 allows at most 1)
  TooManyMemories     : (count : Nat) -> ModuleError
  ||| Table element type is not a reference type
  InvalidTableElemType : ModuleError

public export
Show ModuleError where
  show EmptyExportName           = "Export name must not be empty"
  show (DuplicateExport n)       = "Duplicate export name: " ++ n
  show (InvalidTypeIndex fi ti)  = "Function " ++ show fi
                                   ++ " references invalid type " ++ show ti
  show (TooManyFunctions c)      = "Too many functions: " ++ show c
  show (TooManyMemories c)       = "Too many memories: " ++ show c
  show InvalidTableElemType      = "Table element type must be a reference type"

||| Maximum number of functions allowed (implementation limit).
public export
maxFunctions : Nat
maxFunctions = 1000000

||| Validate a WASM module.
||| Returns a list of all validation errors found (empty = valid).
public export
validateModule : WASMModule -> List ModuleError
validateModule m =
  let -- Check export names
      emptyExports = if any (\e => e.name == "") m.exports
                       then [EmptyExportName] else []
      -- Check duplicate exports
      dupExports = findDups (map (.name) m.exports)
      -- Check function count
      funcCount = if length m.funcs > maxFunctions
                    then [TooManyFunctions (length m.funcs)] else []
      -- Check memory count (WASM 1.0: at most 1)
      memCount = if length m.memories > 1
                   then [TooManyMemories (length m.memories)] else []
      -- Check type indices
      typeErrors = checkTypeIndices 0 m.funcs (length m.types)
      -- Check table element types
      tableErrors = if any (\t => not (validateTableType t)) m.tables
                      then [InvalidTableElemType] else []
  in emptyExports ++ dupExports ++ funcCount ++ memCount ++ typeErrors ++ tableErrors
  where
    findDups : List String -> List ModuleError
    findDups [] = []
    findDups (n :: ns) = if any (== n) ns
                           then DuplicateExport n :: findDups ns
                           else findDups ns
    checkTypeIndices : Nat -> List FuncDef -> Nat -> List ModuleError
    checkTypeIndices _ [] _ = []
    checkTypeIndices idx (fd :: fds) numTypes =
      let err = if fd.typeIdx >= numTypes
                  then [InvalidTypeIndex idx fd.typeIdx]
                  else []
      in err ++ checkTypeIndices (S idx) fds numTypes

-- ============================================================================
-- Binary Encoding (Header)
-- ============================================================================

||| The WASM binary magic number: \0asm
public export
wasmMagic : List Bits8
wasmMagic = [0x00, 0x61, 0x73, 0x6D]

||| The WASM binary version: 1 (little-endian 32-bit)
public export
wasmVersion : List Bits8
wasmVersion = [0x01, 0x00, 0x00, 0x00]

||| The complete 8-byte WASM module header (magic + version).
public export
wasmHeader : List Bits8
wasmHeader = wasmMagic ++ wasmVersion

-- ============================================================================
-- Section IDs (WASM Spec Section 5.5)
-- ============================================================================

||| WASM binary section identifiers.
public export
data SectionId : Type where
  CustomSection   : SectionId  -- 0
  TypeSection     : SectionId  -- 1
  ImportSection   : SectionId  -- 2
  FunctionSection : SectionId  -- 3
  TableSection    : SectionId  -- 4
  MemorySection   : SectionId  -- 5
  GlobalSection   : SectionId  -- 6
  ExportSection   : SectionId  -- 7
  StartSection    : SectionId  -- 8
  ElementSection  : SectionId  -- 9
  CodeSection     : SectionId  -- 10
  DataSection     : SectionId  -- 11

||| Encode a section ID to its byte value.
public export
sectionIdToByte : SectionId -> Bits8
sectionIdToByte CustomSection   = 0
sectionIdToByte TypeSection     = 1
sectionIdToByte ImportSection   = 2
sectionIdToByte FunctionSection = 3
sectionIdToByte TableSection    = 4
sectionIdToByte MemorySection   = 5
sectionIdToByte GlobalSection   = 6
sectionIdToByte ExportSection   = 7
sectionIdToByte StartSection    = 8
sectionIdToByte ElementSection  = 9
sectionIdToByte CodeSection     = 10
sectionIdToByte DataSection     = 11
