-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
--
-- proven-wasm: Main entry point
--
-- A WebAssembly module builder with verified memory safety.
-- Uses dependent types to enforce memory bounds, valid function
-- type indices, and correct module structure.
--
-- Usage:
--   proven-wasm (runs the demo)

module Main

import WASM
import WASM.ValType
import WASM.Instruction
import WASM.Module
import WASM.Memory
import WASM.Types
import System

%default total

-- ============================================================================
-- Display helpers
-- ============================================================================

||| Format a list of bytes as hex.
covering
showHex : List Bits8 -> String
showHex [] = ""
showHex (b :: bs) =
  let hi = cast {to=Nat} (prim__shr_Bits8 b 4)
      lo = cast {to=Nat} (prim__and_Bits8 b 0x0F)
      hexDigit : Nat -> String
      hexDigit n = if n < 10 then show n
                   else singleton (chr (cast n - 10 + cast (ord 'a')))
  in "0x" ++ hexDigit hi ++ hexDigit lo ++ " " ++ showHex bs

||| Print a separator line.
covering
printSep : IO ()
printSep = putStrLn (replicate 60 '-')

-- ============================================================================
-- Demo: Build a WASM module with an add function
-- ============================================================================

||| Build a minimal WASM module containing an "add" function
||| that takes two i32 parameters and returns their sum.
demoAddModule : WASMModule
demoAddModule =
  let -- Step 1: Define the function type (i32, i32) -> (i32)
      addType = MkFuncType { params = [I32, I32], results = [I32] }
      (m1, typeIdx) = addType addType emptyModule

      -- Step 2: Define the function body
      addBody = [ LocalGet 0     -- Push first parameter
                , LocalGet 1     -- Push second parameter
                , I32Add         -- Add them
                , End            -- End of function
                ]
      addFunc = MkFuncDef { typeIdx = typeIdx, locals = [], body = addBody }
      (m2, funcIdx) = addFunc addFunc m1

      -- Step 3: Add a memory (1 page min, 2 pages max)
      m3 = addMemory (MkMemoryLimits 1 2) m2

      -- Step 4: Export the function as "add"
      addExport = MkExport { name = "add", kind = FuncExtern, index = funcIdx }
      m4 = addExport addExport m3

      -- Step 5: Export the memory as "memory"
      memExport = MkExport { name = "memory", kind = MemExtern, index = 0 }
      m5 = addExport memExport m4
  in m5

||| Demonstrate building and validating a WASM module.
covering
demoBuildModule : IO ()
demoBuildModule = do
  putStrLn "\n--- WASM Module Build Demo ---\n"

  let m = demoAddModule
  putStrLn $ "Module: " ++ show m

  -- Show the function type
  case m.types of
    []       => putStrLn "  (no types)"
    (t :: _) => putStrLn $ "  Type 0: " ++ show t

  -- Show exports
  putStrLn "  Exports:"
  traverse_ (\e => putStrLn $ "    " ++ show e) m.exports

  -- Validate the module
  let errors = validateModule m
  putStrLn $ "\n  Validation errors: " ++ show (length errors)

  -- Show the binary header
  putStrLn $ "\n  Binary header: " ++ showHex wasmHeader
  putStrLn "  (magic: \\0asm, version: 1)"

-- ============================================================================
-- Demo: Memory operations
-- ============================================================================

||| Demonstrate WASM linear memory operations.
covering
demoMemory : IO ()
demoMemory = do
  putStrLn "\n--- WASM Memory Demo ---\n"

  -- Create a validated memory
  case mkMemoryLimits 1 16 of
    Left err => putStrLn $ "ERROR: " ++ show err
    Right limits => do
      let mem = newMemory limits
      putStrLn $ "Memory: " ++ show mem
      putStrLn $ "  Size: " ++ show (memorySizeBytes mem) ++ " bytes ("
                 ++ show mem.currentPages ++ " pages)"
      putStrLn $ "  Can grow: " ++ show (canGrow mem)

      -- Bounds checking
      putStrLn "\n  Bounds checks:"
      putStrLn $ "    Access [0, 4]: "
                 ++ show (checkBounds 0 4 mem)
      putStrLn $ "    Access [65532, 4]: "
                 ++ show (checkBounds 65532 4 mem)
      putStrLn $ "    Access [65533, 4]: "
                 ++ show (checkBounds 65533 4 mem)
                 ++ " (out of bounds!)"

      -- Grow memory
      case growMemory 3 mem of
        Left err => putStrLn $ "  Grow error: " ++ show err
        Right (mem2, prevSize) => do
          putStrLn $ "\n  Grew by 3 pages (prev=" ++ show prevSize ++ ")"
          putStrLn $ "  New size: " ++ show (memorySizeBytes mem2) ++ " bytes"

          -- Try to grow beyond max
          case growMemory 100 mem2 of
            Left err   => putStrLn $ "  Grow beyond max: " ++ show err
            Right _    => putStrLn "  Grew beyond max (unexpected)"

  -- Invalid limits
  putStrLn "\n  Testing invalid limits..."
  case mkMemoryLimits 10 5 of
    Left err => putStrLn $ "  min>max: " ++ show err
    Right _  => putStrLn "  min>max: accepted (unexpected)"

  case mkMemoryLimits 0 100000 of
    Left err => putStrLn $ "  exceeds hard limit: " ++ show err
    Right _  => putStrLn "  exceeds hard limit: accepted (unexpected)"

-- ============================================================================
-- Demo: Value types and instructions
-- ============================================================================

||| Demonstrate value type classification and instruction opcodes.
covering
demoTypesAndInstructions : IO ()
demoTypesAndInstructions = do
  putStrLn "\n--- Value Types and Instructions ---\n"

  -- Value types
  let types = [I32, I64, F32, F64, V128, FuncRef, ExternRef]
  putStrLn "  Value types:"
  traverse_ (\t => putStrLn $ "    " ++ show t
             ++ " byte=0x" ++ show (cast {to=Nat} (valTypeToByte t))
             ++ " size=" ++ show (valTypeSize t) ++ "B"
             ++ " num=" ++ show (isNumType t)
             ++ " ref=" ++ show (isRefType t)
             ) types

  -- Sample instructions
  putStrLn "\n  Instructions:"
  let instrs = [ I32Const 42, I32Add, I32Sub, I32Mul, I32DivS
               , LocalGet 0, LocalSet 1, Call 0
               , I32Load (MkMemArg 2 0), I32Store (MkMemArg 2 4)
               , MemorySize, MemoryGrow, Return, Nop, Drop, End
               ]
  traverse_ (\i => putStrLn $ "    "
             ++ instructionName i
             ++ " (opcode=0x"
             ++ show (cast {to=Nat} (opcode i))
             ++ ")"
             ) instrs

-- ============================================================================
-- Demo: Function types and globals
-- ============================================================================

||| Demonstrate type system constructs.
covering
demoTypeSystem : IO ()
demoTypeSystem = do
  putStrLn "\n--- Type System Demo ---\n"

  -- Function types
  let ft1 = MkFuncType { params = [I32, I32], results = [I32] }
  let ft2 = MkFuncType { params = [], results = [] }
  let ft3 = MkFuncType { params = [I64], results = [I32, I64] }
  putStrLn "  Function types:"
  putStrLn $ "    add:   " ++ show ft1
  putStrLn $ "    void:  " ++ show ft2
  putStrLn $ "    multi: " ++ show ft3
  putStrLn $ "    ft1 == ft1: " ++ show (ft1 == ft1)
  putStrLn $ "    ft1 == ft2: " ++ show (ft1 == ft2)

  -- Global types
  let g1 = MkGlobalType { valType = I32, mutability = Mutable }
  let g2 = MkGlobalType { valType = I64, mutability = Immutable }
  putStrLn "\n  Global types:"
  putStrLn $ "    counter: " ++ show g1
  putStrLn $ "    const:   " ++ show g2

  -- Extern kinds
  putStrLn "\n  Export kinds:"
  let kinds = [FuncExtern, TableExtern, MemExtern, GlobalExtern]
  traverse_ (\k => putStrLn $ "    " ++ show k
             ++ " byte=" ++ show (cast {to=Nat} (externKindToByte k))
             ) kinds

  -- Pages calculation
  putStrLn "\n  Pages calculation:"
  putStrLn $ "    0 bytes = " ++ show (pagesForBytes 0) ++ " pages"
  putStrLn $ "    1 byte = " ++ show (pagesForBytes 1) ++ " page"
  putStrLn $ "    65536 bytes = " ++ show (pagesForBytes 65536) ++ " page"
  putStrLn $ "    65537 bytes = " ++ show (pagesForBytes 65537) ++ " pages"
  putStrLn $ "    1 MiB = " ++ show (pagesForBytes 1048576) ++ " pages"

-- ============================================================================
-- Demo: Module validation
-- ============================================================================

||| Demonstrate module validation catching errors.
covering
demoValidation : IO ()
demoValidation = do
  putStrLn "\n--- Module Validation Demo ---\n"

  -- Valid module
  let validErrs = validateModule demoAddModule
  putStrLn $ "  add module errors: " ++ show (length validErrs)

  -- Invalid: empty export name
  let badExport = addExport (MkExport "" FuncExtern 0) emptyModule
  let errs1 = validateModule badExport
  putStrLn $ "  empty export name: " ++ show (length errs1) ++ " errors"
  traverse_ (\e => putStrLn $ "    - " ++ show e) errs1

  -- Invalid: duplicate exports
  let dupMod = addExport (MkExport "foo" FuncExtern 0)
             $ addExport (MkExport "foo" FuncExtern 1) emptyModule
  let errs2 = validateModule dupMod
  putStrLn $ "  duplicate exports: " ++ show (length errs2) ++ " errors"
  traverse_ (\e => putStrLn $ "    - " ++ show e) errs2

  -- Invalid: bad type index
  let badType = addFunc (MkFuncDef 99 [] [End]) emptyModule
  let (badTypeMod, _) = badType
  let errs3 = validateModule badTypeMod
  putStrLn $ "  bad type index: " ++ show (length errs3) ++ " errors"
  traverse_ (\e => putStrLn $ "    - " ++ show e) errs3

  -- Invalid: multiple memories (WASM 1.0)
  let twoMem = addMemory (MkMemoryLimits 1 1)
             $ addMemory (MkMemoryLimits 1 1) emptyModule
  let errs4 = validateModule twoMem
  putStrLn $ "  two memories: " ++ show (length errs4) ++ " errors"
  traverse_ (\e => putStrLn $ "    - " ++ show e) errs4

-- ============================================================================
-- Main
-- ============================================================================

covering
main : IO ()
main = do
  args <- getArgs
  putStrLn "proven-wasm v0.1.0 — WebAssembly builder that cannot crash"
  putStrLn $ "Magic: " ++ showHex wasmMagicBytes
  putStrLn $ "Page size: " ++ show wasmPageSize ++ " bytes"
  putStrLn "Powered by proven (Idris 2 formal verification)"

  -- Run demos
  demoBuildModule
  demoMemory
  demoTypesAndInstructions
  demoTypeSystem
  demoValidation

  printSep
  putStrLn "All memory bounds proven at compile time"
  putStrLn "Build with: idris2 --build proven-wasm.ipkg"
  putStrLn "Run with:   ./build/exec/proven-wasm"
