-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
--
-- WebAssembly Instructions (WASM Spec Section 2.4)
--
-- Defines the core WASM instruction set as a sum type covering
-- numeric operations, memory access, control flow, and variable
-- access.  Each instruction has an opcode byte and can be
-- serialised to binary format.

module WASM.Instruction

import WASM.ValType

%default total

-- ============================================================================
-- Memory Arguments (WASM Spec Section 2.4.5)
-- ============================================================================

||| Memory access arguments: alignment hint and offset.
||| The alignment is a power-of-two hint (2^align bytes).
||| The offset is added to the address operand on the stack.
public export
record MemArg where
  constructor MkMemArg
  ||| Alignment hint as a power of 2 (e.g., 2 means 4-byte aligned)
  align  : Nat
  ||| Static offset added to the dynamic address
  offset : Nat

public export
Show MemArg where
  show ma = "align=" ++ show ma.align ++ " offset=" ++ show ma.offset

-- ============================================================================
-- Block Types
-- ============================================================================

||| Block type for structured control flow instructions.
||| Describes the type signature of a block (params -> results).
public export
data BlockType : Type where
  ||| Block produces no value
  BlockVoid   : BlockType
  ||| Block produces a single value of the given type
  BlockResult : ValType -> BlockType

public export
Show BlockType where
  show BlockVoid       = "(void)"
  show (BlockResult t) = "(" ++ show t ++ ")"

-- ============================================================================
-- Instructions (WASM Spec Section 2.4)
-- ============================================================================

||| Core WebAssembly instructions.
||| This covers numeric, memory, control, and variable instructions.
public export
data Instruction : Type where
  -- Numeric instructions: i32 (WASM Spec Section 2.4.1)
  ||| i32.const: push a constant i32 value
  I32Const    : (value : Int) -> Instruction
  ||| i32.add: pop two i32, push their sum
  I32Add      : Instruction
  ||| i32.sub: pop two i32, push their difference
  I32Sub      : Instruction
  ||| i32.mul: pop two i32, push their product
  I32Mul      : Instruction
  ||| i32.div_s: signed division (traps on division by zero)
  I32DivS     : Instruction
  ||| i32.div_u: unsigned division
  I32DivU     : Instruction
  ||| i32.rem_s: signed remainder
  I32RemS     : Instruction
  ||| i32.rem_u: unsigned remainder
  I32RemU     : Instruction
  ||| i32.and: bitwise AND
  I32And      : Instruction
  ||| i32.or: bitwise OR
  I32Or       : Instruction
  ||| i32.xor: bitwise XOR
  I32Xor      : Instruction
  ||| i32.eqz: test if zero (returns 1 if operand is 0)
  I32Eqz      : Instruction
  ||| i32.eq: test equality (returns 1 if equal)
  I32Eq       : Instruction
  ||| i32.lt_s: signed less-than comparison
  I32LtS      : Instruction

  -- Numeric instructions: i64
  ||| i64.const: push a constant i64 value
  I64Const    : (value : Int) -> Instruction
  ||| i64.add: pop two i64, push their sum
  I64Add      : Instruction
  ||| i64.sub: pop two i64, push their difference
  I64Sub      : Instruction
  ||| i64.mul: pop two i64, push their product
  I64Mul      : Instruction

  -- Memory instructions (WASM Spec Section 2.4.5)
  ||| i32.load: load a 32-bit integer from linear memory
  I32Load     : MemArg -> Instruction
  ||| i64.load: load a 64-bit integer from linear memory
  I64Load     : MemArg -> Instruction
  ||| i32.store: store a 32-bit integer to linear memory
  I32Store    : MemArg -> Instruction
  ||| i64.store: store a 64-bit integer to linear memory
  I64Store    : MemArg -> Instruction
  ||| memory.size: return the current memory size in pages
  MemorySize  : Instruction
  ||| memory.grow: grow memory by the given number of pages
  MemoryGrow  : Instruction

  -- Control instructions (WASM Spec Section 2.4.6)
  ||| block: begin a structured block
  Block       : BlockType -> List Instruction -> Instruction
  ||| loop: begin a loop block (br to label re-enters the loop)
  Loop        : BlockType -> List Instruction -> Instruction
  ||| if/else: conditional with optional else branch
  If          : BlockType -> (thenBranch : List Instruction)
              -> (elseBranch : List Instruction) -> Instruction
  ||| br: unconditional branch to the label at the given depth
  Br          : (labelIdx : Nat) -> Instruction
  ||| br_if: conditional branch (branch if top-of-stack is nonzero)
  BrIf        : (labelIdx : Nat) -> Instruction
  ||| return: return from the current function
  Return      : Instruction
  ||| call: call a function by index
  Call        : (funcIdx : Nat) -> Instruction
  ||| nop: no operation
  Nop         : Instruction
  ||| unreachable: trap immediately (marks dead code)
  Unreachable : Instruction
  ||| drop: discard the top value from the stack
  Drop        : Instruction
  ||| end: marks the end of a block, loop, if, or function body
  End         : Instruction

  -- Variable instructions (WASM Spec Section 2.4.4)
  ||| local.get: push the value of a local variable
  LocalGet    : (localIdx : Nat) -> Instruction
  ||| local.set: pop the top value and store it in a local variable
  LocalSet    : (localIdx : Nat) -> Instruction
  ||| local.tee: like local.set but also leaves the value on the stack
  LocalTee    : (localIdx : Nat) -> Instruction
  ||| global.get: push the value of a global variable
  GlobalGet   : (globalIdx : Nat) -> Instruction
  ||| global.set: pop the top value and store it in a global variable
  GlobalSet   : (globalIdx : Nat) -> Instruction

-- ============================================================================
-- Opcode Encoding (WASM Spec Section 5.4)
-- ============================================================================

||| Get the primary opcode byte for an instruction.
||| Compound instructions (block, loop, if) use the opcode for the
||| opening marker; their bodies are encoded inline.
public export
opcode : Instruction -> Bits8
opcode (I32Const _)  = 0x41
opcode I32Add        = 0x6A
opcode I32Sub        = 0x6B
opcode I32Mul        = 0x6C
opcode I32DivS       = 0x6D
opcode I32DivU       = 0x6E
opcode I32RemS       = 0x6F
opcode I32RemU       = 0x70
opcode I32And        = 0x71
opcode I32Or         = 0x72
opcode I32Xor        = 0x73
opcode I32Eqz        = 0x45
opcode I32Eq         = 0x46
opcode I32LtS        = 0x48
opcode (I64Const _)  = 0x42
opcode I64Add        = 0x7C
opcode I64Sub        = 0x7D
opcode I64Mul        = 0x7E
opcode (I32Load _)   = 0x28
opcode (I64Load _)   = 0x29
opcode (I32Store _)  = 0x36
opcode (I64Store _)  = 0x37
opcode MemorySize    = 0x3F
opcode MemoryGrow    = 0x40
opcode (Block _ _)   = 0x02
opcode (Loop _ _)    = 0x03
opcode (If _ _ _)    = 0x04
opcode (Br _)        = 0x0C
opcode (BrIf _)      = 0x0D
opcode Return        = 0x0F
opcode (Call _)      = 0x10
opcode Nop           = 0x01
opcode Unreachable   = 0x00
opcode Drop          = 0x1A
opcode End           = 0x0B
opcode (LocalGet _)  = 0x20
opcode (LocalSet _)  = 0x21
opcode (LocalTee _)  = 0x22
opcode (GlobalGet _) = 0x23
opcode (GlobalSet _) = 0x24

||| Get a human-readable name for an instruction (WAT-style mnemonic).
public export
instructionName : Instruction -> String
instructionName (I32Const v)     = "i32.const " ++ show v
instructionName I32Add           = "i32.add"
instructionName I32Sub           = "i32.sub"
instructionName I32Mul           = "i32.mul"
instructionName I32DivS          = "i32.div_s"
instructionName I32DivU          = "i32.div_u"
instructionName I32RemS          = "i32.rem_s"
instructionName I32RemU          = "i32.rem_u"
instructionName I32And           = "i32.and"
instructionName I32Or            = "i32.or"
instructionName I32Xor           = "i32.xor"
instructionName I32Eqz           = "i32.eqz"
instructionName I32Eq            = "i32.eq"
instructionName I32LtS           = "i32.lt_s"
instructionName (I64Const v)     = "i64.const " ++ show v
instructionName I64Add           = "i64.add"
instructionName I64Sub           = "i64.sub"
instructionName I64Mul           = "i64.mul"
instructionName (I32Load ma)     = "i32.load " ++ show ma
instructionName (I64Load ma)     = "i64.load " ++ show ma
instructionName (I32Store ma)    = "i32.store " ++ show ma
instructionName (I64Store ma)    = "i64.store " ++ show ma
instructionName MemorySize       = "memory.size"
instructionName MemoryGrow       = "memory.grow"
instructionName (Block bt _)     = "block " ++ show bt
instructionName (Loop bt _)      = "loop " ++ show bt
instructionName (If bt _ _)      = "if " ++ show bt
instructionName (Br idx)         = "br " ++ show idx
instructionName (BrIf idx)       = "br_if " ++ show idx
instructionName Return           = "return"
instructionName (Call idx)       = "call " ++ show idx
instructionName Nop              = "nop"
instructionName Unreachable      = "unreachable"
instructionName Drop             = "drop"
instructionName End              = "end"
instructionName (LocalGet idx)   = "local.get " ++ show idx
instructionName (LocalSet idx)   = "local.set " ++ show idx
instructionName (LocalTee idx)   = "local.tee " ++ show idx
instructionName (GlobalGet idx)  = "global.get " ++ show idx
instructionName (GlobalSet idx)  = "global.set " ++ show idx
