-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
||| SafeISA - Instruction set architecture safety
|||
||| This module provides type-safe primitives for instruction set architecture
||| identification: ISA families, SIMD/vector extensions, endianness detection,
||| register width queries, and extension compatibility checking. Invalid
||| platform configurations are caught at validation time.
module Proven.SafeISA

import Data.List

%default total

--------------------------------------------------------------------------------
-- Instruction Set Architectures
--------------------------------------------------------------------------------

||| Major instruction set architecture families
public export
data ISA = X86_64 | AArch64 | RISCV64 | MIPS64 | PowerPC | S390x | WASM32

public export
Eq ISA where
  X86_64  == X86_64  = True
  AArch64 == AArch64 = True
  RISCV64 == RISCV64 = True
  MIPS64  == MIPS64  = True
  PowerPC == PowerPC = True
  S390x   == S390x   = True
  WASM32  == WASM32  = True
  _       == _       = False

public export
Show ISA where
  show X86_64  = "x86_64"
  show AArch64 = "AArch64"
  show RISCV64 = "RISC-V 64"
  show MIPS64  = "MIPS64"
  show PowerPC = "PowerPC"
  show S390x   = "s390x"
  show WASM32  = "WASM32"

--------------------------------------------------------------------------------
-- SIMD / Vector Extensions
--------------------------------------------------------------------------------

||| Hardware SIMD and vector extensions
public export
data Extension = SSE | AVX | AVX2 | AVX512 | NEON | SVE | RVV

public export
Eq Extension where
  SSE    == SSE    = True
  AVX    == AVX    = True
  AVX2   == AVX2   = True
  AVX512 == AVX512 = True
  NEON   == NEON   = True
  SVE    == SVE    = True
  RVV    == RVV    = True
  _      == _      = False

public export
Show Extension where
  show SSE    = "SSE"
  show AVX    = "AVX"
  show AVX2   = "AVX2"
  show AVX512 = "AVX-512"
  show NEON   = "NEON"
  show SVE    = "SVE"
  show RVV    = "RVV"

||| SIMD register width in bits for a given extension
public export
extensionWidth : Extension -> Nat
extensionWidth SSE    = 128
extensionWidth AVX    = 256
extensionWidth AVX2   = 256
extensionWidth AVX512 = 512
extensionWidth NEON   = 128
extensionWidth SVE    = 2048  -- Scalable: up to 2048 bits
extensionWidth RVV    = 4096  -- Scalable: up to 4096 bits (VLEN)

--------------------------------------------------------------------------------
-- Endianness
--------------------------------------------------------------------------------

||| Byte ordering
public export
data Endianness = Little | Big

public export
Eq Endianness where
  Little == Little = True
  Big    == Big    = True
  _      == _      = False

public export
Show Endianness where
  show Little = "Little-endian"
  show Big    = "Big-endian"

||| Determine the native byte ordering for an ISA
||| Most modern ISAs are little-endian; S390x and PowerPC are big-endian
public export
endianness : ISA -> Endianness
endianness X86_64  = Little
endianness AArch64 = Little
endianness RISCV64 = Little
endianness MIPS64  = Big
endianness PowerPC = Big
endianness S390x   = Big
endianness WASM32  = Little

--------------------------------------------------------------------------------
-- Register Width
--------------------------------------------------------------------------------

||| General-purpose register width in bits
public export
registerWidth : ISA -> Nat
registerWidth X86_64  = 64
registerWidth AArch64 = 64
registerWidth RISCV64 = 64
registerWidth MIPS64  = 64
registerWidth PowerPC = 64
registerWidth S390x   = 64
registerWidth WASM32  = 32

||| Address space width in bits (determines max addressable memory)
public export
addressWidth : ISA -> Nat
addressWidth = registerWidth  -- Currently matches register width for all listed ISAs

--------------------------------------------------------------------------------
-- Target Platform
--------------------------------------------------------------------------------

||| A compilation or execution target platform
||| @ isa        The instruction set architecture
||| @ extensions List of available SIMD/vector extensions
||| @ osName     Target operating system name (e.g. "linux", "macos")
public export
record TargetPlatform where
  constructor MkTargetPlatform
  isa        : ISA
  extensions : List Extension
  osName     : String

public export
Eq TargetPlatform where
  a == b = a.isa        == b.isa
        && a.extensions == b.extensions
        && a.osName     == b.osName

public export
Show TargetPlatform where
  show tp = "Target(" ++ show tp.isa
         ++ ", ext=[" ++ showExts tp.extensions ++ "]"
         ++ ", os=" ++ tp.osName ++ ")"
  where
    showExts : List Extension -> String
    showExts [] = ""
    showExts [x] = show x
    showExts (x :: xs) = show x ++ ", " ++ showExts xs

--------------------------------------------------------------------------------
-- Extension Compatibility
--------------------------------------------------------------------------------

||| Check whether a SIMD/vector extension is compatible with an ISA
||| SSE/AVX/AVX2/AVX512 are x86_64-only; NEON/SVE are AArch64-only;
||| RVV is RISC-V-only
public export
isExtensionSupported : ISA -> Extension -> Bool
isExtensionSupported X86_64  SSE    = True
isExtensionSupported X86_64  AVX    = True
isExtensionSupported X86_64  AVX2   = True
isExtensionSupported X86_64  AVX512 = True
isExtensionSupported AArch64 NEON   = True
isExtensionSupported AArch64 SVE    = True
isExtensionSupported RISCV64 RVV    = True
isExtensionSupported _       _      = False

||| Validate that all extensions in a target platform are compatible
||| with the platform's ISA
public export
validatePlatform : TargetPlatform -> Maybe TargetPlatform
validatePlatform tp =
  if all (isExtensionSupported tp.isa) tp.extensions
    then Just tp
    else Nothing
