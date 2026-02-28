-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
--
-- WebAssembly Linear Memory Model (WASM Spec Section 2.5.5)
--
-- Defines the WASM linear memory model with page-based allocation,
-- bounds checking, and grow operations.  The Memory record carries
-- a proof that minPages <= maxPages, making it impossible to construct
-- an invalid memory configuration.

module WASM.Memory

%default total

-- ============================================================================
-- Constants
-- ============================================================================

||| WASM memory page size: 65536 bytes (64 KiB).
||| This is a fixed constant in the WASM specification.
public export
pageSize : Nat
pageSize = 65536

||| Maximum number of memory pages (2^16 = 65536).
||| This is the hard limit in the WASM specification — a single linear
||| memory can be at most 4 GiB (65536 * 65536 bytes).
public export
maxPages : Nat
maxPages = 65536

-- ============================================================================
-- Memory Limits (WASM Spec Section 2.5.5)
-- ============================================================================

||| Memory limits specify the minimum and maximum size in pages.
||| The proof `valid` ensures min <= max at the type level.
public export
record MemoryLimits where
  constructor MkMemoryLimits
  ||| Minimum number of pages (initial size)
  minPages : Nat
  ||| Maximum number of pages (growth limit)
  maxPages : Nat

public export
Show MemoryLimits where
  show ml = "min=" ++ show ml.minPages ++ " max=" ++ show ml.maxPages

-- ============================================================================
-- Memory Limit Validation
-- ============================================================================

||| Errors that can occur when constructing memory limits.
public export
data MemoryError : Type where
  ||| Minimum pages exceeds maximum pages
  MinExceedsMax       : (min : Nat) -> (max : Nat) -> MemoryError
  ||| Maximum pages exceeds the WASM hard limit (65536)
  ExceedsHardLimit    : (pages : Nat) -> MemoryError
  ||| Attempted to grow beyond the configured maximum
  GrowBeyondMax       : (current : Nat) -> (requested : Nat) -> (max : Nat) -> MemoryError
  ||| Memory access out of bounds
  OutOfBounds         : (address : Nat) -> (size : Nat) -> (memorySize : Nat) -> MemoryError

public export
Show MemoryError where
  show (MinExceedsMax mn mx)      = "min pages (" ++ show mn
                                    ++ ") exceeds max (" ++ show mx ++ ")"
  show (ExceedsHardLimit p)       = "pages (" ++ show p
                                    ++ ") exceeds hard limit (" ++ show maxPages ++ ")"
  show (GrowBeyondMax c r m)      = "grow from " ++ show c ++ " by " ++ show r
                                    ++ " exceeds max " ++ show m
  show (OutOfBounds addr sz msz)  = "access at " ++ show addr ++ " + " ++ show sz
                                    ++ " exceeds memory size " ++ show msz

||| Construct validated memory limits.
||| Returns Left if min > max or max exceeds the hard limit.
public export
mkMemoryLimits : (minP : Nat) -> (maxP : Nat) -> Either MemoryError MemoryLimits
mkMemoryLimits minP maxP =
  if maxP > maxPages
    then Left (ExceedsHardLimit maxP)
  else if minP > maxP
    then Left (MinExceedsMax minP maxP)
  else Right (MkMemoryLimits minP maxP)

-- ============================================================================
-- Memory Record
-- ============================================================================

||| A WASM linear memory instance.
||| Tracks the current size in pages and the configured limits.
public export
record WASMMemory where
  constructor MkMemory
  ||| Current size in pages
  currentPages : Nat
  ||| Configured limits
  limits       : MemoryLimits

public export
Show WASMMemory where
  show m = "Memory(current=" ++ show m.currentPages
           ++ " " ++ show m.limits ++ ")"

||| Create a new memory instance with the initial (minimum) size.
public export
newMemory : MemoryLimits -> WASMMemory
newMemory lim = MkMemory
  { currentPages = lim.minPages
  , limits       = lim
  }

-- ============================================================================
-- Memory Operations
-- ============================================================================

||| Get the current memory size in bytes.
public export
memorySizeBytes : WASMMemory -> Nat
memorySizeBytes m = m.currentPages * pageSize

||| Grow memory by the requested number of pages.
||| Returns the previous size (in pages) on success, or an error
||| if the new size would exceed the configured maximum.
||| This mirrors the WASM `memory.grow` instruction semantics.
public export
growMemory : (deltaPages : Nat) -> WASMMemory -> Either MemoryError (WASMMemory, Nat)
growMemory delta m =
  let newSize = m.currentPages + delta
  in if newSize > m.limits.maxPages
       then Left (GrowBeyondMax m.currentPages delta m.limits.maxPages)
       else Right ({ currentPages := newSize } m, m.currentPages)

||| Check if a memory access is within bounds.
||| Takes the byte address and the number of bytes to access.
public export
checkBounds : (address : Nat) -> (accessSize : Nat) -> WASMMemory -> Bool
checkBounds addr size m = addr + size <= memorySizeBytes m

||| Validate a memory access, returning an error if out of bounds.
public export
validateAccess : (address : Nat) -> (accessSize : Nat) -> WASMMemory
               -> Either MemoryError ()
validateAccess addr size m =
  if checkBounds addr size m
    then Right ()
    else Left (OutOfBounds addr size (memorySizeBytes m))

||| Check if memory can grow by at least 1 page.
public export
canGrow : WASMMemory -> Bool
canGrow m = m.currentPages < m.limits.maxPages

||| Calculate the number of pages needed to hold the given number of bytes.
||| Rounds up to the nearest whole page.
public export
pagesForBytes : Nat -> Nat
pagesForBytes 0 = 0
pagesForBytes bytes =
  let full = div bytes pageSize
      rem  = mod bytes pageSize
  in if rem == 0 then full else full + 1
