-- SPDX-License-Identifier: Apache-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
||| C FFI exports for SafeLRU
|||
||| This module provides C-compatible FFI exports for the SafeLRU module
||| to be called from Zig/Rust/C/Ephapax
module Proven.FFI.LRU

import Proven.SafeLRU
import Proven.Core
import Data.String

%default total

--------------------------------------------------------------------------------
-- FFI Type Wrappers
--------------------------------------------------------------------------------

-- For FFI, we use String keys and List Bits8 (byte arrays) as values
-- This maps to C char* and byte arrays

public export
ByteLRUCache : Type
ByteLRUCache = LRUCache String (List Bits8)

--------------------------------------------------------------------------------
-- FFI Functions (exported to C)
--------------------------------------------------------------------------------

||| Create a new LRU cache with given capacity
export
%foreign "C:idris_proven_lru_new,proven"
ffi_lru_new : Bits64 -> PrimIO ByteLRUCache
ffi_lru_new capacity = toPrim (empty (cast capacity))

||| Put a key-value pair into the cache
||| Returns updated cache
export
%foreign "C:idris_proven_lru_put,proven"
ffi_lru_put : ByteLRUCache -> String -> List Bits8 -> PrimIO ByteLRUCache
ffi_lru_put cache key value = toPrim (put key value cache)

||| Get a value from the cache
||| Returns (has_value, value_ptr, value_len, updated_cache)
export
%foreign "C:idris_proven_lru_get,proven"
ffi_lru_get : ByteLRUCache -> String -> PrimIO (Maybe (List Bits8), ByteLRUCache)
ffi_lru_get cache key =
  toPrim $ case get key cache of
    Nothing => (Nothing, cache)
    Just (val, newCache) => (Just val, newCache)

||| Check if cache contains a key
export
%foreign "C:idris_proven_lru_contains,proven"
ffi_lru_contains : ByteLRUCache -> String -> PrimIO Bool
ffi_lru_contains cache key = toPrim (contains key cache)

||| Get current cache size
export
%foreign "C:idris_proven_lru_size,proven"
ffi_lru_size : ByteLRUCache -> PrimIO Bits64
ffi_lru_size cache = toPrim (cast (size cache))

||| Check if cache is full
export
%foreign "C:idris_proven_lru_is_full,proven"
ffi_lru_is_full : ByteLRUCache -> PrimIO Bool
ffi_lru_is_full cache = toPrim (isFull cache)

||| Free the cache (no-op in Idris2 with GC, but needed for FFI)
export
%foreign "C:idris_proven_lru_free,proven"
ffi_lru_free : ByteLRUCache -> PrimIO ()
ffi_lru_free cache = toPrim ()
