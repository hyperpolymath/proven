-- SPDX-License-Identifier: Apache-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
||| C FFI exports for SafeBuffer
module Proven.FFI.Buffer

import Proven.SafeBuffer
import Proven.Core

%default total

--------------------------------------------------------------------------------
-- FFI Type Wrappers
--------------------------------------------------------------------------------

public export
ByteBuffer : Type
ByteBuffer = DynBuffer Bits8

--------------------------------------------------------------------------------
-- FFI Functions
--------------------------------------------------------------------------------

export
%foreign "C:idris_proven_buffer_new,proven"
ffi_buffer_new : Bits64 -> PrimIO ByteBuffer
ffi_buffer_new capacity = toPrim (emptyDyn (cast capacity))

export
%foreign "C:idris_proven_buffer_write,proven"
ffi_buffer_write : ByteBuffer -> List Bits8 -> PrimIO (Maybe ByteBuffer)
ffi_buffer_write buf data = toPrim (writeMany data buf)

export
%foreign "C:idris_proven_buffer_capacity,proven"
ffi_buffer_capacity : ByteBuffer -> PrimIO Bits64
ffi_buffer_capacity buf = toPrim (cast buf.capacity)

export
%foreign "C:idris_proven_buffer_size,proven"
ffi_buffer_size : ByteBuffer -> PrimIO Bits64
ffi_buffer_size buf = toPrim (cast (dynLength buf))

export
%foreign "C:idris_proven_buffer_is_full,proven"
ffi_buffer_is_full : ByteBuffer -> PrimIO Bool
ffi_buffer_is_full buf = toPrim (isFull buf)

export
%foreign "C:idris_proven_buffer_remaining,proven"
ffi_buffer_remaining : ByteBuffer -> PrimIO Bits64
ffi_buffer_remaining buf = toPrim (cast (remaining buf))

export
%foreign "C:idris_proven_buffer_free,proven"
ffi_buffer_free : ByteBuffer -> PrimIO ()
ffi_buffer_free buf = toPrim ()
