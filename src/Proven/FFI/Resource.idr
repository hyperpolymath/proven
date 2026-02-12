-- SPDX-License-Identifier: Apache-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
||| C FFI exports for SafeResource
module Proven.FFI.Resource

import Proven.SafeResource
import Proven.Core

%default total

--------------------------------------------------------------------------------
-- FFI Type Wrappers
--------------------------------------------------------------------------------

public export
U64ResourceHandle : Type
U64ResourceHandle = ResourceHandle Bits64

--------------------------------------------------------------------------------
-- FFI Functions
--------------------------------------------------------------------------------

export
%foreign "C:idris_proven_resource_new_handle,proven"
ffi_resource_new_handle : Bits64 -> PrimIO U64ResourceHandle
ffi_resource_new_handle id = toPrim (newHandle id)

export
%foreign "C:idris_proven_resource_mark_acquired,proven"
ffi_resource_mark_acquired : U64ResourceHandle -> Bits64 -> PrimIO U64ResourceHandle
ffi_resource_mark_acquired handle timestamp = toPrim (markAcquired (cast timestamp) handle)

export
%foreign "C:idris_proven_resource_mark_released,proven"
ffi_resource_mark_released : U64ResourceHandle -> Bits64 -> PrimIO U64ResourceHandle
ffi_resource_mark_released handle timestamp = toPrim (markReleased (cast timestamp) handle)

export
%foreign "C:idris_proven_resource_is_held,proven"
ffi_resource_is_held : U64ResourceHandle -> PrimIO Bool
ffi_resource_is_held handle = toPrim (isHeld handle)

export
%foreign "C:idris_proven_resource_get_state,proven"
ffi_resource_get_state : U64ResourceHandle -> PrimIO Bits32
ffi_resource_get_state handle = toPrim $ case handle.state of
  Unacquired => 0
  Acquired => 1
  Released => 2

export
%foreign "C:idris_proven_resource_free_handle,proven"
ffi_resource_free_handle : U64ResourceHandle -> PrimIO ()
ffi_resource_free_handle handle = toPrim ()
