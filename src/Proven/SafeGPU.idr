-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
||| SafeGPU - GPU compute safety with validated kernel configurations
|||
||| This module provides type-safe GPU compute primitives including vendor
||| identification, compute API selection, kernel configuration validation,
||| and memory space management. All configurations are validated before use,
||| preventing misconfigured kernel launches that would crash at runtime.
module Proven.SafeGPU

%default total

--------------------------------------------------------------------------------
-- GPU Vendor and Compute API
--------------------------------------------------------------------------------

||| Known GPU hardware vendors
public export
data GPUVendor = NVIDIA | AMD | Intel | Apple | Qualcomm

public export
Eq GPUVendor where
  NVIDIA   == NVIDIA   = True
  AMD      == AMD      = True
  Intel    == Intel    = True
  Apple    == Apple    = True
  Qualcomm == Qualcomm = True
  _        == _        = False

public export
Show GPUVendor where
  show NVIDIA   = "NVIDIA"
  show AMD      = "AMD"
  show Intel    = "Intel"
  show Apple    = "Apple"
  show Qualcomm = "Qualcomm"

||| GPU compute APIs / shader backends
public export
data ComputeAPI = CUDA | ROCm | Vulkan | Metal | OpenCL | WebGPU | SYCL

public export
Eq ComputeAPI where
  CUDA   == CUDA   = True
  ROCm   == ROCm   = True
  Vulkan == Vulkan = True
  Metal  == Metal  = True
  OpenCL == OpenCL = True
  WebGPU == WebGPU = True
  SYCL   == SYCL   = True
  _      == _      = False

public export
Show ComputeAPI where
  show CUDA   = "CUDA"
  show ROCm   = "ROCm"
  show Vulkan = "Vulkan"
  show Metal  = "Metal"
  show OpenCL = "OpenCL"
  show WebGPU = "WebGPU"
  show SYCL   = "SYCL"

--------------------------------------------------------------------------------
-- Memory Spaces
--------------------------------------------------------------------------------

||| GPU memory address spaces
public export
data MemorySpace = Global | Shared | Local | Constant

public export
Eq MemorySpace where
  Global   == Global   = True
  Shared   == Shared   = True
  Local    == Local    = True
  Constant == Constant = True
  _        == _        = False

public export
Show MemorySpace where
  show Global   = "Global"
  show Shared   = "Shared"
  show Local    = "Local"
  show Constant = "Constant"

--------------------------------------------------------------------------------
-- Kernel Configuration
--------------------------------------------------------------------------------

||| GPU compute kernel launch configuration
||| @ workgroupX  Workgroup size in X dimension (1-1024)
||| @ workgroupY  Workgroup size in Y dimension (1-1024)
||| @ workgroupZ  Workgroup size in Z dimension (1-64)
||| @ sharedMem   Shared memory in bytes (bounded by hardware)
||| @ registers   Estimated register count per thread
public export
record KernelConfig where
  constructor MkKernelConfig
  workgroupX : Nat
  workgroupY : Nat
  workgroupZ : Nat
  sharedMem  : Nat
  registers  : Nat

public export
Eq KernelConfig where
  a == b = a.workgroupX == b.workgroupX
        && a.workgroupY == b.workgroupY
        && a.workgroupZ == b.workgroupZ
        && a.sharedMem  == b.sharedMem
        && a.registers  == b.registers

public export
Show KernelConfig where
  show cfg = "KernelConfig("
          ++ show cfg.workgroupX ++ "x"
          ++ show cfg.workgroupY ++ "x"
          ++ show cfg.workgroupZ
          ++ ", shared=" ++ show cfg.sharedMem ++ "B"
          ++ ", regs=" ++ show cfg.registers ++ ")"

--------------------------------------------------------------------------------
-- Buffer Allocation
--------------------------------------------------------------------------------

||| A GPU buffer allocation descriptor
||| @ size      Size in bytes
||| @ alignment Alignment requirement in bytes (must be power of 2)
||| @ space     Target memory space
public export
record BufferAlloc where
  constructor MkBufferAlloc
  size      : Nat
  alignment : Nat
  space     : MemorySpace

public export
Show BufferAlloc where
  show buf = "BufferAlloc(" ++ show buf.size ++ "B, align="
          ++ show buf.alignment ++ ", " ++ show buf.space ++ ")"

--------------------------------------------------------------------------------
-- Validation
--------------------------------------------------------------------------------

||| Maximum workgroup size product (1024 threads is a common hardware limit)
maxWorkgroupProduct : Nat
maxWorkgroupProduct = 1024

||| Maximum shared memory per workgroup (48 KB is a common baseline)
maxSharedMemory : Nat
maxSharedMemory = 49152

||| Check whether a workgroup size dimension is within bounds
public export
isWorkgroupSizeValid : (x : Nat) -> (y : Nat) -> (z : Nat) -> Bool
isWorkgroupSizeValid x y z =
  x >= 1 && x <= 1024
  && y >= 1 && y <= 1024
  && z >= 1 && z <= 64
  && (x * y * z) <= maxWorkgroupProduct

||| Validate a kernel configuration for safe dispatch
||| Checks workgroup dimensions, shared memory limit, and register count
public export
validateKernelConfig : KernelConfig -> Maybe KernelConfig
validateKernelConfig cfg =
  if isWorkgroupSizeValid cfg.workgroupX cfg.workgroupY cfg.workgroupZ
     && cfg.sharedMem <= maxSharedMemory
     && cfg.registers >= 1 && cfg.registers <= 255
    then Just cfg
    else Nothing

||| Check if a value is a power of two
isPowerOfTwo : Nat -> Bool
isPowerOfTwo Z = False
isPowerOfTwo (S Z) = True
isPowerOfTwo n = (n `mod` 2 == 0) && isPowerOfTwo (n `div` 2)

||| Validate a buffer allocation descriptor
||| Checks that size is nonzero and alignment is a power of two
public export
validateBufferAlloc : BufferAlloc -> Maybe BufferAlloc
validateBufferAlloc buf =
  if buf.size >= 1 && buf.alignment >= 1 && isPowerOfTwo buf.alignment
    then Just buf
    else Nothing
