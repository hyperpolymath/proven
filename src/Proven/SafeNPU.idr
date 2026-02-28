-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
||| SafeNPU - Neural processing unit safety
|||
||| This module provides type-safe configuration for neural processing
||| hardware accelerators. It covers backend selection, activation function
||| specification, layer configuration validation, and inference parameter
||| bounds checking. Invalid configurations are caught at validation time
||| rather than causing opaque runtime failures on the NPU.
module Proven.SafeNPU

%default total

--------------------------------------------------------------------------------
-- NPU Backends
--------------------------------------------------------------------------------

||| Neural processing unit runtime backends
public export
data NPUBackend = CoreML | NNAPI | TensorRT | OpenVINO | XNNPACK

public export
Eq NPUBackend where
  CoreML   == CoreML   = True
  NNAPI    == NNAPI    = True
  TensorRT == TensorRT = True
  OpenVINO == OpenVINO = True
  XNNPACK  == XNNPACK  = True
  _        == _        = False

public export
Show NPUBackend where
  show CoreML   = "CoreML"
  show NNAPI    = "NNAPI"
  show TensorRT = "TensorRT"
  show OpenVINO = "OpenVINO"
  show XNNPACK  = "XNNPACK"

--------------------------------------------------------------------------------
-- Activation Functions
--------------------------------------------------------------------------------

||| Common neural network activation functions
public export
data ActivationFn = ReLU | Sigmoid | Tanh | GELU | SiLU | Softmax

public export
Eq ActivationFn where
  ReLU    == ReLU    = True
  Sigmoid == Sigmoid = True
  Tanh    == Tanh    = True
  GELU    == GELU    = True
  SiLU    == SiLU    = True
  Softmax == Softmax = True
  _       == _       = False

public export
Show ActivationFn where
  show ReLU    = "ReLU"
  show Sigmoid = "Sigmoid"
  show Tanh    = "Tanh"
  show GELU    = "GELU"
  show SiLU    = "SiLU"
  show Softmax = "Softmax"

--------------------------------------------------------------------------------
-- Layer Configuration
--------------------------------------------------------------------------------

||| A single neural network layer configuration
||| @ inputDims   Number of input dimensions (must be positive)
||| @ outputDims  Number of output dimensions (must be positive)
||| @ activation  Activation function applied after the layer
public export
record LayerConfig where
  constructor MkLayerConfig
  inputDims  : Nat
  outputDims : Nat
  activation : ActivationFn

public export
Eq LayerConfig where
  a == b = a.inputDims  == b.inputDims
        && a.outputDims == b.outputDims
        && a.activation == b.activation

public export
Show LayerConfig where
  show lc = "Layer(" ++ show lc.inputDims ++ " -> "
         ++ show lc.outputDims ++ ", "
         ++ show lc.activation ++ ")"

||| Validate a layer configuration
||| Both input and output dimensions must be at least 1
public export
validateLayerConfig : LayerConfig -> Maybe LayerConfig
validateLayerConfig lc =
  if lc.inputDims >= 1 && lc.outputDims >= 1
    then Just lc
    else Nothing

--------------------------------------------------------------------------------
-- Inference Configuration
--------------------------------------------------------------------------------

||| NPU inference session configuration
||| @ backend    Target NPU backend / runtime
||| @ batchSize  Inference batch size (must be positive)
||| @ maxLatency Maximum acceptable latency in milliseconds
public export
record InferenceConfig where
  constructor MkInferenceConfig
  backend    : NPUBackend
  batchSize  : Nat
  maxLatency : Nat

public export
Eq InferenceConfig where
  a == b = a.backend    == b.backend
        && a.batchSize  == b.batchSize
        && a.maxLatency == b.maxLatency

public export
Show InferenceConfig where
  show ic = "Inference(" ++ show ic.backend
         ++ ", batch=" ++ show ic.batchSize
         ++ ", maxLat=" ++ show ic.maxLatency ++ "ms)"

--------------------------------------------------------------------------------
-- Validation and Estimation
--------------------------------------------------------------------------------

||| Check whether a given backend is available on common platforms
||| CoreML is Apple-only; NNAPI is Android-only; the rest are cross-platform
public export
isBackendAvailable : NPUBackend -> String -> Bool
isBackendAvailable CoreML   os = os == "macos" || os == "ios"
isBackendAvailable NNAPI    os = os == "android"
isBackendAvailable TensorRT os = os == "linux" || os == "windows"
isBackendAvailable OpenVINO os = os == "linux" || os == "windows" || os == "macos"
isBackendAvailable XNNPACK  _  = True  -- Cross-platform fallback

||| Estimate inference latency in milliseconds given a layer config and batch size
||| This is a rough heuristic: O(input * output) scaled by batch, in microseconds,
||| converted to milliseconds. Useful for budget checks, not precise timing.
public export
estimateLatency : LayerConfig -> (batchSize : Nat) -> Nat
estimateLatency lc bs =
  let ops = lc.inputDims * lc.outputDims * bs
  in ops `div` 1000  -- Very rough: ~1000 ops/ms on typical NPU

||| Validate an inference configuration
||| Batch size must be at least 1
public export
validateInferenceConfig : InferenceConfig -> Maybe InferenceConfig
validateInferenceConfig ic =
  if ic.batchSize >= 1
    then Just ic
    else Nothing
