-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
||| SafeFPGA - FPGA configuration and resource safety
|||
||| This module provides type-safe primitives for FPGA bitstream
||| configuration: device family selection, resource type tracking,
||| utilisation validation, and clock frequency bounds checking.
||| Invalid configurations are caught at validation time rather than
||| during synthesis or at hardware programming time.
module Proven.SafeFPGA

import Data.List

%default total

--------------------------------------------------------------------------------
-- FPGA Families
--------------------------------------------------------------------------------

||| Major FPGA device families
public export
data FPGAFamily = Xilinx7 | UltraScale | Versal | CycloneV | Agilex | Lattice

public export
Eq FPGAFamily where
  Xilinx7    == Xilinx7    = True
  UltraScale == UltraScale = True
  Versal     == Versal     = True
  CycloneV   == CycloneV   = True
  Agilex     == Agilex     = True
  Lattice    == Lattice    = True
  _          == _          = False

public export
Show FPGAFamily where
  show Xilinx7    = "Xilinx 7-Series"
  show UltraScale = "UltraScale+"
  show Versal     = "Versal"
  show CycloneV   = "Cyclone V"
  show Agilex     = "Agilex"
  show Lattice    = "Lattice"

--------------------------------------------------------------------------------
-- Resource Types
--------------------------------------------------------------------------------

||| FPGA fabric resource categories
public export
data ResourceType = LUT | FF | BRAM | DSP | IO

public export
Eq ResourceType where
  LUT  == LUT  = True
  FF   == FF   = True
  BRAM == BRAM = True
  DSP  == DSP  = True
  IO   == IO   = True
  _    == _    = False

public export
Show ResourceType where
  show LUT  = "LUT"
  show FF   = "FF"
  show BRAM = "BRAM"
  show DSP  = "DSP"
  show IO   = "IO"

--------------------------------------------------------------------------------
-- FPGA Resource Descriptor
--------------------------------------------------------------------------------

||| An FPGA resource usage descriptor
||| @ resType      The type of fabric resource
||| @ count        Number of resources used
||| @ utilisation  Percentage utilisation (0-100)
public export
record FPGAResource where
  constructor MkFPGAResource
  resType     : ResourceType
  count       : Nat
  utilisation : Nat

public export
Eq FPGAResource where
  a == b = a.resType     == b.resType
        && a.count       == b.count
        && a.utilisation == b.utilisation

public export
Show FPGAResource where
  show r = show r.resType ++ "(" ++ show r.count
        ++ ", " ++ show r.utilisation ++ "%)"

--------------------------------------------------------------------------------
-- Bitstream Configuration
--------------------------------------------------------------------------------

||| FPGA bitstream / synthesis configuration
||| @ targetFamily   Target FPGA device family
||| @ clockFreqMHz   Target clock frequency in MHz (1-1000)
||| @ resourceBudget List of resource budget limits
public export
record BitstreamConfig where
  constructor MkBitstreamConfig
  targetFamily   : FPGAFamily
  clockFreqMHz   : Nat
  resourceBudget : List FPGAResource

public export
Show BitstreamConfig where
  show cfg = "Bitstream(" ++ show cfg.targetFamily
          ++ ", " ++ show cfg.clockFreqMHz ++ " MHz"
          ++ ", " ++ show (length cfg.resourceBudget) ++ " resources)"

--------------------------------------------------------------------------------
-- Validation
--------------------------------------------------------------------------------

||| Minimum clock frequency (1 MHz)
minClockMHz : Nat
minClockMHz = 1

||| Maximum clock frequency (1000 MHz = 1 GHz)
maxClockMHz : Nat
maxClockMHz = 1000

||| Check whether a single resource is within its budget
||| A resource is within budget if utilisation is at most 100%
public export
isResourceWithinBudget : FPGAResource -> Bool
isResourceWithinBudget r = r.utilisation <= 100

||| Validate a complete bitstream configuration
||| Checks clock frequency bounds and that all resources are within budget
public export
validateBitstream : BitstreamConfig -> Maybe BitstreamConfig
validateBitstream cfg =
  if cfg.clockFreqMHz >= minClockMHz
     && cfg.clockFreqMHz <= maxClockMHz
     && all isResourceWithinBudget cfg.resourceBudget
    then Just cfg
    else Nothing

||| Estimate routing congestion as the maximum utilisation across all
||| resource types. High congestion (> 80%) is a warning signal.
||| Returns 0 if there are no resources in the budget.
public export
estimateRoutingCongestion : BitstreamConfig -> Nat
estimateRoutingCongestion cfg =
  foldl (\acc, r => max acc r.utilisation) 0 cfg.resourceBudget

||| Check whether the design is likely to meet timing closure
||| Heuristic: congestion above 85% at high clock speeds is risky
public export
isTimingLikely : BitstreamConfig -> Bool
isTimingLikely cfg =
  let congestion = estimateRoutingCongestion cfg
  in if cfg.clockFreqMHz > 500
       then congestion <= 85
       else congestion <= 95
