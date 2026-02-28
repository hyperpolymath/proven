-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

||| Proven.Hardware - Convenience re-export module for hardware backend safety
|||
||| Groups all hardware acceleration Safe* modules into a single import for
||| applications targeting heterogeneous compute. Covers GPU kernel launch
||| configuration, video processing unit pipelines, tensor processing unit
||| workloads, FPGA synthesis constraints, neural processing unit inference,
||| digital signal processing chains, and instruction set architecture targeting.
|||
||| Usage:
|||   import Proven.Hardware
|||
||| This single import provides access to all hardware backend safety types,
||| constructors, and validation functions without needing 7 separate imports.
module Proven.Hardware

import public Proven.SafeGPU
import public Proven.SafeVPU
import public Proven.SafeTPU
import public Proven.SafeFPGA
import public Proven.SafeNPU
import public Proven.SafeDSP
import public Proven.SafeISA
