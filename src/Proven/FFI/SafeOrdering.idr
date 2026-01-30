-- SPDX-License-Identifier: Apache-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell <jonathan.jewell@open.ac.uk>
||| FFI exports for SafeOrdering operations
|||
||| Temporal ordering and causality helpers for distributed systems.
||| All functions proven total. Part of 75-module FFI export (55/75 complete).
module Proven.FFI.SafeOrdering

import Proven.SafeOrdering
import Proven.Core
import Data.String

%default total

encodeBool : Bool -> Int
encodeBool False = 0
encodeBool True = 1

-- Vector timestamp operations
%export
proven_idris_vt_component_le : Int -> Int -> Int
proven_idris_vt_component_le v1 v2 = encodeBool (v1 <= v2)

%export
proven_idris_vt_component_lt : Int -> Int -> Int
proven_idris_vt_component_lt v1 v2 = encodeBool (v1 < v2)

%export
proven_idris_vt_merge : Int -> Int -> Int
proven_idris_vt_merge v1 v2 = if v1 > v2 then v1 else v2

-- Plausible clock operations
%export
proven_idris_pc_compare : Int -> Int -> Int -> Int -> Int
proven_idris_pc_compare phys1 log1 phys2 log2 =
  if phys1 < phys2 then (-1)
  else if phys1 > phys2 then 1
  else if log1 < log2 then (-1)
  else if log1 > log2 then 1
  else 0

%export
proven_idris_pc_max_physical : Int -> Int -> Int -> Int
proven_idris_pc_max_physical pt lt rt =
  max pt (max lt rt)

-- Sequence number operations
%export
proven_idris_seq_next : Int -> Int
proven_idris_seq_next n = n + 1

%export
proven_idris_seq_compare : Int -> Int -> Int -> Int -> Int
proven_idris_seq_compare n1 p1 n2 p2 =
  if n1 < n2 then (-1)
  else if n1 > n2 then 1
  else if p1 < p2 then (-1)
  else if p1 > p2 then 1
  else 0

-- Epoch ordering
%export
proven_idris_epoch_compare : Int -> Int -> Int -> Int -> Int
proven_idris_epoch_compare e1 s1 e2 s2 =
  if e1 < e2 then (-1)
  else if e1 > e2 then 1
  else if s1 < s2 then (-1)
  else if s1 > s2 then 1
  else 0

%export
proven_idris_epoch_next : Int -> Int
proven_idris_epoch_next e = e + 1

%export
proven_idris_ordering_friendly_error : String -> String
proven_idris_ordering_friendly_error msg =
  if isInfixOf "causal" (toLower msg)
    then "Causal ordering violation"
  else "Ordering error"
