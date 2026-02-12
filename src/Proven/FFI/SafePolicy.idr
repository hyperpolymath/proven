-- SPDX-License-Identifier: Apache-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
||| FFI exports for SafePolicy operations (63/75)
module Proven.FFI.SafePolicy

import Proven.SafePolicy
import Proven.Core

%default total

encodeBool : Bool -> Int
encodeBool False = 0
encodeBool True = 1

-- Zone classes: Mutable=0, Immutable=1, Hybrid=2, Restricted=3
encodeZone : ZoneClass -> Int
encodeZone Mutable = 0
encodeZone Immutable = 1
encodeZone Hybrid = 2
encodeZone Restricted = 3

-- Policy actions: Allow=0, Deny=1, Audit=2, Transform=3, Delegate=4
encodeAction : PolicyAction -> Int
encodeAction Allow = 0
encodeAction Deny = 1
encodeAction Audit = 2
encodeAction Transform = 3
encodeAction Delegate = 4

export
proven_idris_policy_action_allows : Int -> Int
proven_idris_policy_action_allows action = encodeBool (action == 0 || action == 2)

export
proven_idris_policy_priority_compare : Int -> Int -> Int
proven_idris_policy_priority_compare p1 p2 =
  if p1 > p2 then 1
  else if p1 < p2 then (-1)
  else 0

export
proven_idris_policy_zone_mutable : Int -> Int
proven_idris_policy_zone_mutable zoneClass = encodeBool (zoneClass == 0 || zoneClass == 2)
