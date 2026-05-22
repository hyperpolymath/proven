-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
||| Safety proofs for SafeMonotonic monotonic clock surface.
module Proven.SafeMonotonic.Proofs

import Proven.SafeMonotonic

%default total

public export
mkMonotonicValue : (n : Nat) -> (MkMonotonic n).value = n
mkMonotonicValue n = Refl

public export
mkLamportTimestamp :
  (t, i : Nat) -> (MkLamport t i).timestamp = t
mkLamportTimestamp t i = Refl

public export
mkLamportNodeId :
  (t, i : Nat) -> (MkLamport t i).nodeId = i
mkLamportNodeId t i = Refl

public export
mkVectorClocks :
  (cs : List (Nat, Nat)) -> (s : Nat) -> (MkVector cs s).clocks = cs
mkVectorClocks cs s = Refl

public export
mkVectorSelfId :
  (cs : List (Nat, Nat)) -> (s : Nat) -> (MkVector cs s).selfId = s
mkVectorSelfId cs s = Refl
