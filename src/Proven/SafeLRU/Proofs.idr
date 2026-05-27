-- SPDX-License-Identifier: MPL-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
||| Safety proofs for SafeLRU least-recently-used cache.
module Proven.SafeLRU.Proofs

import Proven.SafeLRU

%default total

public export
mkEntryKey : (k1 : k) -> (v1 : v) -> (n : Nat) -> (MkEntry k1 v1 n).key = k1
mkEntryKey k1 v1 n = Refl

public export
mkEntryVal : (k1 : k) -> (v1 : v) -> (n : Nat) -> (MkEntry k1 v1 n).val = v1
mkEntryVal k1 v1 n = Refl

public export
mkEntryAccessOrder :
  (k1 : k) -> (v1 : v) -> (n : Nat) -> (MkEntry k1 v1 n).accessOrder = n
mkEntryAccessOrder k1 v1 n = Refl

public export
mkLRUCapacity :
  (c : Nat) -> (es : List (CacheEntry k v)) -> (ac : Nat)
  -> (MkLRU c es ac).capacity = c
mkLRUCapacity c es ac = Refl

public export
mkLRUEntries :
  (c : Nat) -> (es : List (CacheEntry k v)) -> (ac : Nat)
  -> (MkLRU c es ac).entries = es
mkLRUEntries c es ac = Refl

public export
mkLRUAccessCounter :
  (c : Nat) -> (es : List (CacheEntry k v)) -> (ac : Nat)
  -> (MkLRU c es ac).accessCounter = ac
mkLRUAccessCounter c es ac = Refl
