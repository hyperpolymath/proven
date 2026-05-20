-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
||| Safety proofs for SafeHeap heap data structure.
module Proven.SafeHeap.Proofs

import Proven.SafeHeap

%default total

public export
mkHeapElements : (es : List a) -> (s : Nat) -> (MkHeap es s).elements = es
mkHeapElements es s = Refl

public export
mkHeapSize : (es : List a) -> (s : Nat) -> (MkHeap es s).size = s
mkHeapSize es s = Refl

public export
mkMaxHeap : (h : Heap a) -> (MkMaxHeap h).heap = h
mkMaxHeap h = Refl

public export
heapEmptyShow : show HeapEmpty = "Heap is empty"
heapEmptyShow = Refl
