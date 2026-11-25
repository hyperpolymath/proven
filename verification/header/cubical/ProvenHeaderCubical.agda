{-# OPTIONS --safe --cubical #-}
-- SPDX-License-Identifier: CC-BY-SA-4.0
-- Copyright (c) 2026 Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
module ProvenHeaderCubical where
open import ProvenHeader
open import Agda.Builtin.Cubical.Path renaming (_≡_ to _≡ᵖ_)
equality-path : ∀ {A : Set} {x y : A} → x ≡ y → x ≡ᵖ y
equality-path {x = x} refl i = x
scan-path : (s : Bool) (bs : List Octet) → scan s bs ≡ᵖ (s ∨ hasBreak (map decode bs))
scan-path s bs = equality-path (scan-correct s bs)
canon-path : (bs : List Octet) → scan false (canonWire bs) ≡ᵖ scan false bs
canon-path bs = equality-path (scan-canon bs)
