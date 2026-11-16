{-# OPTIONS --safe --cubical #-}
-- SPDX-License-Identifier: CC-BY-SA-4.0
-- Copyright (c) 2026 Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
module RefCBits8Cubical where
open import RefCBits8
open import Agda.Builtin.Equality using (_≡_; refl)
open import Agda.Builtin.Maybe using (just)
open import Agda.Builtin.Cubical.Path renaming (_≡_ to _≡ᵖ_)
equality-path : ∀ {A : Set} {x y : A} → x ≡ y → x ≡ᵖ y
equality-path {x = x} refl i = x
round-trip-path : (b : Byte) → decode (encode b) ≡ᵖ just b
round-trip-path b = equality-path (round-trip b)
canonical-path : (w : Word) (b : Byte) → decode w ≡ just b → w ≡ᵖ encode b
canonical-path w b p = equality-path (accepted-canonical w b p)
