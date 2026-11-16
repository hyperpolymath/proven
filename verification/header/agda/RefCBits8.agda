{-# OPTIONS --safe --without-K --cubical-compatible #-}
-- SPDX-License-Identifier: CC-BY-SA-4.0
-- Copyright (c) 2026 Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
-- Abstract 64-bit partition, little-endian by bit significance, not memory.
-- Source/compiler refinement and pointer-carrier validity are separate debts.
module RefCBits8 where
open import Agda.Builtin.Nat using (Nat; zero; suc)
open import Agda.Builtin.Bool using (Bool; true; false)
open import Agda.Builtin.Maybe using (Maybe; just; nothing)
open import Agda.Builtin.Equality using (_≡_; refl)

data Bits : Nat → Set where
  [] : Bits zero
  _∷_ : ∀ {n} → Bool → Bits n → Bits (suc n)
infixr 5 _∷_
zeros : (n : Nat) → Bits n
zeros zero = []
zeros (suc n) = false ∷ zeros n
allZero : ∀ {n} → Bits n → Bool
allZero [] = true
allZero (true ∷ bs) = false
allZero (false ∷ bs) = allZero bs
zeros-test : (n : Nat) → allZero (zeros n) ≡ true
zeros-test zero = refl
zeros-test (suc n) = zeros-test n
allZero-sound : ∀ {n} (bs : Bits n) → allZero bs ≡ true → bs ≡ zeros n
allZero-sound [] p = refl
allZero-sound (true ∷ bs) ()
allZero-sound (false ∷ bs) p rewrite allZero-sound bs p = refl

Byte : Set
Byte = Bits 8
record Word : Set where
  constructor word
  field
    tag : Bool          -- bit 0
    low : Bits 31       -- bits 1..31, must be zero
    payload : Byte      -- bits 32..39
    high : Bits 24      -- bits 40..63, must be zero
open Word
encode : Byte → Word
encode b = word true (zeros 31) b (zeros 24)
decode : Word → Maybe Byte
decode (word false lo b hi) = nothing
decode (word true lo b hi) with allZero lo | allZero hi
... | true | true = just b
... | true | false = nothing
... | false | _ = nothing

nothing≠just : ∀ {A B : Set} {b : A} → nothing ≡ just b → B
nothing≠just ()

round-trip : (b : Byte) → decode (encode b) ≡ just b
round-trip b = refl
accepted-canonical : (w : Word) (b : Byte) → decode w ≡ just b → w ≡ encode b
accepted-canonical (word false lo p hi) b ()
accepted-canonical (word true lo p hi) b ok with allZero lo in lo-ok | allZero hi in hi-ok
... | false | _ = nothing≠just ok
... | true | false = nothing≠just ok
... | true | true with ok
... | refl rewrite allZero-sound lo lo-ok | allZero-sound hi hi-ok = refl

null-word : Word
null-word = word false (zeros 31) (zeros 8) (zeros 24)
null-rejected : decode null-word ≡ nothing
null-rejected = refl
