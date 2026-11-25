{-# OPTIONS --safe --without-K --cubical-compatible #-}
-- SPDX-License-Identifier: CC-BY-SA-4.0
-- Copyright (c) 2026 Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
module ProvenHeader where
open import Sholes.Core public
open import Sholes.Wire public
open import Sholes.Streaming using (_++_)

_∨_ : Bool → Bool → Bool
true ∨ b = true
false ∨ b = b

boolEqual : Bool → Bool → Bool
boolEqual true true = true
boolEqual false false = true
boolEqual _ _ = false
boolEqual-sound : ∀ a b → boolEqual a b ≡ true → a ≡ b
boolEqual-sound true true p = refl
boolEqual-sound true false ()
boolEqual-sound false true ()
boolEqual-sound false false p = refl

-- Numeric octets, with CR=13 and LF=10; no text decoding or sentinel.
isBreak : Octet → Bool
isBreak b = equal {256} b (at 10 {245}) ∨ equal {256} b (at 13 {242})
classifier-check : every 256 (λ b → boolEqual (isBreak b) (hasBreak (decode b ∷ []))) ≡ true
classifier-check = refl
classifies : (b : Octet) → isBreak b ≡ hasBreak (decode b ∷ [])
classifies b = boolEqual-sound _ _
  (every-sound 256 (λ x → boolEqual (isBreak x) (hasBreak (decode x ∷ []))) classifier-check b)

step : Bool → Octet → Bool
step true _ = true
step false b = isBreak b
scan : Bool → List Octet → Bool
scan s [] = s
scan s (b ∷ bs) = scan (step s b) bs

-- This is an unbounded list proof, not the executable finite test corpus.
scan-correct : (s : Bool) (bs : List Octet) → scan s bs ≡ (s ∨ hasBreak (map decode bs))
scan-correct true [] = refl
scan-correct true (b ∷ bs) = scan-correct true bs
scan-correct false [] = refl
scan-correct false (b ∷ bs) rewrite classifies b with decode b
... | cr = scan-correct true bs
... | lf = scan-correct true bs
... | plain _ = scan-correct false bs

scan-append : (s : Bool) (xs ys : List Octet) → scan s (xs ++ ys) ≡ scan (scan s xs) ys
scan-append s [] ys = refl
scan-append s (x ∷ xs) ys = scan-append (step s x) xs ys

scan-canon : (bs : List Octet) → scan false (canonWire bs) ≡ scan false bs
scan-canon bs = trans (scan-correct false (canonWire bs))
  (trans (wire-header-preserved bs) (sym (scan-correct false bs)))
