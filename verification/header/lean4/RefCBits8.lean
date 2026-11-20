-- SPDX-License-Identifier: CC-BY-SA-4.0
-- Copyright (c) 2026 Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
-- Hand re-proof of RefCBits8.agda, not automatic witness translation.
set_option warningAsError true
namespace RefCBits8
inductive Bits : Nat → Type where
  | nil : Bits 0
  | cons {n : Nat} : Bool → Bits n → Bits (n + 1)
def zeros : (n : Nat) → Bits n
  | 0 => .nil
  | n + 1 => .cons false (zeros n)
def allZero : {n : Nat} → Bits n → Bool
  | _, .nil => true
  | _, .cons true _ => false
  | _, .cons false bs => allZero bs
theorem allZero_sound {n : Nat} (bs : Bits n) : allZero bs = true → bs = zeros n := by
  induction bs with
  | nil => intro _; rfl
  | cons b bs ih =>
    cases b with
    | true => intro h; cases h
    | false => intro h; change allZero bs = true at h; cases ih h; rfl
abbrev Byte := Bits 8
structure Word where
  tag : Bool
  low : Bits 31
  payload : Byte
  high : Bits 24
def encode (b : Byte) : Word := ⟨true, zeros 31, b, zeros 24⟩
def decode (w : Word) : Option Byte :=
  match w.tag, allZero w.low, allZero w.high with
  | true, true, true => some w.payload
  | _, _, _ => none
theorem round_trip (b : Byte) : decode (encode b) = some b := rfl
theorem accepted_canonical (w : Word) (b : Byte) : decode w = some b → w = encode b := by
  cases w with
  | mk tag low payload high =>
    cases tag with
    | false => intro h; cases h
    | true =>
      cases hl : allZero low with
      | false => intro h; simp only [decode, hl] at h; cases h
      | true =>
        cases hh : allZero high with
        | false => intro h; simp only [decode, hl, hh] at h; cases h
        | true =>
          intro h
          simp only [decode, hl, hh] at h
          have hp : payload = b := Option.some.inj h
          cases allZero_sound low hl
          cases allZero_sound high hh
          cases hp
          rfl
def nullWord : Word := ⟨false, zeros 31, zeros 8, zeros 24⟩
theorem null_rejected : decode nullWord = none := rfl
end RefCBits8
#print axioms RefCBits8.round_trip
#print axioms RefCBits8.accepted_canonical
#print axioms RefCBits8.null_rejected
