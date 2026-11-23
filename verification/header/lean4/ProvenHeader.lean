-- SPDX-License-Identifier: CC-BY-SA-4.0
-- Copyright (c) 2026 Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
-- Hand re-proof of ProvenHeader.agda; no automatic witness transport claim.
import Sholes.Core
set_option warningAsError true
set_option maxRecDepth 8192
set_option maxHeartbeats 8000000
namespace ProvenHeader
open Sholes Sholes.Byte Sholes.Wire
def isBreak (b : Octet) : Bool := equal 256 b (indexAt 10 (extra := 245)) || equal 256 b (indexAt 13 (extra := 242))
def boolEqual : Bool → Bool → Bool
  | true, true => true | false, false => true | _, _ => false
theorem boolEqual_sound (a b : Bool) : boolEqual a b = true → a = b := by
  cases a <;> cases b <;> intro h <;> first | rfl | cases h
theorem classifier_check : every 256 (fun b => boolEqual (isBreak b) (hasBreak [decode b])) = true := rfl
theorem classifies (b : Octet) : isBreak b = hasBreak [decode b] :=
  boolEqual_sound _ _ (every_sound 256 _ classifier_check b)
def step : Bool → Octet → Bool
  | true, _ => true | false, b => isBreak b
def scan : Bool → List Octet → Bool
  | s, [] => s | s, b :: bs => scan (step s b) bs
theorem scan_correct (s : Bool) (bs : List Octet) : scan s bs = (s || hasBreak (Sholes.Wire.map decode bs)) := by
  induction bs generalizing s with
  | nil => cases s <;> rfl
  | cons b bs ih =>
    cases s with
    | true => exact ih true
    | false =>
      simp only [scan, step, ih, classifies, Sholes.Wire.map]
      cases decode b <;> rfl
theorem scan_append (s : Bool) (xs ys : List Octet) : scan s (xs ++ ys) = scan (scan s xs) ys := by
  induction xs generalizing s with
  | nil => rfl
  | cons x xs ih => exact ih (step s x)
theorem scan_canon (bs : List Octet) : scan false (canonWire bs) = scan false bs := by
  rw [scan_correct, scan_correct, wire_header_preserved]
end ProvenHeader
#print axioms ProvenHeader.classifies
#print axioms ProvenHeader.scan_correct
#print axioms ProvenHeader.scan_append
#print axioms ProvenHeader.scan_canon
