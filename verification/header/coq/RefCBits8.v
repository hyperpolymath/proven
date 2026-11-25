(* SPDX-License-Identifier: CC-BY-SA-4.0
   Copyright (c) 2026 Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
   Hand re-proof of RefCBits8.agda. No source refinement is claimed. *)
From Stdlib Require Import Bool Arith.
Module CodecBits8.
Inductive Bits : nat -> Type :=
| bnil : Bits 0
| bcons : forall {n}, bool -> Bits n -> Bits (S n).
Fixpoint zeros (n : nat) : Bits n :=
  match n with 0 => bnil | S k => bcons false (zeros k) end.
Fixpoint allZero {n} (bs : Bits n) : bool :=
  match bs with bnil => true | bcons b tail => if b then false else allZero tail end.
Lemma allZero_sound n (bs : Bits n) : allZero bs = true -> bs = zeros n.
Proof.
  induction bs as [|n b bs IH]; simpl.
  - reflexivity.
  - destruct b; [discriminate|]. intro H. rewrite (IH H). reflexivity.
Qed.
Definition Byte := Bits 8.
Record Word := word { tag : bool; low : Bits 31; payload : Byte; high : Bits 24 }.
Definition encode (b : Byte) : Word := word true (zeros 31) b (zeros 24).
Definition decode (w : Word) : option Byte :=
  if tag w then if allZero (low w) then if allZero (high w) then Some (payload w)
     else None else None else None.
Lemma round_trip b : decode (encode b) = Some b.
Proof. reflexivity. Qed.
Lemma accepted_canonical w b : decode w = Some b -> w = encode b.
Proof.
  destruct w as [t lo p hi]. destruct t; [|discriminate].
  unfold decode; simpl.
  destruct (allZero lo) eqn:L; [|discriminate].
  destruct (allZero hi) eqn:H; [|discriminate].
  intro E. injection E as E. subst p.
  rewrite (allZero_sound _ lo L), (allZero_sound _ hi H). reflexivity.
Qed.
Definition nullWord : Word := word false (zeros 31) (zeros 8) (zeros 24).
Lemma null_rejected : decode nullWord = None.
Proof. reflexivity. Qed.
Print Assumptions round_trip.
Print Assumptions accepted_canonical.
Print Assumptions null_rejected.
End CodecBits8.
