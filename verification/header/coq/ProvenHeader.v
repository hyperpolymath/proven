(* SPDX-License-Identifier: CC-BY-SA-4.0
   Copyright (c) 2026 Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
   Hand re-proof of ProvenHeader.agda. *)
From Stdlib Require Import List Bool.
Require Import Sholes_Core Sholes_Wire.
Import ListNotations Wire.
Module HeaderState.
Definition isBreak (b : Octet) : bool := equal 256 b (indexAt 10 245) || equal 256 b (indexAt 13 242).
Lemma classifier_check : every 256 (fun b => Bool.eqb (isBreak b) (hasBreak [decode b])) = true.
Proof. reflexivity. Qed.
Lemma classifies b : isBreak b = hasBreak [decode b].
Proof. apply Bool.eqb_prop. exact (every_sound 256 _ classifier_check b). Qed.
Definition step (s : bool) (b : Octet) : bool := if s then true else isBreak b.
Fixpoint scan (s : bool) (bs : list Octet) : bool :=
  match bs with [] => s | b :: bs => scan (step s b) bs end.
Lemma scan_correct s bs : scan s bs = (s || hasBreak (Wire.map decode bs)).
Proof. revert s. induction bs as [|b bs IH]; intro s.
  - destruct s; reflexivity.
  - destruct s; simpl.
    + exact (IH true).
    + rewrite IH, classifies. destruct (decode b); reflexivity.
Qed.
Lemma scan_append s xs ys : scan s (xs ++ ys) = scan (scan s xs) ys.
Proof. revert s. induction xs as [|x xs IH]; intro s; simpl; [reflexivity|apply IH]. Qed.
Lemma scan_canon bs : scan false (canonWire bs) = scan false bs.
Proof. rewrite !scan_correct, wire_header_preserved. reflexivity. Qed.
Print Assumptions classifies.
Print Assumptions scan_correct.
Print Assumptions scan_append.
Print Assumptions scan_canon.
End HeaderState.
