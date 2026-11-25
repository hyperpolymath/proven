-- SPDX-License-Identifier: MPL-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
module Proven.ECHIDNA.Kernel

import public Proven.ECHIDNA.ProofTerm

%default total

||| Denotation of closed natural-number terms. No variables or axioms.
public export
data NatValue : ProofTerm -> Nat -> Type where
  ValueZero : NatValue Zero Z
  ValueSucc : NatValue t n -> NatValue (Succ t) (S n)

||| Denotation of closed Boolean terms.
public export
data BoolValue : ProofTerm -> Bool -> Type where
  ValueTrue : BoolValue ProofTerm.True Prelude.True
  ValueFalse : BoolValue ProofTerm.False Prelude.False

||| Semantic validity in the supported fragment: both sides denote the
||| same value of the same type. There is no arbitrary-validity constructor.
public export
data GoalHolds : ProofTerm -> Type where
  NatEqual : NatValue lhs n -> NatValue rhs n -> GoalHolds (Eq lhs rhs)
  BoolEqual : BoolValue lhs b -> BoolValue rhs b -> GoalHolds (Eq lhs rhs)

||| The certificate is indexed by both the checked proof and its exact goal.
public export
data Certificate : ProofTerm -> ProofTerm -> Type where
  CheckedRefl : GoalHolds goal -> Certificate ProofTerm.Refl goal

public export
evalNat : (term : ProofTerm) -> Maybe (n ** NatValue term n)
evalNat Zero = Just (Z ** ValueZero)
evalNat (Succ t) = case evalNat t of
  Just (n ** value) => Just (S n ** ValueSucc value)
  Nothing => Nothing
evalNat _ = Nothing

public export
evalBool : (term : ProofTerm) -> Maybe (b ** BoolValue term b)
evalBool ProofTerm.True = Just (Prelude.True ** ValueTrue)
evalBool ProofTerm.False = Just (Prelude.False ** ValueFalse)
evalBool _ = Nothing

public export
sameNat : (x, y : Nat) -> Maybe (x = y)
sameNat Z Z = Just Refl
sameNat (S x) (S y) = case sameNat x y of
  Just Refl => Just Refl
  Nothing => Nothing
sameNat _ _ = Nothing

public export
sameBool : (x, y : Bool) -> Maybe (x = y)
sameBool Prelude.True Prelude.True = Just Refl
sameBool Prelude.False Prelude.False = Just Refl
sameBool _ _ = Nothing

public export
equalBools : (lhs, rhs : ProofTerm) -> Maybe (GoalHolds (Eq lhs rhs))
equalBools lhs rhs = case (evalBool lhs, evalBool rhs) of
  (Just (x ** left), Just (y ** right)) => case sameBool x y of
    Just Refl => Just (BoolEqual left right)
    Nothing => Nothing
  _ => Nothing

public export
equalValues : (lhs, rhs : ProofTerm) -> Maybe (GoalHolds (Eq lhs rhs))
equalValues lhs rhs = case (evalNat lhs, evalNat rhs) of
  (Just (x ** left), Just (y ** right)) => case sameNat x y of
    Just Refl => Just (NatEqual left right)
    Nothing => Nothing
  _ => equalBools lhs rhs

||| Only closed Nat/Bool equality with Refl is supported. All other syntax,
||| including named axioms, is rejected until it has a semantic checking rule.
public export
certify : (prf, goal : ProofTerm) -> Maybe (Certificate prf goal)
certify ProofTerm.Refl (Eq lhs rhs) = case equalValues lhs rhs of
  Just holds => Just (CheckedRefl holds)
  Nothing => Nothing
certify _ _ = Nothing

public export
certificateSound : Certificate prf goal -> GoalHolds goal
certificateSound (CheckedRefl holds) = holds

||| Semantic negative control: the audited false goal cannot have evidence.
public export
zeroNotSuccessor : GoalHolds (Eq Zero (Succ Zero)) -> Void
zeroNotSuccessor (NatEqual ValueZero (ValueSucc _)) impossible
zeroNotSuccessor (BoolEqual ValueTrue _) impossible
zeroNotSuccessor (BoolEqual ValueFalse _) impossible
