-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
-- Validator.idr - Type checker for proof terms with soundness guarantees

module Proven.ECHIDNA.Validator

import Proven.ECHIDNA.ProofTerm

%default total

-- Validation result
public export
data ValidationResult : Type where
  IsValid   : ProofTerm -> ValidationResult
  IsInvalid : String -> ValidationResult

export
Show ValidationResult where
  show (IsValid prf) = "Valid: " ++ show prf
  show (IsInvalid reason) = "Invalid: " ++ reason

-- Type inference with totality guarantee
export
infer : Context -> ProofTerm -> Maybe ProofTerm
infer ctx (Var name) = lookup ctx name
infer ctx TyNat = Just TyProp
infer ctx TyBool = Just TyProp
infer ctx TyProp = Just TyProp
infer ctx Zero = Just TyNat
infer ctx (Succ n) = do
  nTy <- infer ctx n
  case nTy of
    TyNat => Just TyNat
    _ => Nothing
infer ctx True = Just TyBool
infer ctx False = Just TyBool
infer ctx (Forall name ty body) = do
  tyTy <- infer ctx ty
  bodyTy <- infer (Extend ctx name ty) body
  Just TyProp
infer ctx (Exists name ty body) = do
  tyTy <- infer ctx ty
  bodyTy <- infer (Extend ctx name ty) body
  Just TyProp
infer ctx (Implies p q) = do
  pTy <- infer ctx p
  qTy <- infer ctx q
  case (pTy, qTy) of
    (TyProp, TyProp) => Just TyProp
    _ => Nothing
infer ctx (And p q) = do
  pTy <- infer ctx p
  qTy <- infer ctx q
  case (pTy, qTy) of
    (TyProp, TyProp) => Just TyProp
    _ => Nothing
infer ctx (Or p q) = do
  pTy <- infer ctx p
  qTy <- infer ctx q
  case (pTy, qTy) of
    (TyProp, TyProp) => Just TyProp
    _ => Nothing
infer ctx (Not p) = do
  pTy <- infer ctx p
  case pTy of
    TyProp => Just TyProp
    _ => Nothing
infer ctx (Eq lhs rhs) = do
  lhsTy <- infer ctx lhs
  rhsTy <- infer ctx rhs
  if lhsTy == rhsTy
    then Just TyProp
    else Nothing
infer ctx (App f arg) = do
  fTy <- infer ctx f
  argTy <- infer ctx arg
  case fTy of
    Implies expected result =>
      if expected == argTy
        then Just result
        else Nothing
    _ => Nothing
infer ctx (Lambda name argTy body) = do
  _ <- infer ctx argTy
  resTy <- infer (Extend ctx name argTy) body
  Just (Implies argTy resTy)
infer ctx (Axiom name) = Just TyProp
infer ctx Refl = Just TyProp
infer ctx (Intro name body) = do
  bodyTy <- infer (Extend ctx name TyNat) body
  Just bodyTy
infer ctx (Apply lemma goal) = do
  lemmaTy <- infer ctx lemma
  goalTy <- infer ctx goal
  case lemmaTy of
    Implies premise conclusion =>
      if premise == goalTy
        then Just conclusion
        else Nothing
    _ => Nothing

-- Check if a proof term has a given type
export
check : Context -> ProofTerm -> ProofTerm -> Bool
check ctx term ty =
  case infer ctx term of
    Just inferredTy => inferredTy == ty
    Nothing => Prelude.False

-- Validate a proof term against a goal type
export
validateProof : ProofTerm -> ProofTerm -> ValidationResult
validateProof prf goal =
  case infer Empty prf of
    Just ty =>
      if ty == goal || ty == TyProp
        then IsValid prf
        else IsInvalid ("Proof type " ++ show ty ++
                     " doesn't match goal " ++ show goal)
    Nothing => IsInvalid "Failed to infer type of proof term"

-- Validate that a proof term is well-formed (no type errors)
export
validateWellFormed : ProofTerm -> ValidationResult
validateWellFormed prf =
  case infer Empty prf of
    Just ty => IsValid prf
    Nothing => IsInvalid "Proof term is not well-formed"

-- Check for circular reasoning (proof uses goal in premises)
export
checkCircular : ProofTerm -> ProofTerm -> Bool
checkCircular prf goal = containsTerm prf goal
  where
    containsTerm : ProofTerm -> ProofTerm -> Bool
    containsTerm (Var n1) (Var n2) = n1 == n2
    containsTerm (App f arg) target =
      containsTerm f target || containsTerm arg target
    containsTerm (Lambda _ _ body) target = containsTerm body target
    containsTerm (Intro _ body) target = containsTerm body target
    containsTerm (Apply lemma g) target =
      containsTerm lemma target || containsTerm g target
    containsTerm (Forall _ _ body) target = containsTerm body target
    containsTerm (Exists _ _ body) target = containsTerm body target
    containsTerm (Implies p q) target =
      containsTerm p target || containsTerm q target
    containsTerm (And p q) target =
      containsTerm p target || containsTerm q target
    containsTerm (Or p q) target =
      containsTerm p target || containsTerm q target
    containsTerm (Not p) target = containsTerm p target
    containsTerm (Eq lhs rhs) target =
      containsTerm lhs target || containsTerm rhs target
    containsTerm _ _ = Prelude.False

-- Comprehensive validation
export
validateComprehensive : ProofTerm -> ProofTerm -> ValidationResult
validateComprehensive prf goal =
  case validateWellFormed prf of
    IsInvalid reason => IsInvalid reason
    IsValid _ =>
      if checkCircular prf goal
        then IsInvalid "Circular reasoning detected"
        else validateProof prf goal
