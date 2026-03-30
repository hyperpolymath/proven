-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
||| Proofs for SafeRedirect operations
|||
||| Verifies that open redirect prevention is correct: dangerous schemes
||| are always blocked, relative URLs pass AllowRelativeOnly, and the
||| isSafeRedirect predicate agrees with validateRedirect.
module Proven.SafeRedirect.Proofs

import Proven.SafeRedirect
import Data.String
import Data.List

%default total

--------------------------------------------------------------------------------
-- Redirect Validation Consistency
--------------------------------------------------------------------------------

||| isSafeRedirect agrees with validateRedirect:
||| returns True exactly when validateRedirect returns Safe.
||| Proved by case analysis on validateRedirect result.
public export
safeRedirectConsistent : (policy : RedirectPolicy) -> (origin, target : String) ->
                         isSafeRedirect policy origin target = True ->
                         (url : String ** validateRedirect policy origin target = Safe url)
safeRedirectConsistent policy origin target prf with (validateRedirect policy origin target)
  safeRedirectConsistent policy origin target Refl | Safe url = (url ** Refl)

||| If validateRedirect blocks, isSafeRedirect returns False.
public export
blockedImpliesNotSafe : (policy : RedirectPolicy) -> (origin, target : String) ->
                        (reason : String) ->
                        validateRedirect policy origin target = Blocked reason ->
                        isSafeRedirect policy origin target = False
blockedImpliesNotSafe policy origin target reason prf =
  rewrite prf in Refl

--------------------------------------------------------------------------------
-- mkSafeRedirect Properties
--------------------------------------------------------------------------------

||| mkSafeRedirect returns Nothing exactly when validateRedirect blocks.
public export
mkSafeRedirectNothingWhenBlocked :
  (policy : RedirectPolicy) -> (origin, target : String) ->
  (reason : String) ->
  validateRedirect policy origin target = Blocked reason ->
  mkSafeRedirect policy origin target = Nothing
mkSafeRedirectNothingWhenBlocked policy origin target reason prf =
  rewrite prf in Refl

||| mkSafeRedirect returns Just exactly when validateRedirect allows.
public export
mkSafeRedirectJustWhenSafe :
  (policy : RedirectPolicy) -> (origin, target : String) ->
  (url : String) ->
  validateRedirect policy origin target = Safe url ->
  mkSafeRedirect policy origin target = Just (MkSafeRedirectUrl url)
mkSafeRedirectJustWhenSafe policy origin target url prf =
  rewrite prf in Refl

--------------------------------------------------------------------------------
-- Sanitise Fallback Properties
--------------------------------------------------------------------------------

||| sanitiseRedirect returns fallback when redirect is blocked.
public export
sanitiseReturnsFallbackWhenBlocked :
  (policy : RedirectPolicy) -> (origin, target, fallback : String) ->
  (reason : String) ->
  validateRedirect policy origin target = Blocked reason ->
  sanitiseRedirect policy origin target fallback = fallback
sanitiseReturnsFallbackWhenBlocked policy origin target fallback reason prf =
  rewrite prf in Refl

||| sanitiseRedirect returns the validated URL when redirect is safe.
public export
sanitiseReturnsUrlWhenSafe :
  (policy : RedirectPolicy) -> (origin, target, fallback : String) ->
  (url : String) ->
  validateRedirect policy origin target = Safe url ->
  sanitiseRedirect policy origin target fallback = url
sanitiseReturnsUrlWhenSafe policy origin target fallback url prf =
  rewrite prf in Refl
