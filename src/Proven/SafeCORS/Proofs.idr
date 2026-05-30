-- SPDX-License-Identifier: MPL-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
||| Proofs for SafeCORS operations
|||
||| Verifies that CORS policy validation catches the critical wildcard+credentials
||| misconfiguration, that the default policy denies all cross-origin requests,
||| and that header generation refuses to serve headers for misconfigured policies.
module Proven.SafeCORS.Proofs

import Proven.SafeCORS
import Data.String
import Data.List

%default total

--------------------------------------------------------------------------------
-- Default Policy Properties
--------------------------------------------------------------------------------

||| The default policy denies all origins.
public export
defaultPolicyDeniesAll : (origin : Origin) -> isOriginAllowed defaultPolicy origin = False
defaultPolicyDeniesAll _ = Refl

||| The default policy is not an unsafe configuration.
public export
defaultPolicyIsSafe : isUnsafeConfiguration defaultPolicy = False
defaultPolicyIsSafe = Refl

||| The default policy has no validation errors.
public export
defaultPolicyNoErrors : validatePolicy defaultPolicy = []
defaultPolicyNoErrors = Refl

||| The default policy does not generate headers (denies all).
public export
defaultPolicyNoHeaders : (origin : Origin) -> generateHeaders defaultPolicy origin = Nothing
defaultPolicyNoHeaders origin = Refl

--------------------------------------------------------------------------------
-- Wildcard+Credentials Safety
--------------------------------------------------------------------------------

||| AllowAny with credentials is always unsafe.
public export
wildcardCredsUnsafe : (policy : CORSPolicy) ->
                      policy.allowOrigins = AllowAny ->
                      policy.allowCredentials = True ->
                      isUnsafeConfiguration policy = True
wildcardCredsUnsafe policy origPrf credPrf =
  rewrite origPrf in rewrite credPrf in Refl

||| AllowAny without credentials is safe.
public export
wildcardNoCredsSafe : (policy : CORSPolicy) ->
                      policy.allowOrigins = AllowAny ->
                      policy.allowCredentials = False ->
                      isUnsafeConfiguration policy = False
wildcardNoCredsSafe policy origPrf credPrf =
  rewrite origPrf in rewrite credPrf in Refl

||| AllowSpecific with credentials is always safe.
public export
specificCredsSafe : (policy : CORSPolicy) ->
                    (origins : List Origin) ->
                    policy.allowOrigins = AllowSpecific origins ->
                    isUnsafeConfiguration policy = False
specificCredsSafe policy origins prf = rewrite prf in Refl

||| AllowNone is always safe regardless of credentials setting.
public export
noneAlwaysSafe : (policy : CORSPolicy) ->
                 policy.allowOrigins = AllowNone ->
                 isUnsafeConfiguration policy = False
noneAlwaysSafe policy prf = rewrite prf in Refl

--------------------------------------------------------------------------------
-- Header Generation Safety
--------------------------------------------------------------------------------

||| Bridge lemma (discharged 2026-05-30): if a policy is unsafely
||| configured, its `validatePolicy` list is non-empty.
|||
||| The earlier OWED comment claimed Idris2 0.8.0 could not reduce
||| `++` past the leading `if isUnsafeConfiguration policy then
||| [WildcardWithCredentials] else []`. In fact it CAN — `rewrite`ing
||| by the hypothesis collapses the `if` to its `True` arm, after
||| which `[WildcardWithCredentials] ++ tail` reduces to
||| `WildcardWithCredentials :: tail` and `isNil (_ :: _)` reduces to
||| `False` by `Refl`. The right operand of `++` (the opaque
||| `emptyMethods` conditional) never needs to be inspected because
||| `isNil` matches on the spine constructor of the *whole* list, not
||| on its content.
public export
validatePolicyNonNil : (policy : CORSPolicy) ->
                       isUnsafeConfiguration policy = True ->
                       isNil (validatePolicy policy) = False
validatePolicyNonNil policy prf = rewrite prf in Refl

||| DISCHARGED (Refs hyperpolymath/standards#158): when
||| `isUnsafeConfiguration policy = True`, `generateHeaders policy
||| origin` returns `Nothing`.
|||
||| Proof: `generateHeaders` short-circuits on
||| `not (isNil (validatePolicy policy))`. Rewriting by
||| `validatePolicyNonNil` reduces that guard to `not False = True`,
||| which fires the `Nothing` arm of the outer `if`. No `believe_me`,
||| no `postulate`, no `assert_total`.
public export
misconfiguredNoHeaders : (policy : CORSPolicy) ->
                         (origin : Origin) ->
                         isUnsafeConfiguration policy = True ->
                         generateHeaders policy origin = Nothing
misconfiguredNoHeaders policy origin prf =
  let nonNil : (isNil (validatePolicy policy) = False)
      nonNil = validatePolicyNonNil policy prf
  in rewrite nonNil in Refl

--------------------------------------------------------------------------------
-- Origin Matching Properties
--------------------------------------------------------------------------------

||| AllowNone rejects all origins.
public export
allowNoneRejectsAll : (policy : CORSPolicy) ->
                      policy.allowOrigins = AllowNone ->
                      (origin : Origin) ->
                      isOriginAllowed policy origin = False
allowNoneRejectsAll policy prf origin = rewrite prf in Refl

||| AllowAny accepts all origins.
public export
allowAnyAcceptsAll : (policy : CORSPolicy) ->
                     policy.allowOrigins = AllowAny ->
                     (origin : Origin) ->
                     isOriginAllowed policy origin = True
allowAnyAcceptsAll policy prf origin = rewrite prf in Refl
