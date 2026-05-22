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

||| OWED: When `isUnsafeConfiguration policy = True`, `generateHeaders
||| policy origin` returns `Nothing`. Held back by Idris2 0.8.0 not
||| reducing the chained `validatePolicy` / `generateHeaders` guard
||| under a single `Refl`: `generateHeaders` branches on `not (isNil
||| (validatePolicy policy))`, and `validatePolicy` itself is `if
||| isUnsafeConfiguration policy then [WildcardWithCredentials] else
||| [] ++ (if isNil allowMethods && not (isNil allowHeaders) then
||| [EmptyMethodList] else [])`. Rewriting by the hypothesis pushes
||| the inner `if` to the `True` arm, but the surrounding `++` /
||| `isNil` / `not` chain does not normalise to `True` definitionally
||| because the right operand of `++` is an opaque conditional over
||| `policy.allowMethods` / `policy.allowHeaders`. Discharge once
||| `validatePolicy` is refactored to expose a top-level disjunctive
||| case on `isUnsafeConfiguration` (or once a `cong`-style proof is
||| written by-hand routing through `appendNilLeftNeutral` / `isNil
||| (x :: xs) = False`). Same blocker family as the FFI-bound
||| `validateLuhn` / `validateISBN*` OWED items in
||| `Proven.SafeChecksum.Proofs` — opaque-spine list reduction under
||| Idris2 0.8.0.
export
0 misconfiguredNoHeaders : (policy : CORSPolicy) ->
                           (origin : Origin) ->
                           isUnsafeConfiguration policy = True ->
                           generateHeaders policy origin = Nothing

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
