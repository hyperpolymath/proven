-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
||| Proofs for SafeCSP operations
|||
||| Verifies that Content Security Policy construction is sound:
||| unsafe sources are correctly identified, the strict policy
||| has no warnings, and policy composition preserves structure.
module Proven.SafeCSP.Proofs

import Proven.SafeCSP
import Data.String
import Data.List

%default total

--------------------------------------------------------------------------------
-- Unsafe Source Identification
--------------------------------------------------------------------------------

||| 'unsafe-inline' is classified as unsafe.
public export
unsafeInlineIsUnsafe : isUnsafeSource UnsafeInline = True
unsafeInlineIsUnsafe = Refl

||| 'unsafe-eval' is classified as unsafe.
public export
unsafeEvalIsUnsafe : isUnsafeSource UnsafeEval = True
unsafeEvalIsUnsafe = Refl

||| Wildcard (*) is classified as unsafe.
public export
wildcardIsUnsafe : isUnsafeSource WildcardHost = True
wildcardIsUnsafe = Refl

||| 'self' is not classified as unsafe.
public export
selfIsNotUnsafe : isUnsafeSource Self = False
selfIsNotUnsafe = Refl

||| 'none' is not classified as unsafe.
public export
noneIsNotUnsafe : isUnsafeSource None = False
noneIsNotUnsafe = Refl

||| 'strict-dynamic' is not classified as unsafe.
public export
strictDynamicIsNotUnsafe : isUnsafeSource StrictDynamic = False
strictDynamicIsNotUnsafe = Refl

||| Nonce-based sources are not classified as unsafe.
public export
nonceIsNotUnsafe : (n : String) -> isUnsafeSource (Nonce n) = False
nonceIsNotUnsafe _ = Refl

||| Hash-based sources are not classified as unsafe.
public export
hashIsNotUnsafe : (alg, h : String) -> isUnsafeSource (Hash alg h) = False
hashIsNotUnsafe _ _ = Refl

||| Host-based sources are not classified as unsafe.
public export
hostIsNotUnsafe : (h : String) -> isUnsafeSource (Host h) = False
hostIsNotUnsafe _ = Refl

--------------------------------------------------------------------------------
-- Strict Policy Properties
--------------------------------------------------------------------------------

||| The strict policy is safe (no audit warnings).
||| strictPolicy uses only Self and None, which are safe sources.
public export
strictPolicyIsSafe : isSafePolicy strictPolicy = True
strictPolicyIsSafe = Refl

--------------------------------------------------------------------------------
-- Empty Policy Properties
--------------------------------------------------------------------------------

||| Empty policy renders to empty string.
public export
emptyPolicyRendersEmpty : renderPolicy emptyPolicy = ""
emptyPolicyRendersEmpty = Refl

||| Empty policy is missing default-src (should warn).
||| An empty policy with no directives triggers MissingDefaultSrc.
public export
emptyPolicyHasWarnings : isSafePolicy emptyPolicy = False
emptyPolicyHasWarnings = Refl

--------------------------------------------------------------------------------
-- Directive Addition Properties
--------------------------------------------------------------------------------

||| Adding a directive to empty policy creates a single-directive policy.
public export
addToEmptyPolicy : (d : Directive) ->
                   (addDirective d emptyPolicy).directives = [d]
addToEmptyPolicy _ = Refl

||| Adding a directive preserves existing directives.
public export
addDirectivePreserves : (d : Directive) -> (p : CSPPolicy) ->
                        (addDirective d p).directives = d :: p.directives
addDirectivePreserves _ _ = Refl
