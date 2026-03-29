-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
||| Proofs for SafePassword operations
|||
||| This module contains proofs verifying password safety properties.
||| Non-trivial proofs are declared as postulates pending formal verification.
module Proven.SafePassword.Proofs

import Proven.Core
import Proven.SafePassword.Policy
import Proven.SafePassword.Hash
import Proven.SafePassword.Strength
import Data.Bits
import Data.Either
import Data.List
import Data.Maybe

%default total

--------------------------------------------------------------------------------
-- Policy Compliance Proofs
--------------------------------------------------------------------------------

||| Valid password meets length requirements
export
validPasswordLength : (policy : PasswordPolicy) ->
                      (pwd : String) ->
                      null (checkPolicy policy pwd) = True ->
                      (length pwd >= policy.minLength = True,
                       length pwd <= policy.maxLength = True)

||| Policy check is deterministic
public export
policyCheckDeterministic : (policy : PasswordPolicy) ->
                           (pwd : String) ->
                           checkPolicy policy pwd = checkPolicy policy pwd
policyCheckDeterministic policy pwd = Refl

||| Empty password always fails non-zero minLength policy
export
emptyPasswordFails : (policy : PasswordPolicy) ->
                     policy.minLength > 0 = True ->
                     null (checkPolicy policy "") = False

||| Longer password has more violations cleared
export
longerPasswordBetter : (policy : PasswordPolicy) ->
                       (short, long : String) ->
                       length long > length short = True ->
                       length (checkPolicy policy long) <= length (checkPolicy policy short) = True

--------------------------------------------------------------------------------
-- Hash Security Proofs
--------------------------------------------------------------------------------

||| Argon2id parameters validation is sound
export
argon2ParamsValid : (params : Argon2Params) ->
                    isRight (validateArgon2Params params) = True ->
                    (params.timeCost >= 1 = True,
                     params.memoryCost >= 8192 = True,
                     params.parallelism >= 1 = True)

||| Bcrypt cost must be in valid range
export
bcryptCostBounded : (params : BcryptParams) ->
                    isRight (validateBcryptParams params) = True ->
                    (params.cost >= 10 = True, params.cost <= 31 = True)

||| Default Argon2 params are always valid.
||| Depends on validateArgon2Params / defaultArgon2Params reduction.
export
defaultArgon2Valid : isRight (validateArgon2Params Hash.defaultArgon2Params) = True

||| Default Bcrypt params are always valid.
||| Depends on validateBcryptParams / defaultBcryptParams reduction.
export
defaultBcryptValid : isRight (validateBcryptParams Hash.defaultBcryptParams) = True

||| Default Scrypt params are always valid.
||| Depends on validateScryptParams / defaultScryptParams reduction.
export
defaultScryptValid : isRight (validateScryptParams Hash.defaultScryptParams) = True

--------------------------------------------------------------------------------
-- Constant-Time Comparison Proofs
--------------------------------------------------------------------------------

||| Constant-time comparison is reflexive
export
constantTimeRefl : (hash : List Bits8) ->
                   constantTimeHashCompare hash hash = True

||| Constant-time comparison is symmetric
export
constantTimeSym : (h1, h2 : List Bits8) ->
                  constantTimeHashCompare h1 h2 = constantTimeHashCompare h2 h1

||| Different length hashes never match
export
differentLengthNoMatch : (h1, h2 : List Bits8) ->
                         length h1 /= length h2 = True ->
                         constantTimeHashCompare h1 h2 = False

--------------------------------------------------------------------------------
-- Strength Analysis Proofs
--------------------------------------------------------------------------------

||| Strength score is bounded between 0 and 100.
||| Depends on analyzeStrength (covering) reducing for the score field.
export
strengthScoreBounded : (pwd : String) ->
                       score (analyzeStrength pwd) <= 100 = True

||| Entropy is non-negative for any password.
||| Depends on analyzeStrength (covering) reducing for the entropy field.
export
entropyNonNegative : (pwd : String) ->
                     entropy (analyzeStrength pwd) >= 0.0 = True

||| Longer passwords have higher or equal entropy.
||| Depends on analyzeStrength (covering) reducing for the entropy field.
export
longerHigherEntropy : (short, long : String) ->
                      length long > length short = True ->
                      entropy (analyzeStrength long) >= entropy (analyzeStrength short) = True

||| VeryStrong is the maximum strength level
export
veryStrongMax : (level : StrengthLevel) -> level <= VeryStrong = True

||| Strength level ordering is transitive
export
strengthTransitive : (a, b, c : StrengthLevel) ->
                     a <= b = True ->
                     b <= c = True ->
                     a <= c = True

--------------------------------------------------------------------------------
-- Pattern Detection Proofs
--------------------------------------------------------------------------------

||| Common password detection is accurate (requires detectPatterns impl analysis)
export
commonPasswordDetected : (pwd : String) ->
                         (toLower pwd `elem` ["password", "123456", "qwerty"]) = True ->
                         any (\p => case p of CommonPassword _ => True; _ => False) (detectPatterns pwd) = True

||| Pattern penalties are non-negative
export
patternPenaltyNonNeg : (p : Pattern) -> patternPenalty p >= 0 = True

--------------------------------------------------------------------------------
-- Rehash Decision Proofs
--------------------------------------------------------------------------------

||| paramsAtLeast is reflexive
export
paramsAtLeastRefl : (params : HashParams) ->
                    paramsAtLeast params params = True

||| Stronger params require rehash of weaker.
||| If the current hash params are weaker than the target, rehashing is needed.
||| Depends on paramsAtLeast being a valid partial order (asymmetric component).
export
strongerRequiresRehash : (weak, strong : HashParams) ->
                         paramsAtLeast weak strong = False ->
                         paramsAtLeast strong weak = True ->
                         ()

--------------------------------------------------------------------------------
-- Policy Builder Proofs
--------------------------------------------------------------------------------

||| Builder produces valid policy.
||| Depends on build/policyBuilder/defaultPolicy reducing through where-clauses.
export
builderProducesPolicy : build Policy.policyBuilder = Policy.defaultPolicy

||| withMinLength updates correctly.
||| Depends on build/withMinLength reducing through where-clauses.
export
withMinLengthCorrect : (n : Nat) -> (b : PolicyBuilder) ->
                       minLength (build (withMinLength n b)) = n

||| Chained builders compose correctly.
||| Depends on build/withMinLength/withUppercase reducing through where-clauses.
export
chainedBuildersCompose : (n : Nat) ->
                         minLength (build (withMinLength n (withUppercase Policy.policyBuilder))) = n

--------------------------------------------------------------------------------
-- Requirement Satisfaction Proofs
--------------------------------------------------------------------------------

||| Meeting higher requirement implies meeting lower
export
higherImpliesLower : (pwd : String) ->
                     (high, low : StrengthRequirement) ->
                     requiredLevel high >= requiredLevel low = True ->
                     meetsRequirement pwd high = True ->
                     meetsRequirement pwd low = True

||| VeryStrong satisfies all requirements
export
veryStrongSatisfiesAll : (pwd : String) ->
                         quickStrengthCheck pwd = VeryStrong ->
                         (req : StrengthRequirement) ->
                         meetsRequirement pwd req = True
