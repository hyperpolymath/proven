-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
||| Proofs for SafePassword operations
|||
||| This module contains proofs verifying password safety properties.
||| Non-trivial proofs are declared as postulates pending formal verification.
module Proven.SafePassword.Proofs

import Proven.Core
import Proven.SafePassword.Policy
import Proven.SafePassword.Hash
import Proven.SafePassword.Strength
import Data.List

%default total

--------------------------------------------------------------------------------
-- Policy Compliance Proofs
--------------------------------------------------------------------------------

||| Valid password meets length requirements
export postulate
validPasswordLength : (policy : PasswordPolicy) ->
                      (pwd : String) ->
                      null (checkPolicy policy pwd) = True ->
                      (length pwd >= policy.minLength,
                       length pwd <= policy.maxLength)

||| Policy check is deterministic
public export
policyCheckDeterministic : (policy : PasswordPolicy) ->
                           (pwd : String) ->
                           checkPolicy policy pwd = checkPolicy policy pwd
policyCheckDeterministic policy pwd = Refl

||| Empty password always fails non-zero minLength policy
export postulate
emptyPasswordFails : (policy : PasswordPolicy) ->
                     policy.minLength > 0 = True ->
                     null (checkPolicy policy "") = False

||| Longer password has more violations cleared
export postulate
longerPasswordBetter : (policy : PasswordPolicy) ->
                       (short, long : String) ->
                       length long > length short = True ->
                       length (checkPolicy policy long) <= length (checkPolicy policy short)

--------------------------------------------------------------------------------
-- Hash Security Proofs
--------------------------------------------------------------------------------

||| Argon2id parameters validation is sound
export postulate
argon2ParamsValid : (params : Argon2Params) ->
                    isRight (validateArgon2Params params) = True ->
                    (params.timeCost >= 1,
                     params.memoryCost >= 8192,
                     params.parallelism >= 1)

||| Bcrypt cost must be in valid range
export postulate
bcryptCostBounded : (params : BcryptParams) ->
                    isRight (validateBcryptParams params) = True ->
                    (params.cost >= 10, params.cost <= 31)

||| Default Argon2 params are always valid
export postulate
defaultArgon2Valid : isRight (validateArgon2Params defaultArgon2Params) = True

||| Default Bcrypt params are always valid
export postulate
defaultBcryptValid : isRight (validateBcryptParams defaultBcryptParams) = True

||| Default Scrypt params are always valid
export postulate
defaultScryptValid : isRight (validateScryptParams defaultScryptParams) = True

--------------------------------------------------------------------------------
-- Constant-Time Comparison Proofs
--------------------------------------------------------------------------------

||| Constant-time comparison is reflexive
export postulate
constantTimeRefl : (hash : List Bits8) ->
                   constantTimeHashCompare hash hash = True

||| Constant-time comparison is symmetric
export postulate
constantTimeSym : (h1, h2 : List Bits8) ->
                  constantTimeHashCompare h1 h2 = constantTimeHashCompare h2 h1

||| Different length hashes never match
export postulate
differentLengthNoMatch : (h1, h2 : List Bits8) ->
                         length h1 /= length h2 = True ->
                         constantTimeHashCompare h1 h2 = False

--------------------------------------------------------------------------------
-- Strength Analysis Proofs
--------------------------------------------------------------------------------

||| Strength score is bounded between 0 and 100
export postulate
strengthScoreBounded : (pwd : String) ->
                       (analyzeStrength pwd).score <= 100

||| Entropy is non-negative for any password
export postulate
entropyNonNegative : (pwd : String) ->
                     (analyzeStrength pwd).entropy >= 0.0

||| Longer passwords have higher or equal entropy
export postulate
longerHigherEntropy : (short, long : String) ->
                      length long > length short = True ->
                      (analyzeStrength long).entropy >= (analyzeStrength short).entropy

||| VeryStrong is the maximum strength level
export postulate
veryStrongMax : (level : StrengthLevel) -> level <= VeryStrong

||| Strength level ordering is transitive
export postulate
strengthTransitive : (a, b, c : StrengthLevel) ->
                     a <= b = True ->
                     b <= c = True ->
                     a <= c = True

--------------------------------------------------------------------------------
-- Pattern Detection Proofs
--------------------------------------------------------------------------------

||| Common password detection is accurate (requires detectPatterns impl analysis)
export postulate
commonPasswordDetected : (pwd : String) ->
                         (toLower pwd `elem` ["password", "123456", "qwerty"]) = True ->
                         any (\p => case p of CommonPassword _ => True; _ => False) (detectPatterns pwd) = True

||| Pattern penalties are non-negative
export postulate
patternPenaltyNonNeg : (p : Pattern) -> patternPenalty p >= 0

--------------------------------------------------------------------------------
-- Rehash Decision Proofs
--------------------------------------------------------------------------------

||| paramsAtLeast is reflexive
export postulate
paramsAtLeastRefl : (params : HashParams) ->
                    paramsAtLeast params params = True

||| Stronger params require rehash of weaker
export postulate
strongerRequiresRehash : (weak, strong : HashParams) ->
                         paramsAtLeast weak strong = False ->
                         paramsAtLeast strong weak = True ->
                         needsRehash (MkHashedPassword (paramsAlgorithm weak) "" "" weak) strong = True

--------------------------------------------------------------------------------
-- Policy Builder Proofs
--------------------------------------------------------------------------------

||| Builder produces valid policy
public export
builderProducesPolicy : build policyBuilder = defaultPolicy
builderProducesPolicy = Refl

||| withMinLength updates correctly
public export
withMinLengthCorrect : (n : Nat) -> (b : PolicyBuilder) ->
                       (build (withMinLength n b)).minLength = n
withMinLengthCorrect n b = Refl

||| Chained builders compose correctly
public export
chainedBuildersCompose : (n : Nat) ->
                         (build (withMinLength n (withUppercase policyBuilder))).minLength = n
chainedBuildersCompose n = Refl

--------------------------------------------------------------------------------
-- Requirement Satisfaction Proofs
--------------------------------------------------------------------------------

||| Meeting higher requirement implies meeting lower
export postulate
higherImpliesLower : (pwd : String) ->
                     (high, low : StrengthRequirement) ->
                     requiredLevel high >= requiredLevel low = True ->
                     meetsRequirement pwd high = True ->
                     meetsRequirement pwd low = True

||| VeryStrong satisfies all requirements
export postulate
veryStrongSatisfiesAll : (pwd : String) ->
                         quickStrengthCheck pwd = VeryStrong ->
                         (req : StrengthRequirement) ->
                         meetsRequirement pwd req = True
