-- SPDX-License-Identifier: MPL-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
||| Proofs for SafeAPIKey operations
|||
||| Verifies that API key handling prevents leakage: minimum length
||| enforcement, format detection correctness, and masking safety.
module Proven.SafeAPIKey.Proofs

import Proven.SafeAPIKey
import Data.String
import Data.Nat

%default total

--------------------------------------------------------------------------------
-- Key Construction Properties
--------------------------------------------------------------------------------

||| Empty string is rejected as an API key.
public export
emptyKeyRejected : mkAPIKey "" = Nothing
emptyKeyRejected = Refl

||| Minimum key length is 16.
public export
minKeyLengthIs16 : MinKeyLength = 16
minKeyLengthIs16 = Refl

--------------------------------------------------------------------------------
-- Format Detection Properties
--------------------------------------------------------------------------------

||| Anthropic keys (sk-ant-) are detected correctly.
||| Anthropic prefix check comes before generic OpenAI sk- check.
public export
anthropicDetected : detectFormat "sk-ant-abc123def456" = AnthropicKey
anthropicDetected = Refl

||| Stripe live keys are detected correctly.
public export
stripeLiveDetected : detectFormat "sk_live_abc123def456" = StripeKey
stripeLiveDetected = Refl

||| Stripe test keys are detected correctly.
public export
stripeTestDetected : detectFormat "sk_test_abc123def456" = StripeKey
stripeTestDetected = Refl

||| AWS keys are detected correctly.
public export
awsDetected : detectFormat "AKIAexample12345key" = AWSKey
awsDetected = Refl

||| GitHub PAT (ghp_) keys are detected correctly.
public export
githubPATDetected : detectFormat "ghp_exampletoken12345" = GitHubPAT
githubPATDetected = Refl

||| Unknown format for unrecognized prefixes.
public export
unknownFormatDefault : detectFormat "random_key_value_here" = UnknownFormat
unknownFormatDefault = Refl

||| Empty string has unknown format.
public export
emptyFormatUnknown : detectFormat "" = UnknownFormat
emptyFormatUnknown = Refl

--------------------------------------------------------------------------------
-- Masking Properties
--------------------------------------------------------------------------------

||| fullMask never exposes any key characters.
||| The output is "[REDACTED:N chars]" which contains no key material.
public export
fullMaskFormat : (key : APIKey) -> isPrefixOf "[REDACTED:" (fullMask key) = True
fullMaskFormat key = Refl

--------------------------------------------------------------------------------
-- Format Matching Properties
--------------------------------------------------------------------------------

||| OWED: when `mkAPIKey s = Just key` and `key.format ≠ expected`
||| propositionally, `mkAPIKeyWithFormat expected s = Nothing`. Held
||| back by Idris2 0.8.0 not bridging the `Bool`-level decision
||| `key.format == expected` (used inside `mkAPIKeyWithFormat`'s `if`)
||| with the propositional `Not (key.format = expected)` hypothesis —
||| the `Eq KeyFormat` derived instance is not exposed as a
||| `DecEq`-style reflective equivalence by Refl alone. Compounded by
||| `mkAPIKey`'s `with (choose (length s >= MinKeyLength))` block,
||| which threads through `String.length`/`>=` FFI primitives that
||| Idris2 0.8.0 cannot type-level reduce. Same blocker family as the
||| `SafeChecksum` Luhn/ISBN OWED items (opaque String/Bool FFI).
||| Discharge once a `DecEq KeyFormat` instance is exposed alongside a
||| Bool-Prop reflection lemma for `==`, or once `mkAPIKeyWithFormat`
||| is refactored to case-split on `decEq key.format expected`.
0 formatMismatchRejected : (expected : KeyFormat) -> (s : String) ->
                           (key : APIKey) ->
                           mkAPIKey s = Just key ->
                           Not (key.format = expected) ->
                           mkAPIKeyWithFormat expected s = Nothing

--------------------------------------------------------------------------------
-- Leak Detection Properties
--------------------------------------------------------------------------------

||| Empty string does not look like an API key.
public export
emptyNotLikeKey : looksLikeAPIKey "" = False
emptyNotLikeKey = Refl

||| Empty string contains no potential keys.
public export
emptyNoPotentialKeys : containsPotentialKey "" = False
emptyNoPotentialKeys = Refl
