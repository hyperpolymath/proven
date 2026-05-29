-- SPDX-License-Identifier: MPL-2.0
-- SPDX-FileCopyrightText: 2025 Hyperpolymath
module SafeCryptoProps

import Proven.Core
import Proven.SafeCrypto

%default total

||| OWED: SHA256 hash has correct length (64 hex chars)
||| Held back by Idris2 0.8.0 not reducing the underlying computation by
||| Refl at the type level (the original orphan-test author wrote this as
||| a `?prop_sha256Length_rhs` hole). May discharge as `= Refl` once the relevant
||| String/List/Char reduction blocker lifts, or by manual case analysis;
||| revisit during Phase 2 Day-3+ DISCHARGE pass.
0 prop_sha256Length : length (sha256 "test") = 64

||| Property: Same input produces same hash (deterministic)
prop_hashDeterministic : (s : String) -> sha256 s = sha256 s
prop_hashDeterministic s = Refl

||| OWED: Different algorithms produce different lengths
||| Held back by Idris2 0.8.0 not reducing the underlying computation by
||| Refl at the type level (the original orphan-test author wrote this as
||| a `?prop_sha512LongerThanSha256_rhs` hole). May discharge as `= Refl` once the relevant
||| String/List/Char reduction blocker lifts, or by manual case analysis;
||| revisit during Phase 2 Day-3+ DISCHARGE pass.
0 prop_sha512LongerThanSha256 : length (sha512 "test") > length (sha256 "test")

||| Property: HMAC with same key and message is deterministic
prop_hmacDeterministic : (key, msg : String) -> hmacSHA256 key msg = hmacSHA256 key msg
prop_hmacDeterministic key msg = Refl

||| OWED: Random bytes returns requested length
||| Held back by Idris2 0.8.0 not reducing the underlying computation by
||| Refl at the type level (the original orphan-test author wrote this as
||| a `?prop_randomBytesLength_rhs` hole). May discharge as `= Refl` once the relevant
||| String/List/Char reduction blocker lifts, or by manual case analysis;
||| revisit during Phase 2 Day-3+ DISCHARGE pass.
0 prop_randomBytesLength : (n : Nat) -> length (randomBytes n) = n

||| Property: Constant time comparison is reflexive
prop_constantTimeEqReflexive : (s : String) -> constantTimeEq s s = True
prop_constantTimeEqReflexive s = Refl

||| OWED: Constant time comparison is symmetric
||| Held back by Idris2 0.8.0 not reducing the underlying computation by
||| Refl at the type level (the original orphan-test author wrote this as
||| a `?prop_constantTimeEqSymmetric_rhs` hole). May discharge as `= Refl` once the relevant
||| String/List/Char reduction blocker lifts, or by manual case analysis;
||| revisit during Phase 2 Day-3+ DISCHARGE pass.
0 prop_constantTimeEqSymmetric : (a, b : String) -> constantTimeEq a b = constantTimeEq b a

||| Test runner for crypto properties
export
runCryptoProperties : IO ()
runCryptoProperties = do
  putStrLn "SafeCrypto Property Tests"
  putStrLn "========================="
  putStrLn "prop_hashDeterministic: PASS (proven by type)"
  putStrLn "prop_hmacDeterministic: PASS (proven by type)"
  putStrLn "prop_constantTimeEqReflexive: PASS (proven by type)"
  putStrLn ""
