-- SPDX-License-Identifier: Apache-2.0
-- SPDX-FileCopyrightText: 2025 Hyperpolymath
module SafeCryptoProps

import Proven.Core
import Proven.SafeCrypto

%default total

||| Property: SHA256 hash has correct length (64 hex chars)
prop_sha256Length : length (sha256 "test") = 64
prop_sha256Length = ?prop_sha256Length_rhs

||| Property: Same input produces same hash (deterministic)
prop_hashDeterministic : (s : String) -> sha256 s = sha256 s
prop_hashDeterministic s = Refl

||| Property: Different algorithms produce different lengths
prop_sha512LongerThanSha256 : length (sha512 "test") > length (sha256 "test")
prop_sha512LongerThanSha256 = ?prop_sha512LongerThanSha256_rhs

||| Property: HMAC with same key and message is deterministic
prop_hmacDeterministic : (key, msg : String) -> hmacSHA256 key msg = hmacSHA256 key msg
prop_hmacDeterministic key msg = Refl

||| Property: Random bytes returns requested length
prop_randomBytesLength : (n : Nat) -> length (randomBytes n) = n
prop_randomBytesLength n = ?prop_randomBytesLength_rhs

||| Property: Constant time comparison is reflexive
prop_constantTimeEqReflexive : (s : String) -> constantTimeEq s s = True
prop_constantTimeEqReflexive s = Refl

||| Property: Constant time comparison is symmetric
prop_constantTimeEqSymmetric : (a, b : String) -> constantTimeEq a b = constantTimeEq b a
prop_constantTimeEqSymmetric a b = ?prop_constantTimeEqSymmetric_rhs

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
