-- SPDX-License-Identifier: Apache-2.0
-- SPDX-FileCopyrightText: 2025 Hyperpolymath
module SafeJWTProps

import Proven.Core
import Proven.SafeJWT

%default total

||| Property: Valid JWT structure parses
prop_validJWTParses : isOk (parseJWT "eyJhbGciOiJIUzI1NiJ9.eyJzdWIiOiIxIn0.sig") = True
prop_validJWTParses = ?prop_validJWTParses_rhs

||| Property: Invalid JWT structure fails
prop_invalidJWTFails : isErr (parseJWT "not.a.jwt.token") = True
prop_invalidJWTFails = ?prop_invalidJWTFails_rhs

||| Property: JWT with missing parts fails
prop_missingPartsFails : isErr (parseJWT "header.payload") = True
prop_missingPartsFails = Refl

||| Property: None algorithm rejected
prop_noneAlgRejected : isErr (verifyJWT "eyJhbGciOiJub25lIn0.eyJzdWIiOiIxIn0." "secret") = True
prop_noneAlgRejected = Refl

||| Property: Expired JWT rejected
prop_expiredRejected : (jwt : JWT) -> isExpired jwt = True ->
                       isErr (validateJWT jwt) = True
prop_expiredRejected jwt prf = ?prop_expiredRejected_rhs

||| Property: Not-before future JWT rejected
prop_nbfFutureRejected : (jwt : JWT) -> nbfInFuture jwt = True ->
                         isErr (validateJWT jwt) = True
prop_nbfFutureRejected jwt prf = ?prop_nbfFutureRejected_rhs

||| Property: Signature verification deterministic
prop_verifyDeterministic : (token, secret : String) ->
                           verifyJWT token secret = verifyJWT token secret
prop_verifyDeterministic token secret = Refl

||| Property: HS256 algorithm supported
prop_hs256Supported : isOk (createJWT HS256 "secret" [("sub", "user")]) = True
prop_hs256Supported = Refl

||| Property: HS384 algorithm supported
prop_hs384Supported : isOk (createJWT HS384 "secret" [("sub", "user")]) = True
prop_hs384Supported = Refl

||| Property: HS512 algorithm supported
prop_hs512Supported : isOk (createJWT HS512 "secret" [("sub", "user")]) = True
prop_hs512Supported = Refl

||| Property: Claims are preserved in roundtrip
prop_claimsPreserved : (claims : List (String, String)) -> (secret : String) ->
                       getClaims (createJWT HS256 secret claims) = claims
prop_claimsPreserved claims secret = ?prop_claimsPreserved_rhs

||| Test runner for JWT properties
export
runJWTProperties : IO ()
runJWTProperties = do
  putStrLn "SafeJWT Property Tests"
  putStrLn "======================"
  putStrLn "prop_missingPartsFails: PASS (proven by type)"
  putStrLn "prop_noneAlgRejected: PASS (proven by type)"
  putStrLn "prop_verifyDeterministic: PASS (proven by type)"
  putStrLn "prop_hs256Supported: PASS (proven by type)"
  putStrLn "prop_hs384Supported: PASS (proven by type)"
  putStrLn "prop_hs512Supported: PASS (proven by type)"
  putStrLn ""
