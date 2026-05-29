-- SPDX-License-Identifier: MPL-2.0
-- SPDX-FileCopyrightText: 2025 Hyperpolymath
module SafeJWTProps

import Proven.Core
import Proven.SafeJWT

%default total

||| OWED: Valid JWT structure parses
||| Held back by Idris2 0.8.0 not reducing the underlying computation by
||| Refl at the type level (the original orphan-test author wrote this as
||| a `?prop_validJWTParses_rhs` hole). May discharge as `= Refl` once the relevant
||| String/List/Char reduction blocker lifts, or by manual case analysis;
||| revisit during Phase 2 Day-3+ DISCHARGE pass.
0 prop_validJWTParses : isOk (parseJWT "eyJhbGciOiJIUzI1NiJ9.eyJzdWIiOiIxIn0.sig") = True

||| OWED: Invalid JWT structure fails
||| Held back by Idris2 0.8.0 not reducing the underlying computation by
||| Refl at the type level (the original orphan-test author wrote this as
||| a `?prop_invalidJWTFails_rhs` hole). May discharge as `= Refl` once the relevant
||| String/List/Char reduction blocker lifts, or by manual case analysis;
||| revisit during Phase 2 Day-3+ DISCHARGE pass.
0 prop_invalidJWTFails : isErr (parseJWT "not.a.jwt.token") = True

||| Property: JWT with missing parts fails
prop_missingPartsFails : isErr (parseJWT "header.payload") = True
prop_missingPartsFails = Refl

||| Property: None algorithm rejected
prop_noneAlgRejected : isErr (verifyJWT "eyJhbGciOiJub25lIn0.eyJzdWIiOiIxIn0." "secret") = True
prop_noneAlgRejected = Refl

||| OWED: Expired JWT rejected
||| Held back by Idris2 0.8.0 not reducing the underlying computation by
||| Refl at the type level (the original orphan-test author wrote this as
||| a `?prop_expiredRejected_rhs` hole). May discharge as `= Refl` once the relevant
||| String/List/Char reduction blocker lifts, or by manual case analysis;
||| revisit during Phase 2 Day-3+ DISCHARGE pass.
0 prop_expiredRejected : (jwt : JWT) -> isExpired jwt = True ->
                         isErr (validateJWT jwt) = True

||| OWED: Not-before future JWT rejected
||| Held back by Idris2 0.8.0 not reducing the underlying computation by
||| Refl at the type level (the original orphan-test author wrote this as
||| a `?prop_nbfFutureRejected_rhs` hole). May discharge as `= Refl` once the relevant
||| String/List/Char reduction blocker lifts, or by manual case analysis;
||| revisit during Phase 2 Day-3+ DISCHARGE pass.
0 prop_nbfFutureRejected : (jwt : JWT) -> nbfInFuture jwt = True ->
                           isErr (validateJWT jwt) = True

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

||| OWED: Claims are preserved in roundtrip
||| Held back by Idris2 0.8.0 not reducing the underlying computation by
||| Refl at the type level (the original orphan-test author wrote this as
||| a `?prop_claimsPreserved_rhs` hole). May discharge as `= Refl` once the relevant
||| String/List/Char reduction blocker lifts, or by manual case analysis;
||| revisit during Phase 2 Day-3+ DISCHARGE pass.
0 prop_claimsPreserved : (claims : List (String, String)) -> (secret : String) ->
                         getClaims (createJWT HS256 secret claims) = claims

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
