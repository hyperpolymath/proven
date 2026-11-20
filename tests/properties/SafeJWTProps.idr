-- SPDX-License-Identifier: MPL-2.0
-- SPDX-FileCopyrightText: 2025 Hyperpolymath
module SafeJWTProps

import Proven.Core
import Proven.SafeJWT
import System

%default total

-- The real API: token splitting (`splitToken`, total), creation
-- (`createToken`/`createHS256Token`, total; signing is a stub), and the
-- validation pipeline (`validateAlgorithm`, `validateExp`, `validateNbf`,
-- `validateDecoded`, `validate`, all total over a `DecodedJWT`). The
-- string-level entry points `decode`, `decodeClaims`, `verifyJWT`,
-- `validateToken` are `covering`, so this total module cannot call them; the
-- properties are therefore stated on the total layers they delegate to.
-- There is no `parseJWT`, `createJWT`, `validateJWT` or `getClaims`.

||| Property: the 'none' algorithm is rejected by the default validation
||| pipeline, for every current time, header fields, claims and signature,
||| and hence by `validate` for every key.
prop_noneAlgRejected : (t : Integer) -> (key : SigningKey) ->
                       (raw : String) -> (typ, cty, kid, x5u, x5t, x5tS : Maybe String) ->
                       (claims : JWTClaims) -> (sig : List Bits8) -> (hs, ps : String) ->
                       validate Types.defaultValidation key t
                         (MkDecodedJWT raw (MkJWTHeader None typ cty kid x5u x5t x5tS)
                                       claims sig hs ps)
                       = Err AlgorithmNoneNotAllowed
prop_noneAlgRejected t key raw typ cty kid x5u x5t x5tS claims sig hs ps = Refl

||| Property: whenever `isExpired` holds, `validateExp` fails, for every time,
||| clock skew and claim set.
prop_expiredRejected : (t : Integer) -> (skew : ClockSkew) -> (c : JWTClaims) ->
                       isExpired t skew c = True -> isOk (validateExp t skew c) = False
prop_expiredRejected t skew (MkJWTClaims _ _ _ Nothing _ _ _ _) prf = absurd prf
prop_expiredRejected t skew (MkJWTClaims _ _ _ (Just e) _ _ _ _) prf = rewrite prf in Refl

||| Property: whenever `isNotYetValid` holds, `validateNbf` fails, for every
||| time, clock skew and claim set.
prop_nbfFutureRejected : (t : Integer) -> (skew : ClockSkew) -> (c : JWTClaims) ->
                         isNotYetValid t skew c = True -> isOk (validateNbf t skew c) = False
prop_nbfFutureRejected t skew (MkJWTClaims _ _ _ _ Nothing _ _ _) prf = absurd prf
prop_nbfFutureRejected t skew (MkJWTClaims _ _ _ _ (Just n) _ _ _) prf = rewrite prf in Refl

||| Property: HS256 token creation succeeds for every secret and claim set
prop_hs256Supported : (secret : List Bits8) -> (claims : JWTClaims) ->
                      isOk (createToken (minimalHeader HS256) claims (SecretKey secret)) = True
prop_hs256Supported secret claims = Refl

||| Property: HS384 token creation succeeds for every secret and claim set
prop_hs384Supported : (secret : List Bits8) -> (claims : JWTClaims) ->
                      isOk (createToken (minimalHeader HS384) claims (SecretKey secret)) = True
prop_hs384Supported secret claims = Refl

||| Property: HS512 token creation succeeds for every secret and claim set
prop_hs512Supported : (secret : List Bits8) -> (claims : JWTClaims) ->
                      isOk (createToken (minimalHeader HS512) claims (SecretKey secret)) = True
prop_hs512Supported secret claims = Refl

-- The orphan's `prop_verifyDeterministic` (`verifyJWT t s = verifyJWT t s`)
-- holds by Refl for any function and says nothing; it is replaced by
-- `prop_noneAlgRejected` above, a real statement about the pipeline.

||| OWED: claims survive a create/decode round trip, for `decode =
||| decodeClaims`. Blocked by: `decodeClaims` (and the JSON claim parser it
||| uses) is `covering`, so this total module can neither run nor mention it,
||| hence the family over the decoder; encoding and base64 also run over
||| `unpack`. This names the proposition; it is not a witness.
public export
ClaimsRoundTripObligation : (String -> Result JWTError JWTClaims) -> Type
ClaimsRoundTripObligation decode =
  (secret : String) -> (claims : JWTClaims) -> (tok : String) ->
  createHS256Token secret claims = Ok tok -> decode tok = Ok claims

private
check : String -> Bool -> IO ()
check label True = putStrLn ("PASS (execution): " ++ label)
check label False = do
  putStrLn ("FAIL: " ++ label)
  exitFailure

private
subUser : JWTClaims
subUser = { sub := Just "user" } emptyClaims

||| Test runner for JWT properties
export
runJWTProperties : IO ()
runJWTProperties = do
  putStrLn "SafeJWT Property Tests"
  putStrLn "======================"
  putStrLn "prop_noneAlgRejected: PASS (proven by type, all tokens/keys/times)"
  putStrLn "prop_expiredRejected: PASS (proven by type, all claims/times)"
  putStrLn "prop_nbfFutureRejected: PASS (proven by type, all claims/times)"
  putStrLn "prop_hs256Supported: PASS (proven by type, all secrets/claims)"
  putStrLn "prop_hs384Supported: PASS (proven by type, all secrets/claims)"
  putStrLn "prop_hs512Supported: PASS (proven by type, all secrets/claims)"
  check "valid three-part JWT structure splits"
    (case splitToken "eyJhbGciOiJIUzI1NiJ9.eyJzdWIiOiIxIn0.sig" of
       Ok segs => segs.header == "eyJhbGciOiJIUzI1NiJ9" &&
                  segs.payload == "eyJzdWIiOiIxIn0" && segs.signature == "sig"
       Err _ => False)
  check "four-part string rejected"
    (isErr (splitToken "not.a.jwt.token"))
  check "unsecured JWT (empty signature segment) splits"
    (isOk (splitToken "eyJhbGciOiJub25lIn0.eyJzdWIiOiIxIn0."))
  check "created HS256 token has three segments and the encoded claims"
    (case createHS256Token "secret" subUser of
       Ok tok => case splitToken tok of
         Ok segs => case base64UrlDecodeString segs.payload of
           Ok json => json == "{\"sub\":\"user\"}"
           Err _ => False
         Err _ => False
       Err _ => False)
  check "concrete expiry: no exp accepted, exp 1000 rejected at t=2000 (default skew)"
    (isOk (validateExp 2000 defaultClockSkew subUser) &&
     isErr (validateExp 2000 defaultClockSkew ({ exp := Just 1000 } subUser)))
  check "concrete nbf: nbf 5000 rejected at t=1000, accepted at t=5000"
    (isErr (validateNbf 1000 defaultClockSkew ({ nbf := Just 5000 } subUser)) &&
     isOk (validateNbf 5000 defaultClockSkew ({ nbf := Just 5000 } subUser)))
  -- Kept last: compact serialisation (RFC 7515 s7.1 / RFC 7519 s6.1) always
  -- has three segments; a FAIL here is a suspected library defect.
  check "two-part string (missing signature segment) rejected"
    (isErr (splitToken "header.payload"))
  putStrLn ""
