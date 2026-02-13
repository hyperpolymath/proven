-- SPDX-License-Identifier: Apache-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
||| JWT validation and claim verification
|||
||| This module provides comprehensive validation of JWT tokens
||| including signature verification, expiration, and claim validation.
module Proven.SafeJWT.Validate

import Proven.Core
import Proven.SafeJWT.Types
import Proven.SafeJWT.Decode
import Data.List
import Data.String

%default total

--------------------------------------------------------------------------------
-- Time Validation
--------------------------------------------------------------------------------

||| Check if token is expired
public export
isExpired : Integer -> ClockSkew -> JWTClaims -> Bool
isExpired currentTime skew claims =
  case claims.exp of
    Nothing => False  -- No expiration = not expired
    Just exp => currentTime > exp + cast skew.expLeeway

||| Check if token is not yet valid (nbf)
public export
isNotYetValid : Integer -> ClockSkew -> JWTClaims -> Bool
isNotYetValid currentTime skew claims =
  case claims.nbf of
    Nothing => False  -- No nbf = immediately valid
    Just nbf => currentTime < nbf - cast skew.nbfLeeway

||| Check if token was issued in the future
public export
isIssuedInFuture : Integer -> ClockSkew -> JWTClaims -> Bool
isIssuedInFuture currentTime skew claims =
  case claims.iat of
    Nothing => False
    Just iat => iat > currentTime + cast skew.nbfLeeway

||| Calculate token age in seconds
public export
tokenAge : Integer -> JWTClaims -> Maybe Integer
tokenAge currentTime claims = map (\iat => currentTime - iat) claims.iat

||| Check if token exceeds max age
public export
exceedsMaxAge : Integer -> Nat -> JWTClaims -> Bool
exceedsMaxAge currentTime maxAge claims =
  case tokenAge currentTime claims of
    Nothing => False  -- No iat = can't check age
    Just age => age > cast maxAge

--------------------------------------------------------------------------------
-- Time Validation Results
--------------------------------------------------------------------------------

||| Validate expiration time
public export
validateExp : Integer -> ClockSkew -> JWTClaims -> Result JWTError ()
validateExp currentTime skew claims =
  case claims.exp of
    Nothing => Ok ()
    Just exp =>
      if currentTime > exp + cast skew.expLeeway
        then Err (TokenExpired exp currentTime)
        else Ok ()

||| Validate not-before time
public export
validateNbf : Integer -> ClockSkew -> JWTClaims -> Result JWTError ()
validateNbf currentTime skew claims =
  case claims.nbf of
    Nothing => Ok ()
    Just nbf =>
      if currentTime < nbf - cast skew.nbfLeeway
        then Err (TokenNotYetValid nbf currentTime)
        else Ok ()

||| Validate issued-at time
public export
validateIat : Integer -> ClockSkew -> JWTClaims -> Result JWTError ()
validateIat currentTime skew claims =
  case claims.iat of
    Nothing => Ok ()
    Just iat =>
      if iat > currentTime + cast skew.nbfLeeway
        then Err (TokenIssuedInFuture iat currentTime)
        else Ok ()

||| Validate max age
public export
validateMaxAge : Integer -> Nat -> JWTClaims -> Result JWTError ()
validateMaxAge currentTime maxAge claims =
  case tokenAge currentTime claims of
    Nothing => Ok ()  -- No iat means we can't check age
    Just age =>
      if age > cast maxAge
        then Err (TokenTooOld age maxAge)
        else Ok ()

--------------------------------------------------------------------------------
-- Issuer and Audience Validation
--------------------------------------------------------------------------------

||| Validate issuer
public export
validateIssuer : String -> JWTClaims -> Result JWTError ()
validateIssuer expected claims =
  case claims.iss of
    Nothing => Err (IssuerMismatch expected Nothing)
    Just iss =>
      if iss == expected
        then Ok ()
        else Err (IssuerMismatch expected (Just iss))

||| Validate audience (checks if expected audience is in the list)
public export
validateAudience : String -> JWTClaims -> Result JWTError ()
validateAudience expected claims =
  let audList = getAudienceList claims
  in if expected `elem` audList
       then Ok ()
       else Err (AudienceMismatch expected audList)

||| Validate audience with multiple allowed values
public export
validateAudienceAny : List String -> JWTClaims -> Result JWTError ()
validateAudienceAny [] claims = Ok ()  -- No required audience
validateAudienceAny allowed claims =
  let audList = getAudienceList claims
  in if any (`elem` audList) allowed
       then Ok ()
       else Err (AudienceMismatch (show allowed) audList)

--------------------------------------------------------------------------------
-- Algorithm Validation
--------------------------------------------------------------------------------

||| Validate algorithm is allowed
public export
validateAlgorithm : List JWTAlgorithm -> Bool -> JWTHeader -> Result JWTError ()
validateAlgorithm allowed rejectNone header =
  let alg = header.alg
  in if rejectNone && alg == None
       then Err AlgorithmNoneNotAllowed
     else if null allowed
       then Ok ()  -- Empty list means allow all (except 'none' if rejectNone)
     else if alg `elem` allowed
       then Ok ()
       else Err (AlgorithmMismatch allowed alg)

--------------------------------------------------------------------------------
-- Required Claims Validation
--------------------------------------------------------------------------------

||| Check if a required claim is present
public export
hasRequiredClaim : String -> JWTClaims -> Bool
hasRequiredClaim name claims = isJust (getClaim name claims)

||| Validate all required claims are present
public export
validateRequiredClaims : List String -> JWTClaims -> Result JWTError ()
validateRequiredClaims [] _ = Ok ()
validateRequiredClaims (name :: rest) claims =
  if hasRequiredClaim name claims
    then validateRequiredClaims rest claims
    else Err (MissingClaim name)

--------------------------------------------------------------------------------
-- Signature Verification (Stubs for External Implementation)
--------------------------------------------------------------------------------

||| Verify HMAC signature
||| Note: Actual implementation requires crypto library
public export
verifyHMAC : JWTAlgorithm -> List Bits8 -> String -> List Bits8 -> Bool
verifyHMAC alg secret signingInput signature =
  -- Stub: In real implementation, compute HMAC and compare
  -- HMAC(secret, signingInput) == signature
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
  believe_me True

||| Verify RSA signature
public export
verifyRSA : JWTAlgorithm -> SigningKey -> String -> List Bits8 -> Bool
verifyRSA alg key signingInput signature =
  -- Stub: In real implementation, verify RSA signature
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
  believe_me True

||| Verify ECDSA signature
public export
verifyECDSA : JWTAlgorithm -> SigningKey -> String -> List Bits8 -> Bool
verifyECDSA alg key signingInput signature =
  -- Stub: In real implementation, verify ECDSA signature
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
  believe_me True

||| Verify EdDSA signature
public export
verifyEdDSA : SigningKey -> String -> List Bits8 -> Bool
verifyEdDSA key signingInput signature =
  -- Stub: In real implementation, verify EdDSA signature
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
  believe_me True

||| Verify signature using appropriate algorithm
public export
verifySignature : SigningKey -> DecodedJWT -> Result JWTError ()
verifySignature key jwt =
  let alg = jwt.header.alg
      sigInput = jwt.headerSegment ++ "." ++ jwt.payloadSegment
      sig = jwt.signature
  in if not (isKeyValidForAlgorithm key alg)
       then Err (KeyError ("Key not valid for algorithm " ++ show alg))
     else case key of
       NoKey =>
         if alg == None
           then Ok ()
           else Err (KeyError "NoKey only valid for 'none' algorithm")
       SecretKey secret =>
         if verifyHMAC alg secret sigInput sig
           then Ok ()
           else Err InvalidSignature
       RSAPublicKey _ _ =>
         if verifyRSA alg key sigInput sig
           then Ok ()
           else Err InvalidSignature
       RSAPrivateKey _ _ _ =>
         if verifyRSA alg key sigInput sig
           then Ok ()
           else Err InvalidSignature
       ECPublicKey _ _ _ =>
         if verifyECDSA alg key sigInput sig
           then Ok ()
           else Err InvalidSignature
       ECPrivateKey _ _ _ _ =>
         if verifyECDSA alg key sigInput sig
           then Ok ()
           else Err InvalidSignature
       EdPublicKey _ =>
         if verifyEdDSA key sigInput sig
           then Ok ()
           else Err InvalidSignature
       EdPrivateKey _ =>
         if verifyEdDSA key sigInput sig
           then Ok ()
           else Err InvalidSignature

--------------------------------------------------------------------------------
-- Full Validation
--------------------------------------------------------------------------------

||| Validate a decoded JWT against options
public export
validateDecoded : ValidationOptions -> Integer -> DecodedJWT -> Result JWTError ()
validateDecoded opts currentTime jwt = do
  -- Validate algorithm
  validateAlgorithm opts.allowedAlgorithms opts.rejectNone jwt.header
  -- Validate time claims
  when opts.validateExp $ validateExp currentTime opts.clockSkew jwt.claims
  when opts.validateNbf $ validateNbf currentTime opts.clockSkew jwt.claims
  when opts.validateIat $ validateIat currentTime opts.clockSkew jwt.claims
  -- Validate max age if specified
  case opts.maxAge of
    Just age => validateMaxAge currentTime age jwt.claims
    Nothing => Ok ()
  -- Validate issuer if required
  case opts.requiredIssuer of
    Just iss => validateIssuer iss jwt.claims
    Nothing => Ok ()
  -- Validate audience if required
  case opts.requiredAudience of
    Just aud => validateAudience aud jwt.claims
    Nothing => Ok ()
  -- Validate required claims
  validateRequiredClaims opts.requiredClaims jwt.claims
  where
    when : Bool -> Result JWTError () -> Result JWTError ()
    when False _ = Ok ()
    when True action = action

||| Full validation including signature verification
public export
validate : ValidationOptions -> SigningKey -> Integer -> DecodedJWT ->
           Result JWTError ValidatedJWT
validate opts key currentTime jwt = do
  -- Validate claims and options
  validateDecoded opts currentTime jwt
  -- Verify signature
  verifySignature key jwt
  -- Return validated JWT
  Ok (MkValidatedJWT jwt currentTime "validated")

||| Validate a token string (decode + validate)
public export
validateToken : ValidationOptions -> SigningKey -> Integer -> String ->
                Result JWTError ValidatedJWT
validateToken opts key currentTime token = do
  jwt <- decode token
  validate opts key currentTime jwt

--------------------------------------------------------------------------------
-- Quick Validation Helpers
--------------------------------------------------------------------------------

||| Quick validation with just a secret key (HMAC)
public export
validateWithSecret : String -> Integer -> String -> Result JWTError ValidatedJWT
validateWithSecret secret currentTime token =
  let key = SecretKey (map (cast . ord) (unpack secret))
  in validateToken defaultValidation key currentTime token

||| Quick validation without signature check (claims only)
||| WARNING: Only use when you trust the token source
public export
validateClaimsOnly : ValidationOptions -> Integer -> String -> Result JWTError DecodedJWT
validateClaimsOnly opts currentTime token = do
  jwt <- decode token
  validateDecoded opts currentTime jwt
  Ok jwt

||| Check if token is valid (returns Bool instead of Result)
public export
isValid : ValidationOptions -> SigningKey -> Integer -> String -> Bool
isValid opts key currentTime token =
  isOk (validateToken opts key currentTime token)

--------------------------------------------------------------------------------
-- Validation Builders
--------------------------------------------------------------------------------

||| Start building validation options
public export
validationBuilder : ValidationOptions
validationBuilder = defaultValidation

||| Require specific algorithms
public export
withAlgorithms : List JWTAlgorithm -> ValidationOptions -> ValidationOptions
withAlgorithms algs opts = { allowedAlgorithms := algs } opts

||| Require specific issuer
public export
withIssuer : String -> ValidationOptions -> ValidationOptions
withIssuer iss opts = { requiredIssuer := Just iss } opts

||| Require specific audience
public export
withAudience : String -> ValidationOptions -> ValidationOptions
withAudience aud opts = { requiredAudience := Just aud } opts

||| Set max token age
public export
withMaxAge : Nat -> ValidationOptions -> ValidationOptions
withMaxAge age opts = { maxAge := Just age } opts

||| Require specific claims
public export
withRequiredClaims : List String -> ValidationOptions -> ValidationOptions
withRequiredClaims claims opts = { requiredClaims := claims } opts

||| Set clock skew tolerance
public export
withClockSkew : Nat -> ValidationOptions -> ValidationOptions
withClockSkew seconds opts = { clockSkew := MkClockSkew seconds seconds } opts

||| Disable expiration validation
public export
ignoreExpiration : ValidationOptions -> ValidationOptions
ignoreExpiration opts = { validateExp := False } opts

||| Allow 'none' algorithm (dangerous!)
public export
allowNone : ValidationOptions -> ValidationOptions
allowNone opts = { rejectNone := False } opts

--------------------------------------------------------------------------------
-- Claim Extractors (Post-Validation)
--------------------------------------------------------------------------------

||| Extract subject from validated JWT
public export
getSubject : ValidatedJWT -> Maybe String
getSubject vjwt = vjwt.decoded.claims.sub

||| Extract issuer from validated JWT
public export
getIssuer : ValidatedJWT -> Maybe String
getIssuer vjwt = vjwt.decoded.claims.iss

||| Extract expiration time from validated JWT
public export
getExpiration : ValidatedJWT -> Maybe Integer
getExpiration vjwt = vjwt.decoded.claims.exp

||| Extract custom claim from validated JWT
public export
getCustomClaim : String -> ValidatedJWT -> Maybe ClaimValue
getCustomClaim name vjwt = getClaim name vjwt.decoded.claims

||| Get time until expiration (in seconds)
public export
timeUntilExpiration : Integer -> ValidatedJWT -> Maybe Integer
timeUntilExpiration currentTime vjwt =
  map (\exp => exp - currentTime) vjwt.decoded.claims.exp

||| Check if token is close to expiring (within threshold seconds)
public export
isCloseToExpiring : Integer -> Nat -> ValidatedJWT -> Bool
isCloseToExpiring currentTime threshold vjwt =
  case timeUntilExpiration currentTime vjwt of
    Nothing => False
    Just remaining => remaining <= cast threshold
