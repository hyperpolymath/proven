-- SPDX-License-Identifier: Palimpsest-MPL-1.0
||| SafeJWT - JWT operations that cannot crash
|||
||| This module provides safe JWT operations including:
||| - Token decoding without exceptions
||| - Claim validation with type safety
||| - Signature verification (with crypto stubs)
||| - Formal proofs of security properties
|||
||| Example usage:
||| ```idris
||| -- Validate a token with secret key
||| result <- validateWithSecret "my-secret-key" currentTime tokenString
|||
||| -- Validate with custom options
||| let opts = defaultValidation
|||            |> withIssuer "https://auth.example.com"
|||            |> withAudience "my-app"
|||            |> withMaxAge 3600
||| result <- validateToken opts key currentTime tokenString
|||
||| -- Decode without validation (inspect only)
||| decoded <- decode tokenString
||| ```
module Proven.SafeJWT

import public Proven.Core
import public Proven.SafeJWT.Types
import public Proven.SafeJWT.Decode
import public Proven.SafeJWT.Validate
import public Proven.SafeJWT.Proofs

import Data.List
import Data.String

%default total

--------------------------------------------------------------------------------
-- High-Level API
--------------------------------------------------------------------------------

||| Decode and validate a JWT in one step
|||
||| This is the primary entry point for JWT validation.
||| Uses default validation options with the provided secret.
public export
verifyJWT : (secret : String) -> (currentTime : Integer) -> (token : String) ->
            Result JWTError ValidatedJWT
verifyJWT secret = validateWithSecret secret

||| Decode a JWT without validation
|||
||| Use this to inspect a token before deciding how to validate it.
||| WARNING: The token is NOT verified - do not trust claims until validated.
public export
inspectJWT : String -> Result JWTError DecodedJWT
inspectJWT = decode

||| Quick check if a token appears valid
|||
||| Performs full validation and returns a boolean.
public export
isTokenValid : (secret : String) -> (currentTime : Integer) -> (token : String) -> Bool
isTokenValid secret currentTime token =
  let key = SecretKey (map (cast . ord) (unpack secret))
  in isValid defaultValidation key currentTime token

--------------------------------------------------------------------------------
-- Token Creation (Encoding)
--------------------------------------------------------------------------------

||| Base64URL encode bytes
base64UrlEncode : List Bits8 -> String
base64UrlEncode bytes = pack (go bytes)
  where
    alphabet : String
    alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_"

    charAt : Nat -> Char
    charAt n = case index' n (unpack alphabet) of
                 Just c => c
                 Nothing => 'A'
      where
        index' : Nat -> List a -> Maybe a
        index' _ [] = Nothing
        index' Z (x :: _) = Just x
        index' (S k) (_ :: xs) = index' k xs

    go : List Bits8 -> List Char
    go [] = []
    go [a] =
      let b0 = cast a `shiftR` 2
          b1 = (cast a .&. 0x03) `shiftL` 4
      in [charAt b0, charAt b1]
    go [a, b] =
      let b0 = cast a `shiftR` 2
          b1 = ((cast a .&. 0x03) `shiftL` 4) .|. (cast b `shiftR` 4)
          b2 = (cast b .&. 0x0F) `shiftL` 2
      in [charAt b0, charAt b1, charAt b2]
    go (a :: b :: c :: rest) =
      let b0 = cast a `shiftR` 2
          b1 = ((cast a .&. 0x03) `shiftL` 4) .|. (cast b `shiftR` 4)
          b2 = ((cast b .&. 0x0F) `shiftL` 2) .|. (cast c `shiftR` 6)
          b3 = cast c .&. 0x3F
      in charAt b0 :: charAt b1 :: charAt b2 :: charAt b3 :: go rest

||| Base64URL encode a string
base64UrlEncodeString : String -> String
base64UrlEncodeString s = base64UrlEncode (map (cast . ord) (unpack s))

||| Encode header to JSON
encodeHeader : JWTHeader -> String
encodeHeader h =
  let algPart = "\"alg\":\"" ++ show h.alg ++ "\""
      typPart = case h.typ of
                  Just t => ",\"typ\":\"" ++ t ++ "\""
                  Nothing => ""
      kidPart = case h.kid of
                  Just k => ",\"kid\":\"" ++ k ++ "\""
                  Nothing => ""
  in "{" ++ algPart ++ typPart ++ kidPart ++ "}"

||| Encode claim value to JSON
encodeClaimValue : ClaimValue -> String
encodeClaimValue (ClaimString s) = "\"" ++ escapeJsonString s ++ "\""
  where
    escapeJsonString : String -> String
    escapeJsonString str = pack (go (unpack str))
      where
        go : List Char -> List Char
        go [] = []
        go ('"' :: cs) = '\\' :: '"' :: go cs
        go ('\\' :: cs) = '\\' :: '\\' :: go cs
        go ('\n' :: cs) = '\\' :: 'n' :: go cs
        go ('\r' :: cs) = '\\' :: 'r' :: go cs
        go ('\t' :: cs) = '\\' :: 't' :: go cs
        go (c :: cs) = c :: go cs
encodeClaimValue (ClaimInt i) = show i
encodeClaimValue (ClaimBool True) = "true"
encodeClaimValue (ClaimBool False) = "false"
encodeClaimValue ClaimNull = "null"
encodeClaimValue (ClaimArray xs) = "[" ++ join "," (map (\s => "\"" ++ s ++ "\"") xs) ++ "]"
  where
    join : String -> List String -> String
    join _ [] = ""
    join _ [x] = x
    join sep (x :: xs) = x ++ sep ++ join sep xs
encodeClaimValue (ClaimObject _) = "{}"

||| Encode claims to JSON
encodeClaims : JWTClaims -> String
encodeClaims claims =
  let parts = catMaybes
        [ map (\s => "\"iss\":\"" ++ s ++ "\"") claims.iss
        , map (\s => "\"sub\":\"" ++ s ++ "\"") claims.sub
        , map encodeAud claims.aud
        , map (\e => "\"exp\":" ++ show e) claims.exp
        , map (\n => "\"nbf\":" ++ show n) claims.nbf
        , map (\i => "\"iat\":" ++ show i) claims.iat
        , map (\j => "\"jti\":\"" ++ j ++ "\"") claims.jti
        ] ++ map encodeCustom claims.customClaims
  in "{" ++ join "," parts ++ "}"
  where
    join : String -> List String -> String
    join _ [] = ""
    join _ [x] = x
    join sep (x :: xs) = x ++ sep ++ join sep xs

    encodeAud : Either String (List String) -> String
    encodeAud (Left s) = "\"aud\":\"" ++ s ++ "\""
    encodeAud (Right xs) = "\"aud\":[" ++ join "," (map (\s => "\"" ++ s ++ "\"") xs) ++ "]"

    encodeCustom : (String, ClaimValue) -> String
    encodeCustom (k, v) = "\"" ++ k ++ "\":" ++ encodeClaimValue v

||| Sign the token (stub - needs crypto implementation)
signToken : JWTAlgorithm -> SigningKey -> String -> List Bits8
signToken None _ _ = []
signToken _ _ signingInput =
  -- Stub: actual implementation needs crypto library
  map (cast . ord) (unpack "stub-signature")

||| Create a JWT token
|||
||| Note: Actual signing requires crypto library integration.
||| This provides the structure for token creation.
public export
createToken : JWTHeader -> JWTClaims -> SigningKey -> Result JWTError String
createToken header claims key =
  if not (isKeyValidForAlgorithm key header.alg)
    then Err (KeyError "Key type does not match algorithm")
    else
      let headerJson = encodeHeader header
          headerB64 = base64UrlEncodeString headerJson
          claimsJson = encodeClaims claims
          claimsB64 = base64UrlEncodeString claimsJson
          signingInput = headerB64 ++ "." ++ claimsB64
          signature = signToken header.alg key signingInput
          sigB64 = base64UrlEncode signature
      in Ok (signingInput ++ "." ++ sigB64)

||| Create a simple JWT with HMAC-SHA256
public export
createHS256Token : (secret : String) -> JWTClaims -> Result JWTError String
createHS256Token secret claims =
  let header = minimalHeader HS256
      key = SecretKey (map (cast . ord) (unpack secret))
  in createToken header claims key

--------------------------------------------------------------------------------
-- Claim Builders
--------------------------------------------------------------------------------

||| Create claims with expiration
public export
claimsWithExp : (sub : String) -> (exp : Integer) -> JWTClaims
claimsWithExp sub exp =
  { sub := Just sub
  , exp := Just exp
  } emptyClaims

||| Create claims with full timestamp info
public export
claimsWithTimes : (sub : String) -> (iat : Integer) -> (exp : Integer) -> JWTClaims
claimsWithTimes sub iat exp =
  { sub := Just sub
  , iat := Just iat
  , exp := Just exp
  } emptyClaims

||| Add issuer to claims
public export
withClaimIssuer : String -> JWTClaims -> JWTClaims
withClaimIssuer iss claims = { iss := Just iss } claims

||| Add subject to claims
public export
withClaimSubject : String -> JWTClaims -> JWTClaims
withClaimSubject sub claims = { sub := Just sub } claims

||| Add single audience to claims
public export
withClaimAudience : String -> JWTClaims -> JWTClaims
withClaimAudience aud claims = { aud := Just (Left aud) } claims

||| Add multiple audiences to claims
public export
withClaimAudiences : List String -> JWTClaims -> JWTClaims
withClaimAudiences auds claims = { aud := Just (Right auds) } claims

||| Add custom claim
public export
withCustomClaim : String -> ClaimValue -> JWTClaims -> JWTClaims
withCustomClaim name val claims =
  { customClaims := claims.customClaims ++ [(name, val)] } claims

||| Add custom string claim
public export
withStringClaim : String -> String -> JWTClaims -> JWTClaims
withStringClaim name val = withCustomClaim name (ClaimString val)

||| Add custom integer claim
public export
withIntClaim : String -> Integer -> JWTClaims -> JWTClaims
withIntClaim name val = withCustomClaim name (ClaimInt val)

||| Add custom boolean claim
public export
withBoolClaim : String -> Bool -> JWTClaims -> JWTClaims
withBoolClaim name val = withCustomClaim name (ClaimBool val)

--------------------------------------------------------------------------------
-- Validation Presets
--------------------------------------------------------------------------------

||| Validation for access tokens (short-lived)
public export
accessTokenValidation : ValidationOptions
accessTokenValidation =
  { allowedAlgorithms := [HS256, RS256, ES256]
  , validateExp := True
  , validateNbf := True
  , validateIat := True
  , maxAge := Just 3600  -- 1 hour max
  , rejectNone := True
  } defaultValidation

||| Validation for refresh tokens (longer-lived)
public export
refreshTokenValidation : ValidationOptions
refreshTokenValidation =
  { allowedAlgorithms := [HS256, RS256, ES256]
  , validateExp := True
  , validateNbf := True
  , maxAge := Just 604800  -- 1 week max
  , rejectNone := True
  } defaultValidation

||| Validation for ID tokens (OpenID Connect)
public export
idTokenValidation : String -> String -> ValidationOptions
idTokenValidation issuer audience =
  { allowedAlgorithms := [RS256, ES256]
  , requiredIssuer := Just issuer
  , requiredAudience := Just audience
  , validateExp := True
  , validateIat := True
  , requiredClaims := ["sub", "iat", "exp"]
  , rejectNone := True
  } defaultValidation

||| Permissive validation (for debugging only!)
public export
permissiveValidation : ValidationOptions
permissiveValidation =
  { allowedAlgorithms := []  -- Allow all
  , validateExp := False
  , validateNbf := False
  , validateIat := False
  , rejectNone := False  -- Even allow 'none' - DANGEROUS!
  } defaultValidation

--------------------------------------------------------------------------------
-- Token Refresh Helpers
--------------------------------------------------------------------------------

||| Check if token needs refresh (within threshold of expiration)
public export
needsRefresh : Integer -> Nat -> ValidatedJWT -> Bool
needsRefresh = isCloseToExpiring

||| Get remaining validity time in seconds
public export
remainingValidity : Integer -> ValidatedJWT -> Maybe Integer
remainingValidity = timeUntilExpiration

||| Create a refreshed token with new timestamps
public export
refreshClaims : Integer -> Integer -> JWTClaims -> JWTClaims
refreshClaims newIat newExp claims =
  { iat := Just newIat
  , exp := Just newExp
  } claims

--------------------------------------------------------------------------------
-- Error Helpers
--------------------------------------------------------------------------------

||| Check if error is due to expiration
public export
isExpiredError : JWTError -> Bool
isExpiredError (TokenExpired _ _) = True
isExpiredError _ = False

||| Check if error is due to invalid signature
public export
isSignatureError : JWTError -> Bool
isSignatureError InvalidSignature = True
isSignatureError _ = False

||| Check if error is due to algorithm issues
public export
isAlgorithmError : JWTError -> Bool
isAlgorithmError (UnsupportedAlgorithm _) = True
isAlgorithmError AlgorithmNoneNotAllowed = True
isAlgorithmError (AlgorithmMismatch _ _) = True
isAlgorithmError _ = False

||| Get user-friendly error message
public export
friendlyError : JWTError -> String
friendlyError (TokenExpired _ _) = "Your session has expired. Please log in again."
friendlyError (TokenNotYetValid _ _) = "Token is not yet valid. Please check your system clock."
friendlyError InvalidSignature = "Invalid token. Please log in again."
friendlyError AlgorithmNoneNotAllowed = "Invalid token format."
friendlyError (IssuerMismatch _ _) = "Token was not issued by a trusted source."
friendlyError (AudienceMismatch _ _) = "Token was not intended for this application."
friendlyError (MissingClaim name) = "Token is missing required information: " ++ name
friendlyError err = "Authentication failed: " ++ show err

--------------------------------------------------------------------------------
-- Debugging Helpers
--------------------------------------------------------------------------------

||| Pretty-print decoded JWT for debugging
public export
prettyPrintJWT : DecodedJWT -> String
prettyPrintJWT jwt =
  "JWT Token:\n" ++
  "  Algorithm: " ++ show jwt.header.alg ++ "\n" ++
  "  Type: " ++ show jwt.header.typ ++ "\n" ++
  "  Key ID: " ++ show jwt.header.kid ++ "\n" ++
  "  Claims:\n" ++
  "    Issuer: " ++ show jwt.claims.iss ++ "\n" ++
  "    Subject: " ++ show jwt.claims.sub ++ "\n" ++
  "    Audience: " ++ show (getAudienceList jwt.claims) ++ "\n" ++
  "    Expiration: " ++ show jwt.claims.exp ++ "\n" ++
  "    Not Before: " ++ show jwt.claims.nbf ++ "\n" ++
  "    Issued At: " ++ show jwt.claims.iat ++ "\n" ++
  "    JWT ID: " ++ show jwt.claims.jti ++ "\n" ++
  "    Custom Claims: " ++ show (map fst jwt.claims.customClaims)

||| Get token info without sensitive data (for logging)
public export
tokenInfo : String -> Result JWTError String
tokenInfo token = do
  jwt <- decode token
  Ok ("alg=" ++ show jwt.header.alg ++
      " sub=" ++ show jwt.claims.sub ++
      " exp=" ++ show jwt.claims.exp)
