-- SPDX-License-Identifier: Palimpsest-MPL-1.0
||| Core types for safe JWT operations
|||
||| This module provides type-safe representations of JWT tokens,
||| headers, claims, and validation rules per RFC 7519.
module Proven.SafeJWT.Types

import Proven.Core
import Data.List
import Data.String

%default total

--------------------------------------------------------------------------------
-- JWT Algorithms
--------------------------------------------------------------------------------

||| Supported signing algorithms (RFC 7518)
public export
data JWTAlgorithm : Type where
  ||| No digital signature (use with extreme caution)
  None : JWTAlgorithm
  ||| HMAC with SHA-256
  HS256 : JWTAlgorithm
  ||| HMAC with SHA-384
  HS384 : JWTAlgorithm
  ||| HMAC with SHA-512
  HS512 : JWTAlgorithm
  ||| RSA PKCS#1 v1.5 with SHA-256
  RS256 : JWTAlgorithm
  ||| RSA PKCS#1 v1.5 with SHA-384
  RS384 : JWTAlgorithm
  ||| RSA PKCS#1 v1.5 with SHA-512
  RS512 : JWTAlgorithm
  ||| ECDSA with P-256 and SHA-256
  ES256 : JWTAlgorithm
  ||| ECDSA with P-384 and SHA-384
  ES384 : JWTAlgorithm
  ||| ECDSA with P-521 and SHA-512
  ES512 : JWTAlgorithm
  ||| RSA-PSS with SHA-256
  PS256 : JWTAlgorithm
  ||| RSA-PSS with SHA-384
  PS384 : JWTAlgorithm
  ||| RSA-PSS with SHA-512
  PS512 : JWTAlgorithm
  ||| EdDSA (Ed25519/Ed448)
  EdDSA : JWTAlgorithm

public export
Eq JWTAlgorithm where
  None == None = True
  HS256 == HS256 = True
  HS384 == HS384 = True
  HS512 == HS512 = True
  RS256 == RS256 = True
  RS384 == RS384 = True
  RS512 == RS512 = True
  ES256 == ES256 = True
  ES384 == ES384 = True
  ES512 == ES512 = True
  PS256 == PS256 = True
  PS384 == PS384 = True
  PS512 == PS512 = True
  EdDSA == EdDSA = True
  _ == _ = False

public export
Show JWTAlgorithm where
  show None = "none"
  show HS256 = "HS256"
  show HS384 = "HS384"
  show HS512 = "HS512"
  show RS256 = "RS256"
  show RS384 = "RS384"
  show RS512 = "RS512"
  show ES256 = "ES256"
  show ES384 = "ES384"
  show ES512 = "ES512"
  show PS256 = "PS256"
  show PS384 = "PS384"
  show PS512 = "PS512"
  show EdDSA = "EdDSA"

||| Parse algorithm string to JWTAlgorithm
public export
parseAlgorithm : String -> Maybe JWTAlgorithm
parseAlgorithm "none" = Just None
parseAlgorithm "HS256" = Just HS256
parseAlgorithm "HS384" = Just HS384
parseAlgorithm "HS512" = Just HS512
parseAlgorithm "RS256" = Just RS256
parseAlgorithm "RS384" = Just RS384
parseAlgorithm "RS512" = Just RS512
parseAlgorithm "ES256" = Just ES256
parseAlgorithm "ES384" = Just ES384
parseAlgorithm "ES512" = Just ES512
parseAlgorithm "PS256" = Just PS256
parseAlgorithm "PS384" = Just PS384
parseAlgorithm "PS512" = Just PS512
parseAlgorithm "EdDSA" = Just EdDSA
parseAlgorithm _ = Nothing

||| Check if algorithm is symmetric (HMAC-based)
public export
isSymmetric : JWTAlgorithm -> Bool
isSymmetric HS256 = True
isSymmetric HS384 = True
isSymmetric HS512 = True
isSymmetric _ = False

||| Check if algorithm is asymmetric (RSA/ECDSA/EdDSA)
public export
isAsymmetric : JWTAlgorithm -> Bool
isAsymmetric alg = not (isSymmetric alg) && alg /= None

||| Check if algorithm is considered secure
public export
isSecureAlgorithm : JWTAlgorithm -> Bool
isSecureAlgorithm None = False
isSecureAlgorithm _ = True

--------------------------------------------------------------------------------
-- JWT Header
--------------------------------------------------------------------------------

||| JWT Header (JOSE Header)
public export
record JWTHeader where
  constructor MkJWTHeader
  ||| Algorithm used for signing
  alg : JWTAlgorithm
  ||| Token type (usually "JWT")
  typ : Maybe String
  ||| Content type
  cty : Maybe String
  ||| Key ID (for key rotation)
  kid : Maybe String
  ||| X.509 certificate chain URL
  x5u : Maybe String
  ||| X.509 certificate thumbprint (SHA-1)
  x5t : Maybe String
  ||| X.509 certificate thumbprint (SHA-256)
  x5tS256 : Maybe String

public export
Eq JWTHeader where
  h1 == h2 = h1.alg == h2.alg && h1.typ == h2.typ && h1.kid == h2.kid

||| Create a minimal header with just algorithm
public export
minimalHeader : JWTAlgorithm -> JWTHeader
minimalHeader alg = MkJWTHeader alg (Just "JWT") Nothing Nothing Nothing Nothing Nothing

--------------------------------------------------------------------------------
-- JWT Claims
--------------------------------------------------------------------------------

||| Standard registered claim names (RFC 7519 Section 4.1)
public export
data RegisteredClaim : Type where
  ||| Issuer
  Iss : RegisteredClaim
  ||| Subject
  Sub : RegisteredClaim
  ||| Audience
  Aud : RegisteredClaim
  ||| Expiration Time
  Exp : RegisteredClaim
  ||| Not Before
  Nbf : RegisteredClaim
  ||| Issued At
  Iat : RegisteredClaim
  ||| JWT ID
  Jti : RegisteredClaim

public export
Show RegisteredClaim where
  show Iss = "iss"
  show Sub = "sub"
  show Aud = "aud"
  show Exp = "exp"
  show Nbf = "nbf"
  show Iat = "iat"
  show Jti = "jti"

||| Claim value types
public export
data ClaimValue : Type where
  ||| String value
  ClaimString : String -> ClaimValue
  ||| Integer/Number value (timestamps, counts)
  ClaimInt : Integer -> ClaimValue
  ||| Boolean value
  ClaimBool : Bool -> ClaimValue
  ||| Array of strings (e.g., audience)
  ClaimArray : List String -> ClaimValue
  ||| Nested object (JSON)
  ClaimObject : List (String, ClaimValue) -> ClaimValue
  ||| Null value
  ClaimNull : ClaimValue

public export
Eq ClaimValue where
  ClaimString s1 == ClaimString s2 = s1 == s2
  ClaimInt i1 == ClaimInt i2 = i1 == i2
  ClaimBool b1 == ClaimBool b2 = b1 == b2
  ClaimArray a1 == ClaimArray a2 = a1 == a2
  ClaimNull == ClaimNull = True
  _ == _ = False

public export
Show ClaimValue where
  show (ClaimString s) = show s
  show (ClaimInt i) = show i
  show (ClaimBool b) = show b
  show (ClaimArray xs) = show xs
  show (ClaimObject _) = "{...}"
  show ClaimNull = "null"

||| JWT Claims Set
public export
record JWTClaims where
  constructor MkJWTClaims
  ||| Issuer (iss)
  iss : Maybe String
  ||| Subject (sub)
  sub : Maybe String
  ||| Audience (aud) - can be single string or array
  aud : Maybe (Either String (List String))
  ||| Expiration time (exp) - Unix timestamp
  exp : Maybe Integer
  ||| Not before (nbf) - Unix timestamp
  nbf : Maybe Integer
  ||| Issued at (iat) - Unix timestamp
  iat : Maybe Integer
  ||| JWT ID (jti)
  jti : Maybe String
  ||| Custom/private claims
  customClaims : List (String, ClaimValue)

public export
Eq JWTClaims where
  c1 == c2 = c1.iss == c2.iss && c1.sub == c2.sub && c1.exp == c2.exp

||| Create empty claims
public export
emptyClaims : JWTClaims
emptyClaims = MkJWTClaims Nothing Nothing Nothing Nothing Nothing Nothing Nothing []

||| Get audience as list (normalizes single value to list)
public export
getAudienceList : JWTClaims -> List String
getAudienceList claims =
  case claims.aud of
    Nothing => []
    Just (Left single) => [single]
    Just (Right multiple) => multiple

--------------------------------------------------------------------------------
-- Decoded JWT
--------------------------------------------------------------------------------

||| A decoded (but not necessarily validated) JWT
public export
record DecodedJWT where
  constructor MkDecodedJWT
  ||| Original token string
  rawToken : String
  ||| Decoded header
  header : JWTHeader
  ||| Decoded claims/payload
  claims : JWTClaims
  ||| Signature bytes (base64url decoded)
  signature : List Bits8
  ||| Header segment (for signature verification)
  headerSegment : String
  ||| Payload segment (for signature verification)
  payloadSegment : String

||| A validated JWT (type-level proof that validation passed)
public export
record ValidatedJWT where
  constructor MkValidatedJWT
  ||| The underlying decoded JWT
  decoded : DecodedJWT
  ||| Timestamp when validation was performed
  validatedAt : Integer
  ||| Validation options used
  validationId : String

--------------------------------------------------------------------------------
-- Validation Configuration
--------------------------------------------------------------------------------

||| Clock skew tolerance for time-based validations
public export
record ClockSkew where
  constructor MkClockSkew
  ||| Seconds of tolerance for expiration
  expLeeway : Nat
  ||| Seconds of tolerance for not-before
  nbfLeeway : Nat

||| Default clock skew (60 seconds)
public export
defaultClockSkew : ClockSkew
defaultClockSkew = MkClockSkew 60 60

||| No clock skew tolerance (strict)
public export
noClockSkew : ClockSkew
noClockSkew = MkClockSkew 0 0

||| Validation options
public export
record ValidationOptions where
  constructor MkValidationOptions
  ||| Allowed signing algorithms (empty = allow all)
  allowedAlgorithms : List JWTAlgorithm
  ||| Required issuer (if set)
  requiredIssuer : Maybe String
  ||| Required audience (if set)
  requiredAudience : Maybe String
  ||| Validate expiration time
  validateExp : Bool
  ||| Validate not-before time
  validateNbf : Bool
  ||| Validate issued-at time
  validateIat : Bool
  ||| Clock skew tolerance
  clockSkew : ClockSkew
  ||| Maximum token age in seconds (from iat)
  maxAge : Maybe Nat
  ||| Required claims that must be present
  requiredClaims : List String
  ||| Reject tokens with 'none' algorithm
  rejectNone : Bool

||| Default validation options (secure defaults)
public export
defaultValidation : ValidationOptions
defaultValidation = MkValidationOptions
  []           -- allow all algorithms except 'none'
  Nothing      -- no required issuer
  Nothing      -- no required audience
  True         -- validate expiration
  True         -- validate not-before
  False        -- don't validate issued-at
  defaultClockSkew
  Nothing      -- no max age
  []           -- no required claims
  True         -- reject 'none' algorithm

||| Strict validation options
public export
strictValidation : ValidationOptions
strictValidation = MkValidationOptions
  [HS256, HS384, HS512, RS256, RS384, RS512, ES256, ES384, ES512]
  Nothing
  Nothing
  True
  True
  True
  noClockSkew
  (Just 3600)  -- max 1 hour
  ["iss", "sub", "iat"]
  True

--------------------------------------------------------------------------------
-- JWT Errors
--------------------------------------------------------------------------------

||| Errors that can occur during JWT operations
public export
data JWTError : Type where
  ||| Token format is invalid (not 3 parts separated by dots)
  InvalidFormat : (reason : String) -> JWTError
  ||| Base64 decoding failed
  Base64Error : (segment : String) -> (reason : String) -> JWTError
  ||| JSON parsing failed
  JsonError : (segment : String) -> (reason : String) -> JWTError
  ||| Unknown or unsupported algorithm
  UnsupportedAlgorithm : (alg : String) -> JWTError
  ||| Algorithm 'none' is not allowed
  AlgorithmNoneNotAllowed : JWTError
  ||| Algorithm mismatch (token uses different alg than expected)
  AlgorithmMismatch : (expected : List JWTAlgorithm) -> (got : JWTAlgorithm) -> JWTError
  ||| Signature verification failed
  InvalidSignature : JWTError
  ||| Token has expired
  TokenExpired : (expiredAt : Integer) -> (currentTime : Integer) -> JWTError
  ||| Token is not yet valid (nbf in future)
  TokenNotYetValid : (notBefore : Integer) -> (currentTime : Integer) -> JWTError
  ||| Token issued in the future (iat > current time)
  TokenIssuedInFuture : (issuedAt : Integer) -> (currentTime : Integer) -> JWTError
  ||| Token is too old (exceeds maxAge)
  TokenTooOld : (age : Integer) -> (maxAge : Nat) -> JWTError
  ||| Required issuer mismatch
  IssuerMismatch : (expected : String) -> (got : Maybe String) -> JWTError
  ||| Required audience not found
  AudienceMismatch : (expected : String) -> (got : List String) -> JWTError
  ||| Required claim is missing
  MissingClaim : (claimName : String) -> JWTError
  ||| Claim value is invalid
  InvalidClaimValue : (claimName : String) -> (reason : String) -> JWTError
  ||| Key error (wrong key type, etc.)
  KeyError : (reason : String) -> JWTError
  ||| Generic validation error
  ValidationError : (reason : String) -> JWTError

public export
Show JWTError where
  show (InvalidFormat reason) = "Invalid JWT format: " ++ reason
  show (Base64Error seg reason) = "Base64 decode error in " ++ seg ++ ": " ++ reason
  show (JsonError seg reason) = "JSON parse error in " ++ seg ++ ": " ++ reason
  show (UnsupportedAlgorithm alg) = "Unsupported algorithm: " ++ alg
  show AlgorithmNoneNotAllowed = "Algorithm 'none' is not allowed"
  show (AlgorithmMismatch exp got) = "Algorithm mismatch: expected one of " ++
                                      show (map show exp) ++ ", got " ++ show got
  show InvalidSignature = "Invalid signature"
  show (TokenExpired at now) = "Token expired at " ++ show at ++ " (current: " ++ show now ++ ")"
  show (TokenNotYetValid nbf now) = "Token not valid until " ++ show nbf ++ " (current: " ++ show now ++ ")"
  show (TokenIssuedInFuture iat now) = "Token issued in future: " ++ show iat ++ " > " ++ show now
  show (TokenTooOld age max) = "Token too old: " ++ show age ++ "s exceeds max " ++ show max ++ "s"
  show (IssuerMismatch exp got) = "Issuer mismatch: expected " ++ exp ++ ", got " ++ show got
  show (AudienceMismatch exp got) = "Audience mismatch: expected " ++ exp ++ ", got " ++ show got
  show (MissingClaim name) = "Missing required claim: " ++ name
  show (InvalidClaimValue name reason) = "Invalid claim '" ++ name ++ "': " ++ reason
  show (KeyError reason) = "Key error: " ++ reason
  show (ValidationError reason) = "Validation error: " ++ reason

--------------------------------------------------------------------------------
-- Signing Keys
--------------------------------------------------------------------------------

||| Key material for signing/verification
public export
data SigningKey : Type where
  ||| Secret key for HMAC algorithms
  SecretKey : (secret : List Bits8) -> SigningKey
  ||| RSA public key (for verification)
  RSAPublicKey : (n : List Bits8) -> (e : List Bits8) -> SigningKey
  ||| RSA private key (for signing)
  RSAPrivateKey : (n : List Bits8) -> (e : List Bits8) -> (d : List Bits8) -> SigningKey
  ||| EC public key
  ECPublicKey : (curve : String) -> (x : List Bits8) -> (y : List Bits8) -> SigningKey
  ||| EC private key
  ECPrivateKey : (curve : String) -> (x : List Bits8) -> (y : List Bits8) -> (d : List Bits8) -> SigningKey
  ||| Ed25519/Ed448 public key
  EdPublicKey : (bytes : List Bits8) -> SigningKey
  ||| Ed25519/Ed448 private key
  EdPrivateKey : (bytes : List Bits8) -> SigningKey
  ||| No key (for 'none' algorithm - dangerous!)
  NoKey : SigningKey

||| Check if key is suitable for algorithm
public export
isKeyValidForAlgorithm : SigningKey -> JWTAlgorithm -> Bool
isKeyValidForAlgorithm NoKey None = True
isKeyValidForAlgorithm (SecretKey _) HS256 = True
isKeyValidForAlgorithm (SecretKey _) HS384 = True
isKeyValidForAlgorithm (SecretKey _) HS512 = True
isKeyValidForAlgorithm (RSAPublicKey _ _) RS256 = True
isKeyValidForAlgorithm (RSAPublicKey _ _) RS384 = True
isKeyValidForAlgorithm (RSAPublicKey _ _) RS512 = True
isKeyValidForAlgorithm (RSAPublicKey _ _) PS256 = True
isKeyValidForAlgorithm (RSAPublicKey _ _) PS384 = True
isKeyValidForAlgorithm (RSAPublicKey _ _) PS512 = True
isKeyValidForAlgorithm (RSAPrivateKey _ _ _) RS256 = True
isKeyValidForAlgorithm (RSAPrivateKey _ _ _) RS384 = True
isKeyValidForAlgorithm (RSAPrivateKey _ _ _) RS512 = True
isKeyValidForAlgorithm (RSAPrivateKey _ _ _) PS256 = True
isKeyValidForAlgorithm (RSAPrivateKey _ _ _) PS384 = True
isKeyValidForAlgorithm (RSAPrivateKey _ _ _) PS512 = True
isKeyValidForAlgorithm (ECPublicKey "P-256" _ _) ES256 = True
isKeyValidForAlgorithm (ECPublicKey "P-384" _ _) ES384 = True
isKeyValidForAlgorithm (ECPublicKey "P-521" _ _) ES512 = True
isKeyValidForAlgorithm (ECPrivateKey "P-256" _ _ _) ES256 = True
isKeyValidForAlgorithm (ECPrivateKey "P-384" _ _ _) ES384 = True
isKeyValidForAlgorithm (ECPrivateKey "P-521" _ _ _) ES512 = True
isKeyValidForAlgorithm (EdPublicKey _) EdDSA = True
isKeyValidForAlgorithm (EdPrivateKey _) EdDSA = True
isKeyValidForAlgorithm _ _ = False

--------------------------------------------------------------------------------
-- Token Segments
--------------------------------------------------------------------------------

||| Represents the three parts of a JWT
public export
record TokenSegments where
  constructor MkTokenSegments
  ||| Base64url-encoded header
  header : String
  ||| Base64url-encoded payload
  payload : String
  ||| Base64url-encoded signature
  signature : String

||| Reconstruct token from segments
public export
segmentsToToken : TokenSegments -> String
segmentsToToken segs = segs.header ++ "." ++ segs.payload ++ "." ++ segs.signature

||| Get the signing input (header.payload)
public export
signingInput : TokenSegments -> String
signingInput segs = segs.header ++ "." ++ segs.payload
