-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
||| JWT type definitions
|||
||| Core types for JWT handling: algorithms, errors, headers, claims,
||| keys, decoded/validated tokens, validation options.
module Proven.SafeJWT.Types

import Proven.Core
import Data.List
import Data.Maybe
import Data.String
import Data.Bits

%default total

--------------------------------------------------------------------------------
-- JWT Algorithm
--------------------------------------------------------------------------------

||| Supported JWT signing algorithms
public export
data JWTAlgorithm : Type where
  ||| No algorithm (unsigned)
  None  : JWTAlgorithm
  ||| HMAC-SHA256
  HS256 : JWTAlgorithm
  ||| HMAC-SHA384
  HS384 : JWTAlgorithm
  ||| HMAC-SHA512
  HS512 : JWTAlgorithm
  ||| RSA-SHA256
  RS256 : JWTAlgorithm
  ||| RSA-SHA384
  RS384 : JWTAlgorithm
  ||| RSA-SHA512
  RS512 : JWTAlgorithm
  ||| ECDSA-SHA256
  ES256 : JWTAlgorithm
  ||| ECDSA-SHA384
  ES384 : JWTAlgorithm
  ||| ECDSA-SHA512
  ES512 : JWTAlgorithm
  ||| RSASSA-PSS SHA256
  PS256 : JWTAlgorithm
  ||| RSASSA-PSS SHA384
  PS384 : JWTAlgorithm
  ||| RSASSA-PSS SHA512
  PS512 : JWTAlgorithm
  ||| Edwards-curve DSA
  EdDSA : JWTAlgorithm

public export
Show JWTAlgorithm where
  show None  = "none"
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

public export
Eq JWTAlgorithm where
  None  == None  = True
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

||| Parse algorithm string
public export
parseAlgorithm : String -> Maybe JWTAlgorithm
parseAlgorithm "none"  = Just None
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
parseAlgorithm _       = Nothing

||| Check if algorithm is symmetric (HMAC)
public export
isSymmetric : JWTAlgorithm -> Bool
isSymmetric HS256 = True
isSymmetric HS384 = True
isSymmetric HS512 = True
isSymmetric _     = False

--------------------------------------------------------------------------------
-- JWT Errors
--------------------------------------------------------------------------------

||| JWT operation errors
public export
data JWTError : Type where
  ||| Token has expired
  TokenExpired : (exp : Integer) -> (current : Integer) -> JWTError
  ||| Token not yet valid
  TokenNotYetValid : (nbf : Integer) -> (current : Integer) -> JWTError
  ||| Token issued in the future
  TokenIssuedInFuture : (iat : Integer) -> (current : Integer) -> JWTError
  ||| Token exceeds max age
  TokenTooOld : (age : Integer) -> (maxAge : Nat) -> JWTError
  ||| Invalid signature
  InvalidSignature : JWTError
  ||| Algorithm 'none' not allowed
  AlgorithmNoneNotAllowed : JWTError
  ||| Unsupported algorithm
  UnsupportedAlgorithm : String -> JWTError
  ||| Algorithm mismatch with allowed list
  AlgorithmMismatch : (allowed : List JWTAlgorithm) -> (actual : JWTAlgorithm) -> JWTError
  ||| Issuer mismatch
  IssuerMismatch : (expected : String) -> (actual : Maybe String) -> JWTError
  ||| Audience mismatch
  AudienceMismatch : (expected : String) -> (actual : List String) -> JWTError
  ||| Missing required claim
  MissingClaim : String -> JWTError
  ||| Key error
  KeyError : String -> JWTError
  ||| Invalid token format
  InvalidFormat : String -> JWTError
  ||| Base64 decode error
  Base64Error : (segment : String) -> (reason : String) -> JWTError
  ||| JSON parse error
  JsonError : (segment : String) -> (reason : String) -> JWTError

public export
Show JWTError where
  show (TokenExpired exp cur) = "Token expired at " ++ show exp ++ ", current time " ++ show cur
  show (TokenNotYetValid nbf cur) = "Token not valid until " ++ show nbf ++ ", current time " ++ show cur
  show (TokenIssuedInFuture iat cur) = "Token issued in future: iat=" ++ show iat ++ ", current=" ++ show cur
  show (TokenTooOld age maxAge) = "Token too old: age " ++ show age ++ "s, max " ++ show maxAge ++ "s"
  show InvalidSignature = "Invalid signature"
  show AlgorithmNoneNotAllowed = "Algorithm 'none' not allowed"
  show (UnsupportedAlgorithm alg) = "Unsupported algorithm: " ++ alg
  show (AlgorithmMismatch allowed actual) = "Algorithm " ++ show actual ++ " not in allowed: " ++ show allowed
  show (IssuerMismatch expected actual) = "Issuer mismatch: expected " ++ expected ++ ", got " ++ show actual
  show (AudienceMismatch expected actual) = "Audience mismatch: expected " ++ expected ++ ", got " ++ show actual
  show (MissingClaim name) = "Missing required claim: " ++ name
  show (KeyError msg) = "Key error: " ++ msg
  show (InvalidFormat msg) = "Invalid format: " ++ msg
  show (Base64Error seg reason) = "Base64 error in " ++ seg ++ ": " ++ reason
  show (JsonError seg reason) = "JSON error in " ++ seg ++ ": " ++ reason

public export
Eq JWTError where
  TokenExpired e1 c1 == TokenExpired e2 c2 = e1 == e2 && c1 == c2
  InvalidSignature == InvalidSignature = True
  AlgorithmNoneNotAllowed == AlgorithmNoneNotAllowed = True
  UnsupportedAlgorithm a1 == UnsupportedAlgorithm a2 = a1 == a2
  MissingClaim n1 == MissingClaim n2 = n1 == n2
  KeyError m1 == KeyError m2 = m1 == m2
  InvalidFormat m1 == InvalidFormat m2 = m1 == m2
  _ == _ = False

--------------------------------------------------------------------------------
-- Signing Keys
--------------------------------------------------------------------------------

||| Signing/verification key types
public export
data SigningKey : Type where
  ||| No key (for 'none' algorithm)
  NoKey : SigningKey
  ||| HMAC secret key (symmetric)
  SecretKey : List Bits8 -> SigningKey
  ||| RSA public key (modulus, exponent)
  RSAPublicKey : List Bits8 -> List Bits8 -> SigningKey
  ||| RSA private key (modulus, exponent, private exponent)
  RSAPrivateKey : List Bits8 -> List Bits8 -> List Bits8 -> SigningKey
  ||| EC public key (curve name, x, y)
  ECPublicKey : String -> List Bits8 -> List Bits8 -> SigningKey
  ||| EC private key (curve name, x, y, d)
  ECPrivateKey : String -> List Bits8 -> List Bits8 -> List Bits8 -> SigningKey
  ||| EdDSA public key
  EdPublicKey : List Bits8 -> SigningKey
  ||| EdDSA private key
  EdPrivateKey : List Bits8 -> SigningKey

||| Check if key is valid for a given algorithm
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
isKeyValidForAlgorithm (ECPublicKey _ _ _) ES256 = True
isKeyValidForAlgorithm (ECPublicKey _ _ _) ES384 = True
isKeyValidForAlgorithm (ECPublicKey _ _ _) ES512 = True
isKeyValidForAlgorithm (ECPrivateKey _ _ _ _) ES256 = True
isKeyValidForAlgorithm (ECPrivateKey _ _ _ _) ES384 = True
isKeyValidForAlgorithm (ECPrivateKey _ _ _ _) ES512 = True
isKeyValidForAlgorithm (EdPublicKey _) EdDSA = True
isKeyValidForAlgorithm (EdPrivateKey _) EdDSA = True
isKeyValidForAlgorithm _ _ = False

--------------------------------------------------------------------------------
-- JWT Header
--------------------------------------------------------------------------------

||| JWT header
public export
record JWTHeader where
  constructor MkJWTHeader
  alg     : JWTAlgorithm
  typ     : Maybe String
  cty     : Maybe String
  kid     : Maybe String
  x5u     : Maybe String
  x5t     : Maybe String
  x5tS256 : Maybe String

||| Minimal header for a given algorithm
public export
minimalHeader : JWTAlgorithm -> JWTHeader
minimalHeader alg = MkJWTHeader alg (Just "JWT") Nothing Nothing Nothing Nothing Nothing

--------------------------------------------------------------------------------
-- Claim Values
--------------------------------------------------------------------------------

||| Claim value type
public export
data ClaimValue : Type where
  ClaimString : String -> ClaimValue
  ClaimInt    : Integer -> ClaimValue
  ClaimBool   : Bool -> ClaimValue
  ClaimNull   : ClaimValue
  ClaimArray  : List String -> ClaimValue
  ClaimObject : List (String, ClaimValue) -> ClaimValue

public export
Show ClaimValue where
  show (ClaimString s) = show s
  show (ClaimInt i) = show i
  show (ClaimBool b) = show b
  show ClaimNull = "null"
  show (ClaimArray xs) = show xs
  show (ClaimObject _) = "{...}"

--------------------------------------------------------------------------------
-- JWT Claims
--------------------------------------------------------------------------------

||| Standard JWT claims
public export
record JWTClaims where
  constructor MkJWTClaims
  iss          : Maybe String
  sub          : Maybe String
  aud          : Maybe (Either String (List String))
  exp          : Maybe Integer
  nbf          : Maybe Integer
  iat          : Maybe Integer
  jti          : Maybe String
  customClaims : List (String, ClaimValue)

||| Empty claims
public export
emptyClaims : JWTClaims
emptyClaims = MkJWTClaims Nothing Nothing Nothing Nothing Nothing Nothing Nothing []

||| Get audience as a flat list
public export
getAudienceList : JWTClaims -> List String
getAudienceList claims = case claims.aud of
  Nothing         => []
  Just (Left s)   => [s]
  Just (Right xs) => xs

--------------------------------------------------------------------------------
-- Token Segments
--------------------------------------------------------------------------------

||| Raw JWT token segments (header.payload.signature)
public export
record TokenSegments where
  constructor MkTokenSegments
  header    : String
  payload   : String
  signature : String

--------------------------------------------------------------------------------
-- Decoded JWT
--------------------------------------------------------------------------------

||| A decoded (but not validated) JWT
public export
record DecodedJWT where
  constructor MkDecodedJWT
  rawToken       : String
  header         : JWTHeader
  claims         : JWTClaims
  signature      : List Bits8
  headerSegment  : String
  payloadSegment : String

--------------------------------------------------------------------------------
-- Clock Skew
--------------------------------------------------------------------------------

||| Clock skew tolerance
public export
record ClockSkew where
  constructor MkClockSkew
  expLeeway : Nat
  nbfLeeway : Nat

||| Default clock skew (60 seconds)
public export
defaultClockSkew : ClockSkew
defaultClockSkew = MkClockSkew 60 60

--------------------------------------------------------------------------------
-- Validation Options
--------------------------------------------------------------------------------

||| Options for JWT validation
public export
record ValidationOptions where
  constructor MkValidationOptions
  allowedAlgorithms : List JWTAlgorithm
  rejectNone        : Bool
  validateExp       : Bool
  validateNbf       : Bool
  validateIat       : Bool
  maxAge            : Maybe Nat
  requiredIssuer    : Maybe String
  requiredAudience  : Maybe String
  requiredClaims    : List String
  clockSkew         : ClockSkew

||| Default validation options
public export
defaultValidation : ValidationOptions
defaultValidation = MkValidationOptions
  { allowedAlgorithms = []
  , rejectNone = True
  , validateExp = True
  , validateNbf = True
  , validateIat = False
  , maxAge = Nothing
  , requiredIssuer = Nothing
  , requiredAudience = Nothing
  , requiredClaims = []
  , clockSkew = defaultClockSkew
  }

||| Strict validation: only HS256, RS256, ES256
public export
strictValidation : ValidationOptions
strictValidation = MkValidationOptions
  { allowedAlgorithms = [HS256, RS256, ES256]
  , rejectNone = True
  , validateExp = True
  , validateNbf = True
  , validateIat = True
  , maxAge = Just 3600
  , requiredIssuer = Nothing
  , requiredAudience = Nothing
  , requiredClaims = ["sub", "iat", "exp"]
  , clockSkew = MkClockSkew 30 30
  }

--------------------------------------------------------------------------------
-- Validated JWT
--------------------------------------------------------------------------------

||| A fully validated JWT (can only be constructed through validation)
public export
record ValidatedJWT where
  constructor MkValidatedJWT
  decoded       : DecodedJWT
  validatedAt   : Integer
  validationTag : String

-- isOk is provided by Proven.Core
