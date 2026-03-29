-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
||| SafeJWK - JSON Web Key validation and strength checking
|||
||| Provides type-safe JWK operations per RFC 7517:
||| - Key type validation (RSA, EC, OKP, oct)
||| - Algorithm allowlisting
||| - Minimum key strength enforcement
||| - Key rotation safety checks
||| Prevents: weak keys, algorithm confusion, key misuse.
module Proven.SafeJWK

import Data.String
import Data.List
import Data.Nat

%default total

-- ============================================================================
-- KEY TYPES
-- ============================================================================

||| JWK key type (kty parameter)
public export
data KeyType = RSA | EC | OKP | Symmetric

public export
Show KeyType where
  show RSA = "RSA"
  show EC = "EC"
  show OKP = "OKP"
  show Symmetric = "oct"

public export
Eq KeyType where
  RSA == RSA = True
  EC == EC = True
  OKP == OKP = True
  Symmetric == Symmetric = True
  _ == _ = False

||| Parse key type from string
public export
parseKeyType : String -> Maybe KeyType
parseKeyType "RSA" = Just RSA
parseKeyType "EC"  = Just EC
parseKeyType "OKP" = Just OKP
parseKeyType "oct" = Just Symmetric
parseKeyType _     = Nothing

-- ============================================================================
-- ALGORITHMS
-- ============================================================================

||| JWK algorithm (alg parameter)
public export
data JWKAlgorithm =
    RS256 | RS384 | RS512       -- RSA PKCS#1 v1.5
  | PS256 | PS384 | PS512       -- RSA-PSS
  | ES256 | ES384 | ES512       -- ECDSA
  | EdDSA                        -- Edwards-curve DSA
  | HS256 | HS384 | HS512       -- HMAC
  | A128KW | A256KW              -- AES Key Wrap
  | Dir                          -- Direct encryption

public export
Show JWKAlgorithm where
  show RS256 = "RS256"; show RS384 = "RS384"; show RS512 = "RS512"
  show PS256 = "PS256"; show PS384 = "PS384"; show PS512 = "PS512"
  show ES256 = "ES256"; show ES384 = "ES384"; show ES512 = "ES512"
  show EdDSA = "EdDSA"
  show HS256 = "HS256"; show HS384 = "HS384"; show HS512 = "HS512"
  show A128KW = "A128KW"; show A256KW = "A256KW"
  show Dir = "dir"

public export
Eq JWKAlgorithm where
  RS256 == RS256 = True; RS384 == RS384 = True; RS512 == RS512 = True
  PS256 == PS256 = True; PS384 == PS384 = True; PS512 == PS512 = True
  ES256 == ES256 = True; ES384 == ES384 = True; ES512 == ES512 = True
  EdDSA == EdDSA = True
  HS256 == HS256 = True; HS384 == HS384 = True; HS512 == HS512 = True
  A128KW == A128KW = True; A256KW == A256KW = True
  Dir == Dir = True
  _ == _ = False

||| Compatible key types for each algorithm
public export
compatibleKeyType : JWKAlgorithm -> KeyType
compatibleKeyType RS256 = RSA; compatibleKeyType RS384 = RSA; compatibleKeyType RS512 = RSA
compatibleKeyType PS256 = RSA; compatibleKeyType PS384 = RSA; compatibleKeyType PS512 = RSA
compatibleKeyType ES256 = EC; compatibleKeyType ES384 = EC; compatibleKeyType ES512 = EC
compatibleKeyType EdDSA = OKP
compatibleKeyType HS256 = Symmetric; compatibleKeyType HS384 = Symmetric; compatibleKeyType HS512 = Symmetric
compatibleKeyType A128KW = Symmetric; compatibleKeyType A256KW = Symmetric
compatibleKeyType Dir = Symmetric

-- ============================================================================
-- KEY STRENGTH
-- ============================================================================

||| Minimum key sizes (bits) per NIST SP 800-57
public export
data KeyStrength = Weak | Acceptable | Strong | VeryStrong

public export
Show KeyStrength where
  show Weak = "weak"; show Acceptable = "acceptable"
  show Strong = "strong"; show VeryStrong = "very_strong"

public export
Eq KeyStrength where
  Weak == Weak = True; Acceptable == Acceptable = True
  Strong == Strong = True; VeryStrong == VeryStrong = True
  _ == _ = False

||| Assess RSA key strength by modulus bit length
public export
rsaStrength : Nat -> KeyStrength
rsaStrength bits =
  if bits >= 4096 then VeryStrong
  else if bits >= 3072 then Strong
  else if bits >= 2048 then Acceptable
  else Weak

||| Assess EC key strength by curve bit length
public export
ecStrength : Nat -> KeyStrength
ecStrength bits =
  if bits >= 521 then VeryStrong
  else if bits >= 384 then Strong
  else if bits >= 256 then Acceptable
  else Weak

||| Assess symmetric key strength by byte length
public export
symmetricStrength : Nat -> KeyStrength
symmetricStrength bytes =
  if bytes >= 64 then VeryStrong
  else if bytes >= 48 then Strong
  else if bytes >= 32 then Acceptable
  else Weak

-- ============================================================================
-- JWK VALIDATION
-- ============================================================================

||| JWK validation result
public export
record JWKValidation where
  constructor MkJWKValidation
  keyType    : KeyType
  algorithm  : Maybe JWKAlgorithm
  strength   : KeyStrength
  errors     : List String

||| Check key type / algorithm compatibility
public export
isCompatible : KeyType -> JWKAlgorithm -> Bool
isCompatible kty alg = kty == compatibleKeyType alg

||| Algorithm allowlist policy
public export
record AlgorithmPolicy where
  constructor MkAlgorithmPolicy
  allowed      : List JWKAlgorithm
  minStrength  : KeyStrength

||| Default algorithm policy (no weak algorithms)
public export
defaultPolicy : AlgorithmPolicy
defaultPolicy = MkAlgorithmPolicy
  [PS256, PS384, PS512, ES256, ES384, ES512, EdDSA] Acceptable

||| Check if an algorithm is allowed by policy
public export
isAllowed : AlgorithmPolicy -> JWKAlgorithm -> Bool
isAllowed policy alg = any (== alg) policy.allowed

-- ============================================================================
-- KEY ROTATION
-- ============================================================================

||| Key rotation check: is a key too old?
public export
isKeyExpired : (createdEpoch : Integer) -> (nowEpoch : Integer) -> (maxAgeDays : Nat) -> Bool
isKeyExpired created now maxAge =
  let ageSecs = now - created
      maxAgeSecs = cast maxAge * 86400
  in ageSecs > maxAgeSecs

||| Key use constraints
public export
data KeyUse = Signing | Encryption

public export
Show KeyUse where
  show Signing = "sig"
  show Encryption = "enc"

public export
Eq KeyUse where
  Signing == Signing = True
  Encryption == Encryption = True
  _ == _ = False
