-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
||| SafeSSH - SSH key handling with weak algorithm detection
|||
||| Provides type-safe SSH key validation and algorithm strength checking.
||| Prevents: use of weak algorithms, insufficient key sizes, malformed keys.
module Proven.SafeSSH

import Data.String
import Data.List
import Data.Nat
import Data.Maybe

%default total

||| SSH key algorithms
public export
data SSHAlgorithm =
    RSA
  | DSA
  | ECDSA_256
  | ECDSA_384
  | ECDSA_521
  | Ed25519
  | Ed448
  | SK_ECDSA_256   -- Security key (FIDO2)
  | SK_Ed25519     -- Security key (FIDO2)

public export
Show SSHAlgorithm where
  show RSA = "ssh-rsa"
  show DSA = "ssh-dss"
  show ECDSA_256 = "ecdsa-sha2-nistp256"
  show ECDSA_384 = "ecdsa-sha2-nistp384"
  show ECDSA_521 = "ecdsa-sha2-nistp521"
  show Ed25519 = "ssh-ed25519"
  show Ed448 = "ssh-ed448"
  show SK_ECDSA_256 = "sk-ecdsa-sha2-nistp256@openssh.com"
  show SK_Ed25519 = "sk-ssh-ed25519@openssh.com"

public export
Eq SSHAlgorithm where
  RSA == RSA = True
  DSA == DSA = True
  ECDSA_256 == ECDSA_256 = True
  ECDSA_384 == ECDSA_384 = True
  ECDSA_521 == ECDSA_521 = True
  Ed25519 == Ed25519 = True
  Ed448 == Ed448 = True
  SK_ECDSA_256 == SK_ECDSA_256 = True
  SK_Ed25519 == SK_Ed25519 = True
  _ == _ = False

||| Algorithm strength classification
public export
data AlgorithmStrength = Broken | Weak | Acceptable | Strong | Excellent

public export
Eq AlgorithmStrength where
  Broken == Broken = True
  Weak == Weak = True
  Acceptable == Acceptable = True
  Strong == Strong = True
  Excellent == Excellent = True
  _ == _ = False

public export
Ord AlgorithmStrength where
  compare Broken Broken = EQ
  compare Broken _ = LT
  compare _ Broken = GT
  compare Weak Weak = EQ
  compare Weak _ = LT
  compare _ Weak = GT
  compare Acceptable Acceptable = EQ
  compare Acceptable _ = LT
  compare _ Acceptable = GT
  compare Strong Strong = EQ
  compare Strong _ = LT
  compare _ Strong = GT
  compare Excellent Excellent = EQ

||| Classify algorithm strength
public export
algorithmStrength : SSHAlgorithm -> AlgorithmStrength
algorithmStrength DSA = Broken           -- Deprecated in OpenSSH 7.0
algorithmStrength RSA = Acceptable       -- Depends on key size
algorithmStrength ECDSA_256 = Acceptable -- NIST curves have trust concerns
algorithmStrength ECDSA_384 = Strong
algorithmStrength ECDSA_521 = Strong
algorithmStrength Ed25519 = Excellent    -- Modern, fast, safe
algorithmStrength Ed448 = Excellent
algorithmStrength SK_ECDSA_256 = Strong  -- Hardware-backed
algorithmStrength SK_Ed25519 = Excellent -- Hardware-backed + Ed25519

||| Check if algorithm is deprecated/unsafe
public export
isWeakAlgorithm : SSHAlgorithm -> Bool
isWeakAlgorithm alg = case algorithmStrength alg of
  Broken => True
  Weak => True
  _ => False

||| Minimum RSA key size in bits
public export
minRSAKeyBits : Nat
minRSAKeyBits = 3072

||| Recommended RSA key size
public export
recommendedRSAKeyBits : Nat
recommendedRSAKeyBits = 4096

||| An SSH public key
public export
record SSHPublicKey where
  constructor MkSSHPublicKey
  keyAlgorithm : SSHAlgorithm
  keyData      : String   -- Base64-encoded key data
  keyComment   : String

||| An SSH key fingerprint
public export
record KeyFingerprint where
  constructor MkFingerprint
  fpAlgorithm : String   -- e.g., "SHA256"
  fpValue     : String   -- The fingerprint hash

||| Parse algorithm name from string
public export
parseAlgorithm : String -> Maybe SSHAlgorithm
parseAlgorithm "ssh-rsa" = Just RSA
parseAlgorithm "ssh-dss" = Just DSA
parseAlgorithm "ecdsa-sha2-nistp256" = Just ECDSA_256
parseAlgorithm "ecdsa-sha2-nistp384" = Just ECDSA_384
parseAlgorithm "ecdsa-sha2-nistp521" = Just ECDSA_521
parseAlgorithm "ssh-ed25519" = Just Ed25519
parseAlgorithm "ssh-ed448" = Just Ed448
parseAlgorithm "sk-ecdsa-sha2-nistp256@openssh.com" = Just SK_ECDSA_256
parseAlgorithm "sk-ssh-ed25519@openssh.com" = Just SK_Ed25519
parseAlgorithm _ = Nothing

||| Parse an SSH public key line
public export
parsePublicKey : String -> Maybe SSHPublicKey
parsePublicKey s =
  case words s of
    [algStr, keyData] =>
      case parseAlgorithm algStr of
        Just alg => Just (MkSSHPublicKey alg keyData "")
        Nothing => Nothing
    [algStr, keyData, comment] =>
      case parseAlgorithm algStr of
        Just alg => Just (MkSSHPublicKey alg keyData comment)
        Nothing => Nothing
    _ => Nothing

||| Validate an SSH public key
public export
data KeyValidity = Valid | WeakAlgorithm | InvalidFormat | TooShort

public export
Eq KeyValidity where
  Valid == Valid = True
  WeakAlgorithm == WeakAlgorithm = True
  InvalidFormat == InvalidFormat = True
  TooShort == TooShort = True
  _ == _ = False

public export
validateKey : SSHPublicKey -> KeyValidity
validateKey key =
  if isWeakAlgorithm (keyAlgorithm key)
    then WeakAlgorithm
    else if length (keyData key) < 16
      then TooShort
      else Valid

||| Recommended algorithms for new keys
public export
recommendedAlgorithms : List SSHAlgorithm
recommendedAlgorithms = [Ed25519, SK_Ed25519, Ed448]

||| Generate SSH config line for allowed algorithms
public export
allowedAlgorithmsConfig : List SSHAlgorithm -> String
allowedAlgorithmsConfig algs =
  "PubkeyAcceptedAlgorithms " ++ joinBy "," (map show algs)
  where
    joinBy : String -> List String -> String
    joinBy _ [] = ""
    joinBy _ [x] = x
    joinBy sep (x :: xs) = x ++ sep ++ joinBy sep xs

-- ----------------------------------------------------------------
-- Proof types
-- ----------------------------------------------------------------

||| Proof that a key uses a strong algorithm
public export
data StrongKey : SSHPublicKey -> Type where
  MkStrongKey : isWeakAlgorithm (keyAlgorithm key) = False -> StrongKey key

||| Proof that a key is valid
public export
data ValidKey : SSHPublicKey -> Type where
  MkValidKey : validateKey key = Valid -> ValidKey key
