-- SPDX-License-Identifier: Apache-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
||| SafeCert - X.509 certificate handling with validation
|||
||| Provides type-safe certificate chain validation, expiry checking,
||| and algorithm strength verification.
module Proven.SafeCert

import Data.String
import Data.List
import Data.Nat
import Data.Maybe

%default total

||| Certificate key algorithms
public export
data CertAlgorithm =
    RSA_2048 | RSA_3072 | RSA_4096
  | ECDSA_P256 | ECDSA_P384 | ECDSA_P521
  | Ed25519Cert | Ed448Cert

public export
Show CertAlgorithm where
  show RSA_2048 = "RSA-2048"
  show RSA_3072 = "RSA-3072"
  show RSA_4096 = "RSA-4096"
  show ECDSA_P256 = "ECDSA-P256"
  show ECDSA_P384 = "ECDSA-P384"
  show ECDSA_P521 = "ECDSA-P521"
  show Ed25519Cert = "Ed25519"
  show Ed448Cert = "Ed448"

public export
Eq CertAlgorithm where
  RSA_2048 == RSA_2048 = True
  RSA_3072 == RSA_3072 = True
  RSA_4096 == RSA_4096 = True
  ECDSA_P256 == ECDSA_P256 = True
  ECDSA_P384 == ECDSA_P384 = True
  ECDSA_P521 == ECDSA_P521 = True
  Ed25519Cert == Ed25519Cert = True
  Ed448Cert == Ed448Cert = True
  _ == _ = False

||| Signature hash algorithms
public export
data HashAlgorithm =
    SHA1 | SHA256 | SHA384 | SHA512

public export
Eq HashAlgorithm where
  SHA1 == SHA1 = True
  SHA256 == SHA256 = True
  SHA384 == SHA384 = True
  SHA512 == SHA512 = True
  _ == _ = False

||| Check if a hash algorithm is considered weak
public export
isWeakHash : HashAlgorithm -> Bool
isWeakHash SHA1 = True
isWeakHash _ = False

||| Check if a key algorithm meets minimum strength
public export
isStrongAlgorithm : CertAlgorithm -> Bool
isStrongAlgorithm RSA_2048 = False   -- Minimum but not strong
isStrongAlgorithm RSA_3072 = True
isStrongAlgorithm RSA_4096 = True
isStrongAlgorithm ECDSA_P256 = True
isStrongAlgorithm ECDSA_P384 = True
isStrongAlgorithm ECDSA_P521 = True
isStrongAlgorithm Ed25519Cert = True
isStrongAlgorithm Ed448Cert = True

||| Certificate usage flags
public export
data KeyUsage =
    DigitalSignature
  | KeyEncipherment
  | DataEncipherment
  | KeyAgreement
  | CertSign
  | CRLSign
  | ServerAuth
  | ClientAuth
  | CodeSigning
  | EmailProtection

public export
Eq KeyUsage where
  DigitalSignature == DigitalSignature = True
  KeyEncipherment == KeyEncipherment = True
  DataEncipherment == DataEncipherment = True
  KeyAgreement == KeyAgreement = True
  CertSign == CertSign = True
  CRLSign == CRLSign = True
  ServerAuth == ServerAuth = True
  ClientAuth == ClientAuth = True
  CodeSigning == CodeSigning = True
  EmailProtection == EmailProtection = True
  _ == _ = False

||| A simplified X.509 certificate representation
public export
record Certificate where
  constructor MkCertificate
  subject         : String
  issuer          : String
  serialNumber    : String
  notBefore       : Nat     -- Unix timestamp
  notAfter        : Nat     -- Unix timestamp
  keyAlgorithm    : CertAlgorithm
  signatureHash   : HashAlgorithm
  keyUsage        : List KeyUsage
  subjectAltNames : List String  -- DNS names and IPs
  isCA            : Bool
  pathLength      : Maybe Nat

||| Certificate validation result
public export
data CertValidity =
    CertValid
  | CertExpired
  | CertNotYetValid
  | WeakSignature
  | WeakKeyAlgorithm
  | SelfSigned
  | InvalidChain String
  | MissingUsage KeyUsage
  | HostnameMismatch String

public export
Eq CertValidity where
  CertValid == CertValid = True
  CertExpired == CertExpired = True
  CertNotYetValid == CertNotYetValid = True
  WeakSignature == WeakSignature = True
  WeakKeyAlgorithm == WeakKeyAlgorithm = True
  SelfSigned == SelfSigned = True
  _ == _ = False

||| Check if certificate is expired at a given time
public export
isExpiredAt : Certificate -> Nat -> Bool
isExpiredAt cert time = time > notAfter cert

||| Check if certificate is not yet valid at a given time
public export
isNotYetValidAt : Certificate -> Nat -> Bool
isNotYetValidAt cert time = time < notBefore cert

||| Check if certificate is currently valid at a given time
public export
isValidAt : Certificate -> Nat -> Bool
isValidAt cert time = not (isExpiredAt cert time) && not (isNotYetValidAt cert time)

||| Check if certificate matches a hostname
public export
matchesHostname : Certificate -> String -> Bool
matchesHostname cert hostname =
  elem hostname (subjectAltNames cert) ||
  any (\san => matchWildcard san hostname) (subjectAltNames cert)
  where
    matchWildcard : String -> String -> Bool
    matchWildcard pattern host =
      if isPrefixOf "*." pattern
        then let suffix = strSubstr 2 (length pattern) pattern
             in isSuffixOf suffix host && not (isInfixOf "." (strSubstr 0 (minus (length host) (length suffix)) host))
        else pattern == host

||| Validate a certificate comprehensively
public export
validateCertificate : Certificate -> Nat -> String -> List CertValidity
validateCertificate cert currentTime hostname =
  let checks = []
      checks' = if isExpiredAt cert currentTime then CertExpired :: checks else checks
      checks'' = if isNotYetValidAt cert currentTime then CertNotYetValid :: checks' else checks'
      checks''' = if isWeakHash (signatureHash cert) then WeakSignature :: checks'' else checks''
      checks'''' = if not (isStrongAlgorithm (keyAlgorithm cert)) then WeakKeyAlgorithm :: checks''' else checks'''
      checks''''' = if not (matchesHostname cert hostname) then HostnameMismatch hostname :: checks'''' else checks''''
  in if null checks''''' then [CertValid] else checks'''''

||| Check certificate chain validity
public export
validateChain : List Certificate -> Nat -> Bool
validateChain [] _ = False
validateChain [leaf] time = isValidAt leaf time
validateChain (leaf :: issuerCert :: rest) time =
  isValidAt leaf time &&
  issuer leaf == subject issuerCert &&
  isCA issuerCert &&
  elem CertSign (keyUsage issuerCert) &&
  validateChain (issuerCert :: rest) time

||| Days until certificate expires
public export
daysUntilExpiry : Certificate -> Nat -> Nat
daysUntilExpiry cert currentTime =
  if currentTime >= notAfter cert
    then 0
    else div (minus (notAfter cert) currentTime) 86400

||| Check if certificate will expire within N days
public export
expiresWithin : Certificate -> Nat -> Nat -> Bool
expiresWithin cert currentTime days = daysUntilExpiry cert currentTime <= days

-- ----------------------------------------------------------------
-- Proof types
-- ----------------------------------------------------------------

||| Proof that a certificate is valid at a given time
public export
data ValidCert : Certificate -> Nat -> Type where
  MkValidCert : isValidAt cert time = True -> ValidCert cert time

||| Proof that a certificate uses strong cryptography
public export
data StrongCert : Certificate -> Type where
  MkStrongCert : isStrongAlgorithm (keyAlgorithm cert) = True ->
                 isWeakHash (signatureHash cert) = False ->
                 StrongCert cert

||| Proof that a certificate chain is valid
public export
data ValidChain : List Certificate -> Nat -> Type where
  MkValidChain : validateChain chain time = True -> ValidChain chain time
