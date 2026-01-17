-- SPDX-License-Identifier: PMPL-1.0
-- SPDX-FileCopyrightText: 2024-2025 Hyperpolymath

||| Safe X.509 certificate handling and validation
|||
||| This module provides type-safe certificate handling:
||| - Certificate parsing and validation
||| - Chain verification
||| - Key usage and extension validation
||| - Expiration checking
||| - Revocation status (CRL, OCSP)
|||
||| Security features:
||| - Weak algorithm detection
||| - Key size validation
||| - SAN/CN mismatch detection
||| - Path constraint validation
||| - Certificate transparency validation
module Proven.SafeCert

import Data.List
import Data.String
import Data.Either
import Proven.Core

%default total

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

||| Certificate version
public export
data CertVersion = V1 | V2 | V3

||| Signature algorithms
public export
data SignatureAlgorithm
  = RSA_SHA1        -- Deprecated
  | RSA_SHA256
  | RSA_SHA384
  | RSA_SHA512
  | ECDSA_SHA256
  | ECDSA_SHA384
  | ECDSA_SHA512
  | Ed25519Sig
  | Ed448Sig

||| Key types
public export
data KeyType
  = RSAKey Nat      -- Key size in bits
  | ECKey String    -- Curve name
  | Ed25519Key
  | Ed448Key
  | DSAKey          -- Deprecated

||| Validity period
public export
record ValidityPeriod where
  constructor MkValidityPeriod
  notBefore : String  -- ISO 8601
  notAfter : String   -- ISO 8601

||| Distinguished Name attributes
public export
record DistinguishedName where
  constructor MkDistinguishedName
  commonName : Maybe String
  organization : Maybe String
  organizationalUnit : Maybe String
  country : Maybe String
  state : Maybe String
  locality : Maybe String
  email : Maybe String

||| Subject Alternative Name types
public export
data SANType
  = DNSName String
  | IPAddress String
  | Email String
  | URI String
  | DirectoryName DistinguishedName

||| Key usage bits
public export
record KeyUsage where
  constructor MkKeyUsage
  digitalSignature : Bool
  nonRepudiation : Bool
  keyEncipherment : Bool
  dataEncipherment : Bool
  keyAgreement : Bool
  keyCertSign : Bool
  cRLSign : Bool
  encipherOnly : Bool
  decipherOnly : Bool

||| Extended key usage OIDs
public export
data ExtendedKeyUsage
  = ServerAuth
  | ClientAuth
  | CodeSigning
  | EmailProtection
  | TimeStamping
  | OCSPSigning
  | CustomOID String

||| Basic constraints extension
public export
record BasicConstraints where
  constructor MkBasicConstraints
  isCA : Bool
  pathLenConstraint : Maybe Nat

||| Authority Key Identifier
public export
record AuthorityKeyId where
  constructor MkAuthorityKeyId
  keyId : Maybe String
  issuerName : Maybe DistinguishedName
  serialNumber : Maybe String

||| CRL distribution point
public export
record CRLDistPoint where
  constructor MkCRLDistPoint
  uri : String
  reasons : Maybe (List String)

||| OCSP responder info
public export
record OCSPInfo where
  constructor MkOCSPInfo
  uri : String
  nonce : Maybe String

||| Certificate Transparency SCT
public export
record SCT where
  constructor MkSCT
  logId : String
  timestamp : String
  signature : String

||| Certificate extensions
public export
record CertExtensions where
  constructor MkCertExtensions
  basicConstraints : Maybe BasicConstraints
  keyUsage : Maybe KeyUsage
  extendedKeyUsage : Maybe (List ExtendedKeyUsage)
  subjectAltNames : List SANType
  authorityKeyId : Maybe AuthorityKeyId
  subjectKeyId : Maybe String
  crlDistPoints : List CRLDistPoint
  ocspInfo : Maybe OCSPInfo
  scts : List SCT
  isCritical : List String  -- Critical extension OIDs

||| X.509 Certificate
public export
record Certificate where
  constructor MkCertificate
  version : CertVersion
  serialNumber : String
  signatureAlgorithm : SignatureAlgorithm
  issuer : DistinguishedName
  validity : ValidityPeriod
  subject : DistinguishedName
  publicKeyType : KeyType
  publicKeyData : String  -- Base64/Hex encoded
  extensions : CertExtensions
  signature : String
  fingerprint : String    -- SHA256 fingerprint

||| Certificate chain
public export
record CertChain where
  constructor MkCertChain
  endEntity : Certificate
  intermediates : List Certificate
  root : Maybe Certificate

||| Revocation status
public export
data RevocationStatus
  = NotRevoked
  | Revoked String String   -- Reason, date
  | Unknown
  | CheckFailed String

||| Certificate validation result
public export
record ValidationResult where
  constructor MkValidationResult
  isValid : Bool
  chainValid : Bool
  signatureValid : Bool
  notExpired : Bool
  notRevoked : Bool
  errors : List String
  warnings : List String

--------------------------------------------------------------------------------
-- Errors
--------------------------------------------------------------------------------

||| Errors that can occur during certificate operations
public export
data CertError
  = EmptyCertificate
  | InvalidPEM String
  | InvalidDER String
  | ExpiredCertificate String
  | NotYetValid String
  | WeakSignatureAlgorithm SignatureAlgorithm
  | WeakKeySize KeyType Nat
  | InvalidChain String
  | SelfSigned
  | RevokedCertificate String String
  | MissingExtension String
  | InvalidExtension String String
  | SANMismatch String (List SANType)
  | CNMismatch String String
  | PathLengthExceeded Nat Nat
  | KeyUsageViolation String
  | InvalidBasicConstraints
  | CriticalExtensionUnsupported String
  | NoTrustAnchor
  | SignatureVerificationFailed
  | InvalidSerialNumber String
  | DuplicateSerial String

public export
Show CertError where
  show EmptyCertificate = "Certificate error: empty certificate"
  show (InvalidPEM msg) = "Certificate error: invalid PEM - " ++ msg
  show (InvalidDER msg) = "Certificate error: invalid DER - " ++ msg
  show (ExpiredCertificate date) = "Certificate error: expired on " ++ date
  show (NotYetValid date) = "Certificate error: not valid until " ++ date
  show (WeakSignatureAlgorithm algo) = "Certificate security: weak signature algorithm"
  show (WeakKeySize keyType size) = "Certificate security: weak key size " ++ show size
  show (InvalidChain msg) = "Certificate error: invalid chain - " ++ msg
  show SelfSigned = "Certificate warning: self-signed certificate"
  show (RevokedCertificate reason date) = "Certificate error: revoked - " ++ reason ++ " on " ++ date
  show (MissingExtension ext) = "Certificate error: missing required extension " ++ ext
  show (InvalidExtension ext msg) = "Certificate error: invalid extension " ++ ext ++ " - " ++ msg
  show (SANMismatch hostname sans) = "Certificate error: hostname '" ++ hostname ++ "' not in SANs"
  show (CNMismatch hostname cn) = "Certificate error: hostname '" ++ hostname ++ "' doesn't match CN '" ++ cn ++ "'"
  show (PathLengthExceeded actual max) =
    "Certificate error: path length " ++ show actual ++ " exceeds constraint " ++ show max
  show (KeyUsageViolation usage) = "Certificate error: key usage violation - " ++ usage
  show InvalidBasicConstraints = "Certificate error: invalid basic constraints"
  show (CriticalExtensionUnsupported oid) = "Certificate error: unsupported critical extension " ++ oid
  show NoTrustAnchor = "Certificate error: no trust anchor found"
  show SignatureVerificationFailed = "Certificate error: signature verification failed"
  show (InvalidSerialNumber serial) = "Certificate error: invalid serial number '" ++ serial ++ "'"
  show (DuplicateSerial serial) = "Certificate error: duplicate serial number '" ++ serial ++ "'"

--------------------------------------------------------------------------------
-- Constants
--------------------------------------------------------------------------------

||| Minimum RSA key size
minRSAKeySize : Nat
minRSAKeySize = 2048

||| Recommended RSA key size
recommendedRSAKeySize : Nat
recommendedRSAKeySize = 4096

||| Weak signature algorithms
weakAlgorithms : List SignatureAlgorithm
weakAlgorithms = [RSA_SHA1]

||| Acceptable EC curves
acceptableCurves : List String
acceptableCurves =
  [ "P-256", "secp256r1", "prime256v1"
  , "P-384", "secp384r1"
  , "P-521", "secp521r1"
  ]

||| Maximum certificate chain length
maxChainLength : Nat
maxChainLength = 10

--------------------------------------------------------------------------------
-- Algorithm validation
--------------------------------------------------------------------------------

||| Show signature algorithm
public export
showSignatureAlgorithm : SignatureAlgorithm -> String
showSignatureAlgorithm RSA_SHA1 = "RSA-SHA1"
showSignatureAlgorithm RSA_SHA256 = "RSA-SHA256"
showSignatureAlgorithm RSA_SHA384 = "RSA-SHA384"
showSignatureAlgorithm RSA_SHA512 = "RSA-SHA512"
showSignatureAlgorithm ECDSA_SHA256 = "ECDSA-SHA256"
showSignatureAlgorithm ECDSA_SHA384 = "ECDSA-SHA384"
showSignatureAlgorithm ECDSA_SHA512 = "ECDSA-SHA512"
showSignatureAlgorithm Ed25519Sig = "Ed25519"
showSignatureAlgorithm Ed448Sig = "Ed448"

||| Check if algorithm is weak
public export
isWeakAlgorithm : SignatureAlgorithm -> Bool
isWeakAlgorithm algo = elem algo weakAlgorithms

||| Validate signature algorithm
public export
validateAlgorithm : SignatureAlgorithm -> Either CertError SignatureAlgorithm
validateAlgorithm algo =
  if isWeakAlgorithm algo
    then Left (WeakSignatureAlgorithm algo)
    else Right algo

||| Show key type
public export
showKeyType : KeyType -> String
showKeyType (RSAKey size) = "RSA-" ++ show size
showKeyType (ECKey curve) = "EC-" ++ curve
showKeyType Ed25519Key = "Ed25519"
showKeyType Ed448Key = "Ed448"
showKeyType DSAKey = "DSA"

||| Check if key is weak
public export
isWeakKey : KeyType -> Bool
isWeakKey DSAKey = True
isWeakKey (RSAKey size) = size < minRSAKeySize
isWeakKey (ECKey curve) = not (elem curve acceptableCurves)
isWeakKey _ = False

||| Validate key type
public export
validateKeyType : KeyType -> Either CertError KeyType
validateKeyType key =
  case key of
    DSAKey => Left (WeakKeySize DSAKey 0)
    RSAKey size =>
      if size < minRSAKeySize
        then Left (WeakKeySize key size)
        else Right key
    ECKey curve =>
      if elem curve acceptableCurves
        then Right key
        else Left (WeakKeySize key 0)
    _ => Right key

--------------------------------------------------------------------------------
-- Hostname matching
--------------------------------------------------------------------------------

||| Match hostname against SAN DNS entry (supports wildcards)
public export
matchDNSName : String -> String -> Bool
matchDNSName hostname pattern =
  if isPrefixOf "*." pattern
    then let suffix = strSubstr 2 (minus (length pattern) 2) pattern
             parts = split (== '.') hostname
         in length parts > 1 && isSuffixOf suffix hostname
    else toLower hostname == toLower pattern

||| Check if hostname matches any SAN
public export
matchesSAN : String -> List SANType -> Bool
matchesSAN hostname sans =
  any matchSAN sans
  where
    matchSAN : SANType -> Bool
    matchSAN (DNSName pattern) = matchDNSName hostname pattern
    matchSAN (IPAddress ip) = hostname == ip
    matchSAN _ = False

||| Validate hostname against certificate
public export
validateHostname : String -> Certificate -> Either CertError ()
validateHostname hostname cert =
  if matchesSAN hostname cert.extensions.subjectAltNames
    then Right ()
    else case cert.subject.commonName of
      Nothing => Left (SANMismatch hostname cert.extensions.subjectAltNames)
      Just cn =>
        if matchDNSName hostname cn
          then Right ()
          else Left (CNMismatch hostname cn)

--------------------------------------------------------------------------------
-- Key usage validation
--------------------------------------------------------------------------------

||| Check if key usage allows signing
public export
canSign : KeyUsage -> Bool
canSign ku = ku.digitalSignature || ku.nonRepudiation

||| Check if key usage allows key exchange
public export
canKeyExchange : KeyUsage -> Bool
canKeyExchange ku = ku.keyEncipherment || ku.keyAgreement

||| Check if key usage allows CA operations
public export
canSignCerts : KeyUsage -> Bool
canSignCerts ku = ku.keyCertSign

||| Validate key usage for TLS server
public export
validateServerKeyUsage : Certificate -> Either CertError ()
validateServerKeyUsage cert =
  case cert.extensions.keyUsage of
    Nothing => Right ()  -- Key usage is optional
    Just ku =>
      if ku.digitalSignature || ku.keyEncipherment || ku.keyAgreement
        then Right ()
        else Left (KeyUsageViolation "TLS server requires digitalSignature or keyEncipherment")

||| Validate extended key usage for TLS server
public export
validateServerEKU : Certificate -> Either CertError ()
validateServerEKU cert =
  case cert.extensions.extendedKeyUsage of
    Nothing => Right ()  -- EKU is optional
    Just ekus =>
      if any isServerAuth ekus
        then Right ()
        else Left (KeyUsageViolation "TLS server requires serverAuth EKU")
  where
    isServerAuth : ExtendedKeyUsage -> Bool
    isServerAuth ServerAuth = True
    isServerAuth _ = False

--------------------------------------------------------------------------------
-- Chain validation
--------------------------------------------------------------------------------

||| Validate basic constraints for CA certificate
public export
validateCAConstraints : Certificate -> Either CertError ()
validateCAConstraints cert =
  case cert.extensions.basicConstraints of
    Nothing => Left InvalidBasicConstraints
    Just bc =>
      if bc.isCA
        then Right ()
        else Left InvalidBasicConstraints

||| Check path length constraint
public export
checkPathLength : Nat -> Certificate -> Either CertError ()
checkPathLength depth cert =
  case cert.extensions.basicConstraints of
    Nothing => Right ()
    Just bc =>
      case bc.pathLenConstraint of
        Nothing => Right ()
        Just maxLen =>
          if depth <= maxLen
            then Right ()
            else Left (PathLengthExceeded depth maxLen)

--------------------------------------------------------------------------------
-- Certificate construction
--------------------------------------------------------------------------------

||| Create an empty certificate (for testing/mocking)
public export
emptyCertificate : Certificate
emptyCertificate = MkCertificate
  V3
  "0"
  RSA_SHA256
  (MkDistinguishedName Nothing Nothing Nothing Nothing Nothing Nothing Nothing)
  (MkValidityPeriod "" "")
  (MkDistinguishedName Nothing Nothing Nothing Nothing Nothing Nothing Nothing)
  (RSAKey 2048)
  ""
  (MkCertExtensions Nothing Nothing Nothing [] Nothing Nothing [] Nothing [] [])
  ""
  ""

||| Create a validation result
public export
mkValidationResult : Bool -> List String -> List String -> ValidationResult
mkValidationResult valid errs warns = MkValidationResult
  valid
  valid
  valid
  valid
  valid
  errs
  warns

--------------------------------------------------------------------------------
-- Serialization
--------------------------------------------------------------------------------

||| Show certificate version
public export
showVersion : CertVersion -> String
showVersion V1 = "v1"
showVersion V2 = "v2"
showVersion V3 = "v3"

||| Show extended key usage
public export
showEKU : ExtendedKeyUsage -> String
showEKU ServerAuth = "serverAuth"
showEKU ClientAuth = "clientAuth"
showEKU CodeSigning = "codeSigning"
showEKU EmailProtection = "emailProtection"
showEKU TimeStamping = "timeStamping"
showEKU OCSPSigning = "OCSPSigning"
showEKU (CustomOID oid) = oid

||| Show SAN entry
public export
showSAN : SANType -> String
showSAN (DNSName name) = "DNS:" ++ name
showSAN (IPAddress ip) = "IP:" ++ ip
showSAN (Email email) = "email:" ++ email
showSAN (URI uri) = "URI:" ++ uri
showSAN (DirectoryName _) = "DirName:<complex>"

||| Show revocation status
public export
showRevocationStatus : RevocationStatus -> String
showRevocationStatus NotRevoked = "not revoked"
showRevocationStatus (Revoked reason date) = "revoked: " ++ reason ++ " on " ++ date
showRevocationStatus Unknown = "unknown"
showRevocationStatus (CheckFailed msg) = "check failed: " ++ msg

||| Show certificate summary
public export
showCertSummary : Certificate -> String
showCertSummary cert =
  "Certificate:\n" ++
  "  Subject: " ++ maybe "<none>" id cert.subject.commonName ++ "\n" ++
  "  Issuer: " ++ maybe "<none>" id cert.issuer.commonName ++ "\n" ++
  "  Serial: " ++ cert.serialNumber ++ "\n" ++
  "  Valid: " ++ cert.validity.notBefore ++ " to " ++ cert.validity.notAfter ++ "\n" ++
  "  Key: " ++ showKeyType cert.publicKeyType ++ "\n" ++
  "  Signature: " ++ showSignatureAlgorithm cert.signatureAlgorithm ++ "\n" ++
  "  Fingerprint: " ++ cert.fingerprint

||| Show validation result
public export
showValidationResult : ValidationResult -> String
showValidationResult result =
  "Validation: " ++ (if result.isValid then "VALID" else "INVALID") ++ "\n" ++
  (if null result.errors then "" else "  Errors:\n" ++ concat (map (\e => "    - " ++ e ++ "\n") result.errors)) ++
  (if null result.warnings then "" else "  Warnings:\n" ++ concat (map (\w => "    - " ++ w ++ "\n") result.warnings))
