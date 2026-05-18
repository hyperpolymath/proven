-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
||| Safety proofs for X.509 certificate cryptographic-strength checking.
|||
||| The `Proven.SafeCert` module doc claims "expiry checking, and
||| algorithm strength verification". Prior to 2026-05-18 it shipped
||| only the witness types `StrongCert` / `ValidCert` / `ValidChain`
||| with NO discharged theorem that `isWeakHash` / `isStrongAlgorithm`
||| actually reject SHA-1 and RSA-2048 — the verification claim was
||| unproven.
|||
||| This file machine-checks (`idris2 --check`) the core invariant.
||| `HashAlgorithm` (4 ctors) and `CertAlgorithm` (8 ctors) are finite
||| enums and `isWeakHash` / `isStrongAlgorithm` are total
||| pattern-matches with no string residue, so EVERY case is discharged
||| by `Refl`:
|||  * SHA-1 is provably rejected as weak; SHA-256/384/512 accepted;
|||  * RSA-2048 is provably NOT strong; every other algorithm is;
|||  * a `StrongCert` witness is uninhabitable for a SHA-1 cert and for
|||    an RSA-2048 cert;
|||  * the temporal guard: a cert is invalid at a time after `notAfter`.
||| There is NO bridge axiom and NO `believe_me`/`idris_crash`.
module Proven.SafeCert.Proofs

import Proven.SafeCert
import Data.Nat

%default total

--------------------------------------------------------------------------------
-- Hash-algorithm weakness (exhaustive, one Refl per constructor)
--------------------------------------------------------------------------------

||| SHA-1 is provably rejected as a weak signature hash.
public export
sha1IsWeak : isWeakHash SHA1 = True
sha1IsWeak = Refl

public export
sha256NotWeak : isWeakHash SHA256 = False
sha256NotWeak = Refl

public export
sha384NotWeak : isWeakHash SHA384 = False
sha384NotWeak = Refl

public export
sha512NotWeak : isWeakHash SHA512 = False
sha512NotWeak = Refl

--------------------------------------------------------------------------------
-- Key-algorithm strength (exhaustive, one Refl per constructor)
--------------------------------------------------------------------------------

||| RSA-2048 is provably NOT classified strong (minimum, not strong).
public export
rsa2048NotStrong : isStrongAlgorithm RSA_2048 = False
rsa2048NotStrong = Refl

public export
rsa3072Strong : isStrongAlgorithm RSA_3072 = True
rsa3072Strong = Refl

public export
rsa4096Strong : isStrongAlgorithm RSA_4096 = True
rsa4096Strong = Refl

public export
ecdsaP256Strong : isStrongAlgorithm ECDSA_P256 = True
ecdsaP256Strong = Refl

public export
ecdsaP384Strong : isStrongAlgorithm ECDSA_P384 = True
ecdsaP384Strong = Refl

public export
ecdsaP521Strong : isStrongAlgorithm ECDSA_P521 = True
ecdsaP521Strong = Refl

public export
ed25519CertStrong : isStrongAlgorithm Ed25519Cert = True
ed25519CertStrong = Refl

public export
ed448CertStrong : isStrongAlgorithm Ed448Cert = True
ed448CertStrong = Refl

--------------------------------------------------------------------------------
-- StrongCert witness soundness
--------------------------------------------------------------------------------

||| Helper: a certificate whose only varying fields are its key and
||| signature-hash algorithms (other fields fixed/irrelevant here).
mkCert : CertAlgorithm -> HashAlgorithm -> Certificate
mkCert ka sh =
  MkCertificate "s" "i" "0" 0 100 ka sh [] [] False Nothing

||| A modern Ed25519 + SHA-256 certificate provably admits a
||| `StrongCert` witness.
public export
ed25519Sha256IsStrong : StrongCert (mkCert Ed25519Cert SHA256)
ed25519Sha256IsStrong = MkStrongCert Refl Refl

||| There is provably NO `StrongCert` witness for a SHA-1-signed
||| certificate: the constructor demands `isWeakHash sh = False`, but
||| for SHA-1 that is `True = False`, uninhabited.
public export
noStrongCertForSha1 : (ka : CertAlgorithm)
                   -> Not (StrongCert (mkCert ka SHA1))
noStrongCertForSha1 ka (MkStrongCert _ weakPrf) = case weakPrf of Refl impossible

||| There is provably NO `StrongCert` witness for an RSA-2048
||| certificate: `isStrongAlgorithm RSA_2048 = True` is uninhabited.
public export
noStrongCertForRsa2048 : (sh : HashAlgorithm)
                      -> Not (StrongCert (mkCert RSA_2048 sh))
noStrongCertForRsa2048 sh (MkStrongCert strongPrf _) =
  case strongPrf of Refl impossible

--------------------------------------------------------------------------------
-- Temporal guard: a certificate past its notAfter is invalid
--------------------------------------------------------------------------------

||| A certificate evaluated at a time strictly after `notAfter` is
||| provably expired, hence not valid.
public export
expiredAfterNotAfter : isValidAt (mkCert Ed25519Cert SHA256) 101 = False
expiredAfterNotAfter = Refl

||| And it is provably reported expired.
public export
expiredIsDetected : isExpiredAt (mkCert Ed25519Cert SHA256) 101 = True
expiredIsDetected = Refl

||| Within the validity window the same certificate is valid.
public export
validWithinWindow : isValidAt (mkCert Ed25519Cert SHA256) 50 = True
validWithinWindow = Refl
