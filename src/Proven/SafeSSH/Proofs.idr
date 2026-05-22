-- SPDX-License-Identifier: MPL-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
||| Safety proofs for SSH key algorithm strength enforcement.
|||
||| The `Proven.SafeSSH` module doc claims "Prevents: use of weak
||| algorithms". Prior to 2026-05-18 it shipped only the witness types
||| `StrongKey` / `ValidKey` with NO discharged theorem that
||| `isWeakAlgorithm` / `validateKey` actually reject the broken
||| algorithms — the prevention claim was unproven.
|||
||| This file machine-checks (`idris2 --check`) the core invariant. The
||| invariant is *fully decidable over a finite enum* (`SSHAlgorithm`
||| has 9 constructors and `algorithmStrength` is total pattern-match
||| with no string residue), so EVERY case is discharged by `Refl`:
|||  * `isWeakAlgorithm` rejects the broken algorithm (DSA) and accepts
|||    every non-broken one — exhaustive, one theorem per constructor;
|||  * `validateKey` returns `WeakAlgorithm` for a DSA key regardless of
|||    key data, and never `Valid` for it;
|||  * the `StrongKey` / `ValidKey` witnesses are inhabited exactly for
|||    the strong/valid cases and provably uninhabitable for DSA.
||| There is NO bridge axiom and NO `believe_me`/`idris_crash`: the
||| entire surface is type-level reducible.
module Proven.SafeSSH.Proofs

import Proven.SafeSSH

%default total

--------------------------------------------------------------------------------
-- Exhaustive strength classification (one Refl per constructor)
--------------------------------------------------------------------------------

public export
dsaIsBroken : algorithmStrength DSA = Broken
dsaIsBroken = Refl

public export
rsaIsAcceptable : algorithmStrength RSA = Acceptable
rsaIsAcceptable = Refl

public export
ecdsa256IsAcceptable : algorithmStrength ECDSA_256 = Acceptable
ecdsa256IsAcceptable = Refl

public export
ecdsa384IsStrong : algorithmStrength ECDSA_384 = Strong
ecdsa384IsStrong = Refl

public export
ecdsa521IsStrong : algorithmStrength ECDSA_521 = Strong
ecdsa521IsStrong = Refl

public export
ed25519IsExcellent : algorithmStrength Ed25519 = Excellent
ed25519IsExcellent = Refl

public export
ed448IsExcellent : algorithmStrength Ed448 = Excellent
ed448IsExcellent = Refl

public export
skEcdsa256IsStrong : algorithmStrength SK_ECDSA_256 = Strong
skEcdsa256IsStrong = Refl

public export
skEd25519IsExcellent : algorithmStrength SK_Ed25519 = Excellent
skEd25519IsExcellent = Refl

--------------------------------------------------------------------------------
-- isWeakAlgorithm: the broken algorithm is rejected, strong ones accepted
--------------------------------------------------------------------------------

||| DSA (deprecated in OpenSSH 7.0) is provably rejected as weak.
public export
dsaIsWeak : isWeakAlgorithm DSA = True
dsaIsWeak = Refl

public export
rsaNotWeak : isWeakAlgorithm RSA = False
rsaNotWeak = Refl

public export
ecdsa256NotWeak : isWeakAlgorithm ECDSA_256 = False
ecdsa256NotWeak = Refl

public export
ecdsa384NotWeak : isWeakAlgorithm ECDSA_384 = False
ecdsa384NotWeak = Refl

public export
ecdsa521NotWeak : isWeakAlgorithm ECDSA_521 = False
ecdsa521NotWeak = Refl

public export
ed25519NotWeak : isWeakAlgorithm Ed25519 = False
ed25519NotWeak = Refl

public export
ed448NotWeak : isWeakAlgorithm Ed448 = False
ed448NotWeak = Refl

public export
skEcdsa256NotWeak : isWeakAlgorithm SK_ECDSA_256 = False
skEcdsa256NotWeak = Refl

public export
skEd25519NotWeak : isWeakAlgorithm SK_Ed25519 = False
skEd25519NotWeak = Refl

--------------------------------------------------------------------------------
-- validateKey: a DSA key is rejected regardless of key data
--------------------------------------------------------------------------------

||| A DSA key with arbitrary key data and comment is provably reported
||| as `WeakAlgorithm` — the weak-algorithm guard runs before the
||| length check, so no key-data value can launder a broken algorithm.
public export
dsaKeyRejected : (dat : String) -> (cmt : String)
              -> validateKey (MkSSHPublicKey DSA dat cmt) = WeakAlgorithm
dsaKeyRejected dat cmt = Refl

||| Consequently a DSA key can never validate as `Valid`.
public export
dsaKeyNeverValid : (dat : String) -> (cmt : String)
                -> Not (validateKey (MkSSHPublicKey DSA dat cmt) = Valid)
dsaKeyNeverValid dat cmt prf = case prf of Refl impossible

--------------------------------------------------------------------------------
-- Witness soundness: StrongKey / ValidKey track the predicates exactly
--------------------------------------------------------------------------------

||| A `StrongKey` witness for any Ed25519 key is constructible — the
||| recommended modern algorithm is provably acceptable.
public export
ed25519StrongKey : (dat : String) -> StrongKey (MkSSHPublicKey Ed25519 dat "")
ed25519StrongKey dat = MkStrongKey Refl

||| There is provably NO `StrongKey` witness for a DSA key: its sole
||| constructor demands `isWeakAlgorithm DSA = False`, but that is
||| `True = False`, which is uninhabited.
public export
noStrongKeyForDsa : (dat : String) -> Not (StrongKey (MkSSHPublicKey DSA dat ""))
noStrongKeyForDsa dat (MkStrongKey prf) = case prf of Refl impossible

||| There is provably NO `ValidKey` witness for a DSA key.
public export
noValidKeyForDsa : (dat : String) -> Not (ValidKey (MkSSHPublicKey DSA dat ""))
noValidKeyForDsa dat (MkValidKey prf) = case prf of Refl impossible
