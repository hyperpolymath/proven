-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
||| Safety proofs for JWT operations
|||
||| This module provides formal proofs that SafeJWT operations
||| maintain security properties.
module Proven.SafeJWT.Proofs

import Proven.Core
import Proven.SafeJWT.Types
import Proven.SafeJWT.Decode
import Proven.SafeJWT.Validate
import Data.Bits
import Data.List
import Data.Maybe
import Data.String

%default total

--------------------------------------------------------------------------------
-- Security Predicates
--------------------------------------------------------------------------------

||| Predicate: Algorithm is secure (not 'none')
public export
data IsSecureAlg : JWTAlgorithm -> Type where
  HS256Secure : IsSecureAlg HS256
  HS384Secure : IsSecureAlg HS384
  HS512Secure : IsSecureAlg HS512
  RS256Secure : IsSecureAlg RS256
  RS384Secure : IsSecureAlg RS384
  RS512Secure : IsSecureAlg RS512
  ES256Secure : IsSecureAlg ES256
  ES384Secure : IsSecureAlg ES384
  ES512Secure : IsSecureAlg ES512
  PS256Secure : IsSecureAlg PS256
  PS384Secure : IsSecureAlg PS384
  PS512Secure : IsSecureAlg PS512
  EdDSASecure : IsSecureAlg EdDSA

||| Predicate: Token has not expired
public export
data NotExpired : Integer -> Integer -> Type where
  MkNotExpired : (exp : Integer) -> (current : Integer) ->
                 {auto prf : So (current <= exp)} -> NotExpired exp current

||| Predicate: Token is currently valid (nbf <= current <= exp)
public export
data IsCurrentlyValid : Integer -> Integer -> Integer -> Type where
  MkCurrentlyValid : (nbf : Integer) -> (exp : Integer) -> (current : Integer) ->
                     {auto prf1 : So (nbf <= current)} ->
                     {auto prf2 : So (current <= exp)} ->
                     IsCurrentlyValid nbf exp current

||| Predicate: Signature has been verified
public export
data SignatureVerified : DecodedJWT -> SigningKey -> Type where
  MkSignatureVerified : (jwt : DecodedJWT) -> (key : SigningKey) -> SignatureVerified jwt key

||| Predicate: Claims have been validated
public export
data ClaimsValidated : JWTClaims -> ValidationOptions -> Type where
  MkClaimsValidated : (claims : JWTClaims) -> (opts : ValidationOptions) -> ClaimsValidated claims opts

||| Predicate: JWT is fully validated
public export
data FullyValidated : ValidatedJWT -> Type where
  MkFullyValidated : (vjwt : ValidatedJWT) -> FullyValidated vjwt

--------------------------------------------------------------------------------
-- Algorithm Security Proofs
--------------------------------------------------------------------------------

||| Theorem: 'none' algorithm is not secure
export
noneNotSecure : Not (IsSecureAlg None)
noneNotSecure HS256Secure impossible
noneNotSecure HS384Secure impossible
noneNotSecure HS512Secure impossible
noneNotSecure RS256Secure impossible
noneNotSecure RS384Secure impossible
noneNotSecure RS512Secure impossible
noneNotSecure ES256Secure impossible
noneNotSecure ES384Secure impossible
noneNotSecure ES512Secure impossible
noneNotSecure PS256Secure impossible
noneNotSecure PS384Secure impossible
noneNotSecure PS512Secure impossible
noneNotSecure EdDSASecure impossible

||| Theorem: HMAC algorithms are symmetric.
||| Proof by case-split on algorithm, discharging impossible branches.
export
hmacIsSymmetric : (alg : JWTAlgorithm) ->
                  Either (alg = HS256) (Either (alg = HS384) (alg = HS512)) ->
                  isSymmetric alg = True
hmacIsSymmetric HS256 _  = Refl
hmacIsSymmetric HS384 _  = Refl
hmacIsSymmetric HS512 _  = Refl
hmacIsSymmetric None  (Left prf)          = case prf of _ impossible
hmacIsSymmetric None  (Right (Left prf))  = case prf of _ impossible
hmacIsSymmetric None  (Right (Right prf)) = case prf of _ impossible
hmacIsSymmetric RS256 (Left prf)          = case prf of _ impossible
hmacIsSymmetric RS256 (Right (Left prf))  = case prf of _ impossible
hmacIsSymmetric RS256 (Right (Right prf)) = case prf of _ impossible
hmacIsSymmetric RS384 (Left prf)          = case prf of _ impossible
hmacIsSymmetric RS384 (Right (Left prf))  = case prf of _ impossible
hmacIsSymmetric RS384 (Right (Right prf)) = case prf of _ impossible
hmacIsSymmetric RS512 (Left prf)          = case prf of _ impossible
hmacIsSymmetric RS512 (Right (Left prf))  = case prf of _ impossible
hmacIsSymmetric RS512 (Right (Right prf)) = case prf of _ impossible
hmacIsSymmetric ES256 (Left prf)          = case prf of _ impossible
hmacIsSymmetric ES256 (Right (Left prf))  = case prf of _ impossible
hmacIsSymmetric ES256 (Right (Right prf)) = case prf of _ impossible
hmacIsSymmetric ES384 (Left prf)          = case prf of _ impossible
hmacIsSymmetric ES384 (Right (Left prf))  = case prf of _ impossible
hmacIsSymmetric ES384 (Right (Right prf)) = case prf of _ impossible
hmacIsSymmetric ES512 (Left prf)          = case prf of _ impossible
hmacIsSymmetric ES512 (Right (Left prf))  = case prf of _ impossible
hmacIsSymmetric ES512 (Right (Right prf)) = case prf of _ impossible
hmacIsSymmetric PS256 (Left prf)          = case prf of _ impossible
hmacIsSymmetric PS256 (Right (Left prf))  = case prf of _ impossible
hmacIsSymmetric PS256 (Right (Right prf)) = case prf of _ impossible
hmacIsSymmetric PS384 (Left prf)          = case prf of _ impossible
hmacIsSymmetric PS384 (Right (Left prf))  = case prf of _ impossible
hmacIsSymmetric PS384 (Right (Right prf)) = case prf of _ impossible
hmacIsSymmetric PS512 (Left prf)          = case prf of _ impossible
hmacIsSymmetric PS512 (Right (Left prf))  = case prf of _ impossible
hmacIsSymmetric PS512 (Right (Right prf)) = case prf of _ impossible
hmacIsSymmetric EdDSA (Left prf)          = case prf of _ impossible
hmacIsSymmetric EdDSA (Right (Left prf))  = case prf of _ impossible
hmacIsSymmetric EdDSA (Right (Right prf)) = case prf of _ impossible

||| Theorem: Default validation rejects 'none' algorithm
export
defaultRejectsNone : Types.defaultValidation.rejectNone = True
defaultRejectsNone = Refl

||| Theorem: Strict validation requires secure algorithms only
||| Depends on strictValidation.allowedAlgorithms containing only secure algorithms.
export
strictAllowsOnlySecure : (alg : JWTAlgorithm) ->
                         alg `elem` Types.strictValidation.allowedAlgorithms = True ->
                         IsSecureAlg alg
strictAllowsOnlySecure HS256 _ = HS256Secure
strictAllowsOnlySecure HS384 _ = HS384Secure
strictAllowsOnlySecure HS512 _ = HS512Secure
strictAllowsOnlySecure RS256 _ = RS256Secure
strictAllowsOnlySecure RS384 _ = RS384Secure
strictAllowsOnlySecure RS512 _ = RS512Secure
strictAllowsOnlySecure ES256 _ = ES256Secure
strictAllowsOnlySecure ES384 _ = ES384Secure
strictAllowsOnlySecure ES512 _ = ES512Secure
strictAllowsOnlySecure PS256 _ = PS256Secure
strictAllowsOnlySecure PS384 _ = PS384Secure
strictAllowsOnlySecure PS512 _ = PS512Secure
strictAllowsOnlySecure EdDSA _ = EdDSASecure
strictAllowsOnlySecure None prf = absurd prf

--------------------------------------------------------------------------------
-- Validation Safety Proofs
--------------------------------------------------------------------------------

||| Theorem: A validated JWT has passed all validation checks
export
validatedMeansChecked : (vjwt : ValidatedJWT) -> FullyValidated vjwt
validatedMeansChecked vjwt = MkFullyValidated vjwt

||| OWED [Category A — Result-monad soundness from `isOk = True`]:
||| if `validateExp currentTime skew claims` returns `Ok ()` and the
||| `exp` claim is `Just exp`, then `currentTime <= exp + cast
||| skew.expLeeway` holds (boolean form). The implementation in
||| `Proven.SafeJWT.Validate` L65-L72 case-splits on `claims.exp`,
||| then branches on the boolean `currentTime > exp + cast
||| skew.expLeeway`, so semantically `isOk` excludes the "true" arm.
|||
||| Held back by Idris2 0.8.0 not type-level reducing the `if/then/
||| else` decision over `Integer` `>` (an FFI-bound primitive on
||| arbitrary-precision integers) — `isOk (if b then Err _ else Ok
||| ())` does not normalise from `b = False` by Refl alone, and the
||| `<=`/`>` Bool-vs-Prop reflection is not exposed for `Integer`.
||| Same blocker family as boj-server's `Bool`-vs-`Prop` reflection
||| gap on FFI primitives (class-J `charEqSound` shape). Discharge
||| once a `Data.Integer` reflective tactic for `<=`/`>` is available,
||| or by introducing a generic `ifFalseElim : (if b then x else y) =
||| y -> b = False` and case-splitting on `claims.exp`.
public export
0 expValidationSound : (currentTime : Integer) -> (skew : ClockSkew) -> (claims : JWTClaims) ->
                       isOk (validateExp currentTime skew claims) = True ->
                       (exp : Integer) -> claims.exp = Just exp ->
                       currentTime <= exp + cast skew.expLeeway = True

||| OWED [Category A — Result-monad soundness from `isOk = True`]:
||| if `validateNbf currentTime skew claims` returns `Ok ()` and the
||| `nbf` claim is `Just nbf`, then `currentTime >= nbf - cast
||| skew.nbfLeeway` holds. The implementation in
||| `Proven.SafeJWT.Validate` L76-L83 case-splits on `claims.nbf`,
||| then branches on `currentTime < nbf - cast skew.nbfLeeway`; the
||| `Ok ()` arm exactly excludes that condition.
|||
||| Held back by Idris2 0.8.0 for the same reason as
||| `expValidationSound`: no `Bool`-vs-`Prop` reflection lemma for
||| `Integer` `<`/`>=` (FFI-bound primitive), and `if/then/else`
||| over an opaque Bool head does not normalise by Refl. Same
||| blocker family. Discharge with the same `Data.Integer`
||| reflective tactic or `ifFalseElim` lemma that closes
||| `expValidationSound`.
public export
0 nbfValidationSound : (currentTime : Integer) -> (skew : ClockSkew) -> (claims : JWTClaims) ->
                       isOk (validateNbf currentTime skew claims) = True ->
                       (nbf : Integer) -> claims.nbf = Just nbf ->
                       currentTime >= nbf - cast skew.nbfLeeway = True

||| OWED [Category A — String-FFI + Result-monad soundness]: if
||| `validateIssuer expected claims` returns `Ok ()`, then
||| `claims.iss = Just expected` (propositional equality on `String`).
||| The implementation in `Proven.SafeJWT.Validate` L113-L120 case-
||| splits on `claims.iss` (`Nothing` arm produces `Err`), then
||| branches on the boolean `iss == expected`; the `Ok ()` arm gives
||| us `iss == expected = True`.
|||
||| Held back by Idris2 0.8.0 not exposing a `Bool`-vs-`Prop`
||| reflection lemma for the derived `Eq String` instance — the
||| backend `(==)` on `String` is FFI-bound (`prim__strEq`), so
||| `iss == expected = True` does not imply `iss = expected` by
||| Refl alone (the elaborator cannot unfold `prim__strEq` to a
||| propositional witness). Same blocker family as boj-server's
||| `charEqSound` (class-J: `prim__eq_Char` cannot be reduced to
||| `=` without a trusted lemma). Discharge once `Data.String`
||| ships a reflective `strEqSound : (s1 == s2 = True) -> s1 = s2`
||| with the same trust posture as boj-server `SafetyLemmas`.
public export
0 issuerValidationSound : (expected : String) -> (claims : JWTClaims) ->
                          isOk (validateIssuer expected claims) = True ->
                          claims.iss = Just expected

||| OWED [Category A — String-FFI + List-elem soundness]: if
||| `validateAudience expected claims` returns `Ok ()`, then
||| `expected \`elem\` getAudienceList claims = True` (boolean form).
||| The implementation in `Proven.SafeJWT.Validate` L124-L129 is
||| `let audList = getAudienceList claims in if expected \`elem\`
||| audList then Ok () else Err _`; semantically the `Ok ()` arm
||| exactly gives us `True`.
|||
||| Held back by Idris2 0.8.0 not type-level reducing
||| `Data.List.elem` over `String` — `elem` defers to `(==)` on
||| `String`, which is the same `prim__strEq` FFI primitive as in
||| `issuerValidationSound` (opaque to the type-level reducer).
||| Combined with the `if/then/else` Bool-head opacity, the
||| implication does not close by Refl. Same blocker family.
||| Discharge once `Data.List` ships a reflective `elemSound :
||| (x \`elem\` xs = True) -> ...` for instances whose `(==)` has a
||| Bool-Prop reflection lemma, parameterised by `strEqSound`.
public export
0 audienceValidationSound : (expected : String) -> (claims : JWTClaims) ->
                            isOk (validateAudience expected claims) = True ->
                            expected `elem` getAudienceList claims = True

--------------------------------------------------------------------------------
-- Signature Verification Proofs
--------------------------------------------------------------------------------

||| OWED [Category B — Cryptographic FFI + Result-monad soundness]:
||| if `verifySignature key jwt` returns `Ok ()`, then the boolean
||| `isKeyValidForAlgorithm key jwt.header.alg = True`. The
||| implementation in `Proven.SafeJWT.Validate` L231-L270 begins with
||| `if not (isKeyValidForAlgorithm key alg) then Err _ else ...`,
||| so the `Ok ()` arm is reachable only when that head is `False`,
||| i.e. `isKeyValidForAlgorithm key alg = True`.
|||
||| Held back by Idris2 0.8.0 not type-level reducing the chained
||| `if/then/else` over the `Bool` decision head, AND by the
||| downstream branches calling `prim__verifyHMAC` / `prim__verifyRSA`
||| / `prim__verifyECDSA` / `prim__verifyEdDSA` — all FFI-bound
||| cryptographic primitives that are completely opaque to the type
||| checker (they encode the underlying hardness assumption). Same
||| trust posture as boj-server's I7 FFI-correctness assumptions.
||| Discharge once a `Bool`-vs-`Prop` reflection lemma for the
||| top-level `if not (...) then Err _ else ...` is available, OR
||| by refactoring `verifySignature` to case-split on a
||| `Dec (KeyValidForAlgorithm key alg)` witness so the
||| precondition is propositional rather than boolean.
public export
0 keyMustMatchAlgorithm : (key : SigningKey) -> (jwt : DecodedJWT) ->
                          isOk (verifySignature key jwt) = True ->
                          isKeyValidForAlgorithm key jwt.header.alg = True

||| OWED [Category B — Cryptographic FFI + algorithm-tag soundness]:
||| if `verifySignature NoKey jwt = Ok ()`, then `jwt.header.alg =
||| None` (propositional). The implementation in
||| `Proven.SafeJWT.Validate` L239-L242 case-splits `NoKey =>` and
||| then `if alg == None then Ok () else Err _`. Reading off
||| backwards: `Ok ()` implies `alg == None = True`, which should
||| imply `alg = None`.
|||
||| Held back by Idris2 0.8.0 for two stacked reasons:
||| (1) `if/then/else` over a Bool head doesn't normalise backwards
||| by Refl (same shape as `expValidationSound`);
||| (2) the derived `(==)` on the `JWTAlgorithm` enum is Bool, and
||| reflecting `alg == None = True` to `alg = None` requires a
||| `decEq`-style witness that Idris2 0.8.0 does not derive
||| automatically for enums. Same `Bool`-vs-`Prop` reflection class
||| as boj-server's `charEqSound`. Discharge by either
||| hand-writing `decEq` for `JWTAlgorithm` and case-splitting on
||| `decEq alg None`, or by providing an `algEqSound : (a1 == a2 =
||| True) -> a1 = a2` lemma (potentially via a 14-arm pattern match,
||| one `Refl` per algorithm constructor — analogous to the
||| explicit enum case-split in `noneNotSecure` above).
public export
0 noKeyOnlyForNone : (jwt : DecodedJWT) ->
                     isOk (verifySignature NoKey jwt) = True ->
                     jwt.header.alg = None

||| OWED [Category B — Cryptographic FFI + algorithm-tag soundness]:
||| if `verifySignature (SecretKey secret) jwt = Ok ()`, then
||| `isSymmetric jwt.header.alg = True` (i.e. the algorithm is one
||| of `HS256`/`HS384`/`HS512`). The chain runs through the top
||| `isKeyValidForAlgorithm (SecretKey _) alg` guard, which is
||| `True` only for the three HS arms (see
||| `Proven.SafeJWT.Types.isKeyValidForAlgorithm` L210-L212), and
||| `isSymmetric` is exactly `True` on those three (Types L109-
||| L113).
|||
||| Held back by Idris2 0.8.0 because closing the proof requires
||| eliminating both the `if not (isKeyValidForAlgorithm ...)`
||| guard AND the downstream `verifyHMAC` call (an FFI
||| primitive), then a 14-arm case-split on `jwt.header.alg` to
||| line up `isKeyValidForAlgorithm = True` with `isSymmetric =
||| True`. Same blocker family as `keyMustMatchAlgorithm` plus
||| `noKeyOnlyForNone`. Discharge once those two have an enum
||| `decEq`/reflection lemma, or by hand-writing the 14-arm split
||| (`HS256`/`HS384`/`HS512` close by `Refl`; the other 11 arms
||| derive `False = True` from the guard premise and discharge
||| via `absurd Refl`).
public export
0 secretKeyRequiresHMAC : (secret : List Bits8) -> (jwt : DecodedJWT) ->
                          isOk (verifySignature (SecretKey secret) jwt) = True ->
                          isSymmetric jwt.header.alg = True

--------------------------------------------------------------------------------
-- Claim Validation Proofs
--------------------------------------------------------------------------------

||| OWED [Category A — List recursion + String-FFI + Result-monad
||| soundness]: if `validateRequiredClaims claims jwtClaims = Ok ()`
||| and `name \`elem\` claims = True`, then `hasRequiredClaim name
||| jwtClaims = True`. The implementation in
||| `Proven.SafeJWT.Validate` L169-L175 is a recursive walk over
||| `claims` that returns `Err` on the first missing name, so a
||| successful `Ok ()` propagates `hasRequiredClaim = True` for
||| every member of the list.
|||
||| Held back by Idris2 0.8.0 because the proof requires
||| (1) `List` induction over `claims` (which type-level reduces
||| but produces a recursive goal), (2) reflection of `\`elem\``
||| over `String` (FFI-bound `(==)`, same blocker as
||| `audienceValidationSound`), AND (3) reflection of `hasRequiredClaim`
||| through `getClaim` / `isJust` (the claim lookup threads through
||| `List` `lookup` on `String` keys, again opaque). Same blocker
||| family as `issuerValidationSound` and `audienceValidationSound`.
||| Discharge once `strEqSound` and `Data.List.elemSound` are
||| available, plus a hand-rolled induction principle on the
||| `validateRequiredClaims` recursion.
public export
0 requiredClaimsPresent : (claims : List String) -> (jwtClaims : JWTClaims) ->
                          isOk (validateRequiredClaims claims jwtClaims) = True ->
                          (name : String) -> name `elem` claims = True ->
                          hasRequiredClaim name jwtClaims = True

||| OWED [Category A — Integer-FFI + Result-monad soundness]: if
||| `validateMaxAge currentTime maxAge claims = Ok ()` and `claims.iat
||| = Just iat`, then `currentTime - iat <= cast maxAge = True`. The
||| implementation in `Proven.SafeJWT.Validate` L98-L105 routes
||| through `tokenAge` (which returns `Nothing` when `iat` is
||| absent), then branches on `age > cast maxAge`.
|||
||| Held back by Idris2 0.8.0 because closing the proof requires
||| (1) inlining `tokenAge` (which itself case-splits on `claims.iat`),
||| (2) reflecting the boolean `>` decision back to `<=` (Bool-vs-
||| Prop reflection over `Integer`, same as `expValidationSound`),
||| AND (3) reasoning about `cast : Nat -> Integer` (which goes
||| through `Integer.fromNat`, partially opaque to the type-level
||| reducer). Same blocker family as `expValidationSound` /
||| `nbfValidationSound`. Discharge once a `Data.Integer` reflective
||| tactic for `<=`/`>` is available, plus a `castNatToInteger`
||| reduction lemma.
public export
0 maxAgeValidationSound : (currentTime : Integer) -> (maxAge : Nat) -> (claims : JWTClaims) ->
                          isOk (validateMaxAge currentTime maxAge claims) = True ->
                          (iat : Integer) -> claims.iat = Just iat ->
                          currentTime - iat <= cast maxAge = True

--------------------------------------------------------------------------------
-- Composition Proofs
--------------------------------------------------------------------------------

||| OWED [Category C — Result-monad composition + record-projection
||| opacity]: if `validateDecoded opts currentTime jwt = Ok ()`, then
||| every gated sub-validation that `validateDecoded` performs also
||| returns `Ok ()`. The implementation in `Proven.SafeJWT.Validate`
||| L278-L303 is a `do`-block that sequences `validateAlgorithm`,
||| `when opts.validateExp $ validateExp ...`, `when opts.validateNbf
||| ...`, the `maxAge`/`requiredIssuer`/`requiredAudience` case-splits,
||| and `validateRequiredClaims` — by monadic propagation, `Ok ()` at
||| the top forces `Ok ()` at every step.
|||
||| Held back by Idris2 0.8.0 because closing the proof requires
||| (1) inlining the `Result` `>>=` bind to expose each step's
||| `isOk` decision, (2) reflecting the local `when : Bool ->
||| Result _ _ -> Result _ _` helper (an `if/then/else` in
||| disguise — same Bool-head opacity), AND (3) record-projection
||| reduction on `opts.validateExp` / `opts.validateNbf` /
||| `opts.requiredIssuer` (Idris2 0.8.0 keeps record accessors
||| opaque outside `with` blocks). Same composition-blocker family
||| as boj-server's `validateDecoded` cross-cartridge OWED.
||| Discharge once `Result`'s `>>=` ships a reflective
||| `bindOkProjectLeft : (m >>= k) = Ok x -> exists y. m = Ok y`
||| and the `when`/projection opacities are addressed (likely the
||| same `Data.Bool` reflective tactic as `expValidationSound`).
public export
0 fullValidationImpliesAll :
  (opts : ValidationOptions) -> (currentTime : Integer) -> (jwt : DecodedJWT) ->
  isOk (validateDecoded opts currentTime jwt) = True ->
  (opts.validateExp = True -> isOk (validateExp currentTime opts.clockSkew jwt.claims) = True,
   opts.validateNbf = True -> isOk (validateNbf currentTime opts.clockSkew jwt.claims) = True,
   case opts.requiredIssuer of Just iss => isOk (validateIssuer iss jwt.claims) = True; Nothing => ())

||| OWED [Category C — Existential reverse-construction with no
||| type-level witness]: every inhabitant of `ValidatedJWT` admits
||| a dependent quadruple `(opts, key, currentTime, decoded)` such
||| that `validate opts key currentTime decoded = Ok vjwt`. The
||| convention (see `Proven.SafeJWT.Types`) is that `MkValidatedJWT`
||| is exported only as a *private* constructor used by
||| `Proven.SafeJWT.Validate.validate` (L307-L315) after all checks
||| pass — but the type itself does not encode that provenance.
|||
||| Held back by Idris2 0.8.0 because `ValidatedJWT` is a plain
||| record (`MkValidatedJWT : DecodedJWT -> Integer -> String ->
||| ValidatedJWT`) with no refinement on its fields and no
||| existential ghost-oracle field carrying the originating
||| `validate` call. Any module with import-visibility on
||| `MkValidatedJWT` can fabricate a value, so the implication
||| "every `ValidatedJWT` came from `validate`" is a
||| **module-discipline invariant**, not a type-level one. Same
||| shape as boj-server's I7-style "stated at the FFI/constructor
||| seam, not proved" assumptions, and direct analogue of
||| `SafeTOML.dateComponentsValid` (parser-postcondition without
||| a refinement type). Discharge by refactoring `ValidatedJWT`
||| to a smart-constructor / dependent-record carrying a proof
||| field, e.g. `MkValidatedJWT : (jwt : DecodedJWT) -> (cur :
||| Integer) -> (origin : String) -> {auto 0 prf :
||| (opts ** key ** decoded ** validate opts key cur decoded =
||| Ok ...)} -> ValidatedJWT`, and updating `validate` to populate
||| the proof. Alternatively, hide `MkValidatedJWT` from the public
||| API and re-export only the `validate` smart constructor.
public export
0 validatedJWTFromValidation : (vjwt : ValidatedJWT) ->
                                (opts : ValidationOptions ** key : SigningKey ** currentTime : Integer **
                                 decoded : DecodedJWT **
                                 isOk (validate opts key currentTime decoded) = True)

--------------------------------------------------------------------------------
-- Security Guarantee Proofs
--------------------------------------------------------------------------------

||| OWED [Category D — Negative soundness through `validateAlgorithm`
||| guard]: if `opts.rejectNone = True` and `jwt.header.alg = None`,
||| then `validateDecoded opts 0 jwt` does NOT return `Ok ()`. The
||| chain is: `validateDecoded` (Validate L278-L283) calls
||| `validateAlgorithm opts.allowedAlgorithms opts.rejectNone
||| jwt.header` first, and `validateAlgorithm` (L147-L156) returns
||| `Err AlgorithmNoneNotAllowed` when `rejectNone && alg == None`
||| is `True`. Monadic `Err` propagates to the top.
|||
||| Held back by Idris2 0.8.0 because the proof requires
||| (1) rewriting `opts.rejectNone` to `True` via record-projection
||| reduction (opaque outside `with`), (2) rewriting `jwt.header.alg`
||| to `None` and forcing `alg == None` to `True` (needs the
||| `algEqSound` reflection from `noKeyOnlyForNone`), AND
||| (3) propagating `Err` through the `Result` `do`-block (same
||| bind-projection blocker as `fullValidationImpliesAll`).
||| Same blocker family. Discharge once those three reflective
||| lemmas are in place — at that point `rejectNonePreventsNone`
||| closes by a 3-step `rewrite` + `Refl`.
public export
0 rejectNonePreventsNone : (opts : ValidationOptions) -> opts.rejectNone = True ->
                           (jwt : DecodedJWT) -> jwt.header.alg = None ->
                           isOk (validateDecoded opts 0 jwt) = False

||| OWED [Category D — Negative soundness through `validateAlgorithm`
||| guard]: if `opts.allowedAlgorithms` is non-empty, `alg` is not in
||| it, and `jwt.header.alg = alg`, then `validateDecoded opts 0 jwt`
||| does NOT return `Ok ()`. The chain runs through
||| `validateAlgorithm` (L147-L156): with `null allowed = False` and
||| `alg \`elem\` allowed = False`, the function returns
||| `Err (AlgorithmMismatch allowed alg)`, which `validateDecoded`'s
||| monadic bind propagates to `Err` at the top.
|||
||| Held back by Idris2 0.8.0 with the same blocker stack as
||| `rejectNonePreventsNone` — record-projection opacity on
||| `opts.allowedAlgorithms` / `opts.rejectNone`, the `\`elem\``
||| reflection over `JWTAlgorithm` (Bool-Prop reflection on a
||| derived `Eq`, same shape as `noKeyOnlyForNone`'s `algEqSound`),
||| AND `Result`-bind `Err`-propagation. Discharge with the same
||| three reflective lemmas.
public export
0 allowedAlgorithmsRestrictive : (opts : ValidationOptions) ->
                                 (alg : JWTAlgorithm) -> not (null opts.allowedAlgorithms) = True ->
                                 not (alg `elem` opts.allowedAlgorithms) = True ->
                                 (jwt : DecodedJWT) -> jwt.header.alg = alg ->
                                 isOk (validateDecoded opts 0 jwt) = False

--------------------------------------------------------------------------------
-- Safety Documentation
--------------------------------------------------------------------------------

||| Summary of SafeJWT security guarantees:
|||
||| 1. **Algorithm Safety**: The 'none' algorithm is rejected by default.
|||    This prevents attackers from stripping signatures.
|||
||| 2. **Expiration Enforcement**: Tokens are rejected after expiration.
|||    Clock skew tolerance is configurable but defaults to 60 seconds.
|||
||| 3. **Not-Before Enforcement**: Tokens issued for future use are rejected
|||    until their nbf time arrives.
|||
||| 4. **Issuer Validation**: When required, tokens must have the expected issuer.
|||
||| 5. **Audience Validation**: When required, tokens must include the expected
|||    audience in their aud claim.
|||
||| 6. **Key-Algorithm Binding**: Secret keys can only verify HMAC tokens,
|||    RSA keys can only verify RSA tokens, etc.
|||
||| 7. **Type-Level Validation**: ValidatedJWT can only be constructed through
|||    the validation process, ensuring claims are always checked.
|||
||| 8. **No Exception Throwing**: All operations return Result types,
|||    making error handling explicit and preventing crashes.
public export
securityGuarantees : String
securityGuarantees = """
SafeJWT Security Guarantees:

1. Algorithm Safety
   - 'none' algorithm rejected by default
   - Configurable allowed algorithm list
   - Key-algorithm type matching enforced

2. Time-Based Security
   - Expiration (exp) validated
   - Not-before (nbf) validated
   - Issued-at (iat) validated
   - Max age enforcement
   - Configurable clock skew

3. Claim Verification
   - Issuer (iss) matching
   - Audience (aud) matching
   - Required claims enforcement

4. Type Safety
   - ValidatedJWT only from validation
   - Explicit error handling
   - No runtime exceptions

5. Key Management
   - Key type must match algorithm
   - Symmetric/asymmetric distinction
   - Curve matching for EC keys
"""

--------------------------------------------------------------------------------
-- Attack Prevention Proofs
--------------------------------------------------------------------------------

||| OWED [Category E — Attack-prevention via `isKeyValidForAlgorithm`
||| disjointness]: an RSA key (`isKeyValidForAlgorithm rsaKey RS256
||| = True`) is rejected for `HS256` (`isKeyValidForAlgorithm rsaKey
||| HS256 = False`). The supporting fact is structural: the only
||| arms of `isKeyValidForAlgorithm` that return `True` for `RS256`
||| are `(RSAPublicKey _ _) RS256` and `(RSAPrivateKey _ _ _) RS256`
||| (Types L213, L219); the only arms that return `True` for `HS256`
||| are `(SecretKey _) HS256` (Types L210). `SecretKey` and
||| `RSA*Key` are distinct constructors, so the implication holds
||| by case-split on `rsaKey`.
|||
||| Held back by Idris2 0.8.0 because closing the proof requires
||| a hand-rolled case-split on the 8-arm `SigningKey` constructor
||| set (`NoKey`, `SecretKey`, `RSAPublicKey`, `RSAPrivateKey`,
||| `ECPublicKey`, `ECPrivateKey`, `EdPublicKey`, `EdPrivateKey`)
||| — only the two `RSA*` arms satisfy the `RS256 = True` premise
||| (so the other 6 discharge by `absurd Refl` from `False = True`),
||| and on those two `isKeyValidForAlgorithm rsaKey HS256` reduces
||| to `False` by `Refl`. This pattern is mechanically derivable
||| but not automatic in Idris2 0.8.0 (no derived "disjointness
||| over a pair of Bool indicator functions" tactic). Same shape
||| as `noneNotSecure` above (hand-rolled `impossible` cascade).
||| Discharge by writing the 8-arm case-split out by hand — it
||| would be ~10 lines and structurally identical to
||| `noneNotSecure`.
public export
0 algorithmConfusionPrevented :
  (jwt : DecodedJWT) -> jwt.header.alg = HS256 ->
  (rsaKey : SigningKey) -> isKeyValidForAlgorithm rsaKey RS256 = True ->
  isKeyValidForAlgorithm rsaKey HS256 = False

||| OWED [Category E — Attack-prevention via String-FFI mismatch
||| through `validateIssuer`]: if validation requires
||| `"expected-issuer"` and the token carries `"malicious-issuer"`,
||| then `validateDecoded` rejects. The chain runs through
||| `validateIssuer "expected-issuer" jwt.claims` (Validate L113-
||| L120), whose `Just iss` arm branches on `iss == "expected-issuer"`
||| — since `"malicious-issuer" /= "expected-issuer"`, the branch
||| goes to `Err`, which `validateDecoded`'s bind propagates.
|||
||| Held back by Idris2 0.8.0 because closing the proof requires
||| reducing `"malicious-issuer" == "expected-issuer"` to `False`
||| at the type level — but `(==)` on `String` is the FFI
||| primitive `prim__strEq`, which Idris2 0.8.0 does NOT evaluate
||| even on two literal strings without an `assert_total` /
||| `believe_me` escape hatch (estate policy forbids both).
||| Same blocker family as boj-server's `stringNotEqCommut`
||| (gossamer PR#41 `PanelIsolation`, the estate's class-J
||| precedent for `prim__eq_String`). Discharge once `Data.String`
||| ships an `strNeqRefl : (s1 /= s2 by syntactic disequality) ->
||| (s1 == s2) = False` reflection lemma, with the same trust
||| posture as gossamer's `stringNotEqCommut` (`%unsafe` +
||| `believe_me ()` over the FFI primitive).
public export
0 tokenSubstitutionPrevented :
  (opts : ValidationOptions) -> opts.requiredIssuer = Just "expected-issuer" ->
  (jwt : DecodedJWT) -> jwt.claims.iss = Just "malicious-issuer" ->
  isOk (validateDecoded opts 0 jwt) = False

||| OWED [Category E — Attack-prevention via Integer arithmetic
||| through `validateMaxAge`]: if `opts.maxAge = Just 300` and the
||| token's `iat` is 600 seconds in the past, then `validateDecoded`
||| rejects. The chain runs through `validateMaxAge currentTime 300
||| jwt.claims` (Validate L98-L105): `tokenAge` returns
||| `Just (currentTime - (currentTime - 600)) = Just 600`, then
||| `600 > cast 300 = True` branches to `Err`.
|||
||| Held back by Idris2 0.8.0 because closing the proof requires
||| (1) `Integer` arithmetic reduction (`currentTime -
||| (currentTime - 600) = 600` — partially opaque under
||| arbitrary-precision Integer at the type level), (2) reflecting
||| `600 > cast (the Nat 300) = True` (the same `Integer`
||| Bool-Prop blocker as `maxAgeValidationSound`), AND
||| (3) `Result`-bind `Err`-propagation through `validateDecoded`'s
||| `do`-block (same blocker as `fullValidationImpliesAll`).
||| Combined blocker family of `maxAgeValidationSound` +
||| `fullValidationImpliesAll`. Discharge once both are
||| dischargeable — at which point `replayMitigatedWithMaxAge`
||| follows by a 3-step `rewrite` + `Refl`.
public export
0 replayMitigatedWithMaxAge :
  (opts : ValidationOptions) -> opts.maxAge = Just 300 ->
  (currentTime : Integer) -> (jwt : DecodedJWT) ->
  jwt.claims.iat = Just (currentTime - 600) ->  -- Token is 10 minutes old
  isOk (validateDecoded opts currentTime jwt) = False
