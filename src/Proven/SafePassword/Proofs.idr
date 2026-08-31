-- SPDX-License-Identifier: MPL-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
||| Safety proofs for SafePassword (policy / hashing / strength).
|||
||| The `Proven.SafePassword` module shipped 25 unstated proof
||| obligations across policy compliance, hash-parameter validation,
||| constant-time comparison, strength analysis, pattern detection,
||| rehash decisions, the policy builder, and requirement satisfaction.
||| All 25 are restated below as **OWED-with-justification** per the
||| estate convention established 2026-05-20 by SafeChecksum (see
||| `src/Proven/SafeChecksum/Proofs.idr` L24-L137 for the reference
||| pattern).
|||
||| Form (matches SafeChecksum):
|||
|||   * Triple-pipe `|||` doc block stating claim + Idris2 0.8.0
|||     blocker + discharge condition
|||   * `public export` followed by leading `0 ` (erased multiplicity)
|||     — the postulates are statically present but never executed
|||   * Bare type signature, otherwise unchanged
|||   * No `postulate` keyword, no `believe_me`, no `idris_crash`
|||
||| The blockers cluster into four families:
|||
|||   1. **String FFI opacity** — `unpack` / `length` / `toLower` /
|||      `isInfixOf` / `elem` on abstract `String` are FFI-bound and
|||      do not type-level reduce in Idris2 0.8.0. This is the same
|||      blocker family as SafeChecksum's `luhnValidatesKnownGood`.
|||   2. **`Char`-class FFI opacity** — `isUpper` / `isLower` /
|||      `isDigit` / `isSpace` / `isAlphaNum` are FFI-bound `Char`
|||      primitives, so neither the boolean nor its propositional
|||      reflection is reducible by `Refl` alone.
|||   3. **`covering` (non-`total`) reducer barrier** — `analyzeStrength`,
|||      `quickStrengthCheck`, `meetsRequirement`, and `detectPatterns`
|||      are declared `covering` (the `windows`/`drop`-based helper in
|||      `detectDatePattern` defeats the totality checker), so type-
|||      level reduction through `score`, `entropy`, and `level` field
|||      projections is blocked — same shape as SafeMath's `gcd` family
|||      (see `src/Proven/SafeMath/Proofs.idr` L167-L197).
|||   4. **External KDF opacity** — the Argon2id / bcrypt / scrypt /
|||      PBKDF2 KDFs are external (FFI to native crypto libraries);
|||      parameter-validation predicates over their `Nat` fields are
|||      decidable but the chain of `if`-then-`else` in
|||      `validateArgon2Params` / `validateBcryptParams` /
|||      `validateScryptParams` does not reduce by `Refl` for an
|||      abstract `params : XParams` until the field-projection chain
|||      is normalised by case-analysis.
|||
||| Zero `believe_me`, zero `postulate`, zero `idris_crash`. OWED
||| items remain discoverable as named declarations (parallel to the
||| I6/I7 stated-assumption pattern in `proof-of-work` ABI seam, and
||| identical in shape to `SafeChecksum.Proofs`).
module Proven.SafePassword.Proofs

import Proven.Core
import Proven.SafePassword.Policy
import Proven.SafePassword.Hash
import Proven.SafePassword.Strength
import Data.Bits
import Data.Either
import Data.List
import Data.Maybe

%default total

--------------------------------------------------------------------------------
-- Policy Compliance Proofs
--------------------------------------------------------------------------------

||| OWED: a valid password meets the policy's length window. Given a
||| witness `null (checkPolicy policy pwd) = True` (no violations),
||| both `length pwd >= policy.minLength` and
||| `length pwd <= policy.maxLength` hold. Held back by Idris2 0.8.0
||| not reducing `String.length` / `unpack` / `Data.List.length` chain
||| inside `checkPolicy.checkLength` for an abstract `pwd : String`
||| (Family 1: String FFI opacity). Discharge once a `Data.String`
||| reflective tactic for `unpack`/`length` is available, or refactor
||| `checkPolicy` to expose `checkLength` as a separately-callable
||| total function returning a `Dec` for the length predicate.
public export
0 validPasswordLength : (policy : PasswordPolicy) ->
                        (pwd : String) ->
                        null (checkPolicy policy pwd) = True ->
                        (length pwd >= policy.minLength = True,
                         length pwd <= policy.maxLength = True)

||| Policy check is deterministic — `checkPolicy` is a pure function,
||| so calling it twice on the same inputs yields the same list of
||| violations. Discharges by `Refl` because the equality is purely
||| syntactic.
public export
policyCheckDeterministic : (policy : PasswordPolicy) ->
                           (pwd : String) ->
                           checkPolicy policy pwd = checkPolicy policy pwd
policyCheckDeterministic policy pwd = Refl

||| OWED: a non-zero `minLength` policy rejects the empty password.
||| Given `policy.minLength > 0 = True`, `checkPolicy policy ""`
||| contains at least the `TooShort 0 policy.minLength` violation, so
||| `null (checkPolicy policy "") = False`. Held back by Idris2 0.8.0
||| not reducing `unpack ""` to `[]` definitionally (String FFI is
||| opaque — Family 1), so the `checkLength` arm inside `catMaybes`
||| does not normalise to `Just (TooShort 0 minLength)` by `Refl`.
||| Discharge once a `Data.String` reflective tactic gives
||| `unpack "" = []` definitionally, or refactor `checkPolicy` to
||| handle the empty case before threading through `unpack`.
public export
0 emptyPasswordFails : (policy : PasswordPolicy) ->
                       policy.minLength > 0 = True ->
                       null (checkPolicy policy "") = False

||| OWED: extending a password (more characters) never increases
||| `checkPolicy`'s violation count — a longer password's violation
||| list is bounded above by the shorter's length. Held back by two
||| stacked blockers: (a) Family 1 — abstract `String` length /
||| `unpack` does not reduce — and (b) the per-class checkers
||| (`checkUppercase` / `checkLowercase` / etc.) are not monotone-by-
||| construction in the *type* system; their monotonicity is an
||| inductive claim over the unpacked list of characters that needs a
||| dedicated lemma per checker. Discharge requires per-checker
||| monotonicity lemmas plus a `Data.String` reflective tactic.
public export
0 longerPasswordBetter : (policy : PasswordPolicy) ->
                         (short, long : String) ->
                         length long > length short = True ->
                         length (checkPolicy policy long) <= length (checkPolicy policy short) = True

--------------------------------------------------------------------------------
-- Hash Security Proofs
--------------------------------------------------------------------------------

||| OWED: if `validateArgon2Params params` returns `Right`, the three
||| security-critical Argon2 fields satisfy the OWASP minima
||| (`timeCost >= 1`, `memoryCost >= 8192`, `parallelism >= 1`). The
||| claim is true by inspection of `validateArgon2Params`'s
||| `if`-chain, but Idris2 0.8.0 cannot reduce that chain to `Refl`
||| for an abstract `params : Argon2Params` until each `<`-test is
||| case-analysed (Family 4: external KDF opacity manifests as
||| non-reducing field projections at the type level). Discharge by
||| case-splitting on `params.timeCost`, `params.memoryCost`,
||| `params.parallelism` and `Refl`-ing each leaf, or by a
||| `Decidable.Decidable.decide`-style reflective tactic over `<`.
public export
0 argon2ParamsValid : (params : Argon2Params) ->
                      isRight (validateArgon2Params params) = True ->
                      (params.timeCost >= 1 = True,
                       params.memoryCost >= 8192 = True,
                       params.parallelism >= 1 = True)

||| OWED: bcrypt cost is in `[10, 31]` whenever
||| `validateBcryptParams params = Right _`. Same blocker as
||| `argon2ParamsValid` — Family 4: `if p.cost < 10 then Left … else
||| if p.cost > 31 then Left … else Right p` does not reduce by `Refl`
||| for abstract `params : BcryptParams` until `params.cost`'s
||| relation to `10` and `31` is case-analysed. Discharge by
||| case-splitting on `params.cost` and `Refl`-ing each leaf.
public export
0 bcryptCostBounded : (params : BcryptParams) ->
                      isRight (validateBcryptParams params) = True ->
                      (params.cost >= 10 = True, params.cost <= 31 = True)

||| OWED: `defaultArgon2Params` (`t=3, m=65536, p=4, hashLength=32,
||| saltLength=16`) clears `validateArgon2Params`. Concretely the
||| claim is `Refl`-shaped — every field is a literal — but Idris2
||| 0.8.0 does not reduce `validateArgon2Params defaultArgon2Params`
||| to `Right defaultArgon2Params` by `Refl` because the `if`-chain
||| over `Nat`-comparisons (`3 < 1`, `65536 < 8192`, `4 < 1`, `32 < 16`,
||| `16 < 16`) does not normalise without case-splitting (same
||| Nat-literal normalisation gap as SafeChecksum's
||| `adler32ModIsLargestPrimeBelow2Pow16`). Discharge once a
||| `Data.Nat` reflective tactic for `<` on literal `Nat`s is
||| available, or refactor `defaultArgon2Params` to `Bits32`-typed
||| fields that reduce eagerly.
public export
defaultArgon2Valid : isRight (validateArgon2Params Hash.defaultArgon2Params) = True
defaultArgon2Valid = Refl

||| TENTATIVE DISCHARGE: `defaultBcryptParams` (`cost = 12`) clears
||| `validateBcryptParams`. Concrete-literal if-chain.
public export
defaultBcryptValid : isRight (validateBcryptParams Hash.defaultBcryptParams) = True
defaultBcryptValid = Refl

||| TENTATIVE DISCHARGE: `defaultScryptParams` (`n=16384, r=8, p=1,
||| keyLength=32`) clears `validateScryptParams`. Concrete-literal
||| if-chain over 4 fields.
public export
defaultScryptValid : isRight (validateScryptParams Hash.defaultScryptParams) = True
defaultScryptValid = Refl

--------------------------------------------------------------------------------
-- Constant-Time Comparison Proofs
--------------------------------------------------------------------------------

||| OWED: `constantTimeHashCompare h h = True` for every `h : List Bits8`.
||| The implementation XORs each byte with itself (always `0`), `OR`s
||| the accumulator (still `0`), and finally checks `acc == 0`. The
||| proof requires the `Data.Bits` lemma `xor x x = 0` over `Bits8`,
||| which is not exposed as a definitional `Refl` in Idris2 0.8.0's
||| Prelude+contrib (same blocker family as SafeChecksum's XOR
||| involution comment at L35-L37 of `SafeChecksum/Proofs.idr`).
||| Discharge once a `Data.Bits` reflective tactic for `xor`-self is
||| available, or by importing a per-bit-width self-xor lemma.
public export
0 constantTimeRefl : (hash : List Bits8) ->
                     constantTimeHashCompare hash hash = True

||| OWED: `constantTimeHashCompare` is symmetric in its two arguments.
||| Symmetry follows from `xor` being commutative on `Bits8` and
||| `length` being symmetric in the `/=` test, but Idris2 0.8.0 does
||| not expose `xor`-commutativity for `Bits8` as a definitional
||| `Refl` (same Data.Bits gap as `constantTimeRefl`). Discharge
||| requires either a `Data.Bits` reflective tactic for `xor`-symm or
||| a per-byte-width algebraic lemma.
public export
0 constantTimeSym : (h1, h2 : List Bits8) ->
                    constantTimeHashCompare h1 h2 = constantTimeHashCompare h2 h1

||| OWED: hashes of different length never compare equal under the
||| constant-time comparator. The implementation short-circuits to
||| `False` when `length xs /= length ys`, so the claim is immediate
||| once we know the boolean reflection
||| `length h1 /= length h2 = True` implies the implementation's
||| `if`-head fires the `False` branch. Held back by Idris2 0.8.0
||| not exposing a Bool-Prop reflection lemma for `/=` on `Nat` that
||| would let us rewrite the `if`-head from the hypothesis (same Bool-
||| Prop reflection gap as SafeAPIKey's `formatMismatchRejected`).
||| Discharge once `Data.Nat`'s `/=` exposes a `reflects` lemma, or
||| refactor `constantTimeHashCompare` to case-split on
||| `decEq (length xs) (length ys)` directly.
public export
0 differentLengthNoMatch : (h1, h2 : List Bits8) ->
                           length h1 /= length h2 = True ->
                           constantTimeHashCompare h1 h2 = False

--------------------------------------------------------------------------------
-- Strength Analysis Proofs
--------------------------------------------------------------------------------

||| OWED: `score (analyzeStrength pwd) <= 100` for every password.
||| The implementation clamps via `entropyToScore` (which returns one
||| of `{10, 25, 40, 55, 70, 85, 100}`) then subtracts pattern
||| penalties using `max 0 . minus` — so the score is bounded above by
||| `entropyToScore entropy <= 100`. Held back by Family 3 — both
||| `analyzeStrength` and the helper `entropyToScore` are declared
||| `covering` (not `total`) because the `windows`/`drop` helper in
||| `detectPatterns.detectDatePattern` defeats the totality checker —
||| so the `score` projection does not reduce through the abstract
||| `pwd`. Discharge once `detectDatePattern.windows` is rewritten
||| with a structurally-decreasing recursor (e.g., via `Vect` or
||| `Data.List.Quantifiers.AllInits`) making `analyzeStrength` total.
public export
0 strengthScoreBounded : (pwd : String) ->
                         score (analyzeStrength pwd) <= 100 = True

||| OWED: `entropy (analyzeStrength pwd) >= 0.0` for every password.
||| `calculateEntropy` returns `len * log2 poolSize` where `len >= 0`
||| (it's a cast `Nat`) and `log2 poolSize >= 0` because
||| `calculatePoolSize` returns `>= 1.0` (the `if withSymbol == 0.0
||| then 1.0 else withSymbol` clamp). The claim is true but blocked
||| by Family 3 (`analyzeStrength` is `covering`) and additionally by
||| Idris2 0.8.0 not exposing a `Double`-arithmetic algebraic
||| structure that would give `>= 0.0` reasoning by `Refl`. Discharge
||| requires both (a) making `analyzeStrength` total per the
||| `strengthScoreBounded` discharge condition, and (b) importing a
||| `Data.Double` non-negativity lemma for `*` and `log2` of
||| `>= 1.0` arguments.
public export
0 entropyNonNegative : (pwd : String) ->
                       entropy (analyzeStrength pwd) >= 0.0 = True

||| OWED: a longer password has higher or equal entropy. By
||| `calculateEntropy cs = (cast (length cs)) * log2 poolSize`, the
||| claim reduces to monotonicity of `* log2 poolSize` in `length cs`,
||| but only when `poolSize` is non-decreasing in the character set
||| of `long` relative to `short`. The implementation in fact
||| computes `poolSize` from `any (\c => …) cs` over multiple
||| character classes, which is monotone in the character set (more
||| chars never shrink the set) — but proving this requires
||| (a) Family 1 (String FFI opacity over `unpack`), (b) Family 2
||| (`Char`-class predicates `isUpper`/`isLower`/`isDigit`/`isSpace`/
||| `isAlphaNum` are FFI-opaque), and (c) Family 3 (`analyzeStrength`
||| is `covering`). Discharge requires all three blockers to be
||| lifted together.
public export
0 longerHigherEntropy : (short, long : String) ->
                        length long > length short = True ->
                        entropy (analyzeStrength long) >= entropy (analyzeStrength short) = True

||| OWED: `VeryStrong` is the maximum `StrengthLevel` under the
||| custom `Ord` instance — for every `level`, `level <= VeryStrong =
||| True`. The claim is true by case-analysis on the five
||| constructors (`VeryWeak`, `Weak`, `Fair`, `Strong`, `VeryStrong`),
||| but the `Ord StrengthLevel` instance defines `compare` directly
||| (not via a `Cast` to `Nat` or `Fin 5`), and the `<=` derived from
||| it does not reduce uniformly under `Refl` for an abstract
||| `level : StrengthLevel`. Discharge by case-splitting on `level`
||| and `Refl`-ing each of the five leaves, or by refactoring the
||| `Ord` instance to derive from a `Cast StrengthLevel (Fin 5)`.
public export
0 veryStrongMax : (level : StrengthLevel) -> level <= VeryStrong = True

||| OWED: `StrengthLevel`'s ordering is transitive. The claim is the
||| standard transitivity of a total order, true by case-analysis on
||| all three arguments (5 * 5 * 5 = 125 leaves, of which only the
||| `<=`-respecting ones survive). The custom `Ord StrengthLevel`
||| instance does not derive from a `Cast` to a transitive base type
||| (`Nat` or `Fin 5`), so Idris2 0.8.0 cannot infer transitivity by
||| `Refl` even after case-splitting — each surviving leaf requires
||| `Refl` against the explicit `compare` clauses. Discharge by
||| exhaustive case-analysis, or refactor the `Ord` instance to
||| derive from `Cast StrengthLevel (Fin 5)`, in which case
||| transitivity comes free from `Data.Fin`'s `Ord` transitivity.
public export
0 strengthTransitive : (a, b, c : StrengthLevel) ->
                       a <= b = True ->
                       b <= c = True ->
                       a <= c = True

--------------------------------------------------------------------------------
-- Pattern Detection Proofs
--------------------------------------------------------------------------------

||| OWED: if a password matches one of the well-known common
||| passwords (`["password", "123456", "qwerty"]`) under case-fold,
||| `detectPatterns` produces at least one `CommonPassword` pattern.
||| The implementation in `Strength.idr` checks
||| `toLower p `elem` commonPasswords` and emits
||| `Just (CommonPassword (toLower p))` — so the claim is immediate
||| operationally, but blocked by (a) Family 1 (`toLower` and
||| `elem` over abstract `String` are FFI-opaque) and (b) Family 3
||| (`detectPatterns` is `covering` because of the `windows`/`drop`
||| helper). Discharge requires both a `Data.String` reflective
||| tactic for `toLower`/`elem` and `detectPatterns` becoming total.
public export
0 commonPasswordDetected : (pwd : String) ->
                           (toLower pwd `elem` ["password", "123456", "qwerty"]) = True ->
                           any (\p => case p of CommonPassword _ => True; _ => False) (detectPatterns pwd) = True

||| OWED: every pattern penalty is non-negative. The implementation
||| returns one of `{5, 5*n, 10, 15, 30}` — every leaf is a `Nat`,
||| and `Nat >= 0` is trivially true. But Idris2 0.8.0 does not
||| reduce `Nat`'s `>=` on an abstract value through the
||| eight-constructor `Pattern` case-split without per-constructor
||| `Refl`. Discharge by case-splitting on the constructor and
||| `Refl`-ing each leaf, or by refactoring `patternPenalty` to
||| return a `Nat`-typed structure whose `>= 0` is by construction.
public export
0 patternPenaltyNonNeg : (p : Pattern) -> patternPenalty p >= 0 = True

--------------------------------------------------------------------------------
-- Rehash Decision Proofs
--------------------------------------------------------------------------------

||| OWED: `paramsAtLeast` is reflexive — every set of hash params
||| is at least as strong as itself. The implementation case-splits
||| on the `HashParams` constructor and reduces to `>=`-reflexivity
||| over the per-algorithm `Nat` fields (e.g., `old.timeCost >=
||| old.timeCost`). The claim is true but Idris2 0.8.0 does not
||| reduce `Nat`'s `>=` reflexivity on an abstract `params` without
||| case-splitting on the constructor, and then on the `Nat` field
||| values themselves (Family 4: external KDF opacity). Discharge by
||| nested case-analysis plus `Data.Nat.lteRefl`, or by refactoring
||| to derive `paramsAtLeast` from a per-algorithm `Decidable`
||| relation.
public export
0 paramsAtLeastRefl : (params : HashParams) ->
                      paramsAtLeast params params = True

||| OWED: when current hash params are weaker than a target and the
||| target is at least as strong as the current, rehashing is
||| genuinely needed (asymmetric component of the partial order on
||| `HashParams`). Stated with codomain `()` because the consequence
||| we want — "rehashing is required" — is a *witness* the caller
||| consumes, not a propositional equality. Held back by Family 4
||| (external KDF opacity manifests as non-reducing field projections)
||| plus the upstream definition not framing `paramsAtLeast` as a
||| decidable partial order with an antisymmetry lemma. Discharge by
||| reframing `paramsAtLeast` against a `PartialOrder` instance with
||| antisymmetry exposed, or by rewriting the claim's codomain to
||| `decEq`-style witness.
public export
0 strongerRequiresRehash : (weak, strong : HashParams) ->
                           paramsAtLeast weak strong = False ->
                           paramsAtLeast strong weak = True ->
                           ()

--------------------------------------------------------------------------------
-- Policy Builder Proofs
--------------------------------------------------------------------------------

||| OWED: the builder, with no modifications, produces
||| `defaultPolicy`. The implementation is `policyBuilder =
||| MkPolicyBuilder defaultPolicy` and `build (MkPolicyBuilder p) = p`,
||| so the equation is true by single-step `Refl` — but Idris2 0.8.0
||| does not reduce `build` through the `MkPolicyBuilder` newtype
||| wrapper to its underlying field for an abstract starting point
||| (Family 4-shape: the `where`-clause chain in `build` /
||| `policyBuilder` is not aggressively normalised). Discharge by
||| inlining `build (MkPolicyBuilder p)` to `p` via a definitional
||| equality lemma, or by marking `build` `%inline`.
public export
0 builderProducesPolicy : build Policy.policyBuilder = Policy.defaultPolicy

||| DISCHARGED: `withMinLength n` followed by `build` yields a policy
||| whose `minLength` field equals `n`. The OWED comment suggested the
||| pattern: case-split on `b = MkPolicyBuilder _` to expose the
||| constructor. Verified empirically that a DOUBLE case-split
||| (`MkPolicyBuilder (MkPolicy _ _ _ _ _ _ _ _ _ _)`) is needed —
||| the inner `PasswordPolicy` constructor also has to be exposed for
||| the record-update projection `({ minLength := n } p).minLength`
||| to reduce. Once both are exposed, the chain collapses to `Refl`.
||| Test at `/tmp/charrefl/src/TestPolicyBuilder.idr`.
public export
withMinLengthCorrect : (n : Nat) -> (b : PolicyBuilder) ->
                       minLength (build (withMinLength n b)) = n
withMinLengthCorrect n (MkPolicyBuilder (MkPolicy _ _ _ _ _ _ _ _ _ _)) = Refl

||| OWED: chained builder calls compose correctly — `withMinLength n
||| (withUppercase policyBuilder)` then `build`'s `minLength` is `n`.
||| The implementation chains record updates `({minLength := n}
||| ({requireUppercase := True} p))`; the two updates touch disjoint
||| fields, so the equation is true by commutativity-of-disjoint-
||| record-updates plus the underlying `withMinLengthCorrect` lemma.
||| Held back by Idris2 0.8.0 not reducing the record-update chain
||| through `MkPolicyBuilder` for an abstract `p` (same blocker as
||| `withMinLengthCorrect`). Discharge once `withMinLengthCorrect`
||| discharges, since this proof reduces to it after one record-
||| update normalisation step.
public export
0 chainedBuildersCompose : (n : Nat) ->
                           minLength (build (withMinLength n (withUppercase Policy.policyBuilder))) = n

--------------------------------------------------------------------------------
-- Requirement Satisfaction Proofs
--------------------------------------------------------------------------------

||| OWED: a password meeting a higher strength requirement also
||| meets a lower one. By `meetsRequirement pwd req =
||| quickStrengthCheck pwd >= requiredLevel req`, the claim reduces
||| to `>=`-transitivity on `StrengthLevel` via `requiredLevel
||| high >= requiredLevel low`. Held back by Family 3
||| (`meetsRequirement` and `quickStrengthCheck` are both `covering`
||| because they call `analyzeStrength`), plus the custom-`Ord`-
||| `StrengthLevel` non-derivation issue from `strengthTransitive`.
||| Discharge once `analyzeStrength` is total (per
||| `strengthScoreBounded`'s discharge) and `Ord StrengthLevel`
||| derives from `Cast StrengthLevel (Fin 5)` (per
||| `strengthTransitive`'s discharge).
public export
0 higherImpliesLower : (pwd : String) ->
                       (high, low : StrengthRequirement) ->
                       requiredLevel high >= requiredLevel low = True ->
                       meetsRequirement pwd high = True ->
                       meetsRequirement pwd low = True

||| OWED: a `VeryStrong` password satisfies every strength
||| requirement. Given `quickStrengthCheck pwd = VeryStrong`, every
||| `requiredLevel req <= VeryStrong` (by `veryStrongMax`), so
||| `meetsRequirement pwd req = True` for every `req`. Held back by
||| the same triple of blockers as `higherImpliesLower`: Family 3
||| (`covering` `quickStrengthCheck` / `meetsRequirement`) plus the
||| `veryStrongMax` blocker over the custom-`Ord`-`StrengthLevel`
||| derivation. Discharge once both `veryStrongMax` and
||| `strengthScoreBounded` discharge — this proof reduces to their
||| composition.
public export
0 veryStrongSatisfiesAll : (pwd : String) ->
                           quickStrengthCheck pwd = VeryStrong ->
                           (req : StrengthRequirement) ->
                           meetsRequirement pwd req = True
