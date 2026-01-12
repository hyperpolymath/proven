# Proof Documentation

This document explains the formal proofs in the `proven` library. Each module contains machine-checked proofs that guarantee specific safety properties.

## Table of Contents

- [Understanding Dependent Types](#understanding-dependent-types)
- [How to Read Idris 2 Proofs](#how-to-read-idris-2-proofs)
- [The `believe_me` Escape Hatch](#the-believe_me-escape-hatch)
- [Module Proofs](#module-proofs)
  - [SafeMath](#safemath)
  - [SafeString](#safestring)
  - [SafeJson](#safejson)
  - [SafeUrl](#safeurl)
  - [SafeEmail](#safeemail)
  - [SafePath](#safepath)
  - [SafeCrypto](#safecrypto)
  - [SafePassword](#safepassword)
  - [SafeDateTime](#safedatetime)
  - [SafeNetwork](#safenetwork)
- [Proof Categories](#proof-categories)
- [Contributing New Proofs](#contributing-new-proofs)

---

## Understanding Dependent Types

In most programming languages, types and values are separate. In Idris 2, types can **depend on values**:

```idris
-- A vector with its length encoded in the type
data Vect : Nat -> Type -> Type where
  Nil  : Vect 0 a
  (::) : a -> Vect n a -> Vect (S n) a
```

This means `Vect 3 Int` and `Vect 5 Int` are *different types*. The compiler enforces length constraints at compile time.

### What This Gives Us

| Traditional Types | Dependent Types |
|-------------------|-----------------|
| `List Int` (unknown length) | `Vect n Int` (length is `n`) |
| Runtime crash on empty list | Compile error if proof of non-empty missing |
| "I hope this doesn't overflow" | "The compiler proved no overflow" |

---

## How to Read Idris 2 Proofs

A proof in Idris 2 is just a function that constructs a value of a "proposition type."

### Example: Commutativity of Addition

```idris
plusCommutative : (a, b : Nat) -> a + b = b + a
plusCommutative Z b = rewrite plusZeroRightNeutral b in Refl
plusCommutative (S a) b =
  rewrite plusCommutative a b in
  rewrite plusSuccRightSucc b a in
  Refl
```

Breaking this down:

1. **Type signature**: `(a, b : Nat) -> a + b = b + a`
   - "For any natural numbers `a` and `b`, `a + b` equals `b + a`"
   - The `=` is a *type* representing equality

2. **Base case**: `plusCommutative Z b`
   - When `a = 0`, we need `0 + b = b + 0`
   - `0 + b` reduces to `b` by definition
   - We rewrite using `plusZeroRightNeutral` to show `b + 0 = b`

3. **Inductive case**: `plusCommutative (S a) b`
   - When `a = S a'`, we assume the property holds for `a'`
   - Chain rewrites to transform `S a + b` into `b + S a`

4. **`Refl`**: The reflexivity proof - "these things are definitionally equal"

---

## The `believe_me` Escape Hatch

You'll notice many proofs use `believe_me Refl`. This is an escape hatch that tells the compiler "trust me, this is true."

### Why We Use It

1. **String operations**: Idris 2's string primitives are built-in and don't expose their structure for proofs
2. **FFI boundaries**: Operations that call external code can't be proven internally
3. **Complex recursion**: Some proofs would require more sophisticated proof techniques

### What It Means

```idris
-- This proof is NOT machine-checked
trimNoWhitespace : (s : String) -> ... -> trim s = s
trimNoWhitespace s prf = believe_me Refl

-- This proof IS machine-checked
plusCommutative : (a, b : Nat) -> a + b = b + a
plusCommutative Z b = rewrite plusZeroRightNeutral b in Refl
-- ... actual proof construction
```

### Our Policy

- **Core mathematical properties**: Fully machine-checked (no `believe_me`)
- **String/parsing properties**: Use `believe_me` with extensive testing
- **Security invariants**: Documented assumptions, validated by fuzzing

---

## Module Proofs

### SafeMath

Location: `src/Proven/SafeMath/Proofs.idr`

#### Fully Machine-Checked Proofs

| Proof | Statement | Guarantee |
|-------|-----------|-----------|
| `plusCommutative` | `a + b = b + a` | Addition order doesn't matter |
| `plusAssociative` | `(a + b) + c = a + (b + c)` | Grouping doesn't matter |
| `plusZeroRight` | `n + 0 = n` | Zero is right identity |
| `plusZeroLeft` | `0 + n = n` | Zero is left identity |
| `multCommutative` | `a * b = b * a` | Multiplication order doesn't matter |
| `multDistributesLeft` | `a * (b + c) = (a * b) + (a * c)` | Distributive law |
| `multOneLeft` | `1 * n = n` | One is left identity |
| `multOneRight` | `n * 1 = n` | One is right identity |
| `minusZeroRight` | `n - 0 = n` | Subtracting zero is identity |
| `minusSelf` | `n - n = 0` | Self-subtraction is zero |
| `minusPlusCancel` | `(a - b) + b = a` (when `b <= a`) | Subtraction/addition cancel |
| `lteRefl` | `n <= n` | Every number is <= itself |
| `lteTrans` | `a <= b` and `b <= c` implies `a <= c` | Transitivity |
| `lteAntisym` | `a <= b` and `b <= a` implies `a = b` | Antisymmetry |

#### Division Properties

| Proof | Statement |
|-------|-----------|
| `divByOne` | `n / 1 = n` |
| `modLtDivisor` | `n mod d < d` (for `d > 0`) |

**Security implication**: Safe division in `proven` returns `Maybe` or `Either`, eliminating division-by-zero crashes.

---

### SafeString

Location: `src/Proven/SafeString/Proofs.idr`

#### Length Properties

| Proof | Statement |
|-------|-----------|
| `emptyStringLength` | `length "" = 0` |
| `concatLength` | `length (s1 ++ s2) = length s1 + length s2` |

#### Injection Safety (Critical Security Proofs)

| Proof | Statement | Security Guarantee |
|-------|-----------|-------------------|
| `escapeSQLSafe` | SQL-escaped string has no unescaped quotes | **SQL injection prevention** |
| `escapeHTMLSafe'` | HTML-escaped string has no raw `<` or `>` | **XSS prevention** |

These proofs use dependent types to encode security invariants:

```idris
-- A type that proves a string has no unescaped quotes
data NoUnescapedQuotes : String -> Type where
  MkNoUnescapedQuotes : (s : String) ->
                        (prf : all (\c => c /= '\'') (unpack s) = True) ->
                        NoUnescapedQuotes s

-- The escaping function returns a value of this type
escapeSQLSafe : (s : String) -> NoUnescapedQuotes (escapeSQL s)
```

---

### SafeJson

Location: `src/Proven/SafeJson/Proofs.idr`

#### Type Exclusivity

| Proof | Statement |
|-------|-----------|
| `nullNotBool` | If `isNull v`, then `not (isBool v)` |
| `nullNotNumber` | If `isNull v`, then `not (isNumber v)` |
| `nullNotString` | If `isNull v`, then `not (isString v)` |
| `arrayNotObject` | If `isArray v`, then `not (isObject v)` |

**Why this matters**: These proofs guarantee that JSON type checking is sound - a value can't be misidentified.

#### Extraction Properties

| Proof | Statement |
|-------|-----------|
| `asBoolFromBool` | `asBool (JsonBool b) = Just b` |
| `asNumberFromNumber` | `asNumber (JsonNumber n) = Just n` |
| `asStringFromString` | `asString (JsonString s) = Just s` |

**Guarantee**: Extracting a value from its correct type always succeeds.

#### Object/Array Access

| Proof | Statement |
|-------|-----------|
| `setGetIdentity` | After `set k v obj`, `get k obj = Just v` |
| `setPreservesOther` | Setting key `k1` doesn't affect key `k2 != k1` |
| `removeNotHasKey` | After `remove k`, `hasKey k = False` |
| `prependGetZero` | After `prepend v arr`, `at 0 = Just v` |
| `emptyPathIdentity` | `getPath [] v = Just v` |

#### Parsing Properties

| Proof | Statement |
|-------|-----------|
| `parseNullCorrect` | `parseJson "null" = Just JsonNull` |
| `parseTrueCorrect` | `parseJson "true" = Just (JsonBool True)` |
| `parseFalseCorrect` | `parseJson "false" = Just (JsonBool False)` |
| `parseEmptyFails` | `parseJson "" = Nothing` |

#### Well-Formedness

```idris
-- All JSON values we construct are well-formed
data WellFormedJson : JsonValue -> Type

constructedWellFormed : (v : JsonValue) -> WellFormedJson v
```

---

### SafeUrl

Location: `src/Proven/SafeUrl/Proofs.idr`

#### Encoding/Decoding

| Proof | Statement |
|-------|-----------|
| `encodeEmptyEmpty` | `urlEncode "" = ""` |
| `decodeEmptySucceeds` | `urlDecode "" = Just ""` |
| `encodeDecodeIdentity` | `urlDecode (urlEncode s) = Just s` |
| `encodePreservesAlphaNum` | Alphanumeric chars are unchanged |

#### Query String Properties

| Proof | Statement |
|-------|-----------|
| `parseEmptyQuery` | `parseQueryString "" = []` |
| `emptyBuilderEmpty` | `buildQueryString emptyQuery = ""` |
| `addParamIncreasesCount` | Adding a param increases count by 1 |
| `setGetIdentity` | `getParam k (setParam k v qs) = Just v` |
| `removeHasNot` | `hasParam k (removeAllParams k qs) = False` |
| `mergeEmptyRight` | `merge qs [] = qs` |
| `appendAssociative` | Appending query strings is associative |

#### Security Properties

```idris
-- URLs with javascript: scheme are unsafe
data SafeURL : ParsedURL -> Type where
  MkSafeURL : (url : ParsedURL) ->
              Not (url.scheme = Just (Custom "javascript")) ->
              SafeURL url

-- Validate and get proof of safety
validateSafe : (url : ParsedURL) -> Maybe (SafeURL url)
```

**Blocked schemes**: `javascript:`, `data:`, `vbscript:`

---

### SafeEmail

Location: `src/Proven/SafeEmail/Proofs.idr`

#### Parsing Properties

| Proof | Statement |
|-------|-----------|
| `parseEmptyFails` | `parseEmail "" = Nothing` |
| `parseDeterministic` | Parsing same string gives same result |
| `parseNoAtFails` | String without `@` fails to parse |

#### Validation Properties

| Proof | Statement |
|-------|-----------|
| `validResultIsValid` | A valid result has `isValid = True` |
| `errorMakesInvalid` | Adding an error makes result invalid |
| `warningKeepsValid` | Adding a warning keeps result valid |
| `combineValidValid` | Combining two valid results is valid |

#### Structural Properties

```idris
-- Parsed email always contains @
data ContainsAt : String -> Type

-- Length bounds
data ValidLocalLength : String -> Type   -- <= 64 chars
data ValidDomainLength : String -> Type  -- <= 253 chars
data ValidTotalLength : String -> Type   -- <= 254 chars
```

---

### SafePath

Location: `src/Proven/SafePath/Proofs.idr`

#### Normalization

| Proof | Statement |
|-------|-----------|
| `normalizeIdempotent` | `normalize (normalize p) = normalize p` |
| `normalizeRemovesEmpty` | No empty segments after normalization |
| `normalizeRemovesDot` | No `.` segments after normalization |
| `normalizeAbsNoLeadingDotDot` | Absolute paths have no leading `..` |

#### Path Traversal Prevention (Critical Security)

```idris
-- A path that provably cannot escape its base
data NoEscape : (base : String) -> (combined : String) -> Type

-- Safe join guarantees no escape
safeJoinNoEscape : (base, rel : String) ->
                   (result : String ** safeJoinPaths base rel = Just result) ->
                   NoEscape base result

-- Path without directory traversal
data NoTraversal : String -> Type
```

| Proof | Statement |
|-------|-----------|
| `safeJoinNoEscape` | Safe join result is always under base |
| `sanitizedIsSafe` | Sanitized segment is safe |
| `containedInBase` | ContainedPath is always within its base |
| `sanitizedNoTraversal` | Sanitized path has no `..` |

#### Comparison Properties

| Proof | Statement |
|-------|-----------|
| `pathEqRefl` | Path equality is reflexive |
| `pathEqSym` | Path equality is symmetric |
| `parentIsAncestor` | If `isParentOf a b`, then `isAncestorOf a b` |
| `ancestorTransitive` | Ancestor relation is transitive |

#### Glob Matching

| Proof | Statement |
|-------|-----------|
| `emptyMatchesEmpty` | `matchGlob "" "" = True` |
| `starMatchesAll` | `matchGlob "*" s = True` for any `s` |
| `questionMatchesSingle` | `matchGlob "?" c = True` for single char `c` |
| `literalMatchesSelf` | Literal pattern matches itself |

---

### SafeCrypto

Location: `src/Proven/SafeCrypto/Proofs.idr`

#### Hash Output Sizes

| Proof | Statement |
|-------|-----------|
| `sha256OutputSize` | SHA-256 produces exactly 32 bytes |
| `sha512OutputSize` | SHA-512 produces exactly 64 bytes |
| `sha3_256OutputSize` | SHA3-256 produces exactly 32 bytes |
| `blake3OutputSize` | BLAKE3 produces exactly 32 bytes |

#### Security Levels

| Proof | Statement |
|-------|-----------|
| `sha256Secure` | SHA-256 is marked secure |
| `sha512Secure` | SHA-512 is marked secure |
| `sha3_256Secure` | SHA3-256 is marked secure |
| `md5NotSecure` | MD5 is marked insecure |
| `sha1NotSecure` | SHA-1 is marked insecure |
| `modernIsSecure` | All "Modern" level algorithms are secure |

#### Constant-Time Comparison

| Proof | Statement |
|-------|-----------|
| `constantTimeRefl` | Constant-time compare is reflexive |
| `constantTimeSym` | Constant-time compare is symmetric |
| `digestEqRefl` | Digest equality is reflexive |
| `digestEqSym` | Digest equality is symmetric |

**Why constant-time matters**: Variable-time comparison leaks information through timing side-channels.

#### Random Generation

| Proof | Statement |
|-------|-----------|
| `randomBytesLength` | `randomBytes n` produces exactly `n` bytes |
| `randomNatBounded` | `randomNat max` produces value `< max` |
| `randomRangeBounded` | `randomNatRange min max` produces value in `[min, max]` |
| `uuidLength` | UUID v4 has exactly 36 characters |

---

### SafePassword

Location: `src/Proven/SafePassword/Proofs.idr`

#### Policy Compliance

| Proof | Statement |
|-------|-----------|
| `validPasswordLength` | Valid password meets length requirements |
| `emptyPasswordFails` | Empty password fails non-zero minLength |
| `longerPasswordBetter` | Longer passwords have fewer violations |

#### Hash Security

| Proof | Statement |
|-------|-----------|
| `argon2ParamsValid` | Valid Argon2 params meet minimum requirements |
| `bcryptCostBounded` | Valid bcrypt cost is in range [10, 31] |
| `defaultArgon2Valid` | Default Argon2 params are valid |
| `defaultBcryptValid` | Default bcrypt params are valid |
| `defaultScryptValid` | Default scrypt params are valid |

#### Strength Analysis

| Proof | Statement |
|-------|-----------|
| `strengthScoreBounded` | Strength score is always <= 100 |
| `entropyNonNegative` | Entropy is always >= 0 |
| `longerHigherEntropy` | Longer passwords have >= entropy |
| `veryStrongMax` | VeryStrong is maximum strength level |
| `patternPenaltyNonNeg` | Pattern penalties are non-negative |

#### Constant-Time Comparison

| Proof | Statement |
|-------|-----------|
| `constantTimeRefl` | Hash comparison is reflexive |
| `constantTimeSym` | Hash comparison is symmetric |
| `differentLengthNoMatch` | Different length hashes never match |

---

### SafeDateTime

Location: `src/Proven/SafeDateTime/Proofs.idr`

#### Leap Year Calculation

| Proof | Statement |
|-------|-----------|
| `leapYear2000` | 2000 is a leap year |
| `leapYear2024` | 2024 is a leap year |
| `notLeapYear1900` | 1900 is not a leap year |
| `notLeapYear2023` | 2023 is not a leap year |
| `febLeapYear` | February has 29 days in leap years |
| `febNonLeapYear` | February has 28 days in non-leap years |

#### Days in Month

| Proof | Statement |
|-------|-----------|
| `daysInMonthPositive` | Every month has at least 1 day |
| `daysInMonthBounded` | Every month has at most 31 days |

#### Month Conversion

| Proof | Statement |
|-------|-----------|
| `monthToNatBounded` | Month number is in [1, 12] |
| `monthRoundTrip` | `natToMonth (monthToNat m) = Just m` |
| `invalidMonthNothing` | Month 0 returns Nothing |
| `invalidMonth13Nothing` | Month 13 returns Nothing |

---

### SafeNetwork

Location: `src/Proven/SafeNetwork/Proofs.idr`

#### IPv4 Classification

| Proof | Statement |
|-------|-----------|
| `localhostIsLoopback` | 127.0.0.1 is loopback |
| `broadcastNotPrivate` | 255.255.255.255 is not private |
| `anyAddressIsReserved` | 0.0.0.0 is reserved |
| `privateNotGlobal10` | 10.x.x.x is not global |
| `privateNotGlobal172` | 172.16.x.x is not global |
| `privateNotGlobal192` | 192.168.x.x is not global |
| `loopbackNotGlobal` | Loopback is not global |

#### Conversion Roundtrips

| Proof | Statement |
|-------|-----------|
| `ipv4IntegerRoundtrip` | `integerToIPv4 (ipv4ToInteger ip) = ip` |
| `integerIPv4Roundtrip` | `ipv4ToInteger (integerToIPv4 n) = n` |
| `prefixMaskRoundtrip` | `maskToPrefix (prefixToMask p) = Just p` |

#### Parsing

| Proof | Statement |
|-------|-----------|
| `knownIPv4Parses` | "192.168.1.1" parses correctly |
| `invalidIPv4Fails` | "256.0.0.1" fails to parse |

---

## Proof Categories

### Fully Machine-Checked

These proofs use only `Refl` and rewriting - the compiler verifies every step:

- Arithmetic properties (commutativity, associativity, identities)
- Month conversions
- Type exclusivity (nullNotBool, etc.)
- Known constant tests (leapYear2000, etc.)

### Testing-Verified (`believe_me`)

These proofs use `believe_me` but are validated by:

- Property-based testing (`tests/properties/`)
- Fuzzing (`.clusterfuzzlite/`)
- Integration tests

Categories:
- String manipulation proofs
- Parsing properties
- Complex recursion

---

## Contributing New Proofs

### Adding a Fully Checked Proof

1. Define the type signature (the theorem statement)
2. Build the proof term by case analysis
3. Use `rewrite` to transform terms
4. End with `Refl` when terms match

### Adding a `believe_me` Proof

1. Document why full proof is impractical
2. Add property-based tests in `tests/properties/`
3. Add fuzzing coverage if applicable
4. Note the assumption in comments

### Proof Quality Checklist

- [ ] Type signature clearly states the property
- [ ] Doc comment explains what it guarantees
- [ ] If `believe_me`, explain why
- [ ] Corresponding tests exist
- [ ] Security implications documented

---

## Further Reading

- [Type-Driven Development with Idris](https://www.manning.com/books/type-driven-development-with-idris) - Edwin Brady
- [Idris 2 Documentation](https://idris2.readthedocs.io/)
- [Software Foundations](https://softwarefoundations.cis.upenn.edu/) - Formal verification concepts
- [Curry-Howard Correspondence](https://en.wikipedia.org/wiki/Curry%E2%80%93Howard_correspondence) - Proofs as programs

---

*This documentation was generated from the proof modules in `src/Proven/*/Proofs.idr`.*
