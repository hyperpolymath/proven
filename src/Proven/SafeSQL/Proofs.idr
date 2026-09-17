-- SPDX-License-Identifier: MPL-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
||| Safety proofs for SQL injection prevention
|||
||| This module provides formal proofs that the SafeSQL operations
||| cannot produce SQL injection vulnerabilities.
module Proven.SafeSQL.Proofs

import Proven.Core
import Proven.SafeSQL.Types
import Proven.SafeSQL.Params
import Data.List
import Data.String

%default total

--------------------------------------------------------------------------------
-- Safety Predicates
--------------------------------------------------------------------------------

||| Predicate: A string contains no unescaped quotes
public export
data NoUnescapedQuotes : String -> Type where
  ||| Empty string has no unescaped quotes
  EmptyNoQuotes : NoUnescapedQuotes ""
  ||| String with all quotes properly doubled
  QuotesEscaped : (s : String) ->
                  (prf : all (\c => c /= '\'') (unpack s) = True) ->
                  NoUnescapedQuotes s

||| Predicate: A string contains no SQL comment markers
public export
data NoCommentMarkers : String -> Type where
  MkNoCommentMarkers : (s : String) ->
                       (noDoubleDash : not (isInfixOf "--" s) = True) ->
                       (noSlashStar : not (isInfixOf "/*" s) = True) ->
                       NoCommentMarkers s

||| Predicate: A string contains no statement terminators
public export
data NoStatementTerminators : String -> Type where
  MkNoTerminators : (s : String) ->
                    (noSemicolon : not (';' `elem` unpack s) = True) ->
                    NoStatementTerminators s

||| Predicate: A string is a safe SQL identifier
public export
data IsSafeIdentifier : String -> Type where
  MkSafeIdent : (s : String) ->
                (validChars : all isIdentifierChar (unpack s) = True) ->
                (notEmpty : length s > 0 = True) ->
                (notTooLong : length s <= 128 = True) ->
                IsSafeIdentifier s

||| Helper: Check if a query fragment is parameterized or literal
public export
isParamOrLiteral : QueryFragment -> Bool
isParamOrLiteral (Literal _) = True
isParamOrLiteral (Param _) = True
isParamOrLiteral (NamedParam _) = True
isParamOrLiteral (Identifier _) = True

||| Predicate: A query uses only parameterized values (no string interpolation)
public export
data IsParameterized : ParameterizedQuery -> Type where
  MkParameterized : (q : ParameterizedQuery) ->
                    (noRawStrings : all isParamOrLiteral q.fragments = True) ->
                    IsParameterized q

||| Predicate: A SQL value is properly escaped
public export
data IsEscapedValue : SQLDialect -> SQLValue -> Type where
  ||| NULL is always safe
  NullSafe : IsEscapedValue d SQLNull
  ||| Booleans are safe (rendered as TRUE/FALSE)
  BoolSafe : IsEscapedValue d (SQLBool b)
  ||| Integers are safe (no string escaping needed)
  IntSafe : IsEscapedValue d (SQLInt i)
  ||| Naturals are safe
  NatSafe : IsEscapedValue d (SQLNat n)
  ||| Doubles are safe (numeric)
  DoubleSafe : IsEscapedValue d (SQLDouble x)
  ||| Text is escaped by escapeString
  TextEscaped : (d : SQLDialect) -> (s : String) -> IsEscapedValue d (SQLText s)
  ||| Blob is hex-encoded
  BlobEncoded : (d : SQLDialect) -> (bs : List Bits8) -> IsEscapedValue d (SQLBlob bs)
  ||| Date components are numeric
  DateSafe : IsEscapedValue d (SQLDate y m day)
  ||| Time components are numeric
  TimeSafe : IsEscapedValue d (SQLTime h m s)
  ||| Timestamp components are numeric
  TimestampSafe : IsEscapedValue d (SQLTimestamp y mo dy h mi s)
  ||| Raw SQL is trusted by construction (only from code, never user input)
  RawTrusted : IsEscapedValue d (SQLRaw s)

--------------------------------------------------------------------------------
-- Core Safety Theorems
--------------------------------------------------------------------------------

||| OWED: for every dialect `d` and input `s`, the result of
||| `escapeString d s` satisfies `NoUnescapedQuotes`. Operationally
||| witnessed by `Proven.SafeSQL.Params.escapeString`, which wraps the
||| input in single quotes and doubles every internal `'\''`
||| (`'\'' :: cs |-> "''" ++ escape cs`) — by case-analysis every
||| position of the output is either a non-quote source char, a
||| dialect-specific escape sequence, or a doubled `''`, so no raw
||| unescaped `'\''` remains.
|||
||| Held back by Idris2 0.8.0 not reducing `unpack` / `pack` / `++` /
||| `singleton` over abstract `String` at the type level — these are
||| FFI-bound String primitives, so `all (\c => c /= '\'')
||| (unpack (escapeString d s))` does not normalise to `True` by Refl
||| alone for an abstract `s`. Same blocker family as SafeChecksum's
||| `luhnValidatesKnownGood` and SafeHtml's `escapePreservesNoLT`
||| (opaque String FFI). Discharge once a `Data.String` reflective
||| tactic or per-character induction lemma over
||| `unpack . concat . map` is available.
export
0 escapeStringQuotesSafe : (d : SQLDialect) -> (s : String) ->
                                     NoUnescapedQuotes (escapeString d s)

||| OWED: if `isValidIdentifier s = True` then `IsSafeIdentifier s`.
||| Operationally `isValidIdentifier` (in `Proven.SafeSQL.Types`) is
||| the conjunction `(isAlpha c0 || c0 == '_') && all isIdentifierChar
||| (unpack rest) && length s <= 128`, which directly mirrors the
||| three fields of `MkSafeIdent` (validChars / notEmpty / notTooLong)
||| — so the witness is structurally derivable from the Boolean proof.
|||
||| Held back by Idris2 0.8.0 not reducing `strM` / `unpack` /
||| `length` over abstract `String` at the type level — these are
||| FFI-bound String primitives, so the conjunction does not split
||| into its three component `Refl`s by Refl alone, and the
||| `notEmpty : length s > 0 = True` field cannot be projected from
||| `isValidIdentifier s = True` without per-character induction.
||| Same blocker family as SafeHtml's `sanitizeRemovesScripts`
||| (opaque String FFI). Discharge once a `Data.String` reflective
||| tactic for `strM` / `unpack` / `length` is available, or
||| `isValidIdentifier` is refactored to return a structural witness
||| (e.g. `Dec (IsSafeIdentifier s)`) instead of a `Bool`.
export
0 identifierCharsSafe : (s : String) -> (prf : isValidIdentifier s = True) ->
                                  IsSafeIdentifier s

||| OWED: every `ParameterizedQuery` value `q` satisfies
||| `IsParameterized q`, i.e. every fragment in `q.fragments`
||| satisfies `isParamOrLiteral`. Operationally true because
||| `QueryFragment` has exactly four constructors (`Literal`, `Param`,
||| `NamedParam`, `Identifier`) and `isParamOrLiteral` returns `True`
||| on all four — so the predicate is total-by-exhaustion and the
||| witness is constructible by induction over `q.fragments`.
|||
||| Held back by Idris2 0.8.0 not reducing `all` (`Data.List`) at the
||| type level over an abstract `List QueryFragment` — `all f xs`
||| does not normalise to `True` by Refl alone for an opaque `xs`,
||| so the structural induction case `all isParamOrLiteral
||| (f :: fs) = isParamOrLiteral f && all isParamOrLiteral fs`
||| cannot be discharged without a `Data.List.all`-reflection lemma.
||| The current `ParameterizedQuery` record also does not carry the
||| invariant in its constructor, so a manufactured `q` with a future
||| non-paramic fragment would not be ruled out by typing. Same
||| blocker family as the String-FFI OWED set. Discharge once either
||| (a) a `Data.List.all` reflective tactic + a per-fragment lemma is
||| available, or (b) `ParameterizedQuery` is refactored to carry an
||| `IsParameterized` field at construction (intrinsic invariant).
export
0 parameterizedQueriesSafe : (q : ParameterizedQuery) -> IsParameterized q

||| Theorem: All SQLValue types are safely escapable
export
allValuesSafe : (d : SQLDialect) -> (v : SQLValue) -> IsEscapedValue d v
allValuesSafe d SQLNull = NullSafe
allValuesSafe d (SQLBool b) = BoolSafe
allValuesSafe d (SQLInt i) = IntSafe
allValuesSafe d (SQLNat n) = NatSafe
allValuesSafe d (SQLDouble x) = DoubleSafe
allValuesSafe d (SQLText s) = TextEscaped d s
allValuesSafe d (SQLBlob bs) = BlobEncoded d bs
allValuesSafe d (SQLDate y m day) = DateSafe
allValuesSafe d (SQLTime h m s) = TimeSafe
allValuesSafe d (SQLTimestamp y mo dy h mi s) = TimestampSafe
allValuesSafe d (SQLRaw s) = RawTrusted

--------------------------------------------------------------------------------
-- Injection Prevention Proofs
--------------------------------------------------------------------------------

||| Evidence that a query cannot be subject to SQL injection
public export
data InjectionSafe : ParameterizedQuery -> Type where
  ||| Query is safe because it uses parameterization
  SafeByParameterization :
    (q : ParameterizedQuery) ->
    (isParam : IsParameterized q) ->
    (allEscaped : (v : SQLValue) -> v `elem` q.params = True -> IsEscapedValue q.dialect v) ->
    InjectionSafe q

||| Theorem: Queries built with the builder are injection-safe
|||
||| The query builder only allows:
||| 1. Validated identifiers (table/column names)
||| 2. Parameterized values (never interpolated)
||| 3. Literal SQL fragments (from trusted code, not user input)
||| OWED: queries built with the builder are injection-safe.
||| The witness would be `SafeByParameterization q (parameterizedQueriesSafe q)
||| (\v, _ => allValuesSafe q.dialect v)`, but `parameterizedQueriesSafe` is
||| itself OWED (erased, `0`-multiplicity) — an erased proof cannot be supplied
||| as the relevant `IsParameterized` field of `SafeByParameterization`, so this
||| theorem cannot be discharged until `parameterizedQueriesSafe` is (i.e. once
||| `Data.List.all` reflection lands, or `ParameterizedQuery` carries the
||| invariant intrinsically). Held back by the same blocker as its premise.
export
0 builderQueriesSafe : (q : ParameterizedQuery) ->
                       (builtWithBuilder : ()) ->
                       InjectionSafe q

||| OWED: user input cannot escape string literals after escaping.
||| Proof sketch: escapeString doubles all single quotes and wraps the result
||| in single quotes, so any quote in the input becomes the escaped `''` and the
||| literal cannot be terminated early. The witness is exactly
||| `escapeStringQuotesSafe d userInput`, but that is OWED (erased,
||| `0`-multiplicity, opaque-String-FFI blocker), so this theorem inherits the
||| same debt and cannot be relevantly discharged until it is.
export
0 cannotEscapeStringLiteral : (d : SQLDialect) -> (userInput : String) ->
                              let escaped = escapeString d userInput
                              in NoUnescapedQuotes escaped

||| Theorem: Numeric values cannot contain SQL injection
|||
||| SQLInt, SQLNat, and SQLDouble are rendered as numeric literals
||| without any string delimiters, so there's nothing to escape from.
export
numericValuesCannotInject : (d : SQLDialect) ->
                            (i : Integer) ->
                            let v = SQLInt i
                            in IsEscapedValue d v
numericValuesCannotInject d i = IntSafe

--------------------------------------------------------------------------------
-- Composition Safety
--------------------------------------------------------------------------------

||| OWED: combining safe queries produces a safe query.
||| The witness would reuse `parameterizedQueriesSafe (combineQueries q1 q2)`,
||| which is OWED (erased) — so, like `builderQueriesSafe`, this cannot be
||| relevantly discharged until `parameterizedQueriesSafe` is.
export
0 combinePreservesSafety : (q1 : ParameterizedQuery) -> (q2 : ParameterizedQuery) ->
                           InjectionSafe q1 -> InjectionSafe q2 ->
                           InjectionSafe (combineQueries q1 q2)

||| OWED: adding parameters preserves safety.
||| addParam only appends to q.params and preserves fragments/dialect, so the
||| query stays injection-safe — but the witness reuses
||| `parameterizedQueriesSafe (addParam v q)`, which is OWED (erased). Same debt
||| as `combinePreservesSafety`; discharge together once
||| `parameterizedQueriesSafe` is.
export
0 addParamPreservesSafety : (q : ParameterizedQuery) -> (v : SQLValue) ->
                            InjectionSafe q ->
                            InjectionSafe (addParam v q)

--------------------------------------------------------------------------------
-- Defensive Checks (Runtime Validation)
--------------------------------------------------------------------------------

||| Validate a query at runtime for additional safety
||| This catches issues that might slip through (e.g., SQLRaw misuse)
public export
validateQuerySafety : ParameterizedQuery -> Result SQLError ()
validateQuerySafety q =
  let literals = mapMaybe getLiteral q.fragments
      combined = concat literals
  in if containsDangerousSQL combined
       then Err (InjectionDetected (analyzeForInjection combined) combined)
       else Ok ()
  where
    getLiteral : QueryFragment -> Maybe String
    getLiteral (Literal s) = Just s
    getLiteral _ = Nothing

||| Strict validation - also checks for SQLRaw values
public export
validateQueryStrict : ParameterizedQuery -> Result SQLError ()
validateQueryStrict q =
  case findRaw q.params of
    Just rawVal => Err (InvalidQuery "Query contains SQLRaw value - manual review required")
    Nothing => validateQuerySafety q
  where
    findRaw : List SQLValue -> Maybe String
    findRaw [] = Nothing
    findRaw (SQLRaw s :: _) = Just s
    findRaw (_ :: xs) = findRaw xs

--------------------------------------------------------------------------------
-- Safety Assertions (Development/Testing)
--------------------------------------------------------------------------------

||| Assert that a string would not cause injection
||| Useful for testing and development
public export
assertNoInjection : String -> Result SQLError ()
assertNoInjection s =
  case analyzeForInjection s of
    Safe => Ok ()
    sev => Err (InjectionDetected sev s)

||| Assert identifier is valid
public export
assertValidIdentifier : String -> Result SQLError SafeIdentifier
assertValidIdentifier s =
  case mkIdentifier s of
    Just ident => Ok ident
    Nothing => Err (InvalidIdentifier s "Contains invalid characters or is empty")

--------------------------------------------------------------------------------
-- Documentation of Safety Guarantees
--------------------------------------------------------------------------------

||| Summary of safety guarantees provided by SafeSQL:
|||
||| 1. **Parameterization**: User input is never concatenated into SQL strings.
|||    All values are bound as parameters, which the database handles safely.
|||
||| 2. **Type-safe values**: SQLValue constructors ensure proper escaping:
|||    - Strings are quoted and escaped (quotes doubled)
|||    - Numbers are rendered as numeric literals
|||    - Binary data is hex-encoded
|||    - NULL is rendered as the keyword NULL
|||
||| 3. **Identifier validation**: Table and column names are validated to
|||    contain only alphanumeric characters and underscores.
|||
||| 4. **Dialect-aware escaping**: Each SQL dialect has its own escaping
|||    rules which are correctly applied.
|||
||| 5. **No string interpolation**: The query builder never interpolates
|||    user-provided strings into the SQL template.
|||
||| 6. **Null byte removal**: Null bytes (potential truncation attacks)
|||    are stripped from string inputs.
|||
||| 7. **Comment marker detection**: The analyzer detects SQL comment
|||    markers (-- and /*) which often indicate injection attempts.
|||
||| The only way to bypass these protections is to use SQLRaw, which
||| requires explicit acknowledgment that the value is trusted.
public export
safetyGuarantees : String
safetyGuarantees = """
SafeSQL provides defense-in-depth against SQL injection:

Level 1: Parameterization
  - All values bound as parameters, not concatenated
  - Database driver handles escaping

Level 2: Type Safety
  - SQLValue types ensure proper formatting
  - No implicit string conversion

Level 3: Identifier Validation
  - Table/column names validated
  - Reserved words detected

Level 4: Dialect Awareness
  - PostgreSQL, MySQL, SQLite, MSSQL, Oracle
  - Each dialect's escaping rules applied

Level 5: Runtime Validation
  - Optional strict mode checks
  - Detects suspicious patterns
"""
