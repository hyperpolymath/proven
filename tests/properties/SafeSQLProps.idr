-- SPDX-License-Identifier: MPL-2.0
-- SPDX-FileCopyrightText: 2025 Hyperpolymath
module SafeSQLProps

import Proven.Core
-- Import the concrete parameter and lexical contracts under test.
import Proven.SafeSQL.Types
import Proven.SafeSQL.Params
import Proven.SafeSQL.Proofs
import System

%default total

-- Rewritten against the real API: queries are `ParameterizedQuery` values built
-- with `literal`/`appendParam`/`bindParams` and checked by `validateParams`;
-- escaping functions take an `SQLDialect`. (`buildQuery`, `IntParam`,
-- `validateIdentifier`, `countPlaceholders` never existed.)

--------------------------------------------------------------------------------
-- Type-level proofs
--------------------------------------------------------------------------------

||| One placeholder with one bound value validates, for every dialect, literal
||| prefix and value (was prop_parameterizedSafe).
prop_parameterizedSafe : (d : SQLDialect) -> (sql : String) -> (v : SQLValue) ->
                         isOk (validateParams (bindParams (appendParam (literal d sql)) [v])) = True
prop_parameterizedSafe d sql v = Refl

||| A placeholder with no bound value is rejected.
prop_unboundParamRejected : (d : SQLDialect) -> (sql : String) ->
                            isErr (validateParams (appendParam (literal d sql))) = True
prop_unboundParamRejected d sql = Refl

||| A query with no placeholders needs no bindings.
prop_literalNeedsNoParams : (d : SQLDialect) -> (sql : String) ->
                            isOk (validateParams (literal d sql)) = True
prop_literalNeedsNoParams d sql = Refl

||| NULL renders as the keyword NULL in every dialect.
prop_nullRendersAsKeyword : (d : SQLDialect) -> renderValue d SQLNull = "NULL"
prop_nullRendersAsKeyword d = Refl

||| A NULL parameter binds and validates in every dialect (was prop_nullParamHandled).
prop_nullParamHandled : (d : SQLDialect) -> (sql : String) ->
                        isOk (validateParams (bindParams (appendParam (literal d sql)) [SQLNull])) = True
prop_nullParamHandled d sql = Refl

||| An integer binding excludes SQLRaw; this does not certify a driver.
prop_integerBinding : (i : Integer) -> BoundParameters [SQLInt i]
prop_integerBinding i = MoreParameters Refl NoParameters

||| The production literal renderer constructs the explicit lexical grammar.
prop_literalEncoding : (d : SQLDialect) -> (s : String) ->
  RenderedLiteral (defaultQuoteMode d) (unpack s) (escapeString d s)
prop_literalEncoding = escapeStringQuotesSafe

||| Every quote in the produced body belongs to a doubled pair.
prop_bodyQuotesPaired : (d : SQLDialect) -> (s : String) ->
  PairedQuotes (bodyChars (defaultQuoteMode d) (unpack s))
prop_bodyQuotesPaired = escapedBodyQuotesPaired

--------------------------------------------------------------------------------
-- Outstanding obligations (named propositions, not witnesses)
--------------------------------------------------------------------------------

-- String representation, connection settings and driver semantics remain assumptions.

||| OWED: a query whose positional placeholders are all bound and which has no
||| named placeholders validates (was prop_paramCountMatches). Blocked by:
||| `validateParams` goes through `nub`/`mapMaybe`/`filter` over an abstract
||| fragment list; needs list lemmas linking `countParams`/`getNamedParamNames`
||| to its branch conditions. This names the proposition; it is not a witness.
public export
ParamCountValidatesObligation : Type
ParamCountValidatesObligation =
  (q : ParameterizedQuery) ->
  LTE (countParams q) (length q.params) ->
  getNamedParamNames q = [] ->
  isOk (validateParams q) = True

--------------------------------------------------------------------------------
-- Execution checks
--------------------------------------------------------------------------------

private
check : String -> Bool -> IO ()
check label True = putStrLn ("PASS (execution): " ++ label)
check label False = do
  putStrLn ("FAIL: " ++ label)
  exitFailure

private
rejected : Maybe a -> Bool
rejected Nothing = True
rejected (Just _) = False

||| Test runner for SQL properties
export
runSQLProperties : IO ()
runSQLProperties = do
  putStrLn "SafeSQL Property Tests"
  putStrLn "======================"
  putStrLn "prop_parameterizedSafe: PASS (proven by type)"
  putStrLn "prop_unboundParamRejected: PASS (proven by type)"
  putStrLn "prop_literalNeedsNoParams: PASS (proven by type)"
  putStrLn "prop_nullRendersAsKeyword: PASS (proven by type)"
  putStrLn "prop_nullParamHandled: PASS (proven by type)"
  putStrLn "prop_integerBinding: PASS (SQLRaw exclusion, proven by type)"
  putStrLn "prop_literalEncoding and prop_bodyQuotesPaired: PASS (proven by type)"
  check "prop_escapeQuotes: escapeString doubles single quotes and wraps"
    (escapeString PostgreSQL "O'Brien" == "'O''Brien'" &&
     escapeString MSSQL "O'Brien" == "'O''Brien'")
  check "escapeString strips NUL bytes"
    (escapeString SQLite "a\0b" == "'ab'")
  check "prop_injectionBlocked: dangling quote in literal SQL is flagged"
    (isErr (validateQuerySafety (literal PostgreSQL "SELECT * FROM users WHERE name = '")))
  check "prop_injectionBlocked: OR/comment payload flagged, plain word not"
    (isErr (assertNoInjection "' OR 1=1 --") && isOk (assertNoInjection "alice"))
  check "prop_injectionBlocked: payload bound as text stays inside one literal"
    (escapeString MySQL "x'; DROP TABLE users; --" == "'x''; DROP TABLE users; --'")
  check "prop_identifierEscape: identifiers quoted per dialect"
    (quoteIdentifier PostgreSQL (MkSafeIdentifier "users") == "\"users\"" &&
     quoteIdentifier MySQL (MkSafeIdentifier "users") == "`users`" &&
     quoteIdentifier MSSQL (MkSafeIdentifier "users") == "[users]")
  check "prop_identifierEscape: embedded quote characters doubled"
    (quoteIdentifier PostgreSQL (MkSafeIdentifier "a\"b") == "\"a\"\"b\"" &&
     quoteIdentifier MSSQL (MkSafeIdentifier "a]b") == "[a]]b]")
  check "prop_validIdentifier: users accepted"
    (isOk (assertValidIdentifier "users") &&
     map identifierName (mkIdentifier "users") == Just "users")
  check "prop_invalidIdentifierNum: leading digit rejected"
    (rejected (mkIdentifier "123users") && isErr (assertValidIdentifier "123users"))
  check "identifiers with space, quote, or empty rejected"
    (rejected (mkIdentifier "table name") && rejected (mkIdentifier "a'b") &&
     rejected (mkIdentifier ""))
  check "prop_reservedWordRejected: reserved words detected case-insensitively"
    (isReservedWord "SELECT" && isReservedWord "select" && not (isReservedWord "users"))
  check "prop_nullParamHandled: NULL interpolates as keyword"
    (renderQueryInterpolated
       (bindParams (appendParam (literal PostgreSQL "SELECT * FROM users WHERE deleted_at IS ")) [SQLNull])
     == "SELECT * FROM users WHERE deleted_at IS NULL")
  check "prop_paramCountMatches: placeholders numbered per dialect"
    (renderQuery (appendParam (appendLiteral " AND b = " (appendParam (literal PostgreSQL "a = "))))
       == "a = $1 AND b = $2")
  check "prop_paramCountMatches: two placeholders with one value rejected"
    (isErr (validateParams (bindParams (appendParam (appendParam (literal SQLite "x"))) [SQLInt 1])))
  check "prop_likeWildcardsEscaped: % and _ escaped"
    (escapeLikePattern PostgreSQL "100%" == "100\\%" &&
     escapeLikePattern PostgreSQL "a_b" == "a\\_b" &&
     escapeLikePattern MSSQL "100%" == "100[%]")
  putStrLn ""
