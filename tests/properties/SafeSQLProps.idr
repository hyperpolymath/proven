-- SPDX-License-Identifier: MPL-2.0
-- SPDX-FileCopyrightText: 2025 Hyperpolymath
module SafeSQLProps

import Proven.Core
import Proven.SafeSQL

%default total

||| Property: Parameterized query is safe
prop_parameterizedSafe : isOk (buildQuery "SELECT * FROM users WHERE id = ?" [IntParam 1]) = True
prop_parameterizedSafe = Refl

||| OWED: SQL injection in string blocked
||| Held back by Idris2 0.8.0 not reducing the underlying computation by
||| Refl at the type level (the original orphan-test author wrote this as
||| a `?prop_injectionBlocked_rhs` hole). May discharge as `= Refl` once the relevant
||| String/List/Char reduction blocker lifts, or by manual case analysis;
||| revisit during Phase 2 Day-3+ DISCHARGE pass.
0 prop_injectionBlocked : isErr (buildQuery "SELECT * FROM users WHERE name = '" []) = True

||| Property: Escape quotes in strings
prop_escapeQuotes : escapeString "O'Brien" = "O''Brien"
prop_escapeQuotes = Refl

||| Property: Identifier escaping works
prop_identifierEscape : escapeIdentifier "table name" = "\"table name\""
prop_identifierEscape = Refl

||| Property: Valid identifier passes
prop_validIdentifier : isOk (validateIdentifier "users") = True
prop_validIdentifier = Refl

||| Property: Invalid identifier rejected (starts with number)
prop_invalidIdentifierNum : isErr (validateIdentifier "123users") = True
prop_invalidIdentifierNum = Refl

||| Property: Reserved word rejected as identifier
prop_reservedWordRejected : isErr (validateIdentifier "SELECT") = True
prop_reservedWordRejected = Refl

||| OWED: Parameter count matches placeholders
||| Held back by Idris2 0.8.0 not reducing the underlying computation by
||| Refl at the type level (the original orphan-test author wrote this as
||| a `?prop_paramCountMatches_rhs` hole). May discharge as `= Refl` once the relevant
||| String/List/Char reduction blocker lifts, or by manual case analysis;
||| revisit during Phase 2 Day-3+ DISCHARGE pass.
0 prop_paramCountMatches : (q : String) -> (ps : List SQLParam) ->
                           countPlaceholders q = length ps ->
                           isOk (buildQuery q ps) = True

||| Property: Null parameter handled
prop_nullParamHandled : isOk (buildQuery "SELECT * FROM users WHERE deleted_at IS ?" [NullParam]) = True
prop_nullParamHandled = Refl

||| Property: LIKE wildcards escaped
prop_likeWildcardsEscaped : escapeLikePattern "100%" = "100\\%"
prop_likeWildcardsEscaped = Refl

||| Test runner for SQL properties
export
runSQLProperties : IO ()
runSQLProperties = do
  putStrLn "SafeSQL Property Tests"
  putStrLn "======================"
  putStrLn "prop_parameterizedSafe: PASS (proven by type)"
  putStrLn "prop_escapeQuotes: PASS (proven by type)"
  putStrLn "prop_identifierEscape: PASS (proven by type)"
  putStrLn "prop_validIdentifier: PASS (proven by type)"
  putStrLn "prop_invalidIdentifierNum: PASS (proven by type)"
  putStrLn "prop_reservedWordRejected: PASS (proven by type)"
  putStrLn "prop_nullParamHandled: PASS (proven by type)"
  putStrLn "prop_likeWildcardsEscaped: PASS (proven by type)"
  putStrLn ""
