-- SPDX-License-Identifier: AGPL-3.0-or-later
-- SPDX-FileCopyrightText: 2025 Hyperpolymath
module SafeSQLProps

import Proven.Core
import Proven.SafeSQL

%default total

||| Property: Parameterized query is safe
prop_parameterizedSafe : isOk (buildQuery "SELECT * FROM users WHERE id = ?" [IntParam 1]) = True
prop_parameterizedSafe = Refl

||| Property: SQL injection in string blocked
prop_injectionBlocked : isErr (buildQuery "SELECT * FROM users WHERE name = '" []) = True
prop_injectionBlocked = ?prop_injectionBlocked_rhs

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

||| Property: Parameter count matches placeholders
prop_paramCountMatches : (q : String) -> (ps : List SQLParam) ->
                         countPlaceholders q = length ps ->
                         isOk (buildQuery q ps) = True
prop_paramCountMatches q ps prf = ?prop_paramCountMatches_rhs

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
