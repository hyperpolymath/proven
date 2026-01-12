-- SPDX-License-Identifier: AGPL-3.0-or-later
-- SPDX-FileCopyrightText: 2025 Hyperpolymath
module SafeSQLUnit

import Proven.Core
import Proven.SafeSQL

%default total

assertOk : Show e => String -> Result e a -> IO ()
assertOk name (Ok _) = putStrLn $ "  ✓ " ++ name
assertOk name (Err e) = putStrLn $ "  ✗ " ++ name ++ " (got error: " ++ show e ++ ")"

assertErr : Show a => String -> Result e a -> IO ()
assertErr name (Err _) = putStrLn $ "  ✓ " ++ name
assertErr name (Ok v) = putStrLn $ "  ✗ " ++ name ++ " (expected error, got: " ++ show v ++ ")"

assertEq : (Eq a, Show a) => String -> a -> a -> IO ()
assertEq name expected actual =
  if expected == actual
    then putStrLn $ "  ✓ " ++ name
    else putStrLn $ "  ✗ " ++ name ++ " (expected " ++ show expected ++ ", got " ++ show actual ++ ")"

assertTrue : String -> Bool -> IO ()
assertTrue name True = putStrLn $ "  ✓ " ++ name
assertTrue name False = putStrLn $ "  ✗ " ++ name ++ " (expected True)"

export
runSQLUnitTests : IO ()
runSQLUnitTests = do
  putStrLn "SafeSQL Unit Tests"
  putStrLn "=================="

  -- Identifier validation tests
  putStrLn "\n[Identifier Validation]"
  assertOk "validateIdentifier \"users\" valid" (validateIdentifier "users")
  assertOk "validateIdentifier \"user_profiles\" valid" (validateIdentifier "user_profiles")
  assertErr "validateIdentifier \"users; DROP TABLE\" blocked" (validateIdentifier "users; DROP TABLE")
  assertErr "validateIdentifier \"\" blocked" (validateIdentifier "")
  assertErr "validateIdentifier with injection" (validateIdentifier "users'--")

  -- Value escaping tests
  putStrLn "\n[Value Escaping]"
  assertEq "escapeValue \"O'Brien\" PostgreSQL"
           "'O''Brien'" (escapeValue PostgreSQL (SQLText "O'Brien"))
  assertEq "escapeValue NULL"
           "NULL" (escapeValue PostgreSQL SQLNull)
  assertEq "escapeValue 42"
           "42" (escapeValue PostgreSQL (SQLInt 42))
  assertEq "escapeValue true PostgreSQL"
           "TRUE" (escapeValue PostgreSQL (SQLBool True))
  assertEq "escapeValue true MySQL"
           "1" (escapeValue MySQL (SQLBool True))

  -- Parameterized query tests
  putStrLn "\n[Parameterized Queries]"
  let q1 = parameterizedQuery PostgreSQL "SELECT * FROM users WHERE id = $1" [SQLInt 42]
  assertOk "parameterizedQuery builds valid query" (buildQuery q1)

  let q2 = parameterizedQuery MySQL "SELECT * FROM users WHERE name = ?" [SQLText "Alice"]
  assertOk "parameterizedQuery MySQL style" (buildQuery q2)

  -- Query builder tests
  putStrLn "\n[Query Builder]"
  let selectQuery = select ["id", "name", "email"]
                    |> from "users"
                    |> whereClause "active = $1" [SQLBool True]
                    |> orderBy "created_at" Desc
                    |> limit 10
  assertOk "query builder creates valid query" (buildQuery selectQuery)

  -- Insert builder tests
  putStrLn "\n[Insert Builder]"
  let insertQuery = safeInsert "users"
                    [("name", SQLText "Alice"), ("email", SQLText "alice@example.com")]
  assertOk "safeInsert builds valid INSERT" (buildQuery insertQuery)

  -- Update builder tests
  putStrLn "\n[Update Builder]"
  let updateQuery = safeUpdate "users"
                    [("name", SQLText "Bob")]
                    "id = $2" [SQLInt 1]
  assertOk "safeUpdate builds valid UPDATE" (buildQuery updateQuery)

  -- Delete builder tests
  putStrLn "\n[Delete Builder]"
  let deleteQuery = safeDelete "users" "id = $1" [SQLInt 1]
  assertOk "safeDelete builds valid DELETE" (buildQuery deleteQuery)

  -- Injection detection tests
  putStrLn "\n[Injection Detection]"
  assertTrue "detectInjection finds UNION attack"
             (detectInjection "1 UNION SELECT * FROM passwords")
  assertTrue "detectInjection finds comment attack"
             (detectInjection "admin'--")
  assertTrue "detectInjection finds OR 1=1"
             (detectInjection "' OR 1=1--")
  assertTrue "detectInjection safe input"
             (not $ detectInjection "normal search term")

  putStrLn "\n✓ SafeSQL unit tests complete"
