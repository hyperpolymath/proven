-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
||| SafeSQL - SQL operations that prevent injection attacks
|||
||| This module provides safe SQL operations including:
||| - Parameterized queries (prevents SQL injection)
||| - Type-safe query building
||| - Multi-dialect support (PostgreSQL, MySQL, SQLite, MSSQL, Oracle)
||| - Formal proofs of injection safety
|||
||| Example usage:
||| ```idris
||| -- Simple parameterized query
||| query <- sql PostgreSQL "SELECT * FROM users WHERE id = ?" [SQLInt 42]
|||
||| -- Query builder
||| query <- selectFrom PostgreSQL "users"
|||          >>= whereEq "status" (SQLText "active")
|||          >>= orderByDesc "created_at"
|||          >>= limit 10
|||          >>= build
|||
||| -- Safe INSERT
||| query <- safeInsert PostgreSQL "users"
|||          [ ("name", SQLText "Alice")
|||          , ("email", SQLText "alice@example.com")
|||          , ("age", SQLInt 30)
|||          ]
||| ```
module Proven.SafeSQL

import public Proven.Core
import public Proven.SafeSQL.Types
import public Proven.SafeSQL.Params
import public Proven.SafeSQL.Builder
import public Proven.SafeSQL.Proofs

import Data.List
import Data.String

%default total

--------------------------------------------------------------------------------
-- High-Level Query Construction
--------------------------------------------------------------------------------

||| Create a parameterized query from SQL template and values
||| The template should use dialect-appropriate placeholders
public export
sql : SQLDialect -> String -> List SQLValue -> ParameterizedQuery
sql dialect template vals = MkQuery [Literal template] vals [] dialect

||| Create a parameterized query with named parameters
public export
sqlNamed : SQLDialect -> String -> List (String, SQLValue) -> ParameterizedQuery
sqlNamed dialect template vals = MkQuery [Literal template] [] vals dialect

||| Start a SELECT query builder
public export
selectFrom : SQLDialect -> String -> Result SQLError QueryBuilder
selectFrom dialect tableName = from tableName (selectAll (newBuilder dialect))

||| Start a SELECT with specific columns
public export
selectColumnsFrom : SQLDialect -> List String -> String -> Result SQLError QueryBuilder
selectColumnsFrom dialect cols tableName = do
  qb <- select cols (newBuilder dialect)
  from tableName qb

--------------------------------------------------------------------------------
-- Safe CRUD Operations
--------------------------------------------------------------------------------

||| Safe INSERT - validates all inputs
public export
safeInsert : SQLDialect -> String -> List (String, SQLValue) -> Result SQLError ParameterizedQuery
safeInsert dialect tableName colVals = do
  -- Validate no injection in column names
  _ <- traverse (\(n, _) => assertValidIdentifier n) colVals
  -- Check values for suspicious patterns (warning only for text)
  _ <- traverse checkValue colVals
  -- Build the query
  insert tableName colVals dialect
  where
    checkValue : (String, SQLValue) -> Result SQLError ()
    checkValue (_, SQLText s) =
      case analyzeForInjection s of
        Critical reason => Err (InjectionDetected (Critical reason) s)
        _ => Ok ()  -- Warnings are OK for parameterized queries
    checkValue _ = Ok ()

||| Safe UPDATE - requires WHERE clause for safety
public export
safeUpdate : SQLDialect -> String -> List (String, SQLValue) ->
             String -> SQLValue -> Result SQLError ParameterizedQuery
safeUpdate dialect tableName setCols whereCol whereVal = do
  -- Build WHERE condition
  qb <- whereEq whereCol whereVal (newBuilder dialect)
  -- Build UPDATE
  update tableName setCols qb

||| Safe DELETE - requires WHERE clause for safety
public export
safeDelete : SQLDialect -> String -> String -> SQLValue -> Result SQLError ParameterizedQuery
safeDelete dialect tableName whereCol whereVal = do
  qb <- whereEq whereCol whereVal (newBuilder dialect)
  delete tableName qb

||| Safe SELECT by ID
public export
selectById : SQLDialect -> String -> String -> SQLValue -> Result SQLError ParameterizedQuery
selectById dialect tableName idCol idVal = do
  qb <- selectFrom dialect tableName
  qb' <- whereEq idCol idVal qb
  build qb'

||| Safe SELECT with multiple conditions
public export
selectWhere : SQLDialect -> String -> List (String, CompareOp, SQLValue) ->
              Result SQLError ParameterizedQuery
selectWhere dialect tableName conditions = do
  qb <- selectFrom dialect tableName
  qb' <- foldlM applyCondition qb conditions
  build qb'
  where
    applyCondition : QueryBuilder -> (String, CompareOp, SQLValue) -> Result SQLError QueryBuilder
    applyCondition builder (col, Eq', val) = whereEq col val builder
    applyCondition builder (col, NotEq, val) = whereNotEq col val builder
    applyCondition builder (col, Lt, val) = whereLt col val builder
    applyCondition builder (col, LtEq, val) = whereLtEq col val builder
    applyCondition builder (col, Gt, val) = whereGt col val builder
    applyCondition builder (col, GtEq, val) = whereGtEq col val builder
    applyCondition builder (col, IsNull', _) = whereNull col builder
    applyCondition builder (col, IsNotNull, _) = whereNotNull col builder
    applyCondition builder (col, Like', SQLText pattern) = whereLike col pattern builder
    applyCondition builder _ = Ok builder

--------------------------------------------------------------------------------
-- Query Execution Helpers
--------------------------------------------------------------------------------

||| Get the SQL string for a parameterized query (with placeholders)
public export
toSQL : ParameterizedQuery -> String
toSQL = renderQuery

||| Get the SQL string with values interpolated (for debugging only!)
||| WARNING: Never execute this directly - use parameterized queries
public export
toDebugSQL : ParameterizedQuery -> String
toDebugSQL = renderQueryInterpolated

||| Get the parameter values in order
public export
getParams : ParameterizedQuery -> List SQLValue
getParams q = q.params

||| Get named parameters as a list of pairs
public export
getNamedParams : ParameterizedQuery -> List (String, SQLValue)
getNamedParams q = q.namedParams

||| Validate a query before execution
public export
validate : ParameterizedQuery -> Result SQLError ParameterizedQuery
validate q = do
  _ <- validateParams q
  _ <- validateQuerySafety q
  Ok q

||| Validate strictly (rejects SQLRaw)
public export
validateStrict : ParameterizedQuery -> Result SQLError ParameterizedQuery
validateStrict q = do
  _ <- validateParams q
  _ <- validateQueryStrict q
  Ok q

--------------------------------------------------------------------------------
-- Convenience Value Constructors
--------------------------------------------------------------------------------

||| Create a text value, checking for injection patterns
public export
text : String -> Result SQLError SQLValue
text s =
  case analyzeForInjection s of
    Critical reason => Err (InjectionDetected (Critical reason) s)
    _ => Ok (SQLText s)

||| Create an integer value
public export
int : Integer -> SQLValue
int = SQLInt

||| Create a natural number value
public export
nat : Nat -> SQLValue
nat = SQLNat

||| Create a boolean value
public export
bool : Bool -> SQLValue
bool = SQLBool

||| Create a NULL value
public export
null : SQLValue
null = SQLNull

||| Create a double value
public export
double : Double -> SQLValue
double = SQLDouble

||| Create a date value
public export
date : Int -> Int -> Int -> SQLValue
date = SQLDate

||| Create a time value
public export
time : Int -> Int -> Int -> SQLValue
time = SQLTime

||| Create a timestamp value
public export
timestamp : Int -> Int -> Int -> Int -> Int -> Int -> SQLValue
timestamp = SQLTimestamp

--------------------------------------------------------------------------------
-- Common Query Patterns
--------------------------------------------------------------------------------

||| Count rows in a table
public export
countAll : SQLDialect -> String -> Result SQLError ParameterizedQuery
countAll dialect tableName = do
  tbl <- mkIdentifier tableName |> maybeToResult (InvalidIdentifier tableName "Invalid table name")
  let q = MkQuery [Literal ("SELECT COUNT(*) FROM " ++ quoteIdentifier dialect tbl)] [] [] dialect
  Ok q

||| Count rows with condition
public export
countWhere : SQLDialect -> String -> String -> SQLValue -> Result SQLError ParameterizedQuery
countWhere dialect tableName col val = do
  tbl <- mkIdentifier tableName |> maybeToResult (InvalidIdentifier tableName "Invalid table name")
  c <- mkIdentifier col |> maybeToResult (InvalidIdentifier col "Invalid column name")
  let q = MkQuery
            [Literal ("SELECT COUNT(*) FROM " ++ quoteIdentifier dialect tbl ++
                      " WHERE " ++ quoteIdentifier dialect c ++ " = " ++
                      paramPlaceholder dialect 0)]
            [val] [] dialect
  Ok q

||| Check if a record exists
public export
exists : SQLDialect -> String -> String -> SQLValue -> Result SQLError ParameterizedQuery
exists dialect tableName col val = do
  tbl <- mkIdentifier tableName |> maybeToResult (InvalidIdentifier tableName "Invalid table name")
  c <- mkIdentifier col |> maybeToResult (InvalidIdentifier col "Invalid column name")
  let q = MkQuery
            [Literal ("SELECT EXISTS(SELECT 1 FROM " ++ quoteIdentifier dialect tbl ++
                      " WHERE " ++ quoteIdentifier dialect c ++ " = " ++
                      paramPlaceholder dialect 0 ++ ")")]
            [val] [] dialect
  Ok q

||| Find by unique field (returns single row or none)
public export
findBy : SQLDialect -> String -> String -> SQLValue -> Result SQLError ParameterizedQuery
findBy dialect tableName col val = do
  qb <- selectFrom dialect tableName
  qb' <- whereEq col val qb
  build (limit 1 qb')

||| Find all with pagination
public export
findAllPaginated : SQLDialect -> String -> Nat -> Nat -> Result SQLError ParameterizedQuery
findAllPaginated dialect tableName pageNum pageSize = do
  qb <- selectFrom dialect tableName
  build (paginate pageNum pageSize qb)

||| Search with LIKE pattern
public export
searchLike : SQLDialect -> String -> String -> String -> Result SQLError ParameterizedQuery
searchLike dialect tableName col pattern = do
  qb <- selectFrom dialect tableName
  qb' <- whereLike col pattern qb
  build qb'

--------------------------------------------------------------------------------
-- Upsert Patterns (Dialect-Specific)
--------------------------------------------------------------------------------

||| UPSERT (INSERT ... ON CONFLICT) for PostgreSQL
public export
upsertPostgres : String -> List String -> List (String, SQLValue) ->
                 Result SQLError ParameterizedQuery
upsertPostgres tableName conflictCols colVals = do
  tbl <- mkIdentifier tableName |> maybeToResult (InvalidIdentifier tableName "Invalid table name")
  cols <- traverse (\(n, _) => mkIdentifier n |> maybeToResult (InvalidIdentifier n "Invalid column")) colVals
  conflicts <- traverse (\n => mkIdentifier n |> maybeToResult (InvalidIdentifier n "Invalid conflict column")) conflictCols

  let quotedCols = map (quoteIdentifier PostgreSQL) cols
      quotedConflicts = map (quoteIdentifier PostgreSQL) conflicts
      paramCount = length colVals
      placeholders = map (paramPlaceholder PostgreSQL) [0 .. minus paramCount 1]
      -- UPDATE SET clause for non-conflict columns
      updateCols = filter (\c => not (identifierName c `elem` conflictCols)) cols
      updateParts = zipWith (\c, i => quoteIdentifier PostgreSQL c ++ " = EXCLUDED." ++ quoteIdentifier PostgreSQL c)
                            updateCols [0 .. minus (length updateCols) 1]
      updateClause = if null updateParts then "DO NOTHING" else "DO UPDATE SET " ++ join ", " updateParts

      q = "INSERT INTO " ++ quoteIdentifier PostgreSQL tbl ++
          " (" ++ join ", " quotedCols ++ ") VALUES (" ++
          join ", " placeholders ++ ") ON CONFLICT (" ++
          join ", " quotedConflicts ++ ") " ++ updateClause

      vals = map snd colVals
  Ok (MkQuery [Literal q] vals [] PostgreSQL)

||| UPSERT (INSERT ... ON DUPLICATE KEY) for MySQL
public export
upsertMySQL : String -> List (String, SQLValue) -> Result SQLError ParameterizedQuery
upsertMySQL tableName colVals = do
  tbl <- mkIdentifier tableName |> maybeToResult (InvalidIdentifier tableName "Invalid table name")
  cols <- traverse (\(n, _) => mkIdentifier n |> maybeToResult (InvalidIdentifier n "Invalid column")) colVals

  let quotedCols = map (quoteIdentifier MySQL) cols
      paramCount = length colVals
      placeholders = map (paramPlaceholder MySQL) [0 .. minus paramCount 1]
      updateParts = map (\c => quoteIdentifier MySQL c ++ " = VALUES(" ++ quoteIdentifier MySQL c ++ ")") cols

      q = "INSERT INTO " ++ quoteIdentifier MySQL tbl ++
          " (" ++ join ", " quotedCols ++ ") VALUES (" ++
          join ", " placeholders ++ ") ON DUPLICATE KEY UPDATE " ++
          join ", " updateParts

      vals = map snd colVals
  Ok (MkQuery [Literal q] vals [] MySQL)

--------------------------------------------------------------------------------
-- Transaction Helpers (Query Construction Only)
--------------------------------------------------------------------------------

||| Begin transaction query
public export
beginTransaction : SQLDialect -> ParameterizedQuery
beginTransaction dialect =
  case dialect of
    MSSQL => MkQuery [Literal "BEGIN TRANSACTION"] [] [] dialect
    _ => MkQuery [Literal "BEGIN"] [] [] dialect

||| Commit transaction query
public export
commitTransaction : SQLDialect -> ParameterizedQuery
commitTransaction dialect = MkQuery [Literal "COMMIT"] [] [] dialect

||| Rollback transaction query
public export
rollbackTransaction : SQLDialect -> ParameterizedQuery
rollbackTransaction dialect = MkQuery [Literal "ROLLBACK"] [] [] dialect

||| Savepoint query
public export
savepoint : SQLDialect -> String -> Result SQLError ParameterizedQuery
savepoint dialect name = do
  ident <- mkIdentifier name |> maybeToResult (InvalidIdentifier name "Invalid savepoint name")
  case dialect of
    MSSQL => Ok (MkQuery [Literal ("SAVE TRANSACTION " ++ identifierName ident)] [] [] dialect)
    _ => Ok (MkQuery [Literal ("SAVEPOINT " ++ identifierName ident)] [] [] dialect)

||| Rollback to savepoint query
public export
rollbackToSavepoint : SQLDialect -> String -> Result SQLError ParameterizedQuery
rollbackToSavepoint dialect name = do
  ident <- mkIdentifier name |> maybeToResult (InvalidIdentifier name "Invalid savepoint name")
  case dialect of
    MSSQL => Ok (MkQuery [Literal ("ROLLBACK TRANSACTION " ++ identifierName ident)] [] [] dialect)
    _ => Ok (MkQuery [Literal ("ROLLBACK TO SAVEPOINT " ++ identifierName ident)] [] [] dialect)

--------------------------------------------------------------------------------
-- Safety Information
--------------------------------------------------------------------------------

||| Get safety status of a query
public export
data QuerySafetyStatus : Type where
  ||| Query is fully safe (parameterized, validated)
  FullySafe : QuerySafetyStatus
  ||| Query is safe but uses raw SQL fragments
  SafeWithRaw : (rawFragments : List String) -> QuerySafetyStatus
  ||| Query has validation warnings
  SafeWithWarnings : (warnings : List String) -> QuerySafetyStatus

||| Analyze query safety
public export
analyzeSafety : ParameterizedQuery -> QuerySafetyStatus
analyzeSafety q =
  let raws = mapMaybe getRaw q.params
      textWarnings = mapMaybe checkText q.params
  in case (raws, textWarnings) of
       ([], []) => FullySafe
       (rs, []) => SafeWithRaw rs
       ([], ws) => SafeWithWarnings ws
       (rs, ws) => SafeWithWarnings (rs ++ ws)
  where
    getRaw : SQLValue -> Maybe String
    getRaw (SQLRaw s) = Just s
    getRaw _ = Nothing

    checkText : SQLValue -> Maybe String
    checkText (SQLText s) =
      case analyzeForInjection s of
        Warning reason => Just reason
        _ => Nothing
    checkText _ = Nothing

||| Pretty-print a query for logging (with values masked)
public export
toLogString : ParameterizedQuery -> String
toLogString q =
  let sql = renderQuery q
      paramCount = length q.params
      namedCount = length q.namedParams
  in sql ++ " [" ++ show paramCount ++ " positional, " ++ show namedCount ++ " named params]"
