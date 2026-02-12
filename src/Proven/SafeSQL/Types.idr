-- SPDX-License-Identifier: Apache-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
||| Core types for safe SQL operations
|||
||| This module provides the foundational types for parameterized queries,
||| SQL values, and query components that prevent SQL injection attacks.
module Proven.SafeSQL.Types

import Proven.Core
import Data.List
import Data.String

%default total

--------------------------------------------------------------------------------
-- SQL Dialects
--------------------------------------------------------------------------------

||| Supported SQL dialects with their specific escaping rules
public export
data SQLDialect : Type where
  ||| PostgreSQL - uses $1, $2 for params, " for identifiers
  PostgreSQL : SQLDialect
  ||| MySQL - uses ? for params, ` for identifiers
  MySQL : SQLDialect
  ||| SQLite - uses ? or ?1, ?2 for params, " or ` for identifiers
  SQLite : SQLDialect
  ||| Microsoft SQL Server - uses @p1, @p2 for params, [] for identifiers
  MSSQL : SQLDialect
  ||| Oracle - uses :1, :2 for params, " for identifiers
  Oracle : SQLDialect

public export
Eq SQLDialect where
  PostgreSQL == PostgreSQL = True
  MySQL == MySQL = True
  SQLite == SQLite = True
  MSSQL == MSSQL = True
  Oracle == Oracle = True
  _ == _ = False

public export
Show SQLDialect where
  show PostgreSQL = "PostgreSQL"
  show MySQL = "MySQL"
  show SQLite = "SQLite"
  show MSSQL = "MSSQL"
  show Oracle = "Oracle"

--------------------------------------------------------------------------------
-- SQL Values (Type-Safe Parameters)
--------------------------------------------------------------------------------

||| SQL-safe value types that can be safely interpolated
||| Each value is tagged with its type for proper escaping
public export
data SQLValue : Type where
  ||| NULL value
  SQLNull : SQLValue
  ||| Boolean value
  SQLBool : Bool -> SQLValue
  ||| Integer value (no escaping needed)
  SQLInt : Integer -> SQLValue
  ||| Natural number (non-negative)
  SQLNat : Nat -> SQLValue
  ||| Double/Float value
  SQLDouble : Double -> SQLValue
  ||| Text value (will be properly escaped and quoted)
  SQLText : String -> SQLValue
  ||| Binary data (will be hex-encoded)
  SQLBlob : List Bits8 -> SQLValue
  ||| Date in ISO format (YYYY-MM-DD)
  SQLDate : (year : Int) -> (month : Int) -> (day : Int) -> SQLValue
  ||| Time in ISO format (HH:MM:SS)
  SQLTime : (hour : Int) -> (minute : Int) -> (second : Int) -> SQLValue
  ||| Timestamp in ISO format
  SQLTimestamp : (year : Int) -> (month : Int) -> (day : Int) ->
                 (hour : Int) -> (minute : Int) -> (second : Int) -> SQLValue
  ||| Raw SQL expression (use with extreme caution - must be trusted)
  SQLRaw : (trusted : String) -> SQLValue

public export
Eq SQLValue where
  SQLNull == SQLNull = True
  SQLBool b1 == SQLBool b2 = b1 == b2
  SQLInt i1 == SQLInt i2 = i1 == i2
  SQLNat n1 == SQLNat n2 = n1 == n2
  SQLDouble d1 == SQLDouble d2 = d1 == d2
  SQLText t1 == SQLText t2 = t1 == t2
  SQLBlob b1 == SQLBlob b2 = b1 == b2
  SQLDate y1 m1 d1 == SQLDate y2 m2 d2 = y1 == y2 && m1 == m2 && d1 == d2
  SQLTime h1 m1 s1 == SQLTime h2 m2 s2 = h1 == h2 && m1 == m2 && s1 == s2
  SQLTimestamp y1 mo1 d1 h1 mi1 s1 == SQLTimestamp y2 mo2 d2 h2 mi2 s2 =
    y1 == y2 && mo1 == mo2 && d1 == d2 && h1 == h2 && mi1 == mi2 && s1 == s2
  SQLRaw r1 == SQLRaw r2 = r1 == r2
  _ == _ = False

--------------------------------------------------------------------------------
-- Safe Identifier
--------------------------------------------------------------------------------

||| A validated SQL identifier (table name, column name, etc.)
||| Only allows alphanumeric characters and underscores
public export
data SafeIdentifier : Type where
  ||| Create a validated identifier
  MkSafeIdentifier : (name : String) -> SafeIdentifier

public export
Eq SafeIdentifier where
  MkSafeIdentifier n1 == MkSafeIdentifier n2 = n1 == n2

public export
Show SafeIdentifier where
  show (MkSafeIdentifier n) = n

||| Extract the raw identifier string
public export
identifierName : SafeIdentifier -> String
identifierName (MkSafeIdentifier n) = n

||| Check if a character is valid in an identifier
public export
isIdentifierChar : Char -> Bool
isIdentifierChar c = isAlphaNum c || c == '_'

||| Check if a string is a valid identifier
||| Must start with letter or underscore, rest alphanumeric or underscore
public export
isValidIdentifier : String -> Bool
isValidIdentifier s =
  case strM s of
    StrNil => False
    StrCons c rest =>
      (isAlpha c || c == '_') &&
      all isIdentifierChar (unpack rest) &&
      length s <= 128  -- Reasonable max length

||| SQL reserved words that cannot be used as identifiers without quoting
public export
sqlReservedWords : List String
sqlReservedWords =
  [ "SELECT", "FROM", "WHERE", "INSERT", "UPDATE", "DELETE", "DROP", "CREATE"
  , "TABLE", "INDEX", "VIEW", "DATABASE", "SCHEMA", "ALTER", "ADD", "COLUMN"
  , "PRIMARY", "KEY", "FOREIGN", "REFERENCES", "CONSTRAINT", "UNIQUE", "CHECK"
  , "DEFAULT", "NULL", "NOT", "AND", "OR", "IN", "BETWEEN", "LIKE", "IS"
  , "JOIN", "LEFT", "RIGHT", "INNER", "OUTER", "CROSS", "ON", "AS", "ORDER"
  , "BY", "ASC", "DESC", "GROUP", "HAVING", "LIMIT", "OFFSET", "UNION", "ALL"
  , "DISTINCT", "COUNT", "SUM", "AVG", "MIN", "MAX", "CASE", "WHEN", "THEN"
  , "ELSE", "END", "EXISTS", "CAST", "COALESCE", "NULLIF", "TRUE", "FALSE"
  ]

||| Check if identifier is a reserved word
public export
isReservedWord : String -> Bool
isReservedWord s = toUpper s `elem` sqlReservedWords

||| Try to create a safe identifier from a string
public export
mkIdentifier : String -> Maybe SafeIdentifier
mkIdentifier s =
  if isValidIdentifier s
    then Just (MkSafeIdentifier s)
    else Nothing

--------------------------------------------------------------------------------
-- Parameterized Query
--------------------------------------------------------------------------------

||| A query fragment - either literal SQL or a parameter placeholder
public export
data QueryFragment : Type where
  ||| Literal SQL text (trusted)
  Literal : String -> QueryFragment
  ||| Positional parameter placeholder ($1, ?, etc.)
  Param : Nat -> QueryFragment
  ||| Named parameter placeholder (:name, @name, etc.)
  NamedParam : String -> QueryFragment
  ||| Safe identifier (will be quoted appropriately)
  Identifier : SafeIdentifier -> QueryFragment

public export
Eq QueryFragment where
  Literal s1 == Literal s2 = s1 == s2
  Param n1 == Param n2 = n1 == n2
  NamedParam n1 == NamedParam n2 = n1 == n2
  Identifier i1 == Identifier i2 = i1 == i2
  _ == _ = False

||| A parameterized SQL query
||| The query is split into fragments, with parameters tracked separately
public export
record ParameterizedQuery where
  constructor MkQuery
  ||| The query fragments in order
  fragments : List QueryFragment
  ||| The parameter values (indexed by position)
  params : List SQLValue
  ||| Named parameter values
  namedParams : List (String, SQLValue)
  ||| The SQL dialect for rendering
  dialect : SQLDialect

public export
Eq ParameterizedQuery where
  q1 == q2 = q1.fragments == q2.fragments &&
             q1.params == q2.params &&
             q1.namedParams == q2.namedParams &&
             q1.dialect == q2.dialect

--------------------------------------------------------------------------------
-- SQL Operator Types
--------------------------------------------------------------------------------

||| Comparison operators
public export
data CompareOp : Type where
  Eq' : CompareOp      -- =
  NotEq : CompareOp    -- <> or !=
  Lt : CompareOp       -- <
  LtEq : CompareOp     -- <=
  Gt : CompareOp       -- >
  GtEq : CompareOp     -- >=
  Like' : CompareOp    -- LIKE
  NotLike : CompareOp  -- NOT LIKE
  ILike : CompareOp    -- ILIKE (PostgreSQL)
  In' : CompareOp      -- IN
  NotIn : CompareOp    -- NOT IN
  IsNull' : CompareOp  -- IS NULL
  IsNotNull : CompareOp -- IS NOT NULL

public export
Show CompareOp where
  show Eq' = "="
  show NotEq = "<>"
  show Lt = "<"
  show LtEq = "<="
  show Gt = ">"
  show GtEq = ">="
  show Like' = "LIKE"
  show NotLike = "NOT LIKE"
  show ILike = "ILIKE"
  show In' = "IN"
  show NotIn = "NOT IN"
  show IsNull' = "IS NULL"
  show IsNotNull = "IS NOT NULL"

||| Logical operators
public export
data LogicalOp : Type where
  And : LogicalOp
  Or : LogicalOp

public export
Show LogicalOp where
  show And = "AND"
  show Or = "OR"

||| Sort direction
public export
data SortDir : Type where
  Asc : SortDir
  Desc : SortDir

public export
Show SortDir where
  show Asc = "ASC"
  show Desc = "DESC"

||| Null handling in ORDER BY
public export
data NullsOrder : Type where
  NullsFirst : NullsOrder
  NullsLast : NullsOrder

public export
Show NullsOrder where
  show NullsFirst = "NULLS FIRST"
  show NullsLast = "NULLS LAST"

--------------------------------------------------------------------------------
-- Query Building Blocks
--------------------------------------------------------------------------------

||| A column reference (possibly qualified with table)
public export
record ColumnRef where
  constructor MkColumnRef
  tableName : Maybe SafeIdentifier
  columnName : SafeIdentifier
  alias : Maybe SafeIdentifier

||| A table reference with optional alias
public export
record TableRef where
  constructor MkTableRef
  schemaName : Maybe SafeIdentifier
  tableName : SafeIdentifier
  alias : Maybe SafeIdentifier

||| A WHERE clause condition
public export
data Condition : Type where
  ||| Simple comparison: column op value
  Compare : ColumnRef -> CompareOp -> SQLValue -> Condition
  ||| Column to column comparison
  CompareColumns : ColumnRef -> CompareOp -> ColumnRef -> Condition
  ||| Combine conditions with AND/OR
  Combine : LogicalOp -> Condition -> Condition -> Condition
  ||| Negate a condition
  Not : Condition -> Condition
  ||| Subquery condition (EXISTS, IN subquery, etc.)
  SubqueryCondition : String -> ParameterizedQuery -> Condition
  ||| Raw condition (trusted SQL)
  RawCondition : String -> List SQLValue -> Condition

||| ORDER BY specification
public export
record OrderSpec where
  constructor MkOrderSpec
  column : ColumnRef
  direction : SortDir
  nullsOrder : Maybe NullsOrder

--------------------------------------------------------------------------------
-- Injection Detection
--------------------------------------------------------------------------------

||| Characters that could indicate SQL injection attempts
public export
dangerousChars : List Char
dangerousChars = ['\'', '"', ';', '-', '/', '*', '\\', '\x00']

||| SQL keywords that could indicate injection when in user input
public export
dangerousKeywords : List String
dangerousKeywords =
  [ "DROP", "DELETE", "TRUNCATE", "INSERT", "UPDATE", "ALTER", "CREATE"
  , "EXEC", "EXECUTE", "XP_", "SP_", "UNION", "SCRIPT", "JAVASCRIPT"
  , "--", "/*", "*/", "@@", "CHAR(", "NCHAR(", "VARCHAR(", "NVARCHAR("
  , "0x", "CONVERT(", "CAST("
  ]

||| Check if a string contains potentially dangerous SQL
public export
containsDangerousSQL : String -> Bool
containsDangerousSQL s =
  let upper = toUpper s
      chars = unpack s
  in any (\c => c `elem` dangerousChars) chars ||
     any (\kw => isInfixOf kw upper) dangerousKeywords

||| Severity of potential injection
public export
data InjectionSeverity : Type where
  ||| No injection detected
  Safe : InjectionSeverity
  ||| Potentially suspicious but might be legitimate
  Warning : (reason : String) -> InjectionSeverity
  ||| Likely injection attempt
  Dangerous : (reason : String) -> InjectionSeverity
  ||| Definite injection attempt (e.g., contains DROP TABLE)
  Critical : (reason : String) -> InjectionSeverity

public export
Show InjectionSeverity where
  show Safe = "Safe"
  show (Warning r) = "Warning: " ++ r
  show (Dangerous r) = "Dangerous: " ++ r
  show (Critical r) = "Critical: " ++ r

||| Analyze a string for injection attempts
public export
analyzeForInjection : String -> InjectionSeverity
analyzeForInjection s =
  let upper = toUpper s
  in if isInfixOf "DROP " upper || isInfixOf "TRUNCATE " upper
       then Critical "Contains destructive SQL command"
     else if isInfixOf "UNION " upper && isInfixOf "SELECT " upper
       then Critical "Possible UNION-based injection"
     else if isInfixOf "--" s || isInfixOf "/*" s
       then Dangerous "Contains SQL comment markers"
     else if isInfixOf "'" s && isInfixOf "OR " upper
       then Dangerous "Possible OR-based injection"
     else if any (\c => c `elem` [';', '\x00']) (unpack s)
       then Dangerous "Contains statement terminators or null bytes"
     else if isInfixOf "'" s
       then Warning "Contains quote characters"
     else Safe

--------------------------------------------------------------------------------
-- Error Types
--------------------------------------------------------------------------------

||| Errors that can occur during SQL operations
public export
data SQLError : Type where
  ||| Invalid identifier (contains illegal characters)
  InvalidIdentifier : (name : String) -> (reason : String) -> SQLError
  ||| Missing parameter
  MissingParameter : (index : Nat) -> SQLError
  ||| Missing named parameter
  MissingNamedParameter : (name : String) -> SQLError
  ||| Type mismatch
  TypeMismatch : (expected : String) -> (got : String) -> SQLError
  ||| Potential injection detected
  InjectionDetected : (severity : InjectionSeverity) -> (input : String) -> SQLError
  ||| Invalid query structure
  InvalidQuery : (reason : String) -> SQLError
  ||| Unsupported operation for dialect
  UnsupportedDialect : (operation : String) -> (dialect : SQLDialect) -> SQLError

public export
Show SQLError where
  show (InvalidIdentifier name reason) =
    "Invalid identifier '" ++ name ++ "': " ++ reason
  show (MissingParameter idx) =
    "Missing parameter at index " ++ show idx
  show (MissingNamedParameter name) =
    "Missing named parameter :" ++ name
  show (TypeMismatch expected got) =
    "Type mismatch: expected " ++ expected ++ ", got " ++ got
  show (InjectionDetected severity input) =
    "Potential SQL injection detected (" ++ show severity ++ "): " ++ input
  show (InvalidQuery reason) =
    "Invalid query: " ++ reason
  show (UnsupportedDialect op dialect) =
    "Operation '" ++ op ++ "' not supported for " ++ show dialect
