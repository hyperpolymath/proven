-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
||| Parameter handling and value escaping for safe SQL
|||
||| This module provides functions for escaping SQL values and
||| rendering parameterized queries to safe SQL strings.
module Proven.SafeSQL.Params

import Proven.Core
import Proven.SafeSQL.Types
import Data.List
import Data.String

%default total

--------------------------------------------------------------------------------
-- Value Escaping
--------------------------------------------------------------------------------

||| Escape a string value for SQL (dialect-specific)
public export
escapeString : SQLDialect -> String -> String
escapeString dialect s = "'" ++ escape (unpack s) ++ "'"
  where
    escape : List Char -> String
    escape [] = ""
    escape ('\'' :: cs) = "''" ++ escape cs  -- Double single quotes (universal)
    escape ('\\' :: cs) =
      case dialect of
        MySQL => "\\\\" ++ escape cs      -- MySQL needs escaped backslash
        PostgreSQL => "\\\\" ++ escape cs -- PostgreSQL standard_conforming_strings=off
        _ => "\\" ++ escape cs            -- Others pass through
    escape ('\x00' :: cs) = escape cs      -- Remove null bytes (critical!)
    escape ('\n' :: cs) =
      case dialect of
        MSSQL => "' + CHAR(10) + '" ++ escape cs
        _ => "\\n" ++ escape cs
    escape ('\r' :: cs) =
      case dialect of
        MSSQL => "' + CHAR(13) + '" ++ escape cs
        _ => "\\r" ++ escape cs
    escape (c :: cs) = singleton c ++ escape cs

||| Escape a string for LIKE pattern matching
public export
escapeLikePattern : SQLDialect -> String -> String
escapeLikePattern dialect s = pack (escape (unpack s))
  where
    escapeChar : Char
    escapeChar = case dialect of
      MSSQL => '['   -- MSSQL uses [] for escaping
      _ => '\\'      -- Standard SQL escape

    escape : List Char -> List Char
    escape [] = []
    escape ('%' :: cs) =
      case dialect of
        MSSQL => '[' :: '%' :: ']' :: escape cs
        _ => escapeChar :: '%' :: escape cs
    escape ('_' :: cs) =
      case dialect of
        MSSQL => '[' :: '_' :: ']' :: escape cs
        _ => escapeChar :: '_' :: escape cs
    escape ('\\' :: cs) =
      case dialect of
        MSSQL => '[' :: '\\' :: ']' :: escape cs
        _ => '\\' :: '\\' :: escape cs
    escape ('[' :: cs) =
      case dialect of
        MSSQL => '[' :: '[' :: ']' :: escape cs
        _ => '[' :: escape cs
    escape (c :: cs) = c :: escape cs

||| Quote an identifier appropriately for the dialect
public export
quoteIdentifier : SQLDialect -> SafeIdentifier -> String
quoteIdentifier dialect (MkSafeIdentifier name) =
  case dialect of
    PostgreSQL => "\"" ++ escapeIdent name ++ "\""
    MySQL => "`" ++ escapeIdent name ++ "`"
    SQLite => "\"" ++ escapeIdent name ++ "\""
    MSSQL => "[" ++ escapeIdentMSSQL name ++ "]"
    Oracle => "\"" ++ escapeIdent name ++ "\""
  where
    escapeIdent : String -> String
    escapeIdent s = pack (go (unpack s))
      where
        go : List Char -> List Char
        go [] = []
        go ('"' :: cs) = '"' :: '"' :: go cs  -- Double quotes
        go ('`' :: cs) = '`' :: '`' :: go cs  -- Double backticks
        go (c :: cs) = c :: go cs

    escapeIdentMSSQL : String -> String
    escapeIdentMSSQL s = pack (go (unpack s))
      where
        go : List Char -> List Char
        go [] = []
        go (']' :: cs) = ']' :: ']' :: go cs  -- Double right bracket
        go (c :: cs) = c :: go cs

||| Render a parameter placeholder for the dialect
public export
paramPlaceholder : SQLDialect -> Nat -> String
paramPlaceholder PostgreSQL n = "$" ++ show (n + 1)
paramPlaceholder MySQL _ = "?"
paramPlaceholder SQLite n = "?" ++ show (n + 1)
paramPlaceholder MSSQL n = "@p" ++ show (n + 1)
paramPlaceholder Oracle n = ":" ++ show (n + 1)

||| Render a named parameter placeholder
public export
namedParamPlaceholder : SQLDialect -> String -> String
namedParamPlaceholder PostgreSQL name = "$" ++ name  -- Not standard, would need custom
namedParamPlaceholder MySQL name = "?" -- MySQL doesn't support named params natively
namedParamPlaceholder SQLite name = ":" ++ name
namedParamPlaceholder MSSQL name = "@" ++ name
namedParamPlaceholder Oracle name = ":" ++ name

--------------------------------------------------------------------------------
-- Value Rendering
--------------------------------------------------------------------------------

||| Convert bytes to hex string
hexEncode : List Bits8 -> String
hexEncode bytes = "0x" ++ concat (map toHex bytes)
  where
    hexDigit : Bits8 -> Char
    hexDigit b = if b < 10
                   then chr (cast (ord '0' + cast b))
                   else chr (cast (ord 'A' + cast b - 10))

    toHex : Bits8 -> String
    toHex b = pack [hexDigit (b `div` 16), hexDigit (b `mod` 16)]

||| Pad a number string to a minimum width with leading zeros
padNum : Nat -> Int -> String
padNum width n =
  let s = if n < 0 then "-" ++ show (abs n) else show n
      padding = if length s < width then pack (replicate (minus width (length s)) '0') else ""
  in padding ++ s

||| Render a SQL value to its literal representation
public export
renderValue : SQLDialect -> SQLValue -> String
renderValue _ SQLNull = "NULL"
renderValue _ (SQLBool True) = "TRUE"
renderValue _ (SQLBool False) = "FALSE"
renderValue _ (SQLInt i) = show i
renderValue _ (SQLNat n) = show n
renderValue _ (SQLDouble d) = show d
renderValue dialect (SQLText s) = escapeString dialect s
renderValue dialect (SQLBlob bytes) =
  case dialect of
    PostgreSQL => "'\\x" ++ concat (map toHex bytes) ++ "'"
    MySQL => hexEncode bytes
    SQLite => "X'" ++ concat (map toHex bytes) ++ "'"
    MSSQL => hexEncode bytes
    Oracle => "HEXTORAW('" ++ concat (map toHex bytes) ++ "')"
  where
    toHex : Bits8 -> String
    toHex b =
      let hexDigit = \x => if x < 10 then chr (cast (ord '0' + cast x)) else chr (cast (ord 'A' + cast x - 10))
      in pack [hexDigit (b `div` 16), hexDigit (b `mod` 16)]
renderValue dialect (SQLDate year month day) =
  case dialect of
    MSSQL => "CAST('" ++ padNum 4 year ++ "-" ++ padNum 2 month ++ "-" ++ padNum 2 day ++ "' AS DATE)"
    _ => "'" ++ padNum 4 year ++ "-" ++ padNum 2 month ++ "-" ++ padNum 2 day ++ "'"
renderValue dialect (SQLTime hour minute second) =
  case dialect of
    MSSQL => "CAST('" ++ padNum 2 hour ++ ":" ++ padNum 2 minute ++ ":" ++ padNum 2 second ++ "' AS TIME)"
    _ => "'" ++ padNum 2 hour ++ ":" ++ padNum 2 minute ++ ":" ++ padNum 2 second ++ "'"
renderValue dialect (SQLTimestamp year month day hour minute second) =
  let dateStr = padNum 4 year ++ "-" ++ padNum 2 month ++ "-" ++ padNum 2 day
      timeStr = padNum 2 hour ++ ":" ++ padNum 2 minute ++ ":" ++ padNum 2 second
  in case dialect of
       MSSQL => "CAST('" ++ dateStr ++ " " ++ timeStr ++ "' AS DATETIME2)"
       PostgreSQL => "'" ++ dateStr ++ " " ++ timeStr ++ "'::timestamp"
       _ => "'" ++ dateStr ++ " " ++ timeStr ++ "'"
renderValue _ (SQLRaw trusted) = trusted

--------------------------------------------------------------------------------
-- Query Fragment Rendering
--------------------------------------------------------------------------------

||| Render a query fragment to SQL
public export
renderFragment : SQLDialect -> QueryFragment -> String
renderFragment _ (Literal s) = s
renderFragment dialect (Param n) = paramPlaceholder dialect n
renderFragment dialect (NamedParam name) = namedParamPlaceholder dialect name
renderFragment dialect (Identifier ident) = quoteIdentifier dialect ident

||| Render a complete parameterized query to SQL with placeholders
public export
renderQuery : ParameterizedQuery -> String
renderQuery q = concat (map (renderFragment q.dialect) q.fragments)

||| Render a query with values interpolated (for debugging/logging)
||| WARNING: Do not execute this directly - use parameterized queries
public export
renderQueryInterpolated : ParameterizedQuery -> String
renderQueryInterpolated q = go q.fragments 0
  where
    getParam : Nat -> Maybe SQLValue
    getParam n = index' n q.params
      where
        index' : Nat -> List a -> Maybe a
        index' _ [] = Nothing
        index' Z (x :: _) = Just x
        index' (S k) (_ :: xs) = index' k xs

    getNamedParam : String -> Maybe SQLValue
    getNamedParam name = lookup name q.namedParams

    go : List QueryFragment -> Nat -> String
    go [] _ = ""
    go (Literal s :: rest) idx = s ++ go rest idx
    go (Param n :: rest) idx =
      case getParam n of
        Just val => renderValue q.dialect val ++ go rest idx
        Nothing => "?" ++ show n ++ go rest idx
    go (NamedParam name :: rest) idx =
      case getNamedParam name of
        Just val => renderValue q.dialect val ++ go rest idx
        Nothing => ":" ++ name ++ go rest idx
    go (Identifier ident :: rest) idx =
      quoteIdentifier q.dialect ident ++ go rest idx

--------------------------------------------------------------------------------
-- Parameter Binding
--------------------------------------------------------------------------------

||| Bind positional parameters to a query
public export
bindParams : ParameterizedQuery -> List SQLValue -> ParameterizedQuery
bindParams q vals = { params := vals } q

||| Add a single positional parameter
public export
addParam : SQLValue -> ParameterizedQuery -> ParameterizedQuery
addParam val q = { params := q.params ++ [val] } q

||| Bind named parameters to a query
public export
bindNamedParams : ParameterizedQuery -> List (String, SQLValue) -> ParameterizedQuery
bindNamedParams q vals = { namedParams := vals } q

||| Add a single named parameter
public export
addNamedParam : String -> SQLValue -> ParameterizedQuery -> ParameterizedQuery
addNamedParam name val q = { namedParams := q.namedParams ++ [(name, val)] } q

||| Count the number of parameter placeholders in a query
public export
countParams : ParameterizedQuery -> Nat
countParams q = length (filter isParam q.fragments)
  where
    isParam : QueryFragment -> Bool
    isParam (Param _) = True
    isParam _ = False

||| Count named parameter placeholders
public export
countNamedParams : ParameterizedQuery -> Nat
countNamedParams q = length (filter isNamed q.fragments)
  where
    isNamed : QueryFragment -> Bool
    isNamed (NamedParam _) = True
    isNamed _ = False

||| Get list of named parameter names used in query
public export
getNamedParamNames : ParameterizedQuery -> List String
getNamedParamNames q = nub (mapMaybe getName q.fragments)
  where
    getName : QueryFragment -> Maybe String
    getName (NamedParam n) = Just n
    getName _ = Nothing

||| Validate that all parameters are bound
public export
validateParams : ParameterizedQuery -> Result SQLError ()
validateParams q =
  let paramCount = countParams q
      boundCount = length q.params
      namedNames = getNamedParamNames q
      boundNames = map fst q.namedParams
      missingNamed = filter (\n => not (n `elem` boundNames)) namedNames
  in if paramCount > boundCount
       then Err (MissingParameter boundCount)
     else case missingNamed of
            (n :: _) => Err (MissingNamedParameter n)
            [] => Ok ()

--------------------------------------------------------------------------------
-- Convenience Constructors
--------------------------------------------------------------------------------

||| Create an empty query for a dialect
public export
emptyQuery : SQLDialect -> ParameterizedQuery
emptyQuery dialect = MkQuery [] [] [] dialect

||| Create a query from literal SQL
public export
literal : SQLDialect -> String -> ParameterizedQuery
literal dialect sql = MkQuery [Literal sql] [] [] dialect

||| Append literal SQL to a query
public export
appendLiteral : String -> ParameterizedQuery -> ParameterizedQuery
appendLiteral sql q = { fragments := q.fragments ++ [Literal sql] } q

||| Append a parameter placeholder to a query
public export
appendParam : ParameterizedQuery -> ParameterizedQuery
appendParam q =
  let idx = countParams q
  in { fragments := q.fragments ++ [Param idx] } q

||| Append a named parameter placeholder
public export
appendNamedParam : String -> ParameterizedQuery -> ParameterizedQuery
appendNamedParam name q = { fragments := q.fragments ++ [NamedParam name] } q

||| Append an identifier
public export
appendIdentifier : SafeIdentifier -> ParameterizedQuery -> ParameterizedQuery
appendIdentifier ident q = { fragments := q.fragments ++ [Identifier ident] } q

||| Combine two queries
public export
combineQueries : ParameterizedQuery -> ParameterizedQuery -> ParameterizedQuery
combineQueries q1 q2 =
  MkQuery
    (q1.fragments ++ q2.fragments)
    (q1.params ++ q2.params)
    (q1.namedParams ++ q2.namedParams)
    q1.dialect
