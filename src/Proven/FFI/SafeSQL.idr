-- SPDX-License-Identifier: Apache-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell <jonathan.jewell@open.ac.uk>
||| FFI exports for SafeSQL operations
|||
||| This module exports SQL injection prevention to the C ABI via Idris2's RefC backend.
||| All functions are proven total and use parameterized queries to prevent injection.
|||
||| Return conventions:
||| - Result ParameterizedQuery → (status: Int, sql/error: String, paramCount: Int)
|||   - status = 0: Success, sql is parameterized query string
|||   - status = 1: Error, sql contains error message
||| - SQLValue validation → (status: Int, error: String)
||| - SQLDialect → Int (0=PostgreSQL, 1=MySQL, 2=SQLite, 3=MSSQL, 4=Oracle)
|||
||| CRITICAL: Always use parameterized queries. Never concatenate user input into SQL.
module Proven.FFI.SafeSQL

import Proven.SafeSQL
import Proven.SafeSQL.Types
import Proven.SafeSQL.Params
import Proven.SafeSQL.Builder
import Proven.Core

%default total

--------------------------------------------------------------------------------
-- FFI Result Encoding
--------------------------------------------------------------------------------

||| Encode Bool as Int
encodeBool : Bool -> Int
encodeBool False = 0
encodeBool True = 1

||| Encode SQLDialect as Int
encodeSQLDialect : SQLDialect -> Int
encodeSQLDialect PostgreSQL = 0
encodeSQLDialect MySQL = 1
encodeSQLDialect SQLite = 2
encodeSQLDialect MSSQL = 3
encodeSQLDialect Oracle = 4

||| Decode Int to SQLDialect
decodeSQLDialect : Int -> Maybe SQLDialect
decodeSQLDialect 0 = Just PostgreSQL
decodeSQLDialect 1 = Just MySQL
decodeSQLDialect 2 = Just SQLite
decodeSQLDialect 3 = Just MSSQL
decodeSQLDialect 4 = Just Oracle
decodeSQLDialect _ = Nothing

||| Encode Result ParameterizedQuery as (status, sql, paramCount)
encodeQueryResult : Result SQLError ParameterizedQuery -> (Int, String, Int)
encodeQueryResult (Err err) = (1, friendlyError err, 0)
encodeQueryResult (Ok query) =
  (0, toSQL query, cast (length (getParams query)))

||| Encode Result () as (status, error)
encodeValidationResult : Result SQLError () -> (Int, String)
encodeValidationResult (Err err) = (1, friendlyError err)
encodeValidationResult (Ok ()) = (0, "")

--------------------------------------------------------------------------------
-- SQL Dialect Operations
--------------------------------------------------------------------------------

export
proven_idris_sql_dialect_postgresql : Int
proven_idris_sql_dialect_postgresql = encodeSQLDialect PostgreSQL

export
proven_idris_sql_dialect_mysql : Int
proven_idris_sql_dialect_mysql = encodeSQLDialect MySQL

export
proven_idris_sql_dialect_sqlite : Int
proven_idris_sql_dialect_sqlite = encodeSQLDialect SQLite

export
proven_idris_sql_dialect_mssql : Int
proven_idris_sql_dialect_mssql = encodeSQLDialect MSSQL

export
proven_idris_sql_dialect_oracle : Int
proven_idris_sql_dialect_oracle = encodeSQLDialect Oracle

--------------------------------------------------------------------------------
-- Query Building (Simple Patterns)
--------------------------------------------------------------------------------

export
proven_idris_sql_select_from : Int -> String -> (Int, String, Int)
proven_idris_sql_select_from dialectInt tableName =
  case decodeSQLDialect dialectInt of
    Nothing => (1, "Invalid SQL dialect", 0)
    Just dialect =>
      case selectFrom dialect tableName of
        Err err => (1, friendlyError err, 0)
        Ok builder =>
          case build builder of
            Err err => (1, friendlyError err, 0)
            Ok query => encodeQueryResult (Ok query)

export
proven_idris_sql_count_all : Int -> String -> (Int, String, Int)
proven_idris_sql_count_all dialectInt tableName =
  case decodeSQLDialect dialectInt of
    Nothing => (1, "Invalid SQL dialect", 0)
    Just dialect => encodeQueryResult (countAll dialect tableName)

--------------------------------------------------------------------------------
-- Identifier Validation
--------------------------------------------------------------------------------

export
proven_idris_sql_is_valid_identifier : String -> Int
proven_idris_sql_is_valid_identifier name =
  case mkIdentifier name of
    Nothing => 0
    Just _ => 1

export
proven_idris_sql_validate_table_name : String -> (Int, String)
proven_idris_sql_validate_table_name name =
  case mkTableName name of
    Nothing => (1, "Invalid table name: " ++ name)
    Just _ => (0, name)

export
proven_idris_sql_validate_column_name : String -> (Int, String)
proven_idris_sql_validate_column_name name =
  case mkColumnName name of
    Nothing => (1, "Invalid column name: " ++ name)
    Just _ => (0, name)

--------------------------------------------------------------------------------
-- Injection Analysis
--------------------------------------------------------------------------------

export
proven_idris_sql_analyze_for_injection : String -> Int
proven_idris_sql_analyze_for_injection text =
  case analyzeForInjection text of
    Safe => 0           -- Safe
    Warning _ => 1      -- Warning (suspicious but might be OK)
    Critical _ => 2     -- Critical (definitely injection)

export
proven_idris_sql_has_sql_keywords : String -> Int
proven_idris_sql_has_sql_keywords text =
  encodeBool (hasSQLKeywords text)

export
proven_idris_sql_has_comment_syntax : String -> Int
proven_idris_sql_has_comment_syntax text =
  encodeBool (hasCommentSyntax text)

export
proven_idris_sql_has_string_escape : String -> Int
proven_idris_sql_has_string_escape text =
  encodeBool (hasStringEscape text)

--------------------------------------------------------------------------------
-- Error Classification
--------------------------------------------------------------------------------

export
proven_idris_sql_is_injection_error : String -> Int
proven_idris_sql_is_injection_error errorMsg =
  if isInfixOf "injection" (toLower errorMsg)
    then 1
    else 0

export
proven_idris_sql_is_identifier_error : String -> Int
proven_idris_sql_is_identifier_error errorMsg =
  if isInfixOf "identifier" (toLower errorMsg) || isInfixOf "table" (toLower errorMsg) || isInfixOf "column" (toLower errorMsg)
    then 1
    else 0

--------------------------------------------------------------------------------
-- Constants
--------------------------------------------------------------------------------

export
proven_idris_sql_max_identifier_length : Int
proven_idris_sql_max_identifier_length = cast maxIdentifierLength

export
proven_idris_sql_max_query_length : Int
proven_idris_sql_max_query_length = cast maxQueryLength

export
proven_idris_sql_max_param_count : Int
proven_idris_sql_max_param_count = cast maxParamCount
