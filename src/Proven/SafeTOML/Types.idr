-- SPDX-License-Identifier: Apache-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
||| TOML types and security constraints
|||
||| This module defines types for safe TOML processing including:
||| - TOML value representation
||| - Security options to prevent resource exhaustion
||| - Error types for parsing failures
module Proven.SafeTOML.Types

import Proven.Core
import Data.List
import Data.String

%default total

--------------------------------------------------------------------------------
-- TOML Value Types
--------------------------------------------------------------------------------

||| TOML datetime representation
public export
record TOMLDateTime where
  constructor MkTOMLDateTime
  year : Integer
  month : Nat
  day : Nat
  hour : Nat
  minute : Nat
  second : Nat
  millisecond : Nat
  timezone : Maybe String  -- Nothing = local, Just "Z" = UTC, Just "+05:00" etc.

public export
Show TOMLDateTime where
  show dt =
    let dateStr = show dt.year ++ "-" ++
                  padZero dt.month ++ "-" ++
                  padZero dt.day
        timeStr = padZero dt.hour ++ ":" ++
                  padZero dt.minute ++ ":" ++
                  padZero dt.second
        tzStr = case dt.timezone of
                  Nothing => ""
                  Just tz => tz
    in dateStr ++ "T" ++ timeStr ++ tzStr
  where
    padZero : Nat -> String
    padZero n = if n < 10 then "0" ++ show n else show n

public export
Eq TOMLDateTime where
  dt1 == dt2 = dt1.year == dt2.year &&
               dt1.month == dt2.month &&
               dt1.day == dt2.day &&
               dt1.hour == dt2.hour &&
               dt1.minute == dt2.minute &&
               dt1.second == dt2.second &&
               dt1.millisecond == dt2.millisecond &&
               dt1.timezone == dt2.timezone

||| TOML local date (no time)
public export
record TOMLDate where
  constructor MkTOMLDate
  year : Integer
  month : Nat
  day : Nat

public export
Show TOMLDate where
  show d = show d.year ++ "-" ++ padZero d.month ++ "-" ++ padZero d.day
    where
      padZero : Nat -> String
      padZero n = if n < 10 then "0" ++ show n else show n

public export
Eq TOMLDate where
  d1 == d2 = d1.year == d2.year && d1.month == d2.month && d1.day == d2.day

||| TOML local time (no date)
public export
record TOMLTime where
  constructor MkTOMLTime
  hour : Nat
  minute : Nat
  second : Nat
  millisecond : Nat

public export
Show TOMLTime where
  show t = padZero t.hour ++ ":" ++ padZero t.minute ++ ":" ++ padZero t.second
    where
      padZero : Nat -> String
      padZero n = if n < 10 then "0" ++ show n else show n

public export
Eq TOMLTime where
  t1 == t2 = t1.hour == t2.hour &&
             t1.minute == t2.minute &&
             t1.second == t2.second &&
             t1.millisecond == t2.millisecond

||| TOML value representation
public export
data TOMLValue : Type where
  ||| String value
  TString : String -> TOMLValue

  ||| Integer value
  TInt : Integer -> TOMLValue

  ||| Float value
  TFloat : Double -> TOMLValue

  ||| Boolean value
  TBool : Bool -> TOMLValue

  ||| Offset datetime
  TDateTime : TOMLDateTime -> TOMLValue

  ||| Local date
  TDate : TOMLDate -> TOMLValue

  ||| Local time
  TTime : TOMLTime -> TOMLValue

  ||| Array (homogeneous in TOML 1.0)
  TArray : List TOMLValue -> TOMLValue

  ||| Inline table
  TInlineTable : List (String, TOMLValue) -> TOMLValue

  ||| Table (standard or array of tables)
  TTable : List (String, TOMLValue) -> TOMLValue

public export
Show TOMLValue where
  show (TString s) = show s
  show (TInt i) = show i
  show (TFloat f) = show f
  show (TBool True) = "true"
  show (TBool False) = "false"
  show (TDateTime dt) = show dt
  show (TDate d) = show d
  show (TTime t) = show t
  show (TArray xs) = "[" ++ join ", " (map show xs) ++ "]"
    where
      join : String -> List String -> String
      join _ [] = ""
      join _ [x] = x
      join sep (x :: xs) = x ++ sep ++ join sep xs
  show (TInlineTable kvs) = "{" ++ join ", " (map showKV kvs) ++ "}"
    where
      join : String -> List String -> String
      join _ [] = ""
      join _ [x] = x
      join sep (x :: xs) = x ++ sep ++ join sep xs
      showKV : (String, TOMLValue) -> String
      showKV (k, v) = k ++ " = " ++ show v
  show (TTable kvs) = "[table: " ++ show (length kvs) ++ " keys]"

public export
Eq TOMLValue where
  TString a == TString b = a == b
  TInt a == TInt b = a == b
  TFloat a == TFloat b = a == b
  TBool a == TBool b = a == b
  TDateTime a == TDateTime b = a == b
  TDate a == TDate b = a == b
  TTime a == TTime b = a == b
  TArray a == TArray b = assert_total (a == b)
  TInlineTable a == TInlineTable b = assert_total (a == b)
  TTable a == TTable b = assert_total (a == b)
  _ == _ = False

--------------------------------------------------------------------------------
-- TOML Document
--------------------------------------------------------------------------------

||| A TOML document (root table)
public export
TOMLDocument : Type
TOMLDocument = List (String, TOMLValue)

--------------------------------------------------------------------------------
-- TOML Errors
--------------------------------------------------------------------------------

||| TOML processing errors
public export
data TOMLError : Type where
  ||| Invalid TOML syntax
  SyntaxError : (message : String) -> (line : Nat) -> (col : Nat) -> TOMLError

  ||| Duplicate key
  DuplicateKey : (key : String) -> TOMLError

  ||| Invalid key format
  InvalidKey : (key : String) -> (reason : String) -> TOMLError

  ||| Invalid value format
  InvalidValue : (value : String) -> (expectedType : String) -> TOMLError

  ||| Heterogeneous array (pre-TOML 1.1)
  HeterogeneousArray : (line : Nat) -> TOMLError

  ||| Nesting too deep
  NestingTooDeep : (depth : Nat) -> (limit : Nat) -> TOMLError

  ||| Key too long
  KeyTooLong : (length : Nat) -> (limit : Nat) -> TOMLError

  ||| Value too large
  ValueTooLarge : (size : Nat) -> (limit : Nat) -> TOMLError

  ||| Too many keys
  TooManyKeys : (count : Nat) -> (limit : Nat) -> TOMLError

  ||| Invalid datetime
  InvalidDateTime : (value : String) -> TOMLError

  ||| Redefining table
  TableRedefinition : (name : String) -> TOMLError

  ||| Type mismatch
  TypeMismatch : (expected : String) -> (actual : String) -> TOMLError

public export
Show TOMLError where
  show (SyntaxError msg line col) =
    "SyntaxError at line " ++ show line ++ ", col " ++ show col ++ ": " ++ msg
  show (DuplicateKey key) = "DuplicateKey: " ++ key
  show (InvalidKey key reason) = "InvalidKey '" ++ key ++ "': " ++ reason
  show (InvalidValue val expected) =
    "InvalidValue: '" ++ val ++ "' (expected " ++ expected ++ ")"
  show (HeterogeneousArray line) = "HeterogeneousArray at line " ++ show line
  show (NestingTooDeep depth limit) =
    "NestingTooDeep: " ++ show depth ++ " > " ++ show limit
  show (KeyTooLong length limit) =
    "KeyTooLong: " ++ show length ++ " > " ++ show limit
  show (ValueTooLarge size limit) =
    "ValueTooLarge: " ++ show size ++ " > " ++ show limit
  show (TooManyKeys count limit) =
    "TooManyKeys: " ++ show count ++ " > " ++ show limit
  show (InvalidDateTime val) = "InvalidDateTime: " ++ val
  show (TableRedefinition name) = "TableRedefinition: [" ++ name ++ "]"
  show (TypeMismatch expected actual) =
    "TypeMismatch: expected " ++ expected ++ ", got " ++ actual

public export
Eq TOMLError where
  SyntaxError m1 l1 c1 == SyntaxError m2 l2 c2 = m1 == m2 && l1 == l2 && c1 == c2
  DuplicateKey k1 == DuplicateKey k2 = k1 == k2
  InvalidKey k1 r1 == InvalidKey k2 r2 = k1 == k2 && r1 == r2
  InvalidValue v1 e1 == InvalidValue v2 e2 = v1 == v2 && e1 == e2
  HeterogeneousArray l1 == HeterogeneousArray l2 = l1 == l2
  NestingTooDeep d1 l1 == NestingTooDeep d2 l2 = d1 == d2 && l1 == l2
  KeyTooLong l1 m1 == KeyTooLong l2 m2 = l1 == l2 && m1 == m2
  ValueTooLarge s1 l1 == ValueTooLarge s2 l2 = s1 == s2 && l1 == l2
  TooManyKeys c1 l1 == TooManyKeys c2 l2 = c1 == c2 && l1 == l2
  InvalidDateTime v1 == InvalidDateTime v2 = v1 == v2
  TableRedefinition n1 == TableRedefinition n2 = n1 == n2
  TypeMismatch e1 a1 == TypeMismatch e2 a2 = e1 == e2 && a1 == a2
  _ == _ = False

||| Result type for TOML operations
public export
TOMLResult : Type -> Type
TOMLResult = Result TOMLError

--------------------------------------------------------------------------------
-- Security Options
--------------------------------------------------------------------------------

||| Security options for TOML parsing
public export
record TOMLSecurityOptions where
  constructor MkTOMLSecurityOptions
  ||| Maximum nesting depth
  maxDepth : Nat
  ||| Maximum key length
  maxKeyLength : Nat
  ||| Maximum string value size
  maxValueSize : Nat
  ||| Maximum number of keys total
  maxTotalKeys : Nat
  ||| Maximum array length
  maxArrayLength : Nat
  ||| Allow heterogeneous arrays (TOML 1.1+)
  allowHeterogeneousArrays : Bool
  ||| Allow multiline basic strings
  allowMultilineStrings : Bool
  ||| Allow literal strings
  allowLiteralStrings : Bool

||| Secure defaults
public export
secureDefaults : TOMLSecurityOptions
secureDefaults = MkTOMLSecurityOptions
  { maxDepth = 50
  , maxKeyLength = 1024
  , maxValueSize = 1048576  -- 1MB
  , maxTotalKeys = 10000
  , maxArrayLength = 10000
  , allowHeterogeneousArrays = False  -- TOML 1.0 strict
  , allowMultilineStrings = True
  , allowLiteralStrings = True
  }

||| Permissive options for trusted input
public export
permissiveOptions : TOMLSecurityOptions
permissiveOptions = MkTOMLSecurityOptions
  { maxDepth = 200
  , maxKeyLength = 10240
  , maxValueSize = 104857600  -- 100MB
  , maxTotalKeys = 1000000
  , maxArrayLength = 1000000
  , allowHeterogeneousArrays = True  -- TOML 1.1 style
  , allowMultilineStrings = True
  , allowLiteralStrings = True
  }

||| Strict options for untrusted input
public export
strictOptions : TOMLSecurityOptions
strictOptions = MkTOMLSecurityOptions
  { maxDepth = 20
  , maxKeyLength = 256
  , maxValueSize = 65536  -- 64KB
  , maxTotalKeys = 1000
  , maxArrayLength = 1000
  , allowHeterogeneousArrays = False
  , allowMultilineStrings = True
  , allowLiteralStrings = True
  }

--------------------------------------------------------------------------------
-- Type Helpers
--------------------------------------------------------------------------------

||| Get the type name of a TOML value
public export
tomlTypeName : TOMLValue -> String
tomlTypeName (TString _) = "string"
tomlTypeName (TInt _) = "integer"
tomlTypeName (TFloat _) = "float"
tomlTypeName (TBool _) = "boolean"
tomlTypeName (TDateTime _) = "datetime"
tomlTypeName (TDate _) = "date"
tomlTypeName (TTime _) = "time"
tomlTypeName (TArray _) = "array"
tomlTypeName (TInlineTable _) = "inline-table"
tomlTypeName (TTable _) = "table"

||| Check if value is a scalar
public export
isScalar : TOMLValue -> Bool
isScalar (TArray _) = False
isScalar (TInlineTable _) = False
isScalar (TTable _) = False
isScalar _ = True

||| Check if value is a table type
public export
isTable : TOMLValue -> Bool
isTable (TInlineTable _) = True
isTable (TTable _) = True
isTable _ = False

||| Check if value is an array
public export
isArray : TOMLValue -> Bool
isArray (TArray _) = True
isArray _ = False

--------------------------------------------------------------------------------
-- Value Constructors
--------------------------------------------------------------------------------

||| Create string value
public export
str : String -> TOMLValue
str = TString

||| Create integer value
public export
int : Integer -> TOMLValue
int = TInt

||| Create float value
public export
float : Double -> TOMLValue
float = TFloat

||| Create boolean value
public export
bool : Bool -> TOMLValue
bool = TBool

||| Create array value
public export
array : List TOMLValue -> TOMLValue
array = TArray

||| Create inline table value
public export
inlineTable : List (String, TOMLValue) -> TOMLValue
inlineTable = TInlineTable

||| Create table value
public export
table : List (String, TOMLValue) -> TOMLValue
table = TTable

||| Create datetime value
public export
datetime : TOMLDateTime -> TOMLValue
datetime = TDateTime

||| Create date value
public export
date : TOMLDate -> TOMLValue
date = TDate

||| Create time value
public export
time : TOMLTime -> TOMLValue
time = TTime

--------------------------------------------------------------------------------
-- Key Validation
--------------------------------------------------------------------------------

||| Valid bare key characters
public export
isValidBareKeyChar : Char -> Bool
isValidBareKeyChar c =
  isAlphaNum c || c == '_' || c == '-'

||| Check if string is valid bare key
public export
isValidBareKey : String -> Bool
isValidBareKey s =
  not (null (unpack s)) && all isValidBareKeyChar (unpack s)

||| Check if key needs quoting
public export
needsQuoting : String -> Bool
needsQuoting s = not (isValidBareKey s)
