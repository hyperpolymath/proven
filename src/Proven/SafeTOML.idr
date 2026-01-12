-- SPDX-License-Identifier: Palimpsest-MPL-1.0
||| SafeTOML - TOML processing with resource limits
|||
||| This module provides safe TOML operations including:
||| - Resource limit enforcement (depth, size, count)
||| - Type-safe value access
||| - DateTime validation
||| - Array homogeneity checking (TOML 1.0)
|||
||| Example usage:
||| ```idris
||| -- Parse TOML safely
||| case parseTOML tomlString of
|||   Ok doc => processDocument doc
|||   Err (NestingTooDeep _ _) => handleDepthAttack
|||   Err e => handleError e
|||
||| -- Access values safely
||| case getField "server" doc of
|||   Ok (TTable server) =>
|||     case getString "host" server of
|||       Ok host => connectTo host
|||       Err e => useDefault
|||   Err e => handleMissingConfig
||| ```
module Proven.SafeTOML

import public Proven.Core
import public Proven.SafeTOML.Types
import public Proven.SafeTOML.Parser
import public Proven.SafeTOML.Proofs

import Data.List
import Data.String

%default total

--------------------------------------------------------------------------------
-- High-Level API
--------------------------------------------------------------------------------

||| Parse TOML with secure defaults
|||
||| This is the recommended way to parse untrusted TOML.
public export
parse : String -> TOMLResult TOMLDocument
parse = parseTOML

||| Parse TOML with custom security options
|||
||| Use this when you need to relax security constraints (trusted input only).
public export
parseWith : TOMLSecurityOptions -> String -> TOMLResult TOMLDocument
parseWith = parseTOMLWith

||| Render TOML document to string
public export
render : TOMLDocument -> String
render = renderDocument

--------------------------------------------------------------------------------
-- Type Coercion
--------------------------------------------------------------------------------

||| Try to get a string value
public export
asString : TOMLValue -> TOMLResult String
asString (TString s) = Ok s
asString val = Err (TypeMismatch "string" (tomlTypeName val))

||| Try to get an integer value
public export
asInt : TOMLValue -> TOMLResult Integer
asInt (TInt i) = Ok i
asInt val = Err (TypeMismatch "integer" (tomlTypeName val))

||| Try to get a float value
public export
asFloat : TOMLValue -> TOMLResult Double
asFloat (TFloat f) = Ok f
asFloat (TInt i) = Ok (cast i)  -- Allow int -> float coercion
asFloat val = Err (TypeMismatch "float" (tomlTypeName val))

||| Try to get a boolean value
public export
asBool : TOMLValue -> TOMLResult Bool
asBool (TBool b) = Ok b
asBool val = Err (TypeMismatch "boolean" (tomlTypeName val))

||| Try to get an array value
public export
asArray : TOMLValue -> TOMLResult (List TOMLValue)
asArray (TArray xs) = Ok xs
asArray val = Err (TypeMismatch "array" (tomlTypeName val))

||| Try to get a table value
public export
asTable : TOMLValue -> TOMLResult (List (String, TOMLValue))
asTable (TTable kvs) = Ok kvs
asTable (TInlineTable kvs) = Ok kvs
asTable val = Err (TypeMismatch "table" (tomlTypeName val))

||| Try to get datetime value
public export
asDateTime : TOMLValue -> TOMLResult TOMLDateTime
asDateTime (TDateTime dt) = Ok dt
asDateTime val = Err (TypeMismatch "datetime" (tomlTypeName val))

||| Try to get date value
public export
asDate : TOMLValue -> TOMLResult TOMLDate
asDate (TDate d) = Ok d
asDate val = Err (TypeMismatch "date" (tomlTypeName val))

||| Try to get time value
public export
asTime : TOMLValue -> TOMLResult TOMLTime
asTime (TTime t) = Ok t
asTime val = Err (TypeMismatch "time" (tomlTypeName val))

--------------------------------------------------------------------------------
-- Document/Table Access
--------------------------------------------------------------------------------

||| Get a field from a document or table
public export
getField : String -> List (String, TOMLValue) -> TOMLResult TOMLValue
getField key kvs =
  case lookup key kvs of
    Just val => Ok val
    Nothing => Err (TypeMismatch ("field '" ++ key ++ "'") "missing")

||| Get a field with default value
public export
getFieldOr : TOMLValue -> String -> List (String, TOMLValue) -> TOMLValue
getFieldOr def key kvs =
  case lookup key kvs of
    Just val => val
    Nothing => def

||| Check if field exists
public export
hasField : String -> List (String, TOMLValue) -> Bool
hasField key kvs = isJust (lookup key kvs)

||| Get nested field using dot notation
public export
getPath : String -> List (String, TOMLValue) -> TOMLResult TOMLValue
getPath path doc = go (split (== '.') path) doc
  where
    go : List String -> List (String, TOMLValue) -> TOMLResult TOMLValue
    go [] _ = Err (InvalidKey "" "empty path")
    go [k] kvs = getField k kvs
    go (k :: ks) kvs = do
      val <- getField k kvs
      table <- asTable val
      go ks table

||| Get all keys
public export
keys : List (String, TOMLValue) -> List String
keys = map fst

||| Get all values
public export
values : List (String, TOMLValue) -> List TOMLValue
values = map snd

--------------------------------------------------------------------------------
-- Typed Field Access
--------------------------------------------------------------------------------

||| Get string field
public export
getString : String -> List (String, TOMLValue) -> TOMLResult String
getString key kvs = do
  val <- getField key kvs
  asString val

||| Get integer field
public export
getInt : String -> List (String, TOMLValue) -> TOMLResult Integer
getInt key kvs = do
  val <- getField key kvs
  asInt val

||| Get float field
public export
getFloat : String -> List (String, TOMLValue) -> TOMLResult Double
getFloat key kvs = do
  val <- getField key kvs
  asFloat val

||| Get boolean field
public export
getBool : String -> List (String, TOMLValue) -> TOMLResult Bool
getBool key kvs = do
  val <- getField key kvs
  asBool val

||| Get array field
public export
getArray : String -> List (String, TOMLValue) -> TOMLResult (List TOMLValue)
getArray key kvs = do
  val <- getField key kvs
  asArray val

||| Get table field
public export
getTable : String -> List (String, TOMLValue) -> TOMLResult (List (String, TOMLValue))
getTable key kvs = do
  val <- getField key kvs
  asTable val

--------------------------------------------------------------------------------
-- Array Access
--------------------------------------------------------------------------------

||| Get array element by index
public export
getIndex : Nat -> TOMLValue -> TOMLResult TOMLValue
getIndex idx (TArray xs) =
  case index' idx xs of
    Just val => Ok val
    Nothing => Err (InvalidValue (show idx) "index out of bounds")
getIndex idx val = Err (TypeMismatch "array" (tomlTypeName val))

||| Get array length
public export
arrayLength : TOMLValue -> Nat
arrayLength (TArray xs) = length xs
arrayLength _ = 0

||| Map over array elements
public export
mapArray : (TOMLValue -> TOMLResult TOMLValue) -> TOMLValue -> TOMLResult TOMLValue
mapArray f (TArray xs) = do
  mapped <- traverse f xs
  Ok (TArray mapped)
mapArray f val = Err (TypeMismatch "array" (tomlTypeName val))

--------------------------------------------------------------------------------
-- Construction
--------------------------------------------------------------------------------

||| Create a document from key-value pairs
public export
mkDocument : List (String, TOMLValue) -> TOMLDocument
mkDocument = id

||| Create a table
public export
mkTable : List (String, TOMLValue) -> TOMLValue
mkTable = TTable

||| Create an inline table
public export
mkInlineTable : List (String, TOMLValue) -> TOMLValue
mkInlineTable = TInlineTable

||| Create an array
public export
mkArray : List TOMLValue -> TOMLValue
mkArray = TArray

||| Create datetime from components
public export
mkDateTime : Integer -> Nat -> Nat -> Nat -> Nat -> Nat -> Maybe String -> TOMLValue
mkDateTime year month day hour minute second tz =
  TDateTime (MkTOMLDateTime year month day hour minute second 0 tz)

||| Create date from components
public export
mkDate : Integer -> Nat -> Nat -> TOMLValue
mkDate year month day = TDate (MkTOMLDate year month day)

||| Create time from components
public export
mkTime : Nat -> Nat -> Nat -> TOMLValue
mkTime hour minute second = TTime (MkTOMLTime hour minute second 0)

--------------------------------------------------------------------------------
-- Transformation
--------------------------------------------------------------------------------

||| Map over all values
public export
mapValues : (TOMLValue -> TOMLValue) -> TOMLValue -> TOMLValue
mapValues f val = case val of
  TArray xs => f (TArray (map (mapValues f) xs))
  TInlineTable kvs => f (TInlineTable (map (\(k, v) => (k, mapValues f v)) kvs))
  TTable kvs => f (TTable (map (\(k, v) => (k, mapValues f v)) kvs))
  other => f other

||| Map over document
public export
mapDocument : (TOMLValue -> TOMLValue) -> TOMLDocument -> TOMLDocument
mapDocument f doc = map (\(k, v) => (k, mapValues f v)) doc

||| Filter document fields
public export
filterFields : (String -> TOMLValue -> Bool) -> TOMLDocument -> TOMLDocument
filterFields pred doc = filter (uncurry pred) doc

||| Merge documents (second wins on conflicts)
public export
mergeDocuments : TOMLDocument -> TOMLDocument -> TOMLDocument
mergeDocuments doc1 doc2 =
  let keys2 = keys doc2
  in filter (\(k, _) => not (k `elem` keys2)) doc1 ++ doc2

--------------------------------------------------------------------------------
-- Validation
--------------------------------------------------------------------------------

||| Check if TOML is safe to process
public export
isSafe : String -> Bool
isSafe toml = isOk (parse toml)

||| Validate TOML structure
public export
validate : String -> TOMLResult ()
validate toml = do
  _ <- parse toml
  Ok ()

--------------------------------------------------------------------------------
-- Security Presets
--------------------------------------------------------------------------------

||| Maximum security (strictest settings)
public export
maxSecurity : TOMLSecurityOptions
maxSecurity = strictOptions

||| Standard security
public export
standardSecurity : TOMLSecurityOptions
standardSecurity = secureDefaults

||| Permissive security (for trusted input only)
public export
permissiveSecurity : TOMLSecurityOptions
permissiveSecurity = permissiveOptions

--------------------------------------------------------------------------------
-- Error Helpers
--------------------------------------------------------------------------------

||| Check if error is resource limit related
public export
isResourceLimitError : TOMLError -> Bool
isResourceLimitError (NestingTooDeep _ _) = True
isResourceLimitError (KeyTooLong _ _) = True
isResourceLimitError (ValueTooLarge _ _) = True
isResourceLimitError (TooManyKeys _ _) = True
isResourceLimitError _ = False

||| Check if error is syntax related
public export
isSyntaxError : TOMLError -> Bool
isSyntaxError (SyntaxError _ _ _) = True
isSyntaxError _ = False

||| Check if error is type related
public export
isTypeError : TOMLError -> Bool
isTypeError (TypeMismatch _ _) = True
isTypeError (InvalidValue _ _) = True
isTypeError (HeterogeneousArray _) = True
isTypeError _ = False

||| Get user-friendly error message
public export
friendlyError : TOMLError -> String
friendlyError (SyntaxError msg line col) =
  "TOML syntax error at line " ++ show line ++ ", column " ++ show col ++ ": " ++ msg
friendlyError (DuplicateKey key) =
  "Duplicate key '" ++ key ++ "' in TOML document."
friendlyError (InvalidKey key reason) =
  "Invalid key '" ++ key ++ "': " ++ reason
friendlyError (InvalidValue val expected) =
  "Invalid value '" ++ val ++ "' (expected " ++ expected ++ ")."
friendlyError (HeterogeneousArray line) =
  "Mixed-type array at line " ++ show line ++ " (TOML 1.0 requires homogeneous arrays)."
friendlyError (NestingTooDeep depth limit) =
  "TOML nesting too deep (" ++ show depth ++ " > " ++ show limit ++ ")."
friendlyError (KeyTooLong length limit) =
  "TOML key too long (" ++ show length ++ " > " ++ show limit ++ " bytes)."
friendlyError (ValueTooLarge size limit) =
  "TOML value too large (" ++ show size ++ " > " ++ show limit ++ " bytes)."
friendlyError (TooManyKeys count limit) =
  "Too many keys in TOML document (" ++ show count ++ " > " ++ show limit ++ ")."
friendlyError (InvalidDateTime val) =
  "Invalid datetime format: " ++ val
friendlyError (TableRedefinition name) =
  "Table [" ++ name ++ "] defined multiple times."
friendlyError (TypeMismatch expected actual) =
  "Type mismatch: expected " ++ expected ++ ", got " ++ actual

--------------------------------------------------------------------------------
-- Debugging
--------------------------------------------------------------------------------

||| Pretty print TOML value for debugging
public export
debugValue : TOMLValue -> String
debugValue (TString s) = "TString(\"" ++ s ++ "\")"
debugValue (TInt i) = "TInt(" ++ show i ++ ")"
debugValue (TFloat f) = "TFloat(" ++ show f ++ ")"
debugValue (TBool b) = "TBool(" ++ show b ++ ")"
debugValue (TDateTime dt) = "TDateTime(" ++ show dt ++ ")"
debugValue (TDate d) = "TDate(" ++ show d ++ ")"
debugValue (TTime t) = "TTime(" ++ show t ++ ")"
debugValue (TArray xs) = "TArray[" ++ show (length xs) ++ " items]"
debugValue (TInlineTable kvs) = "TInlineTable{" ++ show (length kvs) ++ " keys}"
debugValue (TTable kvs) = "TTable{" ++ show (length kvs) ++ " keys}"

||| Get structure summary
public export
structureSummary : TOMLDocument -> String
structureSummary doc =
  "TOMLDocument{" ++ show (length doc) ++ " top-level keys: " ++
  join ", " (map fst doc) ++ "}"
  where
    join : String -> List String -> String
    join _ [] = ""
    join _ [x] = x
    join sep (x :: xs) = x ++ sep ++ join sep xs
