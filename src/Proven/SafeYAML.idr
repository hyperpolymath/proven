-- SPDX-License-Identifier: Palimpsest-MPL-1.0
||| SafeYAML - YAML processing that prevents deserialization attacks
|||
||| This module provides safe YAML operations including:
||| - Dangerous tag blocking (!!python/object, !!ruby/object, etc.)
||| - Alias bomb (billion laughs) prevention
||| - Resource limit enforcement
||| - Safe type coercion
|||
||| Example usage:
||| ```idris
||| -- Parse YAML safely
||| case parseYAML yamlString of
|||   Ok doc => processDocument doc
|||   Err (DangerousTag tag) => handleAttackAttempt tag
|||   Err e => handleError e
|||
||| -- Parse with custom options
||| let opts = { allowAnchors := True } secureDefaults
||| case parseYAMLWith opts yamlString of
|||   Ok doc => processDocument doc
|||   Err e => handleError e
||| ```
module Proven.SafeYAML

import public Proven.Core
import public Proven.SafeYAML.Types
import public Proven.SafeYAML.Parser
import public Proven.SafeYAML.Proofs

import Data.List
import Data.String

%default total

--------------------------------------------------------------------------------
-- High-Level API
--------------------------------------------------------------------------------

||| Parse YAML with secure defaults (anchors disabled, dangerous tags blocked)
|||
||| This is the recommended way to parse untrusted YAML.
public export
parse : String -> YAMLResult YAMLValue
parse = parseYAML

||| Parse YAML with custom security options
|||
||| Use this when you need to relax security constraints (trusted input only).
public export
parseWith : YAMLSecurityOptions -> String -> YAMLResult YAMLValue
parseWith = parseYAMLWith

||| Parse a YAML stream (multiple documents)
public export
parseAll : String -> YAMLResult YAMLStream
parseAll = parseYAMLStream

||| Parse a YAML stream with custom options
public export
parseAllWith : YAMLSecurityOptions -> String -> YAMLResult YAMLStream
parseAllWith = parseYAMLStreamWith

--------------------------------------------------------------------------------
-- Type Coercion
--------------------------------------------------------------------------------

||| Try to get a string value
public export
asString : YAMLValue -> YAMLResult String
asString (YString s) = Ok s
asString val = Err (TypeMismatch "string" (yamlTypeName val))

||| Try to get an integer value
public export
asInt : YAMLValue -> YAMLResult Integer
asInt (YInt i) = Ok i
asInt val = Err (TypeMismatch "int" (yamlTypeName val))

||| Try to get a float value
public export
asFloat : YAMLValue -> YAMLResult Double
asFloat (YFloat f) = Ok f
asFloat (YInt i) = Ok (cast i)  -- Allow int -> float coercion
asFloat val = Err (TypeMismatch "float" (yamlTypeName val))

||| Try to get a boolean value
public export
asBool : YAMLValue -> YAMLResult Bool
asBool (YBool b) = Ok b
asBool val = Err (TypeMismatch "bool" (yamlTypeName val))

||| Try to get an array value
public export
asArray : YAMLValue -> YAMLResult (List YAMLValue)
asArray (YArray xs) = Ok xs
asArray val = Err (TypeMismatch "array" (yamlTypeName val))

||| Try to get an object value
public export
asObject : YAMLValue -> YAMLResult (List (String, YAMLValue))
asObject (YObject kvs) = Ok kvs
asObject val = Err (TypeMismatch "object" (yamlTypeName val))

||| Try to get binary data
public export
asBinary : YAMLValue -> YAMLResult (List Bits8)
asBinary (YBinary bs) = Ok bs
asBinary val = Err (TypeMismatch "binary" (yamlTypeName val))

||| Try to get timestamp string
public export
asTimestamp : YAMLValue -> YAMLResult String
asTimestamp (YTimestamp ts) = Ok ts
asTimestamp val = Err (TypeMismatch "timestamp" (yamlTypeName val))

--------------------------------------------------------------------------------
-- Object Access
--------------------------------------------------------------------------------

||| Get a field from an object
public export
getField : String -> YAMLValue -> YAMLResult YAMLValue
getField key (YObject kvs) =
  case lookup key kvs of
    Just val => Ok val
    Nothing => Err (TypeMismatch ("field '" ++ key ++ "'") "missing")
getField key val = Err (TypeMismatch "object" (yamlTypeName val))

||| Get a field with default value
public export
getFieldOr : YAMLValue -> String -> YAMLValue -> YAMLValue
getFieldOr def key (YObject kvs) =
  case lookup key kvs of
    Just val => val
    Nothing => def
getFieldOr def key _ = def

||| Check if field exists
public export
hasField : String -> YAMLValue -> Bool
hasField key (YObject kvs) = isJust (lookup key kvs)
hasField key _ = False

||| Get nested field using dot notation
public export
getPath : String -> YAMLValue -> YAMLResult YAMLValue
getPath path val = go (split (== '.') path) val
  where
    go : List String -> YAMLValue -> YAMLResult YAMLValue
    go [] v = Ok v
    go (k :: ks) v = do
      next <- getField k v
      go ks next

||| Get all keys from an object
public export
keys : YAMLValue -> List String
keys (YObject kvs) = map fst kvs
keys _ = []

||| Get all values from an object
public export
values : YAMLValue -> List YAMLValue
values (YObject kvs) = map snd kvs
values _ = []

--------------------------------------------------------------------------------
-- Array Access
--------------------------------------------------------------------------------

||| Get array element by index
public export
getIndex : Nat -> YAMLValue -> YAMLResult YAMLValue
getIndex idx (YArray xs) =
  case index' idx xs of
    Just val => Ok val
    Nothing => Err (TypeMismatch ("index " ++ show idx) "out of bounds")
getIndex idx val = Err (TypeMismatch "array" (yamlTypeName val))

||| Get array length
public export
arrayLength : YAMLValue -> Nat
arrayLength (YArray xs) = length xs
arrayLength _ = 0

--------------------------------------------------------------------------------
-- Validation
--------------------------------------------------------------------------------

||| Check if YAML is safe to process
public export
isSafe : String -> Bool
isSafe yaml = isOk (parse yaml)

||| Validate YAML structure
public export
validate : String -> YAMLResult ()
validate yaml = do
  _ <- parse yaml
  Ok ()

||| Check for dangerous patterns without full parsing
public export
hasDangerousPatterns : String -> Bool
hasDangerousPatterns yaml =
  any (\tag => isInfixOf tag yaml) dangerousTags

||| Check for anchor patterns
public export
hasAnchors : String -> Bool
hasAnchors yaml = isInfixOf "&" yaml || isInfixOf "*" yaml

--------------------------------------------------------------------------------
-- Rendering
--------------------------------------------------------------------------------

||| Render YAML value to string (simple format)
public export
render : YAMLValue -> String
render YNull = "null"
render (YBool True) = "true"
render (YBool False) = "false"
render (YInt i) = show i
render (YFloat f) = show f
render (YString s) = renderString s
render (YArray xs) = renderArray xs
render (YObject kvs) = renderObject kvs
render (YBinary bs) = "!!binary " ++ show (length bs) ++ " bytes"
render (YTimestamp ts) = ts
  where
    needsQuoting : String -> Bool
    needsQuoting s =
      null (unpack s) ||
      any (\c => c `elem` [':', '#', '[', ']', '{', '}', ',', '&', '*', '!', '|', '>', '\'', '"', '%', '@', '`']) (unpack s) ||
      s `elem` ["true", "false", "yes", "no", "on", "off", "null", "~"]

    renderString : String -> String
    renderString s =
      if needsQuoting s
        then "\"" ++ escapeString s ++ "\""
        else s

    escapeString : String -> String
    escapeString s = pack (go (unpack s))
      where
        go : List Char -> List Char
        go [] = []
        go ('"' :: rest) = '\\' :: '"' :: go rest
        go ('\\' :: rest) = '\\' :: '\\' :: go rest
        go ('\n' :: rest) = '\\' :: 'n' :: go rest
        go ('\t' :: rest) = '\\' :: 't' :: go rest
        go (c :: rest) = c :: go rest

    renderArray : List YAMLValue -> String
    renderArray [] = "[]"
    renderArray xs = "[" ++ join ", " (map render xs) ++ "]"
      where
        join : String -> List String -> String
        join _ [] = ""
        join _ [x] = x
        join sep (x :: xs) = x ++ sep ++ join sep xs

    renderObject : List (String, YAMLValue) -> String
    renderObject [] = "{}"
    renderObject kvs = "{" ++ join ", " (map renderKV kvs) ++ "}"
      where
        join : String -> List String -> String
        join _ [] = ""
        join _ [x] = x
        join sep (x :: xs) = x ++ sep ++ join sep xs
        renderKV : (String, YAMLValue) -> String
        renderKV (k, v) = renderString k ++ ": " ++ render v

||| Render YAML with block style (more readable)
public export
renderBlock : YAMLValue -> String
renderBlock val = go 0 val
  where
    indent : Nat -> String
    indent n = pack (replicate (n * 2) ' ')

    go : Nat -> YAMLValue -> String
    go _ YNull = "null"
    go _ (YBool True) = "true"
    go _ (YBool False) = "false"
    go _ (YInt i) = show i
    go _ (YFloat f) = show f
    go _ (YString s) = show s
    go _ (YBinary bs) = "!!binary " ++ show (length bs) ++ " bytes"
    go _ (YTimestamp ts) = ts
    go level (YArray []) = "[]"
    go level (YArray xs) =
      "\n" ++ unlines (map (\x => indent level ++ "- " ++ go (S level) x) xs)
    go level (YObject []) = "{}"
    go level (YObject kvs) =
      "\n" ++ unlines (map (\(k, v) => indent level ++ k ++ ": " ++ go (S level) v) kvs)

||| Render document with optional header
public export
renderDocument : YAMLDocument -> String
renderDocument doc =
  let header = case doc.version of
                 Just v => "%YAML " ++ v ++ "\n---\n"
                 Nothing => ""
  in header ++ render doc.value

--------------------------------------------------------------------------------
-- Construction Helpers
--------------------------------------------------------------------------------

||| Create an object from key-value pairs
public export
mkObject : List (String, YAMLValue) -> YAMLValue
mkObject = YObject

||| Create an array from values
public export
mkArray : List YAMLValue -> YAMLValue
mkArray = YArray

||| Create a document from a value
public export
mkDocument : YAMLValue -> YAMLDocument
mkDocument val = MkYAMLDocument Nothing [] val

||| Create a document with version
public export
mkDocumentWithVersion : String -> YAMLValue -> YAMLDocument
mkDocumentWithVersion ver val = MkYAMLDocument (Just ver) [] val

--------------------------------------------------------------------------------
-- Transformation
--------------------------------------------------------------------------------

||| Map over all values in structure
public export
mapValues : (YAMLValue -> YAMLValue) -> YAMLValue -> YAMLValue
mapValues f val = case val of
  YArray xs => f (YArray (map (mapValues f) xs))
  YObject kvs => f (YObject (map (\(k, v) => (k, mapValues f v)) kvs))
  other => f other

||| Filter object fields
public export
filterFields : (String -> YAMLValue -> Bool) -> YAMLValue -> YAMLValue
filterFields pred (YObject kvs) =
  YObject (filter (uncurry pred) (map (\(k, v) => (k, filterFields pred v)) kvs))
filterFields pred (YArray xs) = YArray (map (filterFields pred) xs)
filterFields pred val = val

||| Merge two objects (second wins on conflicts)
public export
mergeObjects : YAMLValue -> YAMLValue -> YAMLValue
mergeObjects (YObject kvs1) (YObject kvs2) =
  YObject (mergeBy fst kvs1 kvs2)
  where
    mergeBy : (a -> String) -> List a -> List a -> List a
    mergeBy _ [] ys = ys
    mergeBy _ xs [] = xs
    mergeBy f (x :: xs) ys =
      let key = f x
          ys' = filter (\y => f y /= key) ys
      in x :: mergeBy f xs ys'
mergeObjects _ obj2 = obj2

--------------------------------------------------------------------------------
-- Security Presets
--------------------------------------------------------------------------------

||| Maximum security (strictest settings)
public export
maxSecurity : YAMLSecurityOptions
maxSecurity = secureDefaults

||| Standard security (allows anchors with limits)
public export
standardSecurityOpts : YAMLSecurityOptions
standardSecurityOpts = standardSecurity

||| Permissive security (for trusted input only)
public export
permissiveSecurityOpts : YAMLSecurityOptions
permissiveSecurityOpts = permissiveSecurity

--------------------------------------------------------------------------------
-- Error Helpers
--------------------------------------------------------------------------------

||| Check if error is dangerous tag related
public export
isDangerousTagError : YAMLError -> Bool
isDangerousTagError (DangerousTag _) = True
isDangerousTagError _ = False

||| Check if error is alias bomb related
public export
isAliasBombError : YAMLError -> Bool
isAliasBombError (AliasDepthExceeded _ _) = True
isAliasBombError (CircularReference _) = True
isAliasBombError _ = False

||| Check if error is resource limit related
public export
isResourceLimitError : YAMLError -> Bool
isResourceLimitError (NestingTooDeep _ _) = True
isResourceLimitError (KeyTooLong _ _) = True
isResourceLimitError (ValueTooLarge _ _) = True
isResourceLimitError (TooManyDocuments _ _) = True
isResourceLimitError _ = False

||| Get user-friendly error message
public export
friendlyError : YAMLError -> String
friendlyError (DangerousTag tag) =
  "YAML contains dangerous tag '" ++ tag ++ "' which could execute arbitrary code."
friendlyError (AliasDepthExceeded depth limit) =
  "YAML alias expansion too deep (" ++ show depth ++ " > " ++ show limit ++ "). Possible alias bomb attack."
friendlyError (CircularReference anchor) =
  "YAML contains circular reference to anchor '" ++ anchor ++ "'."
friendlyError (NestingTooDeep depth limit) =
  "YAML nesting too deep (" ++ show depth ++ " > " ++ show limit ++ ")."
friendlyError (KeyTooLong length limit) =
  "YAML key too long (" ++ show length ++ " > " ++ show limit ++ " bytes)."
friendlyError (ValueTooLarge size limit) =
  "YAML value too large (" ++ show size ++ " > " ++ show limit ++ " bytes)."
friendlyError (TooManyDocuments count limit) =
  "Too many YAML documents (" ++ show count ++ " > " ++ show limit ++ ")."
friendlyError (SyntaxError msg line col) =
  "YAML syntax error at line " ++ show line ++ ", column " ++ show col ++ ": " ++ msg
friendlyError (AnchorNotFound name) =
  "YAML anchor '&" ++ name ++ "' not found."
friendlyError (UnsupportedVersion ver) =
  "Unsupported YAML version: " ++ ver
friendlyError (TypeMismatch expected actual) =
  "Type mismatch: expected " ++ expected ++ ", got " ++ actual

--------------------------------------------------------------------------------
-- Debugging
--------------------------------------------------------------------------------

||| Pretty print YAML value for debugging
public export
debugValue : YAMLValue -> String
debugValue YNull = "YNull"
debugValue (YBool b) = "YBool(" ++ show b ++ ")"
debugValue (YInt i) = "YInt(" ++ show i ++ ")"
debugValue (YFloat f) = "YFloat(" ++ show f ++ ")"
debugValue (YString s) = "YString(\"" ++ s ++ "\")"
debugValue (YArray xs) = "YArray[" ++ show (length xs) ++ " items]"
debugValue (YObject kvs) = "YObject{" ++ show (length kvs) ++ " fields}"
debugValue (YBinary bs) = "YBinary[" ++ show (length bs) ++ " bytes]"
debugValue (YTimestamp ts) = "YTimestamp(" ++ ts ++ ")"

||| Get structure summary
public export
structureSummary : YAMLValue -> String
structureSummary val = go 0 val
  where
    go : Nat -> YAMLValue -> String
    go _ YNull = "null"
    go _ (YBool _) = "bool"
    go _ (YInt _) = "int"
    go _ (YFloat _) = "float"
    go _ (YString _) = "string"
    go _ (YBinary _) = "binary"
    go _ (YTimestamp _) = "timestamp"
    go depth (YArray []) = "[]"
    go depth (YArray (x :: xs)) =
      if depth > 3
        then "[...]"
        else "[" ++ go (S depth) x ++ ", ...]"
    go depth (YObject []) = "{}"
    go depth (YObject ((k, v) :: kvs)) =
      if depth > 3
        then "{...}"
        else "{" ++ k ++ ": " ++ go (S depth) v ++ ", ...}"
