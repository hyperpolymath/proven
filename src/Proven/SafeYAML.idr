-- SPDX-License-Identifier: MPL-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
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
import Data.List1
import Data.String
import Data.Maybe

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
parseAll = parseStream secureDefaults

||| Parse a YAML stream with custom options
public export
parseAllWith : YAMLSecurityOptions -> String -> YAMLResult YAMLStream
parseAllWith = parseStream

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
getPath path val = go (forget (Data.String.split (== '.') path)) val
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
  case getAt idx xs of
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

-- Rendering helpers.
--
-- These were originally `where`-block siblings of `render`. A `where` block
-- attaches to ONE clause, and `render` has ten, so the helpers were visible
-- only inside `render (YTimestamp ts) = ts` and undefined in every earlier
-- clause that called them. They are hoisted to the top level here.
--
-- `renderItems`/`renderPairs` are mutually recursive with `render`, so their
-- signatures are declared before `render` and their clauses follow it: split
-- declaration/definition is legal at the Idris2 top level, and it avoids
-- re-indenting the whole block into a `mutual`.
--
-- They are also explicit list recursion rather than `map`, because a
-- recursive call handed to `map` hides the structural descent from the
-- totality checker.

||| True if a scalar must be quoted to round-trip as YAML
public export
needsQuoting : String -> Bool
needsQuoting s =
  null (unpack s) ||
  any (\c => c `elem` [':', '#', '[', ']', '{', '}', ',', '&', '*', '!', '|',
                       '>', '\'', '"', '%', '@', '`']) (unpack s) ||
  (s `elem` ["true", "false", "yes", "no", "on", "off", "null", "~"])

||| Escape the characters YAML double-quoted style requires escaping
public export
escapeChars : List Char -> List Char
escapeChars [] = []
escapeChars ('"' :: rest) = '\\' :: '"' :: escapeChars rest
escapeChars ('\\' :: rest) = '\\' :: '\\' :: escapeChars rest
escapeChars ('\n' :: rest) = '\\' :: 'n' :: escapeChars rest
escapeChars ('\t' :: rest) = '\\' :: 't' :: escapeChars rest
escapeChars (c :: rest) = c :: escapeChars rest

||| Escape a string for YAML double-quoted style
public export
escapeString : String -> String
escapeString s = pack (escapeChars (unpack s))

||| Render a scalar string, quoting it only when it would not round-trip
public export
renderString : String -> String
renderString s =
  if needsQuoting s
    then "\"" ++ escapeString s ++ "\""
    else s

||| Join strings with a separator
public export
joinWith : String -> List String -> String
joinWith _ [] = ""
joinWith _ [x] = x
joinWith sep (x :: xs) = x ++ sep ++ joinWith sep xs

public export
renderItems : List YAMLValue -> List String

public export
renderPairs : List (String, YAMLValue) -> List String

||| Render YAML value to string (simple format)
public export
render : YAMLValue -> String
render YNull = "null"
render (YBool True) = "true"
render (YBool False) = "false"
render (YInt i) = show i
render (YFloat f) = show f
render (YString s) = renderString s
render (YArray []) = "[]"
render (YArray xs) = "[" ++ joinWith ", " (renderItems xs) ++ "]"
render (YObject []) = "{}"
render (YObject kvs) = "{" ++ joinWith ", " (renderPairs kvs) ++ "}"
render (YBinary bs) = "!!binary " ++ show (length bs) ++ " bytes"
render (YTimestamp ts) = ts

renderItems [] = []
renderItems (x :: xs) = render x :: renderItems xs

renderPairs [] = []
renderPairs ((k, v) :: kvs) = (renderString k ++ ": " ++ render v) :: renderPairs kvs

-- Block-style rendering. Same treatment as `render` above: the helpers were
-- `where`-block siblings whose recursion was hidden inside `map`, so they are
-- hoisted and defunctionalised.

||| Two spaces per nesting level
public export
indentLevel : Nat -> String
indentLevel n = pack (replicate (n * 2) ' ')

public export
renderBlockItems : Nat -> List YAMLValue -> List String

public export
renderBlockPairs : Nat -> List (String, YAMLValue) -> List String

public export
renderBlockAt : Nat -> YAMLValue -> String
renderBlockAt _ YNull = "null"
renderBlockAt _ (YBool True) = "true"
renderBlockAt _ (YBool False) = "false"
renderBlockAt _ (YInt i) = show i
renderBlockAt _ (YFloat f) = show f
renderBlockAt _ (YString s) = show s
renderBlockAt _ (YBinary bs) = "!!binary " ++ show (length bs) ++ " bytes"
renderBlockAt _ (YTimestamp ts) = ts
renderBlockAt _ (YArray []) = "[]"
renderBlockAt level (YArray xs) = "\n" ++ unlines (renderBlockItems level xs)
renderBlockAt _ (YObject []) = "{}"
renderBlockAt level (YObject kvs) = "\n" ++ unlines (renderBlockPairs level kvs)

renderBlockItems _ [] = []
renderBlockItems level (x :: xs) =
  (indentLevel level ++ "- " ++ renderBlockAt (S level) x)
    :: renderBlockItems level xs

renderBlockPairs _ [] = []
renderBlockPairs level ((k, v) :: kvs) =
  (indentLevel level ++ k ++ ": " ++ renderBlockAt (S level) v)
    :: renderBlockPairs level kvs

||| Render YAML with block style (more readable)
public export
renderBlock : YAMLValue -> String
renderBlock val = renderBlockAt 0 val

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

-- The list traversals below are written as explicit recursion rather than
-- `map`. A recursive call passed to `map` is opaque to the totality checker:
-- it cannot see that the argument is structurally smaller. Spelling the
-- recursion out makes the descent visible and the definition total.
public export
mapValuesItems : (YAMLValue -> YAMLValue) -> List YAMLValue -> List YAMLValue

public export
mapValuesPairs : (YAMLValue -> YAMLValue) -> List (String, YAMLValue) ->
                 List (String, YAMLValue)

||| Map over all values in structure
public export
mapValues : (YAMLValue -> YAMLValue) -> YAMLValue -> YAMLValue
mapValues f (YArray xs) = f (YArray (mapValuesItems f xs))
mapValues f (YObject kvs) = f (YObject (mapValuesPairs f kvs))
mapValues f other = f other

mapValuesItems f [] = []
mapValuesItems f (x :: xs) = mapValues f x :: mapValuesItems f xs

mapValuesPairs f [] = []
mapValuesPairs f ((k, v) :: kvs) = (k, mapValues f v) :: mapValuesPairs f kvs

-- Same defunctionalisation as `mapValues` above, for the same reason.
public export
filterFieldsItems : (String -> YAMLValue -> Bool) -> List YAMLValue ->
                    List YAMLValue

public export
filterFieldsPairs : (String -> YAMLValue -> Bool) ->
                    List (String, YAMLValue) -> List (String, YAMLValue)

||| Filter object fields
public export
filterFields : (String -> YAMLValue -> Bool) -> YAMLValue -> YAMLValue
filterFields pred (YObject kvs) =
  YObject (filter (uncurry pred) (filterFieldsPairs pred kvs))
filterFields pred (YArray xs) = YArray (filterFieldsItems pred xs)
filterFields pred val = val

filterFieldsItems pred [] = []
filterFieldsItems pred (x :: xs) =
  filterFields pred x :: filterFieldsItems pred xs

filterFieldsPairs pred [] = []
filterFieldsPairs pred ((k, v) :: kvs) =
  (k, filterFields pred v) :: filterFieldsPairs pred kvs

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
  "YAML isInfixOf dangerous tag '" ++ tag ++ "' which could execute arbitrary code."
friendlyError (AliasDepthExceeded depth limit) =
  "YAML alias expansion too deep (" ++ show depth ++ " > " ++ show limit ++ "). Possible alias bomb attack."
friendlyError (CircularReference anchor) =
  "YAML isInfixOf circular reference to anchor '" ++ anchor ++ "'."
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
