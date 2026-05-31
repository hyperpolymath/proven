-- SPDX-License-Identifier: MPL-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
||| SafeJson - JSON operations that cannot crash
|||
||| This module provides:
||| - A safe JSON representation
||| - Total parsing (returns Maybe/Result instead of throwing)
||| - Safe accessors that never crash on missing keys
||| - Type-safe JSON construction
module Proven.SafeJson

import public Proven.Core
-- Re-export JsonValue from Parser module
import public Proven.SafeJson.Parser
import public Proven.SafeJson.Access

import Data.List
import Data.Maybe
import Data.String
import Decidable.Equality

%default total

--------------------------------------------------------------------------------
-- Object-Carrier Helpers (DecEq-based)
--
-- These three helpers (`lookupObj`, `updateObj`, `removeObj`) replace
-- the previous `lookup` / `update` / `filter (/=)` chains, which routed
-- through `(==) : String -> String -> Bool` (FFI-bound
-- `prim__eq_String`) and were therefore not Refl-reducible on abstract
-- keys. Using `decEq` instead routes through the `DecEq String`
-- decision procedure, whose `Yes` / `No` arms expose structural
-- proofs that the OWED lemmas in `Proven.SafeJson.Proofs` can pattern
-- on (see `setGetIdentity`, `setPreservesOther`, `setHasKey`,
-- `removeNotHasKey`).
--
-- Runtime semantics: identical to the prior `(==)`-based versions.
-- Both `(==)` and `decEq` for `String` ultimately bottom out at
-- `prim__eq_String`, so the FFI cost is unchanged.
--------------------------------------------------------------------------------

||| Lookup a key in a key-value list using `DecEq` decision procedure.
||| Returns `Nothing` if the key is absent; `Just v` for the first
||| match.
public export
lookupObj : DecEq a => (k : a) -> List (a, b) -> Maybe b
lookupObj k [] = Nothing
lookupObj k ((k', v) :: rest) with (decEq k k')
  _ | Yes _ = Just v
  _ | No _  = lookupObj k rest

||| Update (or insert if absent) a key in a key-value list using
||| `DecEq`. Existing entries with the key are replaced; if no entry
||| exists, a new one is appended at the tail.
public export
updateObj : DecEq a => (k : a) -> b -> List (a, b) -> List (a, b)
updateObj k v [] = [(k, v)]
updateObj k v ((k', v') :: rest) with (decEq k k')
  _ | Yes _ = (k, v) :: rest
  _ | No _  = (k', v') :: updateObj k v rest

||| Remove all entries with the given key from a key-value list using
||| `DecEq`.
public export
removeObj : DecEq a => (k : a) -> List (a, b) -> List (a, b)
removeObj k [] = []
removeObj k ((k', v') :: rest) with (decEq k k')
  _ | Yes _ = removeObj k rest
  _ | No _  = (k', v') :: removeObj k rest

-- Note: JsonValue is defined in Proven.SafeJson.Parser and re-exported here

--------------------------------------------------------------------------------
-- JSON Type Predicates
--------------------------------------------------------------------------------

||| Check if value is null
public export
isNull : JsonValue -> Bool
isNull JsonNull = True
isNull _ = False

||| Check if value is a boolean
public export
isBool : JsonValue -> Bool
isBool (JsonBool _) = True
isBool _ = False

||| Check if value is a number
public export
isNumber : JsonValue -> Bool
isNumber (JsonNumber _) = True
isNumber _ = False

||| Check if value is a string
public export
isString : JsonValue -> Bool
isString (JsonString _) = True
isString _ = False

||| Check if value is an array
public export
isArray : JsonValue -> Bool
isArray (JsonArray _) = True
isArray _ = False

||| Check if value is an object
public export
isObject : JsonValue -> Bool
isObject (JsonObject _) = True
isObject _ = False

--------------------------------------------------------------------------------
-- Safe Value Extraction
--------------------------------------------------------------------------------

||| Extract boolean value safely
public export
asBool : JsonValue -> Maybe Bool
asBool (JsonBool b) = Just b
asBool _ = Nothing

||| Extract number value safely
public export
asNumber : JsonValue -> Maybe Double
asNumber (JsonNumber n) = Just n
asNumber _ = Nothing

||| Extract integer value safely (truncates decimals)
public export
asInt : JsonValue -> Maybe Integer
asInt (JsonNumber n) = Just (cast n)
asInt _ = Nothing

||| Extract string value safely
public export
asString : JsonValue -> Maybe String
asString (JsonString s) = Just s
asString _ = Nothing

||| Extract array safely
public export
asArray : JsonValue -> Maybe (List JsonValue)
asArray (JsonArray arr) = Just arr
asArray _ = Nothing

||| Extract object safely
public export
asObject : JsonValue -> Maybe (List (String, JsonValue))
asObject (JsonObject obj) = Just obj
asObject _ = Nothing

--------------------------------------------------------------------------------
-- JSON Construction Helpers
--------------------------------------------------------------------------------

||| Create a JSON string
public export
str : String -> JsonValue
str = JsonString

||| Create a JSON number from integer
public export
int : Integer -> JsonValue
int n = JsonNumber (cast n)

||| Create a JSON number from natural
public export
nat : Nat -> JsonValue
nat n = JsonNumber (cast n)

||| Create a JSON number from double
public export
num : Double -> JsonValue
num = JsonNumber

||| Create a JSON boolean
public export
bool : Bool -> JsonValue
bool = JsonBool

||| Create a JSON null
public export
null : JsonValue
null = JsonNull

||| Create a JSON array
public export
array : List JsonValue -> JsonValue
array = JsonArray

||| Create a JSON object from key-value pairs
public export
object : List (String, JsonValue) -> JsonValue
object = JsonObject

--------------------------------------------------------------------------------
-- Object Manipulation
--------------------------------------------------------------------------------

||| Get a value from an object by key
public export
get : String -> JsonValue -> Maybe JsonValue
get key (JsonObject pairs) = lookupObj key pairs
get _ _ = Nothing

||| Check if object has a key
public export
hasKey : String -> JsonValue -> Bool
hasKey key json = isJust (get key json)

||| Get all keys from an object
public export
keys : JsonValue -> Maybe (List String)
keys (JsonObject pairs) = Just (map fst pairs)
keys _ = Nothing

||| Get all values from an object
public export
values : JsonValue -> Maybe (List JsonValue)
values (JsonObject pairs) = Just (map snd pairs)
values _ = Nothing

||| Set a value in an object (creates new object). Uses `updateObj`
||| (`DecEq`-based) so that `Proven.SafeJson.Proofs.setGetIdentity` /
||| `setPreservesOther` / `setHasKey` are dischargeable.
public export
set : String -> JsonValue -> JsonValue -> JsonValue
set key val (JsonObject pairs) = JsonObject (updateObj key val pairs)
set _ _ json = json  -- Non-objects unchanged

||| Remove a key from an object. Uses `removeObj` (`DecEq`-based) so
||| that `Proven.SafeJson.Proofs.removeNotHasKey` is dischargeable.
public export
remove : String -> JsonValue -> JsonValue
remove key (JsonObject pairs) = JsonObject (removeObj key pairs)
remove _ json = json

||| Merge two objects (second overwrites first on conflicts). Uses
||| `updateObj` (`DecEq`-based) for the per-key overwrite step so the
||| behaviour matches `set` exactly.
public export
merge : JsonValue -> JsonValue -> JsonValue
merge (JsonObject obj1) (JsonObject obj2) = JsonObject (mergeWith obj1 obj2)
  where
    mergeWith : List (String, JsonValue) -> List (String, JsonValue) -> List (String, JsonValue)
    mergeWith base [] = base
    mergeWith base ((k, v) :: rest) = mergeWith (updateObj k v base) rest
merge _ obj2 = obj2

--------------------------------------------------------------------------------
-- Array Manipulation
--------------------------------------------------------------------------------

||| Get element at index from array
public export
at : Nat -> JsonValue -> Maybe JsonValue
at idx (JsonArray arr) = index' idx arr
  where
    index' : Nat -> List JsonValue -> Maybe JsonValue
    index' _ [] = Nothing
    index' Z (x :: _) = Just x
    index' (S n) (_ :: xs) = index' n xs
at _ _ = Nothing

||| Get array length
public export
arrayLength : JsonValue -> Maybe Nat
arrayLength (JsonArray arr) = Just (length arr)
arrayLength _ = Nothing

||| Append to array
public export
append : JsonValue -> JsonValue -> JsonValue
append elem (JsonArray arr) = JsonArray (arr ++ [elem])
append _ json = json

||| Prepend to array
public export
prepend : JsonValue -> JsonValue -> JsonValue
prepend elem (JsonArray arr) = JsonArray (elem :: arr)
prepend _ json = json

||| Map over array elements
public export
mapArray : (JsonValue -> JsonValue) -> JsonValue -> JsonValue
mapArray f (JsonArray arr) = JsonArray (map f arr)
mapArray _ json = json

||| Filter array elements
public export
filterArray : (JsonValue -> Bool) -> JsonValue -> JsonValue
filterArray p (JsonArray arr) = JsonArray (filter p arr)
filterArray _ json = json

--------------------------------------------------------------------------------
-- JSON Equality
--------------------------------------------------------------------------------

-- covering: mutual recursion between Eq JsonValue / jsonListEq / jsonPairsEq
-- is structurally decreasing but crosses implementation boundaries
covering
jsonListEq : List JsonValue -> List JsonValue -> Bool

covering
jsonPairsEq : List (String, JsonValue) -> List (String, JsonValue) -> Bool

||| Equality for JSON values (structurally recursive on subterms)
public export covering
Eq JsonValue where
  JsonNull == JsonNull = True
  (JsonBool a) == (JsonBool b) = a == b
  (JsonNumber a) == (JsonNumber b) = a == b
  (JsonString a) == (JsonString b) = a == b
  (JsonArray a) == (JsonArray b) = jsonListEq a b
  (JsonObject a) == (JsonObject b) = jsonPairsEq a b
  _ == _ = False

jsonListEq [] [] = True
jsonListEq (x :: xs) (y :: ys) = x == y && jsonListEq xs ys
jsonListEq _ _ = False

jsonPairsEq [] [] = True
jsonPairsEq ((k1, v1) :: ps1) ((k2, v2) :: ps2) =
  k1 == k2 && v1 == v2 && jsonPairsEq ps1 ps2
jsonPairsEq _ _ = False

--------------------------------------------------------------------------------
-- JSON Show Instance
--------------------------------------------------------------------------------

||| Show helper for JSON array elements (structurally recursive)
showJsonElems : List JsonValue -> String

||| Show helper for JSON object pairs (structurally recursive)
showJsonPairs : List (String, JsonValue) -> String

||| Show helper for a single JSON value (structurally recursive)
showJsonValue : JsonValue -> String
showJsonValue JsonNull = "null"
showJsonValue (JsonBool True) = "true"
showJsonValue (JsonBool False) = "false"
showJsonValue (JsonNumber n) = show n
showJsonValue (JsonString s) = show s
showJsonValue (JsonArray arr) = "[" ++ showJsonElems arr ++ "]"
showJsonValue (JsonObject pairs) = "{" ++ showJsonPairs pairs ++ "}"

showJsonElems [] = ""
showJsonElems [x] = showJsonValue x
showJsonElems (x :: xs) = showJsonValue x ++ ", " ++ showJsonElems xs

showJsonPairs [] = ""
showJsonPairs [(k, v)] = show k ++ ": " ++ showJsonValue v
showJsonPairs ((k, v) :: ps) = show k ++ ": " ++ showJsonValue v ++ ", " ++ showJsonPairs ps

||| Show instance for debugging
public export
Show JsonValue where
  show = showJsonValue

--------------------------------------------------------------------------------
-- Type-Safe JSON Schema
--------------------------------------------------------------------------------

||| JSON Schema types for validation
public export
data JsonType
  = TNull
  | TBool
  | TNumber
  | TString
  | TArray JsonType
  | TObject (List (String, JsonType))
  | TAny
  | TOneOf (List JsonType)

||| Check if a value matches a type
-- covering: matchesType and checkField are mutually recursive through
-- the TObject/TArray/TOneOf cases; structurally decreasing but
-- crosses where-clause boundary
public export covering
matchesType : JsonValue -> JsonType -> Bool
matchesType JsonNull TNull = True
matchesType (JsonBool _) TBool = True
matchesType (JsonNumber _) TNumber = True
matchesType (JsonString _) TString = True
matchesType (JsonArray arr) (TArray elemType) = all (\v => matchesType v elemType) arr
matchesType (JsonObject pairs) (TObject schema) = all checkField schema
  where
    checkField : (String, JsonType) -> Bool
    checkField (key, typ) = case lookup key pairs of
      Just v => matchesType v typ
      Nothing => False
matchesType _ TAny = True
matchesType v (TOneOf types) = any (matchesType v) types
matchesType _ _ = False

||| Validate JSON against a type, returning the value if valid
public export covering
validate : JsonValue -> JsonType -> Maybe JsonValue
validate v t = if matchesType v t then Just v else Nothing
