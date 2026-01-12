-- SPDX-License-Identifier: Palimpsest-MPL-1.0
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
import Data.String

%default total

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
get key (JsonObject pairs) = lookup key pairs
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

||| Set a value in an object (creates new object)
public export
set : String -> JsonValue -> JsonValue -> JsonValue
set key val (JsonObject pairs) = JsonObject (update key val pairs)
  where
    update : String -> JsonValue -> List (String, JsonValue) -> List (String, JsonValue)
    update k v [] = [(k, v)]
    update k v ((k', v') :: rest) =
      if k == k' then (k, v) :: rest
                 else (k', v') :: update k v rest
set _ _ json = json  -- Non-objects unchanged

||| Remove a key from an object
public export
remove : String -> JsonValue -> JsonValue
remove key (JsonObject pairs) = JsonObject (filter (\(k, _) => k /= key) pairs)
remove _ json = json

||| Merge two objects (second overwrites first on conflicts)
public export
merge : JsonValue -> JsonValue -> JsonValue
merge (JsonObject obj1) (JsonObject obj2) = JsonObject (mergeWith obj1 obj2)
  where
    mergeWith : List (String, JsonValue) -> List (String, JsonValue) -> List (String, JsonValue)
    mergeWith base [] = base
    mergeWith base ((k, v) :: rest) = mergeWith (update k v base) rest
      where
        update : String -> JsonValue -> List (String, JsonValue) -> List (String, JsonValue)
        update key val [] = [(key, val)]
        update key val ((k', v') :: pairs) =
          if key == k' then (key, val) :: pairs
                       else (k', v') :: update key val pairs
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

||| Equality for JSON values
public export
Eq JsonValue where
  JsonNull == JsonNull = True
  (JsonBool a) == (JsonBool b) = a == b
  (JsonNumber a) == (JsonNumber b) = a == b
  (JsonString a) == (JsonString b) = a == b
  (JsonArray a) == (JsonArray b) = assert_total $ a == b
  (JsonObject a) == (JsonObject b) = assert_total $ a == b
  _ == _ = False

--------------------------------------------------------------------------------
-- JSON Show Instance
--------------------------------------------------------------------------------

||| Show instance for debugging
public export
Show JsonValue where
  show JsonNull = "null"
  show (JsonBool True) = "true"
  show (JsonBool False) = "false"
  show (JsonNumber n) = show n
  show (JsonString s) = show s
  show (JsonArray arr) = "[" ++ showElems arr ++ "]"
    where
      showElems : List JsonValue -> String
      showElems [] = ""
      showElems [x] = assert_total $ show x
      showElems (x :: xs) = assert_total $ show x ++ ", " ++ showElems xs
  show (JsonObject pairs) = "{" ++ showPairs pairs ++ "}"
    where
      showPairs : List (String, JsonValue) -> String
      showPairs [] = ""
      showPairs [(k, v)] = assert_total $ show k ++ ": " ++ show v
      showPairs ((k, v) :: ps) = assert_total $ show k ++ ": " ++ show v ++ ", " ++ showPairs ps

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
public export
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
public export
validate : JsonValue -> JsonType -> Maybe JsonValue
validate v t = if matchesType v t then Just v else Nothing
