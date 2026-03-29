-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
||| Safe JSON Access
|||
||| This module provides safe accessors for JSON values that never crash.
||| All operations return Maybe/Either instead of throwing exceptions.
module Proven.SafeJson.Access

import Proven.Core
import Proven.SafeJson.Parser
import Data.List
import Data.Maybe
import Data.String

-- JSON tree traversal functions are structurally recursive over nested
-- JsonValue but the totality checker cannot verify through where clauses.
%default covering

--------------------------------------------------------------------------------
-- JSON Path Types
--------------------------------------------------------------------------------

||| A path segment for navigating JSON
public export
data PathSegment
  = Key String      -- Object key access
  | Index Nat       -- Array index access

||| A complete path through JSON
public export
JsonPath : Type
JsonPath = List PathSegment

parsePathNat : List Char -> Nat
parsePathNat chars = foldl (\n, c => n * 10 + cast (ord c - ord '0')) 0 chars

mutual
  parsePathSegments : List Char -> List Char -> JsonPath -> JsonPath
  parsePathSegments [] [] acc = reverse acc
  parsePathSegments [] current acc = reverse (Key (pack (reverse current)) :: acc)
  parsePathSegments ('.' :: cs) [] acc = parsePathSegments cs [] acc
  parsePathSegments ('.' :: cs) current acc =
    parsePathSegments cs [] (Key (pack (reverse current)) :: acc)
  parsePathSegments ('[' :: cs) current acc =
    let acc' = if null current then acc else Key (pack (reverse current)) :: acc
    in parsePathIndex cs [] acc'
  parsePathSegments (c :: cs) current acc = parsePathSegments cs (c :: current) acc

  parsePathIndex : List Char -> List Char -> JsonPath -> JsonPath
  parsePathIndex [] _ acc = reverse acc  -- Malformed, just return what we have
  parsePathIndex (']' :: cs) digits acc =
    let idx = parsePathNat (reverse digits)
    in parsePathSegments cs [] (Index idx :: acc)
  parsePathIndex (c :: cs) digits acc =
    if c >= '0' && c <= '9'
      then parsePathIndex cs (c :: digits) acc
      else parsePathIndex cs digits acc  -- Skip non-digits

||| Parse a simple JSON path string like "user.address.city" or "items[0].name"
public export
parsePath : String -> JsonPath
parsePath s = parsePathSegments (unpack s) [] []

--------------------------------------------------------------------------------
-- Safe Path Access
--------------------------------------------------------------------------------

||| Access JSON value at a path segment
public export
getSegment : PathSegment -> JsonValue -> Maybe JsonValue
getSegment (Key k) (JsonObject pairs) = lookup k pairs
getSegment (Index i) (JsonArray arr) = index' i arr
  where
    index' : Nat -> List a -> Maybe a
    index' _ [] = Nothing
    index' Z (x :: _) = Just x
    index' (S n) (_ :: xs) = index' n xs
getSegment _ _ = Nothing

||| Access JSON value at a path
public export
getPath : JsonPath -> JsonValue -> Maybe JsonValue
getPath [] val = Just val
getPath (seg :: rest) val = case getSegment seg val of
  Nothing => Nothing
  Just val' => getPath rest val'

||| Access using path string (convenience function)
public export
getAt : String -> JsonValue -> Maybe JsonValue
getAt pathStr = getPath (parsePath pathStr)

||| Access with default value
public export
getAtOr : JsonValue -> String -> JsonValue -> JsonValue
getAtOr def pathStr json = case getAt pathStr json of
  Just val => val
  Nothing => def

--------------------------------------------------------------------------------
-- Type-Safe Extractors
--------------------------------------------------------------------------------

||| Get string at path
public export
getStringAt : String -> JsonValue -> Maybe String
getStringAt path json = getAt path json >>= asString
  where
    asString : JsonValue -> Maybe String
    asString (JsonString s) = Just s
    asString _ = Nothing

||| Get string at path with default
public export
getStringAtOr : String -> String -> JsonValue -> String
getStringAtOr def path json = case getStringAt path json of
  Just s => s
  Nothing => def

||| Get number at path
public export
getNumberAt : String -> JsonValue -> Maybe Double
getNumberAt path json = getAt path json >>= asNumber
  where
    asNumber : JsonValue -> Maybe Double
    asNumber (JsonNumber n) = Just n
    asNumber _ = Nothing

||| Get number at path with default
public export
getNumberAtOr : Double -> String -> JsonValue -> Double
getNumberAtOr def path json = case getNumberAt path json of
  Just n => n
  Nothing => def

||| Get integer at path
public export
getIntAt : String -> JsonValue -> Maybe Integer
getIntAt path json = getAt path json >>= asInt
  where
    asInt : JsonValue -> Maybe Integer
    asInt (JsonNumber n) = Just (cast n)
    asInt _ = Nothing

||| Get integer at path with default
public export
getIntAtOr : Integer -> String -> JsonValue -> Integer
getIntAtOr def path json = case getIntAt path json of
  Just n => n
  Nothing => def

||| Get boolean at path
public export
getBoolAt : String -> JsonValue -> Maybe Bool
getBoolAt path json = getAt path json >>= asBool
  where
    asBool : JsonValue -> Maybe Bool
    asBool (JsonBool b) = Just b
    asBool _ = Nothing

||| Get boolean at path with default
public export
getBoolAtOr : Bool -> String -> JsonValue -> Bool
getBoolAtOr def path json = case getBoolAt path json of
  Just b => b
  Nothing => def

||| Get array at path
public export
getArrayAt : String -> JsonValue -> Maybe (List JsonValue)
getArrayAt path json = getAt path json >>= asArray
  where
    asArray : JsonValue -> Maybe (List JsonValue)
    asArray (JsonArray arr) = Just arr
    asArray _ = Nothing

||| Get object at path
public export
getObjectAt : String -> JsonValue -> Maybe (List (String, JsonValue))
getObjectAt path json = getAt path json >>= asObject
  where
    asObject : JsonValue -> Maybe (List (String, JsonValue))
    asObject (JsonObject obj) = Just obj
    asObject _ = Nothing

--------------------------------------------------------------------------------
-- Safe Modification
--------------------------------------------------------------------------------

||| Set value at a path segment (returns Nothing if path invalid)
setSegment : PathSegment -> JsonValue -> JsonValue -> Maybe JsonValue
setSegment (Key k) newVal (JsonObject pairs) =
  Just (JsonObject (updatePairs k newVal pairs))
  where
    updatePairs : String -> JsonValue -> List (String, JsonValue) -> List (String, JsonValue)
    updatePairs key val [] = [(key, val)]
    updatePairs key val ((k', v') :: rest) =
      if key == k'
        then (key, val) :: rest
        else (k', v') :: updatePairs key val rest
setSegment (Index i) newVal (JsonArray arr) =
  Just (JsonArray (updateAt i newVal arr))
  where
    updateAt : Nat -> a -> List a -> List a
    updateAt _ _ [] = []
    updateAt Z x (_ :: xs) = x :: xs
    updateAt (S n) x (y :: xs) = y :: updateAt n x xs
setSegment _ _ _ = Nothing

||| Set value at a path
public export
setPath : JsonPath -> JsonValue -> JsonValue -> Maybe JsonValue
setPath [] newVal _ = Just newVal
setPath [seg] newVal json = setSegment seg newVal json
setPath (seg :: rest) newVal json = do
  current <- getSegment seg json
  updated <- setPath rest newVal current
  setSegment seg updated json

||| Set using path string (convenience function)
public export
setAt : String -> JsonValue -> JsonValue -> Maybe JsonValue
setAt pathStr newVal = setPath (parsePath pathStr) newVal

||| Update value at path with a function
public export
updateAt : String -> (JsonValue -> JsonValue) -> JsonValue -> Maybe JsonValue
updateAt pathStr f json = do
  current <- getAt pathStr json
  setAt pathStr (f current) json

||| Delete value at path
public export
deleteAt : String -> JsonValue -> Maybe JsonValue
deleteAt pathStr json =
  let path = parsePath pathStr
  in deletePath path json
  where
    deletePath : JsonPath -> JsonValue -> Maybe JsonValue
    deletePath [] _ = Nothing  -- Can't delete root
    deletePath [Key k] (JsonObject pairs) =
      Just (JsonObject (filter (\(k', _) => k' /= k) pairs))
    deletePath [Index i] (JsonArray arr) =
      Just (JsonArray (deleteAt' i arr))
      where
        deleteAt' : Nat -> List a -> List a
        deleteAt' _ [] = []
        deleteAt' Z (_ :: xs) = xs
        deleteAt' (S n) (x :: xs) = x :: deleteAt' n xs
    deletePath (seg :: rest) json' = do
      current <- getSegment seg json'
      updated <- deletePath rest current
      setSegment seg updated json'
    deletePath _ _ = Nothing

--------------------------------------------------------------------------------
-- JSON Querying
--------------------------------------------------------------------------------

||| Find all values matching a predicate
public export
findAll : (JsonValue -> Bool) -> JsonValue -> List JsonValue
findAll p json =
  let matches = if p json then [json] else []
  in matches ++ findInChildren p json
  where
    findInChildren : (JsonValue -> Bool) -> JsonValue -> List JsonValue
    findInChildren pred (JsonArray arr) = concatMap (findAll pred) arr
    findInChildren pred (JsonObject pairs) = concatMap (findAll pred . snd) pairs
    findInChildren _ _ = []

||| Find first value matching a predicate
public export
findFirst : (JsonValue -> Bool) -> JsonValue -> Maybe JsonValue
findFirst p json =
  if p json then Just json
  else findInChildren p json
  where
    findInChildren : (JsonValue -> Bool) -> JsonValue -> Maybe JsonValue
    findInChildren pred (JsonArray []) = Nothing
    findInChildren pred (JsonArray (x :: xs)) =
      case findFirst pred x of
        Just v => Just v
        Nothing => findInChildren pred (JsonArray xs)
    findInChildren pred (JsonObject []) = Nothing
    findInChildren pred (JsonObject ((_, v) :: pairs)) =
      case findFirst pred v of
        Just found => Just found
        Nothing => findInChildren pred (JsonObject pairs)
    findInChildren _ _ = Nothing

||| Check if any value matches a predicate
public export
exists : (JsonValue -> Bool) -> JsonValue -> Bool
exists p = isJust . findFirst p

||| Count values matching a predicate
public export
count : (JsonValue -> Bool) -> JsonValue -> Nat
count p = length . findAll p

--------------------------------------------------------------------------------
-- JSON Transformation
--------------------------------------------------------------------------------

||| Map a function over all values in JSON
public export
mapJson : (JsonValue -> JsonValue) -> JsonValue -> JsonValue
mapJson f json = f (mapChildren f json)
  where
    mapChildren : (JsonValue -> JsonValue) -> JsonValue -> JsonValue
    mapChildren g (JsonArray arr) = JsonArray (map (mapJson g) arr)
    mapChildren g (JsonObject pairs) = JsonObject (map (\(k, v) => (k, mapJson g v)) pairs)
    mapChildren _ other = other

||| Filter object keys by predicate
public export
filterKeys : (String -> Bool) -> JsonValue -> JsonValue
filterKeys p (JsonObject pairs) = JsonObject (filter (\(k, _) => p k) pairs)
filterKeys _ json = json

||| Select only specified keys from an object
public export
selectKeys : List String -> JsonValue -> JsonValue
selectKeys keys (JsonObject pairs) =
  JsonObject (filter (\(k, _) => k `elem` keys) pairs)
selectKeys _ json = json

||| Remove specified keys from an object
public export
omitKeys : List String -> JsonValue -> JsonValue
omitKeys keys (JsonObject pairs) =
  JsonObject (filter (\(k, _) => not (k `elem` keys)) pairs)
omitKeys _ json = json

--------------------------------------------------------------------------------
-- JSON Diffing
--------------------------------------------------------------------------------

||| Difference between two JSON values
public export
data JsonDiff
  = Added JsonPath JsonValue
  | Removed JsonPath JsonValue
  | Changed JsonPath JsonValue JsonValue
  | Unchanged

showPathSeg : PathSegment -> String
showPathSeg (Key k) = "." ++ k
showPathSeg (Index i) = "[" ++ show i ++ "]"

showJsonPath : JsonPath -> String
showJsonPath [] = "$"
showJsonPath segs = "$" ++ concatMap showPathSeg segs

-- covering because Show JsonValue uses mutual recursion through showKV
-- which the totality checker cannot verify terminates
public export covering
Show JsonDiff where
  show (Added path val) = "Added at " ++ showJsonPath path ++ ": " ++ show val
  show (Removed path val) = "Removed at " ++ showJsonPath path ++ ": " ++ show val
  show (Changed path old new) = "Changed at " ++ showJsonPath path ++ ": " ++ show old ++ " -> " ++ show new
  show Unchanged = "Unchanged"
