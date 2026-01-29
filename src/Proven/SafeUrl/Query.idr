-- SPDX-License-Identifier: Palimpsest-MPL-1.0
||| Safe Query String Operations
|||
||| This module provides safe operations for URL query strings.
||| All operations are total and handle edge cases gracefully.
module Proven.SafeUrl.Query

import Proven.Core
import Proven.SafeUrl.Parser
import Data.List
import Data.Maybe
import Data.String

%default total

--------------------------------------------------------------------------------
-- Query String Types
--------------------------------------------------------------------------------

||| Query string as list of key-value pairs
public export
QueryString : Type
QueryString = List (String, String)

joinWith : String -> List String -> String
joinWith _ [] = ""
joinWith sep (x :: xs) = foldl (\acc => \y => acc ++ sep ++ y) x xs


||| Query builder for fluent API
public export
record QueryBuilder where
  constructor MkQueryBuilder
  params : QueryString

--------------------------------------------------------------------------------
-- Query String Parsing
--------------------------------------------------------------------------------

||| Parse query string from raw string (without leading ?)
public export
parseQueryString : String -> QueryString
parseQueryString "" = []
parseQueryString s =
  let pairs = splitChar '&' s
  in mapMaybe parsePair pairs
  where
    splitOn : Char -> String -> (String, String)
    splitOn c str = go (unpack str) []
      where
        go : List Char -> List Char -> (String, String)
        go [] acc = (pack (reverse acc), "")
        go (x :: xs) acc =
          if x == c then (pack (reverse acc), pack xs)
                    else go xs (x :: acc)

    parsePair : String -> Maybe (String, String)
    parsePair "" = Nothing
    parsePair pair =
      let (key, val) = splitOn '=' pair
      in if key == "" then Nothing
                      else Just (urlDecodeOr key, urlDecodeOr val)

||| Parse query string with leading ?
public export
parseQuery : String -> QueryString
parseQuery s =
  if isPrefixOf "?" s
    then parseQueryString (strDrop 1 s)
    else parseQueryString s

--------------------------------------------------------------------------------
-- Query String Construction
--------------------------------------------------------------------------------

||| Create empty query builder
public export
emptyQuery : QueryBuilder
emptyQuery = MkQueryBuilder []

||| Add parameter to query builder
public export
addParam : String -> String -> QueryBuilder -> QueryBuilder
addParam key val qb = MkQueryBuilder (qb.params ++ [(key, val)])

||| Add optional parameter (only if Just)
public export
addParamMaybe : String -> Maybe String -> QueryBuilder -> QueryBuilder
addParamMaybe key (Just val) qb = addParam key val qb
addParamMaybe _ Nothing qb = qb

||| Add parameter only if value is non-empty
public export
addParamIfNotEmpty : String -> String -> QueryBuilder -> QueryBuilder
addParamIfNotEmpty key val qb =
  if val == "" then qb else addParam key val qb

||| Add multiple values for same key
public export
addParams : String -> List String -> QueryBuilder -> QueryBuilder
addParams key vals qb = foldl (\q, v => addParam key v q) qb vals

||| Remove parameter by key
public export
removeParam : String -> QueryBuilder -> QueryBuilder
removeParam key qb = MkQueryBuilder (filter (\(k, _) => k /= key) qb.params)

||| Build query string (without leading ?)
public export
buildQueryString : QueryBuilder -> String
buildQueryString qb = joinWith "&" (map formatPair qb.params)
  where
    formatPair : (String, String) -> String
    formatPair (k, v) = urlEncode k ++ "=" ++ urlEncode v

||| Build query string with leading ?
public export
buildQuery : QueryBuilder -> String
buildQuery qb =
  let qs = buildQueryString qb
  in if qs == "" then "" else "?" ++ qs

||| Convert QueryString to builder
public export
fromQueryString : QueryString -> QueryBuilder
fromQueryString qs = MkQueryBuilder qs

--------------------------------------------------------------------------------
-- Query String Access
--------------------------------------------------------------------------------

||| Get first value for a key
public export
getParam : String -> QueryString -> Maybe String
getParam key qs = lookup key qs

||| Get all values for a key
public export
getParams : String -> QueryString -> List String
getParams key qs = map snd (filter (\(k, _) => k == key) qs)

||| Get value with default
public export
getParamOr : String -> String -> QueryString -> String
getParamOr def key qs = case getParam key qs of
  Just val => val
  Nothing => def

||| Check if key exists
public export
hasParam : String -> QueryString -> Bool
hasParam key qs = isJust (getParam key qs)

||| Get all keys (unique)
public export
getKeys : QueryString -> List String
getKeys qs = nub (map fst qs)

||| Count parameters
public export
paramCount : QueryString -> Nat
paramCount = length

--------------------------------------------------------------------------------
-- Query String Manipulation
--------------------------------------------------------------------------------

||| Set parameter (replace if exists, add if not)
public export
setParam : String -> String -> QueryString -> QueryString
setParam key val qs =
  if hasParam key qs
    then map (\(k, v) => if k == key then (k, val) else (k, v)) qs
    else qs ++ [(key, val)]

||| Update parameter value with function
public export
updateParam : String -> (String -> String) -> QueryString -> QueryString
updateParam key f qs =
  map (\(k, v) => if k == key then (k, f v) else (k, v)) qs

||| Remove all occurrences of a key
public export
removeAllParams : String -> QueryString -> QueryString
removeAllParams key = filter (\(k, _) => k /= key)

||| Keep only specified keys
public export
filterParams : List String -> QueryString -> QueryString
filterParams keys = filter (\(k, _) => k `elem` keys)

||| Remove specified keys
public export
excludeParams : List String -> QueryString -> QueryString
excludeParams keys = filter (\(k, _) => not (k `elem` keys))

||| Merge two query strings (second overwrites first on conflicts)
public export
mergeQueryStrings : QueryString -> QueryString -> QueryString
mergeQueryStrings qs1 qs2 = foldl (\q, (k, v) => setParam k v q) qs1 qs2

||| Append query strings (keeps duplicates)
public export
appendQueryStrings : QueryString -> QueryString -> QueryString
appendQueryStrings = (++)

--------------------------------------------------------------------------------
-- Query String Validation
--------------------------------------------------------------------------------

||| Check if query string is empty
public export
isEmptyQuery : QueryString -> Bool
isEmptyQuery = null

||| Check if all keys are non-empty
public export
hasValidKeys : QueryString -> Bool
hasValidKeys = all (\(k, _) => k /= "")

||| Check if a value looks like a list (comma-separated)
public export
isListValue : String -> Bool
isListValue s = ',' `elem` unpack s

||| Split a comma-separated value
public export
splitListValue : String -> List String
splitListValue s = splitChar ',' s

--------------------------------------------------------------------------------
-- Query String Conversion
--------------------------------------------------------------------------------

||| Convert query string to JSON-like object representation
public export
toObject : QueryString -> List (String, String)
toObject = id

||| Convert from list of pairs
public export
fromPairs : List (String, String) -> QueryString
fromPairs = id

||| Convert to string representation
public export
toString : QueryString -> String
toString qs = buildQuery (fromQueryString qs)

--------------------------------------------------------------------------------
-- Safe Parameter Types
--------------------------------------------------------------------------------

||| Parse integer parameter
public export
getIntParam : String -> QueryString -> Maybe Integer
getIntParam key qs = getParam key qs >>= parseInteger
  where
    parseNatDigits : String -> Maybe Integer
    parseNatDigits str =
      let chars = unpack str
      in if all isDigit chars && not (null chars)
           then Just (cast (foldl (\n, c => n * 10 + cast (ord c - ord '0')) 0 chars))
           else Nothing

    parseInteger : String -> Maybe Integer
    parseInteger s = case strM s of
      StrNil => Nothing
      StrCons '-' rest => map negate (parseNatDigits rest)
      StrCons '+' rest => parseNatDigits rest
      StrCons _ _ => parseNatDigits s

||| Parse natural number parameter
public export
getNatParam : String -> QueryString -> Maybe Nat
getNatParam key qs = do
  val <- getParam key qs
  let chars = unpack val
  if all isDigit chars && not (null chars)
    then Just (foldl (\n, c => n * 10 + cast (ord c - ord '0')) 0 chars)
    else Nothing

||| Parse boolean parameter
public export
getBoolParam : String -> QueryString -> Maybe Bool
getBoolParam key qs = getParam key qs >>= parseBool
  where
    parseBool : String -> Maybe Bool
    parseBool s = case toLower s of
      "true" => Just True
      "1" => Just True
      "yes" => Just True
      "on" => Just True
      "false" => Just False
      "0" => Just False
      "no" => Just False
      "off" => Just False
      _ => Nothing

||| Parse list parameter (comma-separated)
public export
getListParam : String -> QueryString -> List String
getListParam key qs = case getParam key qs of
  Nothing => []
  Just val => splitListValue val
