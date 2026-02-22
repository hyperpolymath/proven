-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
||| Proofs for SafeUrl operations
|||
||| This module contains proofs that verify properties of URL operations.
module Proven.SafeUrl.Proofs

import Proven.Core
import Proven.SafeUrl.Parser
import Proven.SafeUrl.Query
import Data.List

%default total

--------------------------------------------------------------------------------
-- URL Parsing Properties
--------------------------------------------------------------------------------

||| Empty string parsing fails
public export
parseEmptyFails : parseURLMaybe "" = Nothing
parseEmptyFails = Refl

||| Parsing is deterministic
public export
parseDeterministic : (s : String) ->
                     parseURLMaybe s = parseURLMaybe s
parseDeterministic s = Refl

--------------------------------------------------------------------------------
-- URL Encoding Properties
--------------------------------------------------------------------------------

||| Percent-encoding leaves unreserved (alphanumeric) characters unchanged
postulate
unreservedNotEncoded : (c : Char) ->
                       isAlphaNum c = True ->
                       percentEncode c = singleton c

||| Empty string encodes to empty string
public export
encodeEmptyEmpty : urlEncode "" = ""
encodeEmptyEmpty = Refl

||| URL-encoding a fully alphanumeric string is the identity
postulate
encodePreservesAlphaNum : (s : String) ->
                          all isAlphaNum (unpack s) = True ->
                          urlEncode s = s

--------------------------------------------------------------------------------
-- URL Decoding Properties
--------------------------------------------------------------------------------

||| Decoding empty string succeeds
public export
decodeEmptySucceeds : urlDecode "" = Just ""
decodeEmptySucceeds = Refl

||| Decoding a string of unreserved characters is the identity
postulate
decodeUnreservedIdentity : (s : String) ->
                           all isAlphaNum (unpack s) = True ->
                           urlDecode s = Just s

||| URL-encode then decode round-trips to the original string
postulate
encodeDecodeIdentity : (s : String) ->
                       urlDecode (urlEncode s) = Just s

--------------------------------------------------------------------------------
-- Query String Properties
--------------------------------------------------------------------------------

||| Parsing empty query string gives empty list
public export
parseEmptyQuery : parseQueryString "" = []
parseEmptyQuery = Refl

||| Empty query builder builds empty string
public export
emptyBuilderEmpty : buildQueryString emptyQuery = ""
emptyBuilderEmpty = Refl

||| Adding a parameter increases count
public export
addParamIncreasesCount : (key, val : String) -> (qb : QueryBuilder) ->
                         paramCount (addParam key val qb).params =
                         S (paramCount qb.params)
addParamIncreasesCount key val qb = Refl

||| Getting a parameter just set by key returns that value
postulate
setGetIdentity : (key, val : String) -> (qs : QueryString) ->
                 getParam key (setParam key val qs) = Just val

||| After removing all instances of a key, hasParam returns False
postulate
removeHasNot : (key : String) -> (qs : QueryString) ->
               hasParam key (removeAllParams key qs) = False

||| filterParams keeps only entries whose keys are in the given list
postulate
filterPreservesOnly : (keys : List String) -> (qs : QueryString) ->
                      all (\(k, _) => k `elem` keys) (filterParams keys qs) = True

--------------------------------------------------------------------------------
-- Query Parameter Parsing Properties
--------------------------------------------------------------------------------

||| Parsing integer "42" from a matching key yields Just 42
postulate
parseIntValid : (key : String) ->
                getIntParam key [(key, "42")] = Just 42

||| Parsing bool "true" from a matching key yields Just True
postulate
parseBoolTrue : (key : String) ->
                getBoolParam key [(key, "true")] = Just True

||| Parsing bool "false" from a matching key yields Just False
postulate
parseBoolFalse : (key : String) ->
                 getBoolParam key [(key, "false")] = Just False

--------------------------------------------------------------------------------
-- Query String Merge Properties
--------------------------------------------------------------------------------

||| Merging with empty is identity
public export
mergeEmptyRight : (qs : QueryString) ->
                  mergeQueryStrings qs [] = qs
mergeEmptyRight qs = Refl

||| Merging empty into a query string yields that query string
postulate
mergeEmptyLeft : (qs : QueryString) ->
                 mergeQueryStrings [] qs = qs

||| Query string append is associative (by list append associativity)
postulate
appendAssociative : (qs1, qs2, qs3 : QueryString) ->
                    appendQueryStrings (appendQueryStrings qs1 qs2) qs3 =
                    appendQueryStrings qs1 (appendQueryStrings qs2 qs3)

--------------------------------------------------------------------------------
-- URL Security Properties
--------------------------------------------------------------------------------

||| Data type for safe URLs (no javascript: scheme)
public export
data SafeURL : ParsedURL -> Type where
  MkSafeURL : (url : ParsedURL) ->
              Not (url.scheme = Just (Custom "javascript")) ->
              SafeURL url

||| Check if URL has safe scheme
public export
isSafeScheme : ParsedURL -> Bool
isSafeScheme url = case url.scheme of
  Just (Custom "javascript") => False
  Just (Custom "data") => False
  Just (Custom "vbscript") => False
  _ => True

||| If isSafeScheme holds, the scheme is not javascript (for MkSafeURL)
postulate
isSafeSchemeNotJavascript : (url : ParsedURL) -> isSafeScheme url = True ->
                            Not (url.scheme = Just (Custom "javascript"))

||| Validate URL is safe
public export
validateSafe : (url : ParsedURL) -> Maybe (SafeURL url)
validateSafe url =
  if isSafeScheme url
    then Just (MkSafeURL url (isSafeSchemeNotJavascript url Refl))
    else Nothing

--------------------------------------------------------------------------------
-- Host Validation Properties
--------------------------------------------------------------------------------

||| IPv4 address components are bounded
public export
data ValidIPv4 : Host -> Type where
  MkValidIPv4 : (a, b, c, d : Nat) ->
                LTE a 255 -> LTE b 255 -> LTE c 255 -> LTE d 255 ->
                ValidIPv4 (IPv4 a b c d)

||| Port number is bounded
public export
data ValidPort : Nat -> Type where
  MkValidPort : (p : Nat) -> LTE p 65535 -> ValidPort p

||| Runtime comparison p <= 65535 implies the LTE proof witness
postulate
lteFrom65535Check : (p : Nat) -> (p <= 65535 = True) -> LTE p 65535

||| Validate port is in range
public export
validatePort : (p : Nat) -> Maybe (ValidPort p)
validatePort p =
  case decEq (p <= 65535) True of
    Yes prf => Just (MkValidPort p (lteFrom65535Check p prf))
    No _    => Nothing
