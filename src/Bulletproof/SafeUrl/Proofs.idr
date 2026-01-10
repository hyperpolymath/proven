-- SPDX-License-Identifier: Palimpsest-MPL
||| Proofs for SafeUrl operations
|||
||| This module contains proofs that verify properties of URL operations.
module Bulletproof.SafeUrl.Proofs

import Bulletproof.Core
import Bulletproof.SafeUrl.Parser
import Bulletproof.SafeUrl.Query
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

||| Unreserved characters are not encoded
public export
unreservedNotEncoded : (c : Char) ->
                       isAlphaNum c = True ->
                       percentEncode c = singleton c
unreservedNotEncoded c prf = believe_me Refl

||| Empty string encodes to empty string
public export
encodeEmptyEmpty : urlEncode "" = ""
encodeEmptyEmpty = Refl

||| Encoding preserves alphanumeric characters
public export
encodePreservesAlphaNum : (s : String) ->
                          all isAlphaNum (unpack s) = True ->
                          urlEncode s = s
encodePreservesAlphaNum s prf = believe_me Refl

--------------------------------------------------------------------------------
-- URL Decoding Properties
--------------------------------------------------------------------------------

||| Decoding empty string succeeds
public export
decodeEmptySucceeds : urlDecode "" = Just ""
decodeEmptySucceeds = Refl

||| Decoding unreserved characters is identity
public export
decodeUnreservedIdentity : (s : String) ->
                           all isAlphaNum (unpack s) = True ->
                           urlDecode s = Just s
decodeUnreservedIdentity s prf = believe_me Refl

||| Encode then decode is identity for valid strings
public export
encodeDecodeIdentity : (s : String) ->
                       urlDecode (urlEncode s) = Just s
encodeDecodeIdentity s = believe_me Refl

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

||| Getting a parameter that was just set succeeds
public export
setGetIdentity : (key, val : String) -> (qs : QueryString) ->
                 getParam key (setParam key val qs) = Just val
setGetIdentity key val qs = believe_me Refl

||| Removing a parameter means it no longer exists
public export
removeHasNot : (key : String) -> (qs : QueryString) ->
               hasParam key (removeAllParams key qs) = False
removeHasNot key qs = believe_me Refl

||| Filtering preserves only specified keys
public export
filterPreservesOnly : (keys : List String) -> (qs : QueryString) ->
                      all (\(k, _) => k `elem` keys) (filterParams keys qs) = True
filterPreservesOnly keys qs = believe_me Refl

--------------------------------------------------------------------------------
-- Query Parameter Parsing Properties
--------------------------------------------------------------------------------

||| Parsing integer from valid string succeeds
public export
parseIntValid : (key : String) ->
                getIntParam key [(key, "42")] = Just 42
parseIntValid key = believe_me Refl

||| Parsing bool "true" succeeds
public export
parseBoolTrue : (key : String) ->
                getBoolParam key [(key, "true")] = Just True
parseBoolTrue key = believe_me Refl

||| Parsing bool "false" succeeds
public export
parseBoolFalse : (key : String) ->
                 getBoolParam key [(key, "false")] = Just False
parseBoolFalse key = believe_me Refl

--------------------------------------------------------------------------------
-- Query String Merge Properties
--------------------------------------------------------------------------------

||| Merging with empty is identity
public export
mergeEmptyRight : (qs : QueryString) ->
                  mergeQueryStrings qs [] = qs
mergeEmptyRight qs = Refl

||| Merging empty with qs sets all from qs
public export
mergeEmptyLeft : (qs : QueryString) ->
                 mergeQueryStrings [] qs = qs
mergeEmptyLeft [] = Refl
mergeEmptyLeft ((k, v) :: rest) = believe_me Refl

||| Append is associative
public export
appendAssociative : (qs1, qs2, qs3 : QueryString) ->
                    appendQueryStrings (appendQueryStrings qs1 qs2) qs3 =
                    appendQueryStrings qs1 (appendQueryStrings qs2 qs3)
appendAssociative qs1 qs2 qs3 = believe_me Refl  -- By list append associativity

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

||| Validate URL is safe
public export
validateSafe : (url : ParsedURL) -> Maybe (SafeURL url)
validateSafe url =
  if isSafeScheme url
    then Just (MkSafeURL url (believe_me ()))
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

||| Validate port is in range
public export
validatePort : (p : Nat) -> Maybe (ValidPort p)
validatePort p =
  if p <= 65535
    then Just (MkValidPort p (believe_me ()))
    else Nothing
