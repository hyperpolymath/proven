-- SPDX-License-Identifier: Apache-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell <jonathan.jewell@open.ac.uk>
||| FFI exports for SafeUrl operations
|||
||| This module exports URL parsing and validation to the C ABI via Idris2's RefC backend.
||| All functions are proven total and handle malformed URLs safely.
|||
||| Return conventions:
||| - URL parsing → (status: Int, result: String)
|||   - status = 0: Valid, result is normalized URL or component
|||   - status = 1: Invalid, result contains error message
||| - Component extraction → String (empty if not present or invalid URL)
||| - Bool → Int (0 = false, 1 = true)
|||
||| CRITICAL: Always validate URLs from untrusted sources to prevent injection attacks.
module Proven.FFI.SafeUrl

import Proven.SafeUrl
import Proven.SafeUrl.Parser
import Proven.SafeUrl.Query
import Proven.Core

%default total

--------------------------------------------------------------------------------
-- FFI Result Encoding
--------------------------------------------------------------------------------

||| Encode Bool as Int
encodeBool : Bool -> Int
encodeBool False = 0
encodeBool True = 1

||| Encode URL parse result as (status, result/error)
encodeURLResult : Either URLParseError ParsedURL -> (Int, String)
encodeURLResult (Left err) = (1, show err)
encodeURLResult (Right url) = (0, renderURL url)
  where
    renderURL : ParsedURL -> String
    renderURL u =
      let scheme = maybe "" (\s => show s ++ "://") u.scheme
          host = maybe "" show u.host
          port = maybe "" (\p => ":" ++ show p) u.port
          path = if null u.path then "" else "/" ++ joinWith "/" u.path
          query = if null u.query then "" else "?" ++ joinWith "&" (map formatPair u.query)
          fragment = maybe "" (\f => "#" ++ f) u.fragment
      in scheme ++ host ++ port ++ path ++ query ++ fragment

    joinWith : String -> List String -> String
    joinWith _ [] = ""
    joinWith sep (x :: xs) = foldl (\acc, y => acc ++ sep ++ y) x xs

    formatPair : (String, String) -> String
    formatPair (k, v) = k ++ "=" ++ v

||| Encode Maybe String as (status, value)
encodeMaybeString : Maybe String -> (Int, String)
encodeMaybeString Nothing = (1, "")
encodeMaybeString (Just s) = (0, s)

--------------------------------------------------------------------------------
-- URL Parsing
--------------------------------------------------------------------------------

export
proven_idris_url_parse : String -> (Int, String)
proven_idris_url_parse s = encodeURLResult (parseURL s)

export
proven_idris_url_is_valid : String -> Int
proven_idris_url_is_valid s = encodeBool (isValidURL s)

--------------------------------------------------------------------------------
-- Component Extraction
--------------------------------------------------------------------------------

export
proven_idris_url_get_scheme : String -> (Int, String)
proven_idris_url_get_scheme s =
  case parseURLMaybe s of
    Nothing => (1, "")
    Just url => case url.scheme of
      Nothing => (1, "")
      Just scheme => (0, show scheme)

export
proven_idris_url_get_host : String -> (Int, String)
proven_idris_url_get_host s =
  case parseURLMaybe s of
    Nothing => (1, "")
    Just url => case url.host of
      Nothing => (1, "")
      Just host => (0, show host)

export
proven_idris_url_get_port : String -> (Int, String)
proven_idris_url_get_port s =
  case parseURLMaybe s of
    Nothing => (1, "")
    Just url => case url.port of
      Nothing => (1, "")
      Just port => (0, show port)

export
proven_idris_url_get_path : String -> (Int, String)
proven_idris_url_get_path s =
  case parseURLMaybe s of
    Nothing => (1, "")
    Just url => (0, if null url.path then "/" else "/" ++ joinWith "/" url.path)
  where
    joinWith : String -> List String -> String
    joinWith _ [] = ""
    joinWith sep (x :: xs) = foldl (\acc, y => acc ++ sep ++ y) x xs

export
proven_idris_url_get_query : String -> (Int, String)
proven_idris_url_get_query s =
  case parseURLMaybe s of
    Nothing => (1, "")
    Just url => (0, renderQuery url.query)
  where
    renderQuery : List (String, String) -> String
    renderQuery [] = ""
    renderQuery pairs = joinWith "&" (map formatPair pairs)

    joinWith : String -> List String -> String
    joinWith _ [] = ""
    joinWith sep (x :: xs) = foldl (\acc, y => acc ++ sep ++ y) x xs

    formatPair : (String, String) -> String
    formatPair (k, v) = k ++ "=" ++ v

export
proven_idris_url_get_fragment : String -> (Int, String)
proven_idris_url_get_fragment s =
  case parseURLMaybe s of
    Nothing => (1, "")
    Just url => case url.fragment of
      Nothing => (1, "")
      Just frag => (0, frag)

--------------------------------------------------------------------------------
-- Scheme Checking
--------------------------------------------------------------------------------

export
proven_idris_url_is_http : String -> Int
proven_idris_url_is_http s =
  case parseURLMaybe s of
    Nothing => 0
    Just url => case url.scheme of
      Just HTTP => 1
      _ => 0

export
proven_idris_url_is_https : String -> Int
proven_idris_url_is_https s =
  case parseURLMaybe s of
    Nothing => 0
    Just url => case url.scheme of
      Just HTTPS => 1
      _ => 0

export
proven_idris_url_is_secure : String -> Int
proven_idris_url_is_secure s =
  case parseURLMaybe s of
    Nothing => 0
    Just url => case url.scheme of
      Just HTTPS => 1
      Just FTPS => 1
      _ => 0

export
proven_idris_url_is_mailto : String -> Int
proven_idris_url_is_mailto s =
  case parseURLMaybe s of
    Nothing => 0
    Just url => case url.scheme of
      Just Mailto => 1
      _ => 0

export
proven_idris_url_is_file : String -> Int
proven_idris_url_is_file s =
  case parseURLMaybe s of
    Nothing => 0
    Just url => case url.scheme of
      Just File => 1
      _ => 0

--------------------------------------------------------------------------------
-- URL Normalization
--------------------------------------------------------------------------------

export
proven_idris_url_normalize : String -> (Int, String)
proven_idris_url_normalize s = encodeURLResult (normalizeURL s)

--------------------------------------------------------------------------------
-- Query String Operations
--------------------------------------------------------------------------------

export
proven_idris_url_query_get : String -> String -> String
proven_idris_url_query_get url key =
  case parseURLMaybe url of
    Nothing => ""
    Just parsed => maybe "" id (lookup key parsed.query)

export
proven_idris_url_query_has : String -> String -> Int
proven_idris_url_query_has url key =
  case parseURLMaybe url of
    Nothing => 0
    Just parsed => encodeBool (isJust (lookup key parsed.query))

export
proven_idris_url_query_keys : String -> String
proven_idris_url_query_keys url =
  case parseURLMaybe url of
    Nothing => ""
    Just parsed => joinWith "," (map fst parsed.query)
  where
    joinWith : String -> List String -> String
    joinWith _ [] = ""
    joinWith sep (x :: xs) = foldl (\acc, y => acc ++ sep ++ y) x xs

--------------------------------------------------------------------------------
-- Default Ports
--------------------------------------------------------------------------------

export
proven_idris_url_default_port_http : Int
proven_idris_url_default_port_http = 80

export
proven_idris_url_default_port_https : Int
proven_idris_url_default_port_https = 443

export
proven_idris_url_default_port_ftp : Int
proven_idris_url_default_port_ftp = 21

export
proven_idris_url_default_port_ftps : Int
proven_idris_url_default_port_ftps = 990

--------------------------------------------------------------------------------
-- Error Checking
--------------------------------------------------------------------------------

export
proven_idris_url_is_parse_error : String -> Int
proven_idris_url_is_parse_error errorMsg =
  if isInfixOf "parse" (toLower errorMsg) || isInfixOf "invalid" (toLower errorMsg)
    then 1
    else 0

export
proven_idris_url_is_scheme_error : String -> Int
proven_idris_url_is_scheme_error errorMsg =
  if isInfixOf "scheme" (toLower errorMsg)
    then 1
    else 0

export
proven_idris_url_is_host_error : String -> Int
proven_idris_url_is_host_error errorMsg =
  if isInfixOf "host" (toLower errorMsg)
    then 1
    else 0

export
proven_idris_url_is_port_error : String -> Int
proven_idris_url_is_port_error errorMsg =
  if isInfixOf "port" (toLower errorMsg)
    then 1
    else 0
