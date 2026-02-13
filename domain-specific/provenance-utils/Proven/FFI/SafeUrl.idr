-- SPDX-License-Identifier: PMPL-1.0-or-later
||| FFI exports for SafeUrl operations (Idris-only logic)
module Proven.FFI.SafeUrl

import Proven.SafeUrl.Parser
import Data.List
import Data.String

%default total

urlPathToString : List String -> String
urlPathToString [] = ""
urlPathToString segs = "/" ++ join "/" segs

queryToString : List (String, String) -> String
queryToString [] = ""
queryToString pairs = join "&" (map (\(k, v) => k ++ "=" ++ v) pairs)

%export
proven_idris_url_is_valid : String -> Bool
proven_idris_url_is_valid s = isValidURL s

%export
proven_idris_url_scheme : String -> String
proven_idris_url_scheme s = case parseURLMaybe s of
  Just url => case url.scheme of
    Just sch => show sch
    Nothing => ""
  Nothing => ""

%export
proven_idris_url_host : String -> String
proven_idris_url_host s = case parseURLMaybe s of
  Just url => case url.host of
    Just h => show h
    Nothing => ""
  Nothing => ""

%export
proven_idris_url_port : String -> Int
proven_idris_url_port s = case parseURLMaybe s of
  Just url => case url.port of
    Just p => cast p
    Nothing => -1
  Nothing => -1

%export
proven_idris_url_path : String -> String
proven_idris_url_path s = case parseURLMaybe s of
  Just url => urlPathToString url.path
  Nothing => ""

%export
proven_idris_url_query : String -> String
proven_idris_url_query s = case parseURLMaybe s of
  Just url => queryToString url.query
  Nothing => ""

%export
proven_idris_url_fragment : String -> String
proven_idris_url_fragment s = case parseURLMaybe s of
  Just url => maybe "" id url.fragment
  Nothing => ""
