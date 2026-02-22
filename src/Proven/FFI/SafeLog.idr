-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
||| FFI exports for SafeLog operations
|||
||| This module exports structured logging with PII redaction to the C ABI
||| via Idris2's RefC backend. All functions are proven total and handle log levels.
|||
||| Return conventions:
||| - Log level → Int (0=Trace, 1=Debug, 2=Info, 3=Warn, 4=Error, 5=Fatal)
||| - Validation → Int (0 = invalid/false, 1 = valid/true)
||| - Counts → Int (number of entries, errors, etc.)
|||
||| CRITICAL: PII (personally identifiable information) must be redacted from logs.
|||           Email, IP addresses, and sensitive data should be masked.
|||
||| Escaping for PII: email → e***@domain, IP → xxx.xxx.xxx.xxx, etc.
module Proven.FFI.SafeLog

import Proven.SafeLog
import Proven.Core
import Data.String

%default total

--------------------------------------------------------------------------------
-- FFI Result Encoding
--------------------------------------------------------------------------------

||| Encode Bool as Int
encodeBool : Bool -> Int
encodeBool False = 0
encodeBool True = 1

||| Encode LogLevel as Int
encodeLogLevel : LogLevel -> Int
encodeLogLevel Trace = 0
encodeLogLevel Debug = 1
encodeLogLevel Info = 2
encodeLogLevel Warn = 3
encodeLogLevel Error = 4
encodeLogLevel Fatal = 5

||| Decode Int to LogLevel
decodeLogLevel : Int -> Maybe LogLevel
decodeLogLevel 0 = Just Trace
decodeLogLevel 1 = Just Debug
decodeLogLevel 2 = Just Info
decodeLogLevel 3 = Just Warn
decodeLogLevel 4 = Just Error
decodeLogLevel 5 = Just Fatal
decodeLogLevel _ = Nothing

||| Parse context from comma-separated k=v pairs
parseContext : String -> List (String, String)
parseContext s =
  if s == "" then []
  else mapMaybe parsePair (split (== ',') s)
  where
    parsePair : String -> Maybe (String, String)
    parsePair pair =
      case split (== '=') pair of
        [k, v] => Just (k, v)
        _ => Nothing

||| Format context to comma-separated k=v pairs
formatContext : List (String, String) -> String
formatContext [] = ""
formatContext [(k, v)] = k ++ "=" ++ v
formatContext ((k, v) :: rest) = k ++ "=" ++ v ++ "," ++ formatContext rest

--------------------------------------------------------------------------------
-- Log Level Operations
--------------------------------------------------------------------------------

export
proven_idris_log_encode_level : String -> Int
proven_idris_log_encode_level s =
  case toLower s of
    "trace" => 0
    "debug" => 1
    "info" => 2
    "warn" => 3
    "warning" => 3
    "error" => 4
    "fatal" => 5
    "critical" => 5
    _ => (-1)

export
proven_idris_log_decode_level : Int -> String
proven_idris_log_decode_level levelCode =
  case decodeLogLevel levelCode of
    Nothing => "unknown"
    Just Trace => "trace"
    Just Debug => "debug"
    Just Info => "info"
    Just Warn => "warn"
    Just Error => "error"
    Just Fatal => "fatal"

export
proven_idris_log_is_valid_level : Int -> Int
proven_idris_log_is_valid_level levelCode =
  encodeBool (isJust (decodeLogLevel levelCode))

export
proven_idris_log_level_priority : Int -> Int
proven_idris_log_level_priority levelCode =
  case decodeLogLevel levelCode of
    Nothing => (-1)
    Just lvl => cast (levelPriority lvl)

export
proven_idris_log_levels_equal : Int -> Int -> Int
proven_idris_log_levels_equal l1 l2 = encodeBool (l1 == l2)

export
proven_idris_log_level_compare : Int -> Int -> Int
proven_idris_log_level_compare l1 l2 =
  case (decodeLogLevel l1, decodeLogLevel l2) of
    (Just lvl1, Just lvl2) =>
      case compare lvl1 lvl2 of
        LT => (-1)
        EQ => 0
        GT => 1
    _ => 0

export
proven_idris_log_should_log : Int -> Int -> Int
proven_idris_log_should_log minLevel currentLevel =
  encodeBool (currentLevel >= minLevel)

--------------------------------------------------------------------------------
-- Logger State Operations
--------------------------------------------------------------------------------

export
proven_idris_log_entry_count : Int -> Int
proven_idris_log_entry_count count = count

export
proven_idris_log_has_entries : Int -> Int
proven_idris_log_has_entries count = encodeBool (count > 0)

export
proven_idris_log_is_empty : Int -> Int
proven_idris_log_is_empty count = encodeBool (count == 0)

export
proven_idris_log_max_entries_reached : Int -> Int -> Int
proven_idris_log_max_entries_reached current max =
  encodeBool (current >= max)

--------------------------------------------------------------------------------
-- Error/Warning Counting
--------------------------------------------------------------------------------

export
proven_idris_log_is_error_level : Int -> Int
proven_idris_log_is_error_level levelCode =
  case decodeLogLevel levelCode of
    Just Error => 1
    Just Fatal => 1
    _ => 0

export
proven_idris_log_is_warning_level : Int -> Int
proven_idris_log_is_warning_level levelCode =
  case decodeLogLevel levelCode of
    Just Warn => 1
    _ => 0

export
proven_idris_log_is_info_or_below : Int -> Int
proven_idris_log_is_info_or_below levelCode =
  case decodeLogLevel levelCode of
    Just Trace => 1
    Just Debug => 1
    Just Info => 1
    _ => 0

--------------------------------------------------------------------------------
-- Time Range Validation
--------------------------------------------------------------------------------

export
proven_idris_log_is_valid_timestamp : Int -> Int
proven_idris_log_is_valid_timestamp timestamp =
  encodeBool (timestamp >= 0)

export
proven_idris_log_in_time_range : Int -> Int -> Int -> Int
proven_idris_log_in_time_range timestamp fromTime toTime =
  encodeBool (timestamp >= fromTime && timestamp <= toTime)

export
proven_idris_log_timestamps_ordered : Int -> Int -> Int
proven_idris_log_timestamps_ordered earlier later =
  encodeBool (earlier <= later)

--------------------------------------------------------------------------------
-- Context Operations
--------------------------------------------------------------------------------

export
proven_idris_log_context_pair_count : String -> Int
proven_idris_log_context_pair_count contextStr =
  cast (length (parseContext contextStr))

export
proven_idris_log_has_context : String -> Int
proven_idris_log_has_context contextStr =
  encodeBool (not (null contextStr))

export
proven_idris_log_add_context_pair : String -> String -> String -> String
proven_idris_log_add_context_pair existingContext key value =
  let existing = parseContext existingContext
      newPair = (key, value)
      updated = newPair :: existing
  in formatContext updated

export
proven_idris_log_merge_context : String -> String -> String
proven_idris_log_merge_context ctx1 ctx2 =
  let pairs1 = parseContext ctx1
      pairs2 = parseContext ctx2
      merged = pairs2 ++ pairs1  -- ctx2 takes precedence
  in formatContext merged

--------------------------------------------------------------------------------
-- PII Redaction Helpers
--------------------------------------------------------------------------------

||| Check if string looks like an email
containsEmail : String -> Bool
containsEmail s = any (== '@') (unpack s)

||| Check if string looks like an IP address
containsIP : String -> Bool
containsIP s =
  let parts = split (== '.') s
  in length parts == 4 && all isDigitString parts
  where
    isDigitString : String -> Bool
    isDigitString str = all isDigit (unpack str)

||| Redact email addresses (keep first letter and domain)
redactEmail : String -> String
redactEmail email =
  case split (== '@') email of
    [local, domain] =>
      let localFirst = case unpack local of
                         (c :: _) => strCons c ""
                         []       => ""
      in localFirst ++ "***@" ++ domain
    _ => email

||| Redact IP address
redactIP : String -> String
redactIP _ = "xxx.xxx.xxx.xxx"

export
proven_idris_log_contains_email : String -> Int
proven_idris_log_contains_email s = encodeBool (containsEmail s)

export
proven_idris_log_contains_ip : String -> Int
proven_idris_log_contains_ip s = encodeBool (containsIP s)

export
proven_idris_log_redact_email : String -> String
proven_idris_log_redact_email s = redactEmail s

export
proven_idris_log_redact_ip : String -> String
proven_idris_log_redact_ip s = redactIP s

export
proven_idris_log_contains_pii : String -> Int
proven_idris_log_contains_pii s =
  encodeBool (containsEmail s || containsIP s)

--------------------------------------------------------------------------------
-- String Matching (for search)
--------------------------------------------------------------------------------

export
proven_idris_log_message_contains : String -> String -> Int
proven_idris_log_message_contains message searchTerm =
  encodeBool (isInfixOf searchTerm message)
  where
    isInfixOf : String -> String -> Bool
    isInfixOf needle haystack =
      let ns = unpack needle
          hs = unpack haystack
      in checkInfix ns hs

    checkInfix : List Char -> List Char -> Bool
    checkInfix [] _ = True
    checkInfix _ [] = False
    checkInfix ns (h :: hs) =
      startsWith ns (h :: hs) || checkInfix ns hs

    startsWith : List Char -> List Char -> Bool
    startsWith [] _ = True
    startsWith _ [] = False
    startsWith (n :: ns) (h :: hs) = n == h && startsWith ns hs

export
proven_idris_log_logger_name_matches : String -> String -> Int
proven_idris_log_logger_name_matches actual expected =
  encodeBool (actual == expected)

--------------------------------------------------------------------------------
-- Configuration Validation
--------------------------------------------------------------------------------

export
proven_idris_log_is_valid_max_entries : Int -> Int
proven_idris_log_is_valid_max_entries maxEntries =
  encodeBool (maxEntries > 0)

export
proven_idris_log_is_valid_logger_name : String -> Int
proven_idris_log_is_valid_logger_name name =
  encodeBool (not (null name))

--------------------------------------------------------------------------------
-- Error Messages
--------------------------------------------------------------------------------

export
proven_idris_log_friendly_error : String -> String
proven_idris_log_friendly_error errorMsg =
  if isInfixOf "level" (toLower errorMsg)
    then "Invalid log level (must be trace/debug/info/warn/error/fatal)"
  else if isInfixOf "timestamp" (toLower errorMsg)
    then "Invalid timestamp (must be non-negative)"
  else if isInfixOf "pii" (toLower errorMsg) || isInfixOf "redact" (toLower errorMsg)
    then "PII detected in log message (must be redacted)"
  else if isInfixOf "context" (toLower errorMsg)
    then "Invalid context format (expected key=value pairs)"
  else if isInfixOf "max" (toLower errorMsg)
    then "Logger at maximum capacity"
  else
    "Logging error"
