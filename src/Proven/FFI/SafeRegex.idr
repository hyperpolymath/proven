-- SPDX-License-Identifier: Apache-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
||| FFI exports for SafeRegex operations
|||
||| This module exports ReDoS prevention and regex operations to the C ABI via Idris2's RefC backend.
||| All functions are proven total and detect exponential backtracking risks.
|||
||| Return conventions:
||| - Pattern parsing → (status: Int, error: String)
|||   - status = 0: Safe pattern
|||   - status = 1: Invalid pattern, error contains reason
||| - Pattern matching → Int (0 = no match, 1 = match)
||| - Complexity analysis → (level: Int, warnings: String)
||| - Bool → Int (0 = false, 1 = true)
|||
||| CRITICAL: Validate regex patterns before use. Untrusted regexes can cause ReDoS attacks.
module Proven.FFI.SafeRegex

import Proven.SafeRegex
import Proven.SafeRegex.Types
import Proven.SafeRegex.Safety
import Proven.SafeRegex.Parser
import Proven.Core

%default total

--------------------------------------------------------------------------------
-- FFI Result Encoding
--------------------------------------------------------------------------------

||| Encode Bool as Int
encodeBool : Bool -> Int
encodeBool False = 0
encodeBool True = 1

||| Encode Either RegexError SafeRegex as (status, error)
encodeRegexResult : Either RegexError SafeRegex -> (Int, String)
encodeRegexResult (Left err) = (1, show err)
encodeRegexResult (Right _) = (0, "")

||| Encode ComplexityLevel as Int
encodeComplexityLevel : ComplexityLevel -> Int
encodeComplexityLevel Linear = 0
encodeComplexityLevel Quadratic = 1
encodeComplexityLevel Exponential = 2
encodeComplexityLevel Unbounded = 3

||| Encode ComplexityAnalysis as (level, quantifiers, alternations, depth, warnings)
encodeComplexity : ComplexityAnalysis -> (Int, Int, Int, Int, String)
encodeComplexity analysis =
  ( encodeComplexityLevel analysis.level
  , cast analysis.quantifierCount
  , cast analysis.alternationCount
  , cast analysis.maxDepth
  , unlines analysis.warnings
  )

--------------------------------------------------------------------------------
-- Pattern Parsing
--------------------------------------------------------------------------------

export
proven_idris_regex_parse : String -> (Int, String)
proven_idris_regex_parse pattern = encodeRegexResult (regex pattern)

export
proven_idris_regex_parse_strict : String -> (Int, String)
proven_idris_regex_parse_strict pattern = encodeRegexResult (regexStrict pattern)

export
proven_idris_regex_parse_relaxed : String -> (Int, String)
proven_idris_regex_parse_relaxed pattern = encodeRegexResult (regexRelaxed pattern)

export
proven_idris_regex_is_pattern_safe : String -> Int
proven_idris_regex_is_pattern_safe pattern = encodeBool (isPatternSafe pattern)

--------------------------------------------------------------------------------
-- ReDoS Risk Detection
--------------------------------------------------------------------------------

export
proven_idris_regex_has_redos_risk : String -> Int
proven_idris_regex_has_redos_risk pattern = encodeBool (hasRedosRisk pattern)

export
proven_idris_regex_analyze_complexity : String -> (Int, Int, Int, Int, String)
proven_idris_regex_analyze_complexity pattern =
  case analyzePattern pattern of
    Left err => (3, 0, 0, 0, show err)  -- Unbounded for errors
    Right analysis => encodeComplexity analysis

export
proven_idris_regex_get_warnings : String -> (Int, String)
proven_idris_regex_get_warnings pattern =
  case patternWarnings pattern of
    Left err => (1, show err)
    Right warnings => (0, unlines warnings)

--------------------------------------------------------------------------------
-- Pattern Matching
--------------------------------------------------------------------------------

export
proven_idris_regex_quick_test : String -> String -> Int
proven_idris_regex_quick_test pattern input =
  encodeBool (quickTest pattern input)

export
proven_idris_regex_quick_test_full : String -> String -> Int
proven_idris_regex_quick_test_full pattern input =
  encodeBool (quickTestFull pattern input)

export
proven_idris_regex_quick_find : String -> String -> (Int, String)
proven_idris_regex_quick_find pattern input =
  case quickFind pattern input of
    Nothing => (1, "")
    Just matched => (0, matched)

export
proven_idris_regex_quick_replace : String -> String -> String -> String
proven_idris_regex_quick_replace pattern input replacement =
  quickReplace pattern input replacement

export
proven_idris_regex_quick_replace_all : String -> String -> String -> String
proven_idris_regex_quick_replace_all pattern input replacement =
  quickReplaceAll pattern input replacement

--------------------------------------------------------------------------------
-- Pre-built Safe Patterns
--------------------------------------------------------------------------------

export
proven_idris_regex_email_pattern : String
proven_idris_regex_email_pattern = emailPattern

export
proven_idris_regex_url_pattern : String
proven_idris_regex_url_pattern = urlPattern

export
proven_idris_regex_ipv4_pattern : String
proven_idris_regex_ipv4_pattern = ipv4Pattern

export
proven_idris_regex_uuid_pattern : String
proven_idris_regex_uuid_pattern = uuidPattern

export
proven_idris_regex_integer_pattern : String
proven_idris_regex_integer_pattern = integerPattern

export
proven_idris_regex_decimal_pattern : String
proven_idris_regex_decimal_pattern = decimalPattern

export
proven_idris_regex_identifier_pattern : String
proven_idris_regex_identifier_pattern = identifierPattern

export
proven_idris_regex_hex_color_pattern : String
proven_idris_regex_hex_color_pattern = hexColorPattern

export
proven_idris_regex_date_pattern : String
proven_idris_regex_date_pattern = datePattern

export
proven_idris_regex_time_pattern : String
proven_idris_regex_time_pattern = timePattern

--------------------------------------------------------------------------------
-- Complexity Level Constants
--------------------------------------------------------------------------------

export
proven_idris_regex_complexity_linear : Int
proven_idris_regex_complexity_linear = encodeComplexityLevel Linear

export
proven_idris_regex_complexity_quadratic : Int
proven_idris_regex_complexity_quadratic = encodeComplexityLevel Quadratic

export
proven_idris_regex_complexity_exponential : Int
proven_idris_regex_complexity_exponential = encodeComplexityLevel Exponential

export
proven_idris_regex_complexity_unbounded : Int
proven_idris_regex_complexity_unbounded = encodeComplexityLevel Unbounded

--------------------------------------------------------------------------------
-- Error Checking
--------------------------------------------------------------------------------

export
proven_idris_regex_is_parse_error : String -> Int
proven_idris_regex_is_parse_error errorMsg =
  if isInfixOf "parse" (toLower errorMsg) || isInfixOf "syntax" (toLower errorMsg)
    then 1
    else 0

export
proven_idris_regex_is_complexity_error : String -> Int
proven_idris_regex_is_complexity_error errorMsg =
  if isInfixOf "complexity" (toLower errorMsg) || isInfixOf "redos" (toLower errorMsg)
    then 1
    else 0

export
proven_idris_regex_is_nested_quantifier_error : String -> Int
proven_idris_regex_is_nested_quantifier_error errorMsg =
  if isInfixOf "nested" (toLower errorMsg) || isInfixOf "quantifier" (toLower errorMsg)
    then 1
    else 0

export
proven_idris_regex_is_overlapping_error : String -> Int
proven_idris_regex_is_overlapping_error errorMsg =
  if isInfixOf "overlapping" (toLower errorMsg) || isInfixOf "ambiguous" (toLower errorMsg)
    then 1
    else 0

--------------------------------------------------------------------------------
-- Safety Recommendations
--------------------------------------------------------------------------------

export
proven_idris_regex_max_safe_quantifiers : Int
proven_idris_regex_max_safe_quantifiers = 5

export
proven_idris_regex_max_safe_alternations : Int
proven_idris_regex_max_safe_alternations = 10

export
proven_idris_regex_max_safe_depth : Int
proven_idris_regex_max_safe_depth = 10

export
proven_idris_regex_default_step_limit : Int
proven_idris_regex_default_step_limit = 10000
