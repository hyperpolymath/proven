-- SPDX-License-Identifier: Apache-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
||| SafeRegex.Safety - ReDoS detection and complexity analysis
|||
||| This module provides functions to analyze regular expressions for
||| potential ReDoS (Regular Expression Denial of Service) vulnerabilities.
|||
||| ReDoS occurs when a regex has exponential backtracking behavior,
||| allowing an attacker to cause CPU exhaustion with crafted input.
|||
||| Common ReDoS patterns:
||| - Nested quantifiers: (a+)+
||| - Overlapping alternations: (a|a)+
||| - Ambiguous quantifiers: (.*a.*)+
module Proven.SafeRegex.Safety

import Proven.Core
import Proven.SafeRegex.Types
import Data.List
import Data.String

%default total

--------------------------------------------------------------------------------
-- ReDoS Pattern Detection
--------------------------------------------------------------------------------

||| Check if a regex has nested quantifiers (major ReDoS indicator)
||| Example: (a+)+ or (a*)*b
public export
hasNestedQuantifiers : Regex -> Bool
hasNestedQuantifiers = go False
  where
    go : Bool -> Regex -> Bool
    go _ Empty = False
    go _ Never = False
    go _ (Match _) = False
    go inQuant (Seq r1 r2) = go inQuant r1 || go inQuant r2
    go inQuant (Alt r1 r2) = go inQuant r1 || go inQuant r2
    go inQuant (Quant r q) =
      if inQuant && not (isBounded q)
        then True  -- Found nested unbounded quantifier!
        else go True r
    go inQuant (Group _ r) = go inQuant r
    go inQuant (NCGroup r) = go inQuant r
    go _ StartAnchor = False
    go _ EndAnchor = False
    go _ WordBoundary = False
    go _ (BackRef _) = False
    go inQuant (Lookahead _ r) = go inQuant r
    go inQuant (Lookbehind _ r) = go inQuant r

||| Check if two character classes can match the same character
public export
classesOverlap : CharClass -> CharClass -> Bool
classesOverlap Any _ = True
classesOverlap _ Any = True
classesOverlap (Char c1) (Char c2) = c1 == c2
classesOverlap (Char c) (Range from to) = c >= from && c <= to
classesOverlap (Range from to) (Char c) = c >= from && c <= to
classesOverlap (Range f1 t1) (Range f2 t2) = not (t1 < f2 || t2 < f1)
classesOverlap Digit Digit = True
classesOverlap Digit (Char c) = c >= '0' && c <= '9'
classesOverlap (Char c) Digit = c >= '0' && c <= '9'
classesOverlap Digit (Range from to) = not (to < '0' || from > '9')
classesOverlap (Range from to) Digit = not (to < '0' || from > '9')
classesOverlap Word Word = True
classesOverlap Space Space = True
classesOverlap (Union c1 c2) other = classesOverlap c1 other || classesOverlap c2 other
classesOverlap other (Union c1 c2) = classesOverlap other c1 || classesOverlap other c2
classesOverlap (Negate _) _ = True  -- Conservative: negation can match many things
classesOverlap _ (Negate _) = True
classesOverlap _ _ = False  -- Conservative default

||| Get the first character class that can be matched by a regex
||| Returns Nothing if regex can match empty string first
public export
firstMatch : Regex -> Maybe CharClass
firstMatch Empty = Nothing
firstMatch Never = Nothing
firstMatch (Match c) = Just c
firstMatch (Seq r1 r2) =
  case firstMatch r1 of
    Just c => Just c
    Nothing => firstMatch r2
firstMatch (Alt r1 r2) =
  case (firstMatch r1, firstMatch r2) of
    (Just c1, Just c2) => Just (Union c1 c2)
    (Just c, Nothing) => Just c
    (Nothing, Just c) => Just c
    (Nothing, Nothing) => Nothing
firstMatch (Quant r q) =
  if q.minCount == 0
    then Nothing  -- Can skip the quantified part
    else firstMatch r
firstMatch (Group _ r) = firstMatch r
firstMatch (NCGroup r) = firstMatch r
firstMatch StartAnchor = Nothing
firstMatch EndAnchor = Nothing
firstMatch WordBoundary = Nothing
firstMatch (BackRef _) = Nothing  -- Can't determine statically
firstMatch (Lookahead _ _) = Nothing
firstMatch (Lookbehind _ _) = Nothing

||| Check if two regexes have overlapping first characters
||| This is a ReDoS indicator when combined with alternation
public export
regexesOverlap : Regex -> Regex -> Bool
regexesOverlap r1 r2 =
  case (firstMatch r1, firstMatch r2) of
    (Just c1, Just c2) => classesOverlap c1 c2
    _ => True  -- Conservative: if we can't determine, assume overlap

||| Check if regex has overlapping alternatives (ReDoS indicator)
||| Example: (a|ab)+ or (.*|.+)
public export
hasOverlappingAlternatives : Regex -> Bool
hasOverlappingAlternatives = go
  where
    go : Regex -> Bool
    go Empty = False
    go Never = False
    go (Match _) = False
    go (Seq r1 r2) = go r1 || go r2
    go (Alt r1 r2) =
      regexesOverlap r1 r2 || go r1 || go r2
    go (Quant r _) = go r
    go (Group _ r) = go r
    go (NCGroup r) = go r
    go StartAnchor = False
    go EndAnchor = False
    go WordBoundary = False
    go (BackRef _) = False
    go (Lookahead _ r) = go r
    go (Lookbehind _ r) = go r

||| Check for quantifier on potentially empty content
||| Example: (a?)*
public export
hasQuantifiedEmpty : Regex -> Bool
hasQuantifiedEmpty = go
  where
    canBeEmpty : Regex -> Bool
    canBeEmpty Empty = True
    canBeEmpty Never = False
    canBeEmpty (Match _) = False
    canBeEmpty (Seq r1 r2) = canBeEmpty r1 && canBeEmpty r2
    canBeEmpty (Alt r1 r2) = canBeEmpty r1 || canBeEmpty r2
    canBeEmpty (Quant _ q) = q.minCount == 0
    canBeEmpty (Group _ r) = canBeEmpty r
    canBeEmpty (NCGroup r) = canBeEmpty r
    canBeEmpty StartAnchor = True
    canBeEmpty EndAnchor = True
    canBeEmpty WordBoundary = True
    canBeEmpty (BackRef _) = True  -- Can match empty if group did
    canBeEmpty (Lookahead _ _) = True
    canBeEmpty (Lookbehind _ _) = True

    go : Regex -> Bool
    go Empty = False
    go Never = False
    go (Match _) = False
    go (Seq r1 r2) = go r1 || go r2
    go (Alt r1 r2) = go r1 || go r2
    go (Quant r q) =
      (canBeEmpty r && not (isBounded q)) || go r
    go (Group _ r) = go r
    go (NCGroup r) = go r
    go StartAnchor = False
    go EndAnchor = False
    go WordBoundary = False
    go (BackRef _) = False
    go (Lookahead _ r) = go r
    go (Lookbehind _ r) = go r

--------------------------------------------------------------------------------
-- Complexity Analysis
--------------------------------------------------------------------------------

||| Count quantifiers in a regex
public export
countQuantifiers : Regex -> Nat
countQuantifiers Empty = 0
countQuantifiers Never = 0
countQuantifiers (Match _) = 0
countQuantifiers (Seq r1 r2) = countQuantifiers r1 + countQuantifiers r2
countQuantifiers (Alt r1 r2) = countQuantifiers r1 + countQuantifiers r2
countQuantifiers (Quant r _) = 1 + countQuantifiers r
countQuantifiers (Group _ r) = countQuantifiers r
countQuantifiers (NCGroup r) = countQuantifiers r
countQuantifiers StartAnchor = 0
countQuantifiers EndAnchor = 0
countQuantifiers WordBoundary = 0
countQuantifiers (BackRef _) = 0
countQuantifiers (Lookahead _ r) = countQuantifiers r
countQuantifiers (Lookbehind _ r) = countQuantifiers r

||| Count alternations in a regex
public export
countAlternations : Regex -> Nat
countAlternations Empty = 0
countAlternations Never = 0
countAlternations (Match _) = 0
countAlternations (Seq r1 r2) = countAlternations r1 + countAlternations r2
countAlternations (Alt r1 r2) = 1 + countAlternations r1 + countAlternations r2
countAlternations (Quant r _) = countAlternations r
countAlternations (Group _ r) = countAlternations r
countAlternations (NCGroup r) = countAlternations r
countAlternations StartAnchor = 0
countAlternations EndAnchor = 0
countAlternations WordBoundary = 0
countAlternations (BackRef _) = 0
countAlternations (Lookahead _ r) = countAlternations r
countAlternations (Lookbehind _ r) = countAlternations r

||| Calculate nesting depth of a regex
public export
nestingDepth : Regex -> Nat
nestingDepth = go 0
  where
    go : Nat -> Regex -> Nat
    go d Empty = d
    go d Never = d
    go d (Match _) = d
    go d (Seq r1 r2) = max (go d r1) (go d r2)
    go d (Alt r1 r2) = max (go (S d) r1) (go (S d) r2)
    go d (Quant r _) = go (S d) r
    go d (Group _ r) = go (S d) r
    go d (NCGroup r) = go (S d) r
    go d StartAnchor = d
    go d EndAnchor = d
    go d WordBoundary = d
    go d (BackRef _) = d
    go d (Lookahead _ r) = go (S d) r
    go d (Lookbehind _ r) = go (S d) r

||| Determine complexity level based on regex structure
public export
determineComplexity : Regex -> ComplexityLevel
determineComplexity r =
  if hasNestedQuantifiers r
    then Exponential
    else if hasOverlappingAlternatives r && countQuantifiers r > 0
      then Exponential
      else if hasQuantifiedEmpty r
        then Quadratic
        else if countQuantifiers r > 3 || countAlternations r > 5
          then Quadratic
          else Linear

||| Generate warnings for potentially problematic patterns
public export
generateWarnings : Regex -> List String
generateWarnings r =
  let warnings = []
      warnings = if hasNestedQuantifiers r
                   then "Nested quantifiers detected - exponential backtracking risk" :: warnings
                   else warnings
      warnings = if hasOverlappingAlternatives r
                   then "Overlapping alternatives detected - potential ReDoS" :: warnings
                   else warnings
      warnings = if hasQuantifiedEmpty r
                   then "Quantifier on potentially empty match - infinite loop risk" :: warnings
                   else warnings
      warnings = if hasBackrefs r
                   then "Backreferences present - NP-hard matching" :: warnings
                   else warnings
      warnings = if nestingDepth r > 5
                   then "Deep nesting (" ++ show (nestingDepth r) ++ " levels) - performance concern" :: warnings
                   else warnings
  in warnings

||| Perform full complexity analysis
public export
analyzeComplexity : Regex -> ComplexityAnalysis
analyzeComplexity r = MkComplexityAnalysis
  { level = determineComplexity r
  , quantifierCount = countQuantifiers r
  , alternationCount = countAlternations r
  , maxDepth = nestingDepth r
  , hasNestedQuantifiers = hasNestedQuantifiers r
  , hasOverlappingAlts = hasOverlappingAlternatives r
  , warnings = generateWarnings r
  }

--------------------------------------------------------------------------------
-- Safe Regex Construction
--------------------------------------------------------------------------------

||| Maximum allowed complexity level for safe regexes
public export
data SafetyLevel : Type where
  ||| Only allow linear complexity (very strict)
  StrictSafety : SafetyLevel
  ||| Allow up to quadratic complexity (recommended)
  NormalSafety : SafetyLevel
  ||| Allow any complexity (use with caution)
  RelaxedSafety : SafetyLevel

||| Check if complexity meets safety requirements
public export
meetsSafetyLevel : ComplexityLevel -> SafetyLevel -> Bool
meetsSafetyLevel Linear _ = True
meetsSafetyLevel Quadratic StrictSafety = False
meetsSafetyLevel Quadratic _ = True
meetsSafetyLevel Exponential StrictSafety = False
meetsSafetyLevel Exponential NormalSafety = False
meetsSafetyLevel Exponential RelaxedSafety = True
meetsSafetyLevel Unbounded _ = False

||| Calculate step limit based on complexity and input size estimate
public export
calculateStepLimit : ComplexityAnalysis -> (inputSizeEstimate : Nat) -> Nat
calculateStepLimit analysis inputSize =
  let base = inputSize * 10  -- 10 steps per input char as baseline
  in case analysis.level of
       Linear => base
       Quadratic => base * inputSize  -- O(n^2)
       Exponential => min (base * 100) 1000000  -- Cap exponential
       Unbounded => 1000000  -- Hard cap

||| Try to create a safe regex, rejecting if too complex
public export
makeSafe : Regex -> SafetyLevel -> Either RegexError SafeRegex
makeSafe r safety =
  let analysis = analyzeComplexity r
  in if meetsSafetyLevel analysis.level safety
       then Right $ MkSafeRegex r analysis (calculateStepLimit analysis 1000)
       else Left $ TooComplex analysis

||| Create a safe regex with normal safety level
public export
safe : Regex -> Either RegexError SafeRegex
safe r = makeSafe r NormalSafety

||| Create a safe regex with strict safety level
public export
safeStrict : Regex -> Either RegexError SafeRegex
safeStrict r = makeSafe r StrictSafety

--------------------------------------------------------------------------------
-- Known Safe Patterns
--------------------------------------------------------------------------------

||| Check if a regex is a known-safe pattern
||| These patterns have been analyzed and are guaranteed safe
public export
isKnownSafe : Regex -> Bool
isKnownSafe Empty = True
isKnownSafe Never = True
isKnownSafe (Match _) = True
isKnownSafe (Seq r1 r2) = isKnownSafe r1 && isKnownSafe r2 && not (hasNestedQuantifiers (Seq r1 r2))
isKnownSafe (Alt r1 r2) = isKnownSafe r1 && isKnownSafe r2 && not (regexesOverlap r1 r2)
isKnownSafe (Quant r q) = isKnownSafe r && isBounded q && not (hasNestedQuantifiers (Quant r q))
isKnownSafe (Group _ r) = isKnownSafe r
isKnownSafe (NCGroup r) = isKnownSafe r
isKnownSafe StartAnchor = True
isKnownSafe EndAnchor = True
isKnownSafe WordBoundary = True
isKnownSafe (BackRef _) = False  -- Backrefs are NP-hard
isKnownSafe (Lookahead _ r) = isKnownSafe r
isKnownSafe (Lookbehind _ r) = isKnownSafe r

--------------------------------------------------------------------------------
-- Input Validation for Matching
--------------------------------------------------------------------------------

||| Maximum input length for safe matching
public export
maxSafeInputLength : SafeRegex -> Nat
maxSafeInputLength sr =
  case sr.complexity.level of
    Linear => 10000000     -- 10MB for linear
    Quadratic => 10000     -- 10KB for quadratic
    Exponential => 100     -- 100 chars for exponential
    Unbounded => 50        -- 50 chars for unbounded

||| Check if input is safe to match against this regex
public export
isInputSafe : SafeRegex -> String -> Bool
isInputSafe sr input = length input <= maxSafeInputLength sr
