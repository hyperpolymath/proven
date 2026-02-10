-- SPDX-License-Identifier: Palimpsest-MPL-1.0
||| SafeRegex - Provably safe regular expressions with ReDoS protection
|||
||| This module provides a safe regex library that:
||| - Detects and prevents ReDoS (Regular Expression Denial of Service) attacks
||| - Analyzes regex complexity before matching
||| - Enforces step limits based on complexity analysis
||| - Provides type-safe regex construction
||| - Includes formal proofs of safety properties
|||
||| Usage:
|||   ```idris
|||   import Proven.SafeRegex
|||
|||   -- Parse and validate a regex pattern
|||   case parseSafe "[a-z]+@[a-z]+\\.[a-z]+" of
|||     Right sr => if test sr "test@example.com"
|||                   then putStrLn "Valid email!"
|||                   else putStrLn "Invalid email"
|||     Left err => putStrLn $ "Invalid pattern: " ++ show err
|||
|||   -- Use pre-built safe patterns
|||   case safeEmailPattern of
|||     Right sr => match sr "user@domain.com"
|||     Left _ => noMatch 0
|||   ```
|||
||| ReDoS Prevention:
|||   The library detects three main ReDoS indicators:
|||   1. Nested quantifiers: (a+)+ - exponential backtracking
|||   2. Overlapping alternatives: (a|ab)+ - ambiguous matching
|||   3. Quantified empty patterns: (a?)*  - infinite loops
|||
|||   Patterns with these issues are either rejected or given strict step limits.
module Proven.SafeRegex
import Data.String
import Data.List

import public Proven.SafeRegex.Types
import public Proven.SafeRegex.Safety
import public Proven.SafeRegex.Parser
import public Proven.SafeRegex.Matcher
import public Proven.SafeRegex.Proofs

%default total

--------------------------------------------------------------------------------
-- Convenience API
--------------------------------------------------------------------------------

||| Parse a regex pattern and create a safe regex with normal safety level
||| Rejects patterns that are likely to cause ReDoS
public export
regex : String -> Either RegexError SafeRegex
regex = parseSafe

||| Parse a regex pattern with strict safety (only allows linear complexity)
public export
regexStrict : String -> Either RegexError SafeRegex
regexStrict = parseSafeStrict

||| Parse a regex pattern with relaxed safety (allows exponential but with limits)
public export
regexRelaxed : String -> Either RegexError SafeRegex
regexRelaxed pattern = do
  r <- parseRegex pattern
  makeSafe r RelaxedSafety

||| Quick test if a pattern matches anywhere in the input
||| Returns False if pattern is invalid
public export
quickTest : String -> String -> Bool
quickTest pattern input =
  case regex pattern of
    Right sr => test sr input
    Left _ => False

||| Quick test if a pattern matches the entire input
||| Returns False if pattern is invalid
public export
quickTestFull : String -> String -> Bool
quickTestFull pattern input =
  case regex pattern of
    Right sr => testFull sr input
    Left _ => False

||| Find first match, returning the matched text
public export
quickFind : String -> String -> Maybe String
quickFind pattern input =
  case regex pattern of
    Right sr =>
      let result = match sr input
      in if result.matched
           then case result.fullMatch of
                  Just (start, end) => Just (substr start (minus end start) input)
                  Nothing => Nothing
           else Nothing
    Left _ => Nothing

||| Find all matches, returning list of matched texts
public export
quickFindAll : String -> String -> List String
quickFindAll pattern input =
  case regex pattern of
    Right sr =>
      let results = findAll sr input defaultFlags
      in mapMaybe extractText results
    Left _ => []
  where
    extractText : MatchResult -> Maybe String
    extractText result =
      if result.matched
        then case result.fullMatch of
               Just (start, end) => Just (substr start (minus end start) input)
               Nothing => Nothing
        else Nothing

||| Quick replace first match
public export
quickReplace : String -> String -> String -> String
quickReplace pattern input replacement =
  case regex pattern of
    Right sr => replaceFirst sr input replacement
    Left _ => input

||| Quick replace all matches
public export
quickReplaceAll : String -> String -> String -> String
quickReplaceAll pattern input replacement =
  case regex pattern of
    Right sr => replaceAll sr input replacement
    Left _ => input

||| Quick split by pattern
public export
quickSplit : String -> String -> List String
quickSplit pattern input =
  case regex pattern of
    Right sr => split sr input
    Left _ => [input]

--------------------------------------------------------------------------------
-- Validation Helpers
--------------------------------------------------------------------------------

||| Check if a pattern is safe to use
public export
isPatternSafe : String -> Bool
isPatternSafe pattern =
  case regex pattern of
    Right _ => True
    Left _ => False

||| Get complexity analysis for a pattern
public export
analyzePattern : String -> Either RegexError ComplexityAnalysis
analyzePattern pattern = do
  r <- parseRegex pattern
  Right (analyzeComplexity r)

||| Get warnings for a pattern
public export
patternWarnings : String -> Either RegexError (List String)
patternWarnings pattern = do
  r <- parseRegex pattern
  Right (generateWarnings r)

||| Check if a pattern has ReDoS risk
public export
hasRedosRisk : String -> Bool
hasRedosRisk pattern =
  case analyzePattern pattern of
    Right analysis => analysis.level == Exponential || analysis.level == Unbounded
    Left _ => True  -- Parse errors are considered risky

--------------------------------------------------------------------------------
-- Safe Pattern Library
--------------------------------------------------------------------------------

||| Pre-built safe email pattern
public export
safeEmailPattern : Either RegexError SafeRegex
safeEmailPattern = parseSafe emailPattern

||| Pre-built safe URL pattern
public export
safeUrlPattern : Either RegexError SafeRegex
safeUrlPattern = parseSafe urlPattern

||| Pre-built safe IPv4 pattern
public export
safeIpv4Pattern : Either RegexError SafeRegex
safeIpv4Pattern = parseSafe ipv4Pattern

||| Pre-built safe UUID pattern
public export
safeUuidPattern : Either RegexError SafeRegex
safeUuidPattern = parseSafe uuidPattern

||| Pre-built safe integer pattern
public export
safeIntegerPattern : Either RegexError SafeRegex
safeIntegerPattern = parseSafe integerPattern

||| Pre-built safe decimal pattern
public export
safeDecimalPattern : Either RegexError SafeRegex
safeDecimalPattern = parseSafe decimalPattern

||| Pre-built safe identifier pattern (programming language identifiers)
public export
safeIdentifierPattern : Either RegexError SafeRegex
safeIdentifierPattern = parseSafe identifierPattern

||| Pre-built safe hex color pattern
public export
safeHexColorPattern : Either RegexError SafeRegex
safeHexColorPattern = parseSafe hexColorPattern

||| Pre-built safe date pattern (YYYY-MM-DD)
public export
safeDatePattern : Either RegexError SafeRegex
safeDatePattern = parseSafe datePattern

||| Pre-built safe time pattern (HH:MM:SS)
public export
safeTimePattern : Either RegexError SafeRegex
safeTimePattern = parseSafe timePattern

--------------------------------------------------------------------------------
-- Monad-like Operations for Pattern Composition
--------------------------------------------------------------------------------

||| Sequence two patterns (match first, then second)
public export
(>>>) : Either RegexError SafeRegex -> Either RegexError SafeRegex -> Either RegexError SafeRegex
(>>>) (Right sr1) (Right sr2) =
  let combined = Seq sr1.regex sr2.regex
  in safe combined
(>>>) (Left e) _ = Left e
(>>>) _ (Left e) = Left e

||| Alternative between two patterns (match first, or second if first fails)
public export
(<|>) : Either RegexError SafeRegex -> Either RegexError SafeRegex -> Either RegexError SafeRegex
(<|>) (Right sr1) (Right sr2) =
  let combined = Alt sr1.regex sr2.regex
  in safe combined
(<|>) (Left e) _ = Left e
(<|>) _ (Left e) = Left e

||| Optional pattern (zero or one)
public export
optional : Either RegexError SafeRegex -> Either RegexError SafeRegex
optional (Right sr) = safe (Quant sr.regex zeroOrOne)
optional (Left e) = Left e

||| One or more repetitions
public export
oneOrMoreOf : Either RegexError SafeRegex -> Either RegexError SafeRegex
oneOrMoreOf (Right sr) = safe (Quant sr.regex oneOrMore)
oneOrMoreOf (Left e) = Left e

||| Zero or more repetitions
public export
zeroOrMoreOf : Either RegexError SafeRegex -> Either RegexError SafeRegex
zeroOrMoreOf (Right sr) = safe (Quant sr.regex zeroOrMore)
zeroOrMoreOf (Left e) = Left e

||| Exactly n repetitions
public export
exactlyN : Nat -> Either RegexError SafeRegex -> Either RegexError SafeRegex
exactlyN n (Right sr) = safe (Quant sr.regex (exactly n))
exactlyN _ (Left e) = Left e

||| Between n and m repetitions
public export
betweenNM : Nat -> Nat -> Either RegexError SafeRegex -> Either RegexError SafeRegex
betweenNM n m (Right sr) = safe (Quant sr.regex (between n m))
betweenNM _ _ (Left e) = Left e

--------------------------------------------------------------------------------
-- Example Patterns
--------------------------------------------------------------------------------

||| Example: Match a US phone number
public export
usPhonePattern : Either RegexError SafeRegex
usPhonePattern = regex "\\(?[0-9]{3}\\)?[-. ]?[0-9]{3}[-. ]?[0-9]{4}"

||| Example: Match a credit card number (spaces or dashes)
public export
creditCardPattern : Either RegexError SafeRegex
creditCardPattern = regex "[0-9]{4}[-\\s]?[0-9]{4}[-\\s]?[0-9]{4}[-\\s]?[0-9]{4}"

||| Example: Match a slug (URL-friendly identifier)
public export
slugPattern : Either RegexError SafeRegex
slugPattern = regex "[a-z0-9]+(?:-[a-z0-9]+)*"

||| Example: Match a semantic version
public export
semverPattern : Either RegexError SafeRegex
semverPattern = regex "[0-9]+\\.[0-9]+\\.[0-9]+(?:-[a-z0-9.]+)?(?:\\+[a-z0-9.]+)?"
