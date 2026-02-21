-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
||| SafeRegex.Types - Core types for safe regular expressions
|||
||| This module defines the abstract syntax tree for regular expressions
||| and related types for safe regex operations.
module Proven.SafeRegex.Types

import Proven.Core
import Data.List
import Data.String

%default total

--------------------------------------------------------------------------------
-- Character Classes
--------------------------------------------------------------------------------

||| Character class - a set of characters to match
public export
data CharClass : Type where
  ||| Single character
  Char : Char -> CharClass
  ||| Character range (inclusive)
  Range : (from : Char) -> (to : Char) -> CharClass
  ||| Any digit [0-9]
  Digit : CharClass
  ||| Any word character [a-zA-Z0-9_]
  Word : CharClass
  ||| Any whitespace character
  Space : CharClass
  ||| Any character (except newline by default)
  Any : CharClass
  ||| Negation of a character class
  Negate : CharClass -> CharClass
  ||| Union of character classes
  Union : CharClass -> CharClass -> CharClass

||| Check if a character matches a character class
public export
matchesClass : Char -> CharClass -> Bool
matchesClass c (Char x) = c == x
matchesClass c (Range from to) = c >= from && c <= to
matchesClass c Digit = c >= '0' && c <= '9'
matchesClass c Word = isAlphaNum c || c == '_'
matchesClass c Space = isSpace c
matchesClass c Any = c /= '\n'
matchesClass c (Negate cls) = not (matchesClass c cls)
matchesClass c (Union cls1 cls2) = matchesClass c cls1 || matchesClass c cls2

--------------------------------------------------------------------------------
-- Quantifiers
--------------------------------------------------------------------------------

||| Quantifier bounds
public export
record Quantifier where
  constructor MkQuantifier
  minCount : Nat
  maxCount : Maybe Nat  -- Nothing means unbounded
  greedy : Bool         -- True = greedy, False = lazy

||| Common quantifiers
public export
zeroOrMore : Quantifier
zeroOrMore = MkQuantifier 0 Nothing True

public export
oneOrMore : Quantifier
oneOrMore = MkQuantifier 1 Nothing True

public export
zeroOrOne : Quantifier
zeroOrOne = MkQuantifier 0 (Just 1) True

public export
exactly : Nat -> Quantifier
exactly n = MkQuantifier n (Just n) True

public export
atLeast : Nat -> Quantifier
atLeast n = MkQuantifier n Nothing True

public export
between : Nat -> Nat -> Quantifier
between min max = MkQuantifier min (Just max) True

public export
lazy : Quantifier -> Quantifier
lazy q = { greedy := False } q

--------------------------------------------------------------------------------
-- Regex AST
--------------------------------------------------------------------------------

||| Regular expression abstract syntax tree
public export
data Regex : Type where
  ||| Empty regex (matches empty string)
  Empty : Regex
  ||| Never matches
  Never : Regex
  ||| Match a character class
  Match : CharClass -> Regex
  ||| Sequence of regexes
  Seq : Regex -> Regex -> Regex
  ||| Alternative (choice)
  Alt : Regex -> Regex -> Regex
  ||| Quantified regex
  Quant : Regex -> Quantifier -> Regex
  ||| Capturing group
  Group : (id : Nat) -> Regex -> Regex
  ||| Non-capturing group
  NCGroup : Regex -> Regex
  ||| Start anchor ^
  StartAnchor : Regex
  ||| End anchor $
  EndAnchor : Regex
  ||| Word boundary \b
  WordBoundary : Regex
  ||| Backreference to group
  BackRef : (groupId : Nat) -> Regex
  ||| Lookahead (?=...) or (?!...)
  Lookahead : (positive : Bool) -> Regex -> Regex
  ||| Lookbehind (?<=...) or (?<!...)
  Lookbehind : (positive : Bool) -> Regex -> Regex

--------------------------------------------------------------------------------
-- Complexity Metrics
--------------------------------------------------------------------------------

||| Complexity level for ReDoS risk assessment
public export
data ComplexityLevel : Type where
  ||| O(n) - linear, safe
  Linear : ComplexityLevel
  ||| O(n^2) - quadratic, usually safe but watch for large inputs
  Quadratic : ComplexityLevel
  ||| O(2^n) - exponential, ReDoS vulnerable
  Exponential : ComplexityLevel
  ||| Unknown/unbounded complexity
  Unbounded : ComplexityLevel

public export
Eq ComplexityLevel where
  Linear == Linear = True
  Quadratic == Quadratic = True
  Exponential == Exponential = True
  Unbounded == Unbounded = True
  _ == _ = False

public export
Ord ComplexityLevel where
  compare Linear Linear = EQ
  compare Linear _ = LT
  compare Quadratic Linear = GT
  compare Quadratic Quadratic = EQ
  compare Quadratic _ = LT
  compare Exponential Unbounded = LT
  compare Exponential Exponential = EQ
  compare Exponential _ = GT
  compare Unbounded Unbounded = EQ
  compare Unbounded _ = GT

||| Regex complexity analysis result
public export
record ComplexityAnalysis where
  constructor MkComplexityAnalysis
  ||| Overall complexity level
  level : ComplexityLevel
  ||| Number of quantifiers
  quantifierCount : Nat
  ||| Number of alternations
  alternationCount : Nat
  ||| Maximum nesting depth
  maxDepth : Nat
  ||| Has nested quantifiers (ReDoS indicator)
  hasNestedQuantifiers : Bool
  ||| Has overlapping alternatives (ReDoS indicator)
  hasOverlappingAlts : Bool
  ||| Specific warnings
  warnings : List String

||| Safe regex - wraps a Regex with proof of safety
public export
record SafeRegex where
  constructor MkSafeRegex
  ||| The underlying regex AST
  regex : Regex
  ||| Complexity analysis
  complexity : ComplexityAnalysis
  ||| Maximum steps for matching (based on complexity)
  stepLimit : Nat

--------------------------------------------------------------------------------
-- Match Results
--------------------------------------------------------------------------------

||| A single capture group result
public export
record Capture where
  constructor MkCapture
  ||| Group index (0 = full match)
  groupId : Nat
  ||| Start position in input
  start : Nat
  ||| End position in input (exclusive)
  end : Nat
  ||| The matched text
  text : String

||| Match result
public export
record MatchResult where
  constructor MkMatchResult
  ||| Whether the match succeeded
  matched : Bool
  ||| Full match position (start, end)
  fullMatch : Maybe (Nat, Nat)
  ||| Captured groups
  captures : List Capture
  ||| Number of steps taken
  stepsTaken : Nat

||| No match result
public export
noMatch : Nat -> MatchResult
noMatch steps = MkMatchResult False Nothing [] steps

||| Successful match result
public export
success : (start : Nat) -> (end : Nat) -> (captures : List Capture) -> (steps : Nat) -> MatchResult
success start end caps steps = MkMatchResult True (Just (start, end)) caps steps

--------------------------------------------------------------------------------
-- Error Types
--------------------------------------------------------------------------------

||| Regex parsing error
public export
data RegexError : Type where
  ||| Invalid escape sequence
  InvalidEscape : (pos : Nat) -> (char : Char) -> RegexError
  ||| Unclosed group
  UnclosedGroup : (pos : Nat) -> RegexError
  ||| Unclosed character class
  UnclosedCharClass : (pos : Nat) -> RegexError
  ||| Invalid quantifier
  InvalidQuantifier : (pos : Nat) -> (detail : String) -> RegexError
  ||| Empty alternation branch
  EmptyAlternation : (pos : Nat) -> RegexError
  ||| Invalid backreference
  InvalidBackRef : (pos : Nat) -> (groupId : Nat) -> RegexError
  ||| Pattern too complex (ReDoS risk)
  TooComplex : (analysis : ComplexityAnalysis) -> RegexError
  ||| Generic parse error
  ParseError : (pos : Nat) -> (msg : String) -> RegexError

public export
Show RegexError where
  show (InvalidEscape pos c) = "Invalid escape '\\" ++ singleton c ++ "' at position " ++ show pos
  show (UnclosedGroup pos) = "Unclosed group at position " ++ show pos
  show (UnclosedCharClass pos) = "Unclosed character class at position " ++ show pos
  show (InvalidQuantifier pos detail) = "Invalid quantifier at position " ++ show pos ++ ": " ++ detail
  show (EmptyAlternation pos) = "Empty alternation branch at position " ++ show pos
  show (InvalidBackRef pos gid) = "Invalid backreference to group " ++ show gid ++ " at position " ++ show pos
  show (TooComplex analysis) = "Pattern too complex: " ++ show analysis.level ++ " complexity"
  show (ParseError pos msg) = "Parse error at position " ++ show pos ++ ": " ++ msg

--------------------------------------------------------------------------------
-- Regex Flags
--------------------------------------------------------------------------------

||| Regex matching flags
public export
record RegexFlags where
  constructor MkRegexFlags
  ||| Case insensitive matching
  caseInsensitive : Bool
  ||| Multiline mode (^ and $ match line boundaries)
  multiline : Bool
  ||| Dot matches newline
  dotAll : Bool
  ||| Extended mode (ignore whitespace and comments)
  extended : Bool
  ||| Global matching (find all matches)
  global : Bool

||| Default flags
public export
defaultFlags : RegexFlags
defaultFlags = MkRegexFlags False False False False False

--------------------------------------------------------------------------------
-- Utility Functions
--------------------------------------------------------------------------------

||| Check if a quantifier is bounded
public export
isBounded : Quantifier -> Bool
isBounded q = isJust q.maxCount

||| Get maximum repetitions for a quantifier (Nothing if unbounded)
public export
maxReps : Quantifier -> Maybe Nat
maxReps = maxCount

||| Check if regex contains backreferences
public export
hasBackrefs : Regex -> Bool
hasBackrefs Empty = False
hasBackrefs Never = False
hasBackrefs (Match _) = False
hasBackrefs (Seq r1 r2) = hasBackrefs r1 || hasBackrefs r2
hasBackrefs (Alt r1 r2) = hasBackrefs r1 || hasBackrefs r2
hasBackrefs (Quant r _) = hasBackrefs r
hasBackrefs (Group _ r) = hasBackrefs r
hasBackrefs (NCGroup r) = hasBackrefs r
hasBackrefs StartAnchor = False
hasBackrefs EndAnchor = False
hasBackrefs WordBoundary = False
hasBackrefs (BackRef _) = True
hasBackrefs (Lookahead _ r) = hasBackrefs r
hasBackrefs (Lookbehind _ r) = hasBackrefs r

||| Count capture groups in a regex
public export
countGroups : Regex -> Nat
countGroups Empty = 0
countGroups Never = 0
countGroups (Match _) = 0
countGroups (Seq r1 r2) = countGroups r1 + countGroups r2
countGroups (Alt r1 r2) = max (countGroups r1) (countGroups r2)
countGroups (Quant r _) = countGroups r
countGroups (Group _ r) = 1 + countGroups r
countGroups (NCGroup r) = countGroups r
countGroups StartAnchor = 0
countGroups EndAnchor = 0
countGroups WordBoundary = 0
countGroups (BackRef _) = 0
countGroups (Lookahead _ r) = countGroups r
countGroups (Lookbehind _ r) = countGroups r
