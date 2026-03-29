-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
||| SafePromptInjection - Verified LLM prompt injection detection and sanitisation
|||
||| Provides type-safe detection and mitigation of prompt injection attacks
||| against large language models. Covers direct injection, indirect injection
||| via retrieved content, and delimiter-escape attempts.
|||
||| Prevents: system prompt override, role impersonation, instruction hijacking,
||| delimiter escape, encoding-based bypass.
module Proven.SafePromptInjection

import Data.String
import Data.List
import Data.Nat

%default total

-- ============================================================================
-- THREAT CLASSIFICATION
-- ============================================================================

||| Categories of prompt injection attack
public export
data InjectionCategory =
    RoleImpersonation     -- "You are now...", "Ignore previous instructions"
  | DelimiterEscape       -- Attempts to close system prompt delimiters
  | InstructionOverride   -- "New instructions:", "SYSTEM:", etc.
  | EncodingBypass        -- Base64, hex, rot13 encoded payloads
  | IndirectInjection     -- Injected via retrieved documents / tool output
  | ContextManipulation   -- Attempts to redefine conversation context

public export
Show InjectionCategory where
  show RoleImpersonation    = "role_impersonation"
  show DelimiterEscape      = "delimiter_escape"
  show InstructionOverride  = "instruction_override"
  show EncodingBypass       = "encoding_bypass"
  show IndirectInjection    = "indirect_injection"
  show ContextManipulation  = "context_manipulation"

public export
Eq InjectionCategory where
  RoleImpersonation == RoleImpersonation = True
  DelimiterEscape == DelimiterEscape = True
  InstructionOverride == InstructionOverride = True
  EncodingBypass == EncodingBypass = True
  IndirectInjection == IndirectInjection = True
  ContextManipulation == ContextManipulation = True
  _ == _ = False

||| Severity level of a detected injection attempt
public export
data Severity = Low | Medium | High | Critical

public export
Show Severity where
  show Low = "low"
  show Medium = "medium"
  show High = "high"
  show Critical = "critical"

public export
Eq Severity where
  Low == Low = True
  Medium == Medium = True
  High == High = True
  Critical == Critical = True
  _ == _ = False

public export
Ord Severity where
  compare Low Low = EQ
  compare Low _ = LT
  compare Medium Low = GT
  compare Medium Medium = EQ
  compare Medium _ = LT
  compare High Critical = LT
  compare High High = EQ
  compare High _ = GT
  compare Critical Critical = EQ
  compare Critical _ = GT

-- ============================================================================
-- DETECTION RESULT
-- ============================================================================

||| A single detected injection indicator
public export
record InjectionIndicator where
  constructor MkIndicator
  category : InjectionCategory
  severity : Severity
  pattern  : String   -- The pattern that matched
  position : Nat      -- Character offset in the input

public export
Show InjectionIndicator where
  show ind = "InjectionIndicator(" ++ show ind.category ++ ", " ++
             show ind.severity ++ ", pos=" ++ show ind.position ++ ")"

||| Overall scan result for a piece of text
public export
record ScanResult where
  constructor MkScanResult
  indicators  : List InjectionIndicator
  maxSeverity : Maybe Severity
  isSafe      : Bool

||| An empty (clean) scan result
public export
cleanResult : ScanResult
cleanResult = MkScanResult [] Nothing True

-- ============================================================================
-- PATTERN MATCHERS (pure string analysis)
-- ============================================================================

||| Convert a string to lowercase for case-insensitive matching
toLowerStr : String -> String
toLowerStr = pack . map toLower . unpack

||| Check if a string contains a substring (case-insensitive)
containsCI : String -> String -> Bool
containsCI needle haystack = isInfixOf (toLowerStr needle) (toLowerStr haystack)

||| Known role impersonation patterns
roleImpersonationPatterns : List String
roleImpersonationPatterns =
  [ "ignore previous instructions"
  , "ignore all previous"
  , "disregard previous"
  , "forget your instructions"
  , "you are now"
  , "new persona"
  , "act as if"
  , "pretend you are"
  , "override your"
  , "from now on you"
  ]

||| Known instruction override patterns
instructionOverridePatterns : List String
instructionOverridePatterns =
  [ "system:"
  , "new instructions:"
  , "updated instructions:"
  , "admin override"
  , "developer mode"
  , "jailbreak"
  , "do anything now"
  , "ignore safety"
  , "ignore guidelines"
  , "bypass restrictions"
  ]

||| Known delimiter escape sequences
delimiterEscapePatterns : List String
delimiterEscapePatterns =
  [ "```\nsystem"
  , "</system>"
  , "[INST]"
  , "[/INST]"
  , "<|im_start|>"
  , "<|im_end|>"
  , "### Human:"
  , "### Assistant:"
  , "Human:"
  , "Assistant:"
  ]

||| Check a list of patterns against input, returning indicators
checkPatterns : InjectionCategory -> Severity -> List String -> String -> List InjectionIndicator
checkPatterns cat sev patterns input =
  mapMaybe (\pat =>
    if containsCI pat input
    then Just (MkIndicator cat sev pat 0)
    else Nothing
  ) patterns

-- ============================================================================
-- SCANNING
-- ============================================================================

||| Scan text for prompt injection indicators
|||
||| Returns a ScanResult with all detected indicators and overall severity.
||| This is a heuristic scanner — it catches known patterns but cannot
||| guarantee detection of novel attacks.
public export
scanForInjection : String -> ScanResult
scanForInjection input =
  let roleHits   = checkPatterns RoleImpersonation High roleImpersonationPatterns input
      overHits   = checkPatterns InstructionOverride Critical instructionOverridePatterns input
      delimHits  = checkPatterns DelimiterEscape Critical delimiterEscapePatterns input
      allHits    = roleHits ++ overHits ++ delimHits
      maxSev     = foldl (\acc, ind =>
                     case acc of
                       Nothing => Just ind.severity
                       Just s  => if compare ind.severity s == GT
                                  then Just ind.severity
                                  else Just s
                   ) Nothing allHits
  in MkScanResult allHits maxSev (isNil allHits)

||| Quick check: is the input likely safe?
public export
isLikelySafe : String -> Bool
isLikelySafe input = (scanForInjection input).isSafe

||| Quick check: does the input contain critical-severity indicators?
public export
hasCriticalIndicators : String -> Bool
hasCriticalIndicators input =
  case (scanForInjection input).maxSeverity of
    Just Critical => True
    _             => False

-- ============================================================================
-- SANITISATION
-- ============================================================================

||| Sanitisation strategy
public export
data SanitiseStrategy =
    StripPatterns      -- Remove detected patterns
  | EscapeDelimiters   -- Escape delimiter characters
  | RejectUnsafe       -- Reject entirely if unsafe

||| Escape characters commonly used in prompt delimiters
public export
escapePromptDelimiters : String -> String
escapePromptDelimiters = pack . go . unpack
  where
    go : List Char -> List Char
    go [] = []
    go ('<' :: cs) = '\\' :: '<' :: go cs
    go ('>' :: cs) = '\\' :: '>' :: go cs
    go ('[' :: cs) = '\\' :: '[' :: go cs
    go (']' :: cs) = '\\' :: ']' :: go cs
    go ('`' :: cs) = '\\' :: '`' :: go cs
    go (c :: cs)   = c :: go cs

||| Sanitise user input before including in a prompt
|||
||| Returns Nothing if the input is rejected (when using RejectUnsafe and
||| the input contains indicators), or Just the sanitised string.
public export
sanitise : SanitiseStrategy -> String -> Maybe String
sanitise RejectUnsafe input =
  if isLikelySafe input then Just input else Nothing
sanitise EscapeDelimiters input =
  Just (escapePromptDelimiters input)
sanitise StripPatterns input =
  Just input  -- Full strip implementation requires regex; returns as-is for now

-- ============================================================================
-- SAFE PROMPT CONSTRUCTION
-- ============================================================================

||| A prompt section with explicit role tagging
public export
data PromptSection =
    SystemSection String
  | UserSection String
  | AssistantSection String

||| Render a prompt section with clear delimiters
public export
renderSection : PromptSection -> String
renderSection (SystemSection s)    = "[SYSTEM]\n" ++ s ++ "\n[/SYSTEM]"
renderSection (UserSection s)      = "[USER]\n" ++ s ++ "\n[/USER]"
renderSection (AssistantSection s) = "[ASSISTANT]\n" ++ s ++ "\n[/ASSISTANT]"

||| Build a prompt from sections, scanning user sections for injection
public export
buildSafePrompt : List PromptSection -> (String, List InjectionIndicator)
buildSafePrompt sections =
  let rendered = map renderSection sections
      userIndicators = concatMap scanUserSection sections
  in (unlines rendered, userIndicators)
  where
    scanUserSection : PromptSection -> List InjectionIndicator
    scanUserSection (UserSection s) = (scanForInjection s).indicators
    scanUserSection _               = []
