-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
||| SafeAPIKey - Type-safe API key handling
|||
||| Provides validated API key operations: format checking, safe masking,
||| entropy estimation, and prefix-based key identification.
||| Prevents: key leakage in logs, weak key acceptance, format confusion.
module Proven.SafeAPIKey

import Data.String
import Data.List
import Data.Nat
import Data.So

%default total

-- ============================================================================
-- KEY FORMAT
-- ============================================================================

||| Known API key format prefixes for identification
public export
data KeyFormat =
    OpenAIKey       -- sk-...
  | AnthropicKey    -- sk-ant-...
  | GitHubPAT       -- ghp_... / gho_... / ghs_...
  | GitHubClassic   -- ghp_...
  | AWSKey          -- AKIA...
  | StripeKey       -- sk_live_... / sk_test_...
  | CustomPrefix String
  | UnknownFormat

public export
Show KeyFormat where
  show OpenAIKey         = "OpenAI"
  show AnthropicKey      = "Anthropic"
  show GitHubPAT         = "GitHub PAT"
  show GitHubClassic     = "GitHub Classic"
  show AWSKey            = "AWS"
  show StripeKey         = "Stripe"
  show (CustomPrefix p)  = "Custom(" ++ p ++ ")"
  show UnknownFormat     = "Unknown"

public export
Eq KeyFormat where
  OpenAIKey == OpenAIKey = True
  AnthropicKey == AnthropicKey = True
  GitHubPAT == GitHubPAT = True
  GitHubClassic == GitHubClassic = True
  AWSKey == AWSKey = True
  StripeKey == StripeKey = True
  (CustomPrefix a) == (CustomPrefix b) = a == b
  UnknownFormat == UnknownFormat = True
  _ == _ = False

||| Detect key format from the key string
public export
detectFormat : String -> KeyFormat
detectFormat key =
  if isPrefixOf "sk-ant-" key then AnthropicKey
  else if isPrefixOf "sk_live_" key || isPrefixOf "sk_test_" key then StripeKey
  else if isPrefixOf "sk-" key then OpenAIKey
  else if isPrefixOf "ghp_" key || isPrefixOf "gho_" key || isPrefixOf "ghs_" key then GitHubPAT
  else if isPrefixOf "AKIA" key then AWSKey
  else UnknownFormat

-- ============================================================================
-- API KEY TYPE
-- ============================================================================

||| Minimum acceptable key length (bytes)
public export
MinKeyLength : Nat
MinKeyLength = 16

||| A validated API key (non-empty, minimum length)
public export
record APIKey where
  constructor MkAPIKey
  raw    : String
  format : KeyFormat
  0 lengthOk : So (length raw >= MinKeyLength)

||| Postulate: length check for key construction
keyLengthOk : (s : String) -> {auto 0 _ : So (length s >= MinKeyLength)} -> So (length s >= MinKeyLength)

||| Attempt to create an APIKey from a raw string
public export
mkAPIKey : String -> Maybe APIKey
mkAPIKey s with (choose (length s >= MinKeyLength))
  mkAPIKey s | Left prf  = Just (MkAPIKey s (detectFormat s) prf)
  mkAPIKey s | Right _   = Nothing

||| Create an APIKey with a specific expected format, rejecting mismatches
public export
mkAPIKeyWithFormat : KeyFormat -> String -> Maybe APIKey
mkAPIKeyWithFormat expected s =
  do key <- mkAPIKey s
     if key.format == expected then Just key else Nothing

-- ============================================================================
-- SAFE MASKING (prevents leakage in logs)
-- ============================================================================

||| Mask an API key for safe display, showing only prefix and last 4 chars
||| Example: "sk-ant-abc123...wxyz"
public export
mask : APIKey -> String
mask key =
  let s = key.raw
      len = length s
  in if len <= 8 then replicate len '*'
     else
       let keyPrefix = substr 0 4 s
           keySuffix = substr (len `minus` 4) 4 s
       in keyPrefix ++ "..." ++ keySuffix
  where
    replicate : Nat -> Char -> String
    replicate Z _ = ""
    replicate (S n) c = singleton c ++ replicate n c

||| Fully mask a key (all asterisks), preserving length indication
public export
fullMask : APIKey -> String
fullMask key = "[REDACTED:" ++ show (length key.raw) ++ " chars]"

-- ============================================================================
-- ENTROPY ESTIMATION
-- ============================================================================

||| Character class for entropy calculation
data CharClass = Lowercase | Uppercase | Digit | Symbol | Other

classifyChar : Char -> CharClass
classifyChar c =
  if isLower c then Lowercase
  else if isUpper c then Uppercase
  else if isDigit c then Digit
  else if ord c >= 33 && ord c <= 126 then Symbol
  else Other

||| Count unique character classes present in the key
uniqueClasses : String -> Nat
uniqueClasses s =
  let chars = unpack s
      hasLower  = any isLower chars
      hasUpper  = any isUpper chars
      hasDigit  = any isDigit chars
      hasSymbol = any (\c => ord c >= 33 && ord c <= 126 &&
                        not (isAlpha c) && not (isDigit c)) chars
  in (if hasLower then 1 else 0) +
     (if hasUpper then 1 else 0) +
     (if hasDigit then 1 else 0) +
     (if hasSymbol then 1 else 0)

||| Rough entropy quality assessment
public export
data EntropyQuality = Weak | Fair | Strong | VeryStrong

public export
Show EntropyQuality where
  show Weak       = "weak"
  show Fair       = "fair"
  show Strong     = "strong"
  show VeryStrong = "very_strong"

public export
Eq EntropyQuality where
  Weak == Weak = True
  Fair == Fair = True
  Strong == Strong = True
  VeryStrong == VeryStrong = True
  _ == _ = False

||| Estimate key entropy quality based on length and character diversity
public export
estimateEntropy : APIKey -> EntropyQuality
estimateEntropy key =
  let len = length key.raw
      classes = uniqueClasses key.raw
  in if len >= 64 && classes >= 3 then VeryStrong
     else if len >= 32 && classes >= 2 then Strong
     else if len >= 20 then Fair
     else Weak

-- ============================================================================
-- VALIDATION UTILITIES
-- ============================================================================

||| Check if a string looks like it might be an API key (heuristic)
||| Useful for preventing accidental key exposure in user-facing strings
public export
looksLikeAPIKey : String -> Bool
looksLikeAPIKey s =
  let len = length s in
  len >= MinKeyLength &&
  (isPrefixOf "sk-" s || isPrefixOf "sk_" s ||
   isPrefixOf "ghp_" s || isPrefixOf "gho_" s || isPrefixOf "ghs_" s ||
   isPrefixOf "AKIA" s || isPrefixOf "key-" s || isPrefixOf "api_" s ||
   isPrefixOf "token_" s)

||| Scan a string for potential API key leaks (useful for log sanitisation)
public export
containsPotentialKey : String -> Bool
containsPotentialKey s =
  any (\word => looksLikeAPIKey word) (words s)
