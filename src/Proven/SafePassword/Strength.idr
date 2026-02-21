-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
||| Password Strength Analysis
|||
||| Provides comprehensive password strength assessment including
||| entropy calculation, pattern detection, and scoring.
module Proven.SafePassword.Strength

import Proven.Core
import Data.List
import Data.String

%default total

--------------------------------------------------------------------------------
-- Strength Score Types
--------------------------------------------------------------------------------

||| Overall password strength level
public export
data StrengthLevel
  = VeryWeak      -- Trivially guessable
  | Weak          -- Easily guessable
  | Fair          -- Provides some resistance
  | Strong        -- Good protection
  | VeryStrong    -- Excellent protection

public export
Show StrengthLevel where
  show VeryWeak = "Very Weak"
  show Weak = "Weak"
  show Fair = "Fair"
  show Strong = "Strong"
  show VeryStrong = "Very Strong"

public export
Eq StrengthLevel where
  VeryWeak == VeryWeak = True
  Weak == Weak = True
  Fair == Fair = True
  Strong == Strong = True
  VeryStrong == VeryStrong = True
  _ == _ = False

public export
Ord StrengthLevel where
  compare VeryWeak VeryWeak = EQ
  compare VeryWeak _ = LT
  compare Weak VeryWeak = GT
  compare Weak Weak = EQ
  compare Weak _ = LT
  compare Fair VeryWeak = GT
  compare Fair Weak = GT
  compare Fair Fair = EQ
  compare Fair _ = LT
  compare Strong VeryStrong = LT
  compare Strong Strong = EQ
  compare Strong _ = GT
  compare VeryStrong VeryStrong = EQ
  compare VeryStrong _ = GT

--------------------------------------------------------------------------------
-- Strength Analysis Result
--------------------------------------------------------------------------------

||| Detailed strength analysis result
public export
record StrengthAnalysis where
  constructor MkStrengthAnalysis
  level : StrengthLevel
  score : Nat              -- 0-100
  entropy : Double         -- Bits of entropy
  crackTime : CrackTime    -- Estimated crack time
  feedback : List String   -- Suggestions for improvement
  warnings : List String   -- Security concerns found
  patterns : List Pattern  -- Detected patterns

||| Estimated time to crack
public export
data CrackTime
  = Instant
  | Seconds Nat
  | Minutes Nat
  | Hours Nat
  | Days Nat
  | Months Nat
  | Years Nat
  | Centuries Nat
  | Never  -- Practically uncrackable

public export
Show CrackTime where
  show Instant = "Instant"
  show (Seconds n) = show n ++ " seconds"
  show (Minutes n) = show n ++ " minutes"
  show (Hours n) = show n ++ " hours"
  show (Days n) = show n ++ " days"
  show (Months n) = show n ++ " months"
  show (Years n) = show n ++ " years"
  show (Centuries n) = show n ++ " centuries"
  show Never = "Never (practically)"

--------------------------------------------------------------------------------
-- Pattern Detection
--------------------------------------------------------------------------------

||| Detected password patterns
public export
data Pattern
  = DictionaryWord String      -- Common dictionary word
  | CommonPassword String      -- Known common password
  | RepeatedPattern String Nat -- Pattern repeated N times
  | SequentialChars String     -- abc, 123, etc.
  | KeyboardPattern String     -- qwerty, asdf, etc.
  | DatePattern String         -- 1990, 2023, etc.
  | LeetSpeak String String    -- Original word and leet version
  | ReversedWord String        -- Reversed dictionary word

public export
Show Pattern where
  show (DictionaryWord w) = "Dictionary word: " ++ w
  show (CommonPassword p) = "Common password: " ++ p
  show (RepeatedPattern p n) = "Repeated " ++ show n ++ " times: " ++ p
  show (SequentialChars s) = "Sequential: " ++ s
  show (KeyboardPattern k) = "Keyboard pattern: " ++ k
  show (DatePattern d) = "Date-like: " ++ d
  show (LeetSpeak orig leet) = "Leet speak of '" ++ orig ++ "': " ++ leet
  show (ReversedWord w) = "Reversed word: " ++ w

||| Penalty associated with each pattern type
public export
patternPenalty : Pattern -> Nat
patternPenalty (DictionaryWord _) = 15
patternPenalty (CommonPassword _) = 30
patternPenalty (RepeatedPattern _ n) = 5 * n
patternPenalty (SequentialChars _) = 10
patternPenalty (KeyboardPattern _) = 15
patternPenalty (DatePattern _) = 10
patternPenalty (LeetSpeak _ _) = 5
patternPenalty (ReversedWord _) = 10

--------------------------------------------------------------------------------
-- Strength Calculation
--------------------------------------------------------------------------------

||| Analyze password strength
public export
analyzeStrength : String -> StrengthAnalysis
analyzeStrength pwd =
  let chars = unpack pwd
      len = length chars
      entropy = calculateEntropy chars
      patterns = detectPatterns pwd
      patternPenalties = sum (map patternPenalty patterns)
      baseScore = entropyToScore entropy
      adjustedScore = max 0 (minus baseScore patternPenalties)
      level = scoreToLevel adjustedScore
      crackTime = estimateCrackTime entropy
      feedback = generateFeedback chars patterns level
      warnings = generateWarnings patterns
  in MkStrengthAnalysis level adjustedScore entropy crackTime feedback warnings patterns
  where
    calculateEntropy : List Char -> Double
    calculateEntropy cs =
      let len = cast (length cs)
          poolSize = calculatePoolSize cs
      in len * log2 poolSize

    log2 : Double -> Double
    log2 x = if x <= 0 then 0 else log x / log 2.0

    calculatePoolSize : List Char -> Double
    calculatePoolSize cs =
      let hasLower = any (\c => c >= 'a' && c <= 'z') cs
          hasUpper = any (\c => c >= 'A' && c <= 'Z') cs
          hasDigit = any (\c => c >= '0' && c <= '9') cs
          hasSymbol = any (\c => not (isAlphaNum c) && not (isSpace c)) cs
          base = 0.0
          withLower = if hasLower then base + 26.0 else base
          withUpper = if hasUpper then withLower + 26.0 else withLower
          withDigit = if hasDigit then withUpper + 10.0 else withUpper
          withSymbol = if hasSymbol then withDigit + 32.0 else withDigit
      in if withSymbol == 0.0 then 1.0 else withSymbol

    entropyToScore : Double -> Nat
    entropyToScore e =
      if e < 20.0 then 10
      else if e < 30.0 then 25
      else if e < 40.0 then 40
      else if e < 50.0 then 55
      else if e < 60.0 then 70
      else if e < 80.0 then 85
      else 100

    scoreToLevel : Nat -> StrengthLevel
    scoreToLevel s =
      if s < 20 then VeryWeak
      else if s < 40 then Weak
      else if s < 60 then Fair
      else if s < 80 then Strong
      else VeryStrong

    estimateCrackTime : Double -> CrackTime
    estimateCrackTime entropy =
      let guessesPerSec = 1000000000.0  -- 1 billion (modern GPU)
          totalGuesses = pow 2.0 entropy
          seconds = totalGuesses / guessesPerSec
      in if seconds < 1.0 then Instant
         else if seconds < 60.0 then Seconds (cast seconds)
         else if seconds < 3600.0 then Minutes (cast (seconds / 60.0))
         else if seconds < 86400.0 then Hours (cast (seconds / 3600.0))
         else if seconds < 2592000.0 then Days (cast (seconds / 86400.0))
         else if seconds < 31536000.0 then Months (cast (seconds / 2592000.0))
         else if seconds < 3153600000.0 then Years (cast (seconds / 31536000.0))
         else if seconds < 315360000000.0 then Centuries (cast (seconds / 3153600000.0))
         else Never

    generateFeedback : List Char -> List Pattern -> StrengthLevel -> List String
    generateFeedback cs patterns level =
      let suggestions = []
          withLength = if length cs < 12
                       then "Use at least 12 characters" :: suggestions
                       else suggestions
          withMixed = if not (any isUpper cs) || not (any isLower cs)
                      then "Mix uppercase and lowercase letters" :: withLength
                      else withLength
          withDigits = if not (any isDigit cs)
                       then "Add some numbers" :: withMixed
                       else withMixed
          withSymbols = if not (any (\c => not (isAlphaNum c)) cs)
                        then "Include special characters" :: withDigits
                        else withDigits
      in case level of
           VeryWeak => "Consider using a password manager" :: withSymbols
           Weak => withSymbols
           _ => withSymbols

    generateWarnings : List Pattern -> List String
    generateWarnings [] = []
    generateWarnings (p :: ps) =
      let warning = case p of
            CommonPassword _ => "This is a commonly used password"
            DictionaryWord w => "Contains dictionary word: " ++ w
            KeyboardPattern _ => "Contains keyboard pattern"
            DatePattern _ => "Contains date-like pattern"
            _ => ""
      in if warning == "" then generateWarnings ps else warning :: generateWarnings ps

--------------------------------------------------------------------------------
-- Pattern Detection Implementation
--------------------------------------------------------------------------------

||| Detect all patterns in password
public export
detectPatterns : String -> List Pattern
detectPatterns pwd =
  catMaybes
    [ detectCommonPassword pwd
    , detectKeyboardPattern pwd
    , detectSequential pwd
    , detectDatePattern pwd
    , detectRepeatedPattern pwd
    ]
  where
    commonPasswords : List String
    commonPasswords = ["password", "123456", "12345678", "qwerty", "abc123",
                       "monkey", "1234567", "letmein", "trustno1", "dragon",
                       "baseball", "iloveyou", "master", "sunshine", "ashley",
                       "bailey", "shadow", "123123", "654321", "superman",
                       "qazwsx", "michael", "football", "password1", "password123"]

    detectCommonPassword : String -> Maybe Pattern
    detectCommonPassword p =
      if toLower p `elem` commonPasswords
        then Just (CommonPassword (toLower p))
        else Nothing

    keyboardPatterns : List String
    keyboardPatterns = ["qwerty", "qwertz", "azerty", "asdf", "zxcv",
                        "qweasd", "1qaz", "2wsx", "3edc", "4rfv"]

    detectKeyboardPattern : String -> Maybe Pattern
    detectKeyboardPattern p =
      let lower = toLower p
      in case find (`isInfixOf` lower) keyboardPatterns of
           Just pat => Just (KeyboardPattern pat)
           Nothing => Nothing

    detectSequential : String -> Maybe Pattern
    detectSequential p =
      let lower = toLower p
          sequences = ["abcd", "bcde", "cdef", "defg", "efgh",
                       "1234", "2345", "3456", "4567", "5678", "6789",
                       "0123", "9876", "8765", "7654", "6543",
                       "dcba", "edcb", "fedc", "gfed"]
      in case find (`isInfixOf` lower) sequences of
           Just seq => Just (SequentialChars seq)
           Nothing => Nothing

    detectDatePattern : String -> Maybe Pattern
    detectDatePattern p =
      let chars = unpack p
          yearLike = any (isYear . pack) (windows 4 chars)
      in if yearLike then Just (DatePattern "year pattern") else Nothing
      where
        isYear : String -> Bool
        isYear s = case parseInteger {a=Integer} s of
          Just n => n >= 1900 && n <= 2100
          Nothing => False

        windows : Nat -> List a -> List (List a)
        windows _ [] = []
        windows n xs = if length xs < n then [] else take n xs :: windows n (drop 1 xs)

    detectRepeatedPattern : String -> Maybe Pattern
    detectRepeatedPattern p =
      let chars = unpack p
          maxRepeat = findMaxRepeat chars
      in if maxRepeat >= 3
           then Just (RepeatedPattern "repeated chars" maxRepeat)
           else Nothing
      where
        findMaxRepeat : List Char -> Nat
        findMaxRepeat [] = 0
        findMaxRepeat (c :: cs) = go c 1 1 cs
          where
            go : Char -> Nat -> Nat -> List Char -> Nat
            go _ _ maxSoFar [] = maxSoFar
            go prev count maxSoFar (x :: xs) =
              if x == prev
                then go prev (S count) (max maxSoFar (S count)) xs
                else go x 1 maxSoFar xs

--------------------------------------------------------------------------------
-- Quick Strength Check
--------------------------------------------------------------------------------

||| Quick strength level check (without full analysis)
public export
quickStrengthCheck : String -> StrengthLevel
quickStrengthCheck pwd = (analyzeStrength pwd).level

||| Check if password meets minimum strength
public export
meetsMinStrength : String -> StrengthLevel -> Bool
meetsMinStrength pwd minLevel = quickStrengthCheck pwd >= minLevel

||| Get strength score (0-100)
public export
strengthScore : String -> Nat
strengthScore pwd = (analyzeStrength pwd).score

--------------------------------------------------------------------------------
-- Strength Requirements
--------------------------------------------------------------------------------

||| Minimum strength requirement for different contexts
public export
data StrengthRequirement
  = BasicAccount           -- Email, social media
  | FinancialAccount       -- Banking, payment
  | PrivilegedAccount      -- Admin, root access
  | CryptographicKey       -- Encryption keys

public export
requiredLevel : StrengthRequirement -> StrengthLevel
requiredLevel BasicAccount = Fair
requiredLevel FinancialAccount = Strong
requiredLevel PrivilegedAccount = VeryStrong
requiredLevel CryptographicKey = VeryStrong

||| Check if password meets requirement
public export
meetsRequirement : String -> StrengthRequirement -> Bool
meetsRequirement pwd req = quickStrengthCheck pwd >= requiredLevel req
