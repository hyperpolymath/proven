-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
||| SafeHomoglyph - Unicode confusable and homoglyph detection
|||
||| Detects visually similar characters used in phishing:
||| - Latin/Cyrillic confusables (а vs a, о vs o)
||| - IDN homograph attacks
||| - Mixed-script detection
||| Prevents: domain spoofing, credential phishing, visual deception.
module Proven.SafeHomoglyph

import Data.String
import Data.List
import Data.Nat

%default total

-- ============================================================================
-- SCRIPT DETECTION
-- ============================================================================

||| Unicode script categories (simplified)
public export
data Script =
    Latin | Cyrillic | Greek | Armenian | Georgian
  | Arabic | Hebrew | Devanagari | CJK
  | Common   -- Digits, punctuation, symbols
  | Unknown

public export
Show Script where
  show Latin = "Latin"; show Cyrillic = "Cyrillic"; show Greek = "Greek"
  show Armenian = "Armenian"; show Georgian = "Georgian"
  show Arabic = "Arabic"; show Hebrew = "Hebrew"
  show Devanagari = "Devanagari"; show CJK = "CJK"
  show Common = "Common"; show Unknown = "Unknown"

public export
Eq Script where
  Latin == Latin = True; Cyrillic == Cyrillic = True; Greek == Greek = True
  Armenian == Armenian = True; Georgian == Georgian = True
  Arabic == Arabic = True; Hebrew == Hebrew = True
  Devanagari == Devanagari = True; CJK == CJK = True
  Common == Common = True; Unknown == Unknown = True
  _ == _ = False

||| Classify a character's script (simplified — covers most confusable ranges)
public export
classifyScript : Char -> Script
classifyScript c =
  let n = ord c
  in if n >= 0x0041 && n <= 0x024F then Latin      -- Basic Latin + Extended
     else if n >= 0x0400 && n <= 0x04FF then Cyrillic
     else if n >= 0x0370 && n <= 0x03FF then Greek
     else if n >= 0x0530 && n <= 0x058F then Armenian
     else if n >= 0x10A0 && n <= 0x10FF then Georgian
     else if n >= 0x0600 && n <= 0x06FF then Arabic
     else if n >= 0x0590 && n <= 0x05FF then Hebrew
     else if n >= 0x0900 && n <= 0x097F then Devanagari
     else if n >= 0x4E00 && n <= 0x9FFF then CJK
     else if n >= 0x30 && n <= 0x39 then Common    -- Digits
     else if n < 0x80 then Common                   -- ASCII punctuation
     else Unknown

-- ============================================================================
-- CONFUSABLE PAIRS
-- ============================================================================

||| Known Latin/Cyrillic confusable pairs (most dangerous for phishing)
confusablePairs : List (Char, Char, String)
confusablePairs =
  [ ('a', '\x0430', "Latin a / Cyrillic а")
  , ('c', '\x0441', "Latin c / Cyrillic с")
  , ('e', '\x0435', "Latin e / Cyrillic е")
  , ('o', '\x043E', "Latin o / Cyrillic о")
  , ('p', '\x0440', "Latin p / Cyrillic р")
  , ('x', '\x0445', "Latin x / Cyrillic х")
  , ('y', '\x0443', "Latin y / Cyrillic у")
  , ('A', '\x0410', "Latin A / Cyrillic А")
  , ('B', '\x0412', "Latin B / Cyrillic В")
  , ('C', '\x0421', "Latin C / Cyrillic С")
  , ('E', '\x0415', "Latin E / Cyrillic Е")
  , ('H', '\x041D', "Latin H / Cyrillic Н")
  , ('K', '\x041A', "Latin K / Cyrillic К")
  , ('M', '\x041C', "Latin M / Cyrillic М")
  , ('O', '\x041E', "Latin O / Cyrillic О")
  , ('P', '\x0420', "Latin P / Cyrillic Р")
  , ('T', '\x0422', "Latin T / Cyrillic Т")
  , ('X', '\x0425', "Latin X / Cyrillic Х")
  ]

||| Check if a character has a known confusable
public export
isConfusable : Char -> Bool
isConfusable c = any (\(a, b, _) => c == a || c == b) confusablePairs

-- ============================================================================
-- MIXED-SCRIPT DETECTION
-- ============================================================================

||| A homoglyph detection result
public export
record HomoglyphResult where
  constructor MkHomoglyphResult
  isMixedScript    : Bool
  scripts          : List Script    -- Unique scripts found
  confusableCount  : Nat            -- Number of confusable characters
  riskLevel        : Nat            -- 0=safe, 1=low, 2=medium, 3=high

||| Analyse a string for homoglyph risk
public export covering
analyseString : String -> HomoglyphResult
analyseString s =
  let chars = unpack s
      scripts = nubScripts (map classifyScript chars)
      nonCommon = filter (/= Common) scripts
      mixed = length nonCommon > 1
      confCount = length (filter isConfusable chars)
      risk = if confCount > 3 then 3
             else if mixed && confCount > 0 then 3
             else if mixed then 2
             else if confCount > 0 then 1
             else 0
  in MkHomoglyphResult mixed scripts confCount risk
  where
    nubScripts : List Script -> List Script
    nubScripts [] = []
    nubScripts (x :: xs) = x :: nubScripts (filter (/= x) xs)

||| Quick check: is a string likely a homoglyph attack?
public export covering
isLikelyHomoglyph : String -> Bool
isLikelyHomoglyph s = (analyseString s).riskLevel >= 2

||| Check a domain name for IDN homograph attacks
public export covering
isDomainSuspicious : String -> Bool
isDomainSuspicious domain =
  let result = analyseString domain
  in result.isMixedScript || result.confusableCount > 0

-- ============================================================================
-- NORMALISATION
-- ============================================================================

||| Skeleton mapping: replace confusables with their Latin equivalents
public export
toSkeleton : String -> String
toSkeleton = pack . map skeletonChar . unpack
  where
    skeletonChar : Char -> Char
    skeletonChar c =
      case find (\(_, b, _) => c == b) confusablePairs of
        Just (a, _, _) => a
        Nothing => c

||| Check if two strings are confusably equivalent
public export
areConfusable : String -> String -> Bool
areConfusable a b = toSkeleton a == toSkeleton b
