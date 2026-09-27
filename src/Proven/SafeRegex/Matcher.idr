-- SPDX-License-Identifier: Palimpsest-MPL-1.0
||| SafeRegex.Matcher - Safe regex matching with step limits
|||
||| This module provides a regex matching engine that guarantees termination
||| by tracking steps and enforcing configurable limits based on complexity.
|||
||| The matcher uses a backtracking algorithm but with strict step counting
||| to prevent catastrophic backtracking from causing denial of service.
module Proven.SafeRegex.Matcher

import Proven.Core
import Proven.SafeRegex.Types
import Proven.SafeRegex.Safety
import Data.List
import Data.String
import Data.Maybe

%default total

--------------------------------------------------------------------------------
-- Matching State
--------------------------------------------------------------------------------

||| State maintained during matching
public export
record MatchState where
  constructor MkMatchState
  ||| Current position in input string
  position : Nat
  ||| Steps taken so far
  steps : Nat
  ||| Maximum steps allowed
  maxSteps : Nat
  ||| Captured groups (group id -> (start, end, text))
  captures : List (Nat, Nat, Nat, String)
  ||| The input string being matched
  input : String
  ||| Input as list of characters (for efficient access)
  inputChars : List Char
  ||| Length of input
  inputLen : Nat
  ||| Regex flags
  flags : RegexFlags

||| Create initial match state
public export
initState : String -> Nat -> RegexFlags -> MatchState
initState s maxSteps flags =
  let chars = unpack s
  in MkMatchState 0 0 maxSteps [] s chars (length chars) flags

||| Increment step counter
public export
step : MatchState -> Maybe MatchState
step st =
  if st.steps >= st.maxSteps
    then Nothing  -- Step limit exceeded
    else Just $ { steps := S st.steps } st

||| Move position forward
public export
advance : MatchState -> Nat -> MatchState
advance st n = { position := st.position + n } st

||| Get character at current position
public export
currentChar : MatchState -> Maybe Char
currentChar st =
  if st.position >= st.inputLen
    then Nothing
    else Just $ assert_total $ strIndex st.input (cast st.position)

||| Get character at specific position
public export
charAt : MatchState -> Nat -> Maybe Char
charAt st pos =
  if pos >= st.inputLen
    then Nothing
    else Just $ assert_total $ strIndex st.input (cast pos)

||| Check if at end of input
public export
atEnd : MatchState -> Bool
atEnd st = st.position >= st.inputLen

||| Check if at start of input
public export
atStart : MatchState -> Bool
atStart st = st.position == 0

||| Get substring from input
public export
substring : MatchState -> Nat -> Nat -> String
substring st start end =
  if start >= st.inputLen || end <= start
    then ""
    else substr start (minus end start) st.input

||| Save a capture
public export
saveCapture : MatchState -> Nat -> Nat -> Nat -> MatchState
saveCapture st groupId start end =
  let text = substring st start end
      newCapture = (groupId, start, end, text)
  in { captures := newCapture :: st.captures } st

||| Get a capture by group ID
public export
getCapture : MatchState -> Nat -> Maybe (Nat, Nat, String)
getCapture st groupId =
  case find (\(gid, _, _, _) => gid == groupId) st.captures of
    Just (_, start, end, text) => Just (start, end, text)
    Nothing => Nothing

--------------------------------------------------------------------------------
-- Character Matching
--------------------------------------------------------------------------------

||| Match a character class with the supplied regex flags.
||| The signature precedes `matchCharClass` deliberately: Idris2 resolves
||| top-level names in declaration order.
matchesClassWithFlags : Char -> CharClass -> RegexFlags -> Bool

||| Match a character class at current position
public export
matchCharClass : MatchState -> CharClass -> Bool
matchCharClass st cls =
  case currentChar st of
    Nothing => False
    Just c =>
      let c' = if st.flags.caseInsensitive then toLower c else c
      in matchesClassWithFlags c' cls st.flags

-- Definition for the forward declaration above.
matchesClassWithFlags c cls flags =
  case cls of
    SingleChar x =>
      let x' = if flags.caseInsensitive then toLower x else x
      in c == x'
    Range from to =>
      let from' = if flags.caseInsensitive then toLower from else from
          to' = if flags.caseInsensitive then toLower to else to
      in c >= from' && c <= to'
    Digit => c >= '0' && c <= '9'
    Word => isAlphaNum c || c == '_'
    Space => isSpace c
    Any => if flags.dotAll then True else c /= '\n'
    Negate inner => not (matchesClassWithFlags c inner flags)
    Union c1 c2 => matchesClassWithFlags c c1 flags || matchesClassWithFlags c c2 flags

--------------------------------------------------------------------------------
-- Anchor Matching
--------------------------------------------------------------------------------

||| Check if at start of line (for multiline mode)
public export
atLineStart : MatchState -> Bool
atLineStart st =
  if st.position == 0
    then True
    else case charAt st (minus st.position 1) of
           Just '\n' => True
           _ => False

||| Check if at end of line (for multiline mode)
public export
atLineEnd : MatchState -> Bool
atLineEnd st =
  if st.position >= st.inputLen
    then True
    else case currentChar st of
           Just '\n' => True
           _ => False

||| Check word boundary
public export
atWordBoundary : MatchState -> Bool
atWordBoundary st =
  let prevIsWord = case charAt st (minus st.position 1) of
                     Nothing => False
                     Just c => isAlphaNum c || c == '_'
      currIsWord = case currentChar st of
                     Nothing => False
                     Just c => isAlphaNum c || c == '_'
  in prevIsWord /= currIsWord

--------------------------------------------------------------------------------
-- Core Matching Engine
--------------------------------------------------------------------------------

||| Result of a match attempt
public export
data MatchAttempt : Type where
  ||| Match succeeded, returning new state
  Success : MatchState -> MatchAttempt
  ||| Match failed but can backtrack
  Failure : MatchState -> MatchAttempt
  ||| Step limit exceeded - abort
  StepLimitExceeded : Nat -> MatchAttempt

-- Keeping the mutually recursive helpers at top level makes every fuel
-- decrease visible to Idris2's totality checker; a `where` block would attach
-- only to one equation.
mutual
  ||| Match a regex against input starting at current position.
  ||| Every recursive path consumes fuel.
  public export
  matchRegex : (fuel : Nat) -> Regex -> MatchState -> MatchAttempt
  matchRegex Z _ st = StepLimitExceeded st.steps
  matchRegex (S fuel) r st =
    case step st of
      Nothing => StepLimitExceeded st.steps
      Just st' => matchRegexStep fuel r st'

  matchRegexStep : Nat -> Regex -> MatchState -> MatchAttempt
  matchRegexStep _ Empty st = Success st
  matchRegexStep _ Never st = Failure st
  matchRegexStep _ (Match cls) st =
    if matchCharClass st cls
      then Success (advance st 1)
      else Failure st
  matchRegexStep fuel (Seq r1 r2) st =
    case matchRegex fuel r1 st of
      Success st' => matchRegex fuel r2 st'
      Failure st' => Failure st'
      StepLimitExceeded n => StepLimitExceeded n
  matchRegexStep fuel (Alt r1 r2) st =
    case matchRegex fuel r1 st of
      Success st' => Success st'
      Failure _ => matchRegex fuel r2 st
      StepLimitExceeded n => StepLimitExceeded n
  matchRegexStep fuel (Quant r q) st =
    matchQuantified fuel r q 0 st
  matchRegexStep fuel (Group gid r) st =
    let startPos = st.position
    in case matchRegex fuel r st of
         Success st' => Success (saveCapture st' gid startPos st'.position)
         other => other
  matchRegexStep fuel (NCGroup r) st = matchRegex fuel r st
  matchRegexStep _ StartAnchor st =
    if st.flags.multiline
      then if atLineStart st then Success st else Failure st
      else if atStart st then Success st else Failure st
  matchRegexStep _ EndAnchor st =
    if st.flags.multiline
      then if atLineEnd st then Success st else Failure st
      else if atEnd st then Success st else Failure st
  matchRegexStep _ WordBoundary st =
    if atWordBoundary st then Success st else Failure st
  matchRegexStep _ (BackRef gid) st =
    case getCapture st gid of
      Nothing => Failure st
      Just (_, _, text) =>
        let textLen = length text
        in if st.position + textLen <= st.inputLen &&
              substring st st.position (st.position + textLen) == text
             then Success (advance st textLen)
             else Failure st
  matchRegexStep fuel (Lookahead True r) st =
    case matchRegex fuel r st of
      Success _ => Success st
      Failure st' => Failure st'
      StepLimitExceeded n => StepLimitExceeded n
  matchRegexStep fuel (Lookahead False r) st =
    case matchRegex fuel r st of
      Success _ => Failure st
      Failure _ => Success st
      StepLimitExceeded n => StepLimitExceeded n
  matchRegexStep fuel (Lookbehind positive r) st =
    matchLookbehind fuel r st positive

  matchQuantified : Nat -> Regex -> Quantifier -> Nat -> MatchState -> MatchAttempt
  matchQuantified Z _ _ _ st = StepLimitExceeded st.steps
  matchQuantified (S fuel) r q count st =
    case step st of
      Nothing => StepLimitExceeded st.steps
      Just st' =>
        let atMax = case q.maxCount of
                      Nothing => False
                      Just m => count >= m
        in if atMax
             then Success st'
             else if q.greedy
               then matchQuantifiedGreedy fuel r q count st'
               else matchQuantifiedLazy fuel r q count st'

  matchQuantifiedGreedy : Nat -> Regex -> Quantifier -> Nat -> MatchState -> MatchAttempt
  matchQuantifiedGreedy Z _ _ _ st = StepLimitExceeded st.steps
  matchQuantifiedGreedy (S fuel) r q count st =
    case matchRegex fuel r st of
      Success st' =>
        case matchQuantified fuel r q (S count) st' of
          Success st'' => Success st''
          Failure _ =>
            if count >= q.minCount then Success st' else Failure st
          StepLimitExceeded n => StepLimitExceeded n
      Failure _ =>
        if count >= q.minCount then Success st else Failure st
      StepLimitExceeded n => StepLimitExceeded n

  matchQuantifiedLazy : Nat -> Regex -> Quantifier -> Nat -> MatchState -> MatchAttempt
  matchQuantifiedLazy Z _ _ _ st = StepLimitExceeded st.steps
  matchQuantifiedLazy (S fuel) r q count st =
    if count >= q.minCount
      then Success st
      else case matchRegex fuel r st of
             Success st' => matchQuantified fuel r q (S count) st'
             other => other

  matchLookbehind : Nat -> Regex -> MatchState -> Bool -> MatchAttempt
  matchLookbehind Z _ st _ = StepLimitExceeded st.steps
  matchLookbehind (S fuel) r st positive =
    tryLookbehindFrom fuel r st st.position positive

  tryLookbehindFrom : Nat -> Regex -> MatchState -> Nat -> Bool -> MatchAttempt
  tryLookbehindFrom Z _ st _ _ = StepLimitExceeded st.steps
  tryLookbehindFrom (S fuel) r st 0 positive =
    let testSt = { position := 0 } st
    in case matchRegex fuel r testSt of
         Success st' =>
           if st'.position == st.position
             then if positive then Success st else Failure st
             else if positive then Failure st else Success st
         Failure _ => if positive then Failure st else Success st
         StepLimitExceeded n => StepLimitExceeded n
  tryLookbehindFrom (S fuel) r st pos positive =
    let testSt = { position := minus pos 1 } st
    in case matchRegex fuel r testSt of
         Success st' =>
           if st'.position == st.position
             then if positive then Success st else Failure st
             else tryLookbehindFrom fuel r st (minus pos 1) positive
         Failure _ => tryLookbehindFrom fuel r st (minus pos 1) positive
         StepLimitExceeded n => StepLimitExceeded n

--------------------------------------------------------------------------------
-- High-Level Matching API
--------------------------------------------------------------------------------

||| Convert captures to Capture records
public export
toCaptures : List (Nat, Nat, Nat, String) -> List Capture
toCaptures = map (\(gid, start, end, text) => MkCapture gid start end text)

||| Try to match regex at a specific position
public export
matchAt : SafeRegex -> String -> Nat -> RegexFlags -> MatchResult
matchAt sr input pos flags =
  let st = initState input sr.stepLimit flags
      st' = { position := pos } st
      fuel = sr.stepLimit * 2  -- Extra fuel for backtracking
  in case matchRegex fuel sr.regex st' of
       Success finalSt =>
         success pos finalSt.position (toCaptures finalSt.captures) finalSt.steps
       Failure finalSt =>
         noMatch finalSt.steps
       StepLimitExceeded steps =>
         noMatch steps

||| Find first match in input string
public export
findFirst : SafeRegex -> String -> RegexFlags -> MatchResult
findFirst sr input flags = findFrom (S inputLen) 0
  where
    inputLen : Nat
    inputLen = length input

    findFrom : (fuel : Nat) -> Nat -> MatchResult
    findFrom Z _ = noMatch 0
    findFrom (S fuel) pos =
      if pos > inputLen
        then noMatch 0
        else case matchAt sr input pos flags of
               result@(MkMatchResult True _ _ _) => result
               MkMatchResult False _ _ steps =>
                 if pos < inputLen
                   then findFrom fuel (S pos)
                   else noMatch steps

||| Find all matches in input string
public export
findAll : SafeRegex -> String -> RegexFlags -> List MatchResult
findAll sr input flags = findFrom (S inputLen) 0
  where
    inputLen : Nat
    inputLen = length input

    findFrom : (fuel : Nat) -> Nat -> List MatchResult
    findFrom Z _ = []
    findFrom (S fuel) pos =
      if pos > inputLen
        then []
        else case matchAt sr input pos flags of
               result@(MkMatchResult True (Just (_, end)) _ _) =>
                 result :: findFrom fuel (max (S pos) end)
               MkMatchResult False _ _ _ =>
                 if pos < inputLen
                   then findFrom fuel (S pos)
                   else []
               _ => findFrom fuel (S pos)

||| Test if regex matches anywhere in input
public export
test : SafeRegex -> String -> Bool
test sr input =
  let result = findFirst sr input defaultFlags
  in result.matched

||| Test if regex matches entire input
public export
testFull : SafeRegex -> String -> Bool
testFull sr input =
  case matchAt sr input 0 defaultFlags of
    MkMatchResult True (Just (0, end)) _ _ => end == length input
    _ => False

||| Replace first match
public export
replaceFirst : SafeRegex -> String -> String -> String
replaceFirst sr input replacement =
  case findFirst sr input defaultFlags of
    MkMatchResult True (Just (start, end)) _ _ =>
      substr 0 start input ++ replacement ++ substr end (minus (length input) end) input
    _ => input

||| Replace all matches
public export
replaceAll : SafeRegex -> String -> String -> String
replaceAll sr input replacement = go (S inputLen) 0 ""
  where
    inputLen : Nat
    inputLen = length input

    go : (fuel : Nat) -> Nat -> String -> String
    go Z pos acc = acc ++ substr pos (minus inputLen pos) input
    go (S fuel) pos acc =
      if pos >= inputLen
        then acc ++ substr pos (minus inputLen pos) input
        else case matchAt sr input pos defaultFlags of
               MkMatchResult True (Just (start, end)) _ _ =>
                 let before = substr pos (minus start pos) input
                     newPos = max (S pos) end
                 in go fuel newPos (acc ++ before ++ replacement)
               _ =>
                 if pos < inputLen
                   then go fuel (S pos) (acc ++ singleton (assert_total $ strIndex input (cast pos)))
                   else acc

||| Split string by regex
public export
split : SafeRegex -> String -> List String
split sr input = go (S inputLen) 0 []
  where
    inputLen : Nat
    inputLen = length input

    go : (fuel : Nat) -> Nat -> List String -> List String
    go Z pos acc = reverse (substr pos (minus inputLen pos) input :: acc)
    go (S fuel) pos acc =
      if pos >= inputLen
        then reverse (substr pos (minus inputLen pos) input :: acc)
        else case matchAt sr input pos defaultFlags of
               MkMatchResult True (Just (start, end)) _ _ =>
                 let part = substr pos (minus start pos) input
                     newPos = max (S pos) end
                 in go fuel newPos (part :: acc)
               _ =>
                 reverse (substr pos (minus inputLen pos) input :: acc)

--------------------------------------------------------------------------------
-- Safe Matching Wrappers
--------------------------------------------------------------------------------

||| Match with default flags
public export
match : SafeRegex -> String -> MatchResult
match sr input = findFirst sr input defaultFlags

||| Match with custom flags
public export
matchWithFlags : SafeRegex -> String -> RegexFlags -> MatchResult
matchWithFlags = findFirst

||| Check if input is safe to match
public export
safeToMatch : SafeRegex -> String -> Bool
safeToMatch sr input = isInputSafe sr input

||| Match only if input is safe, otherwise return error indicator
public export
safeMatch : SafeRegex -> String -> Either String MatchResult
safeMatch sr input =
  if safeToMatch sr input
    then Right (match sr input)
    else Left $ "Input too long for regex complexity level: " ++
                show (length input) ++ " > " ++ show (maxSafeInputLength sr)

--------------------------------------------------------------------------------
-- Capture Group Utilities
--------------------------------------------------------------------------------

||| Get capture by group number (0 = full match)
public export
getGroup : MatchResult -> Nat -> Maybe String
getGroup result n =
  case find (\c => c.groupId == n) result.captures of
    Just cap => Just cap.text
    Nothing => Nothing

||| Get all capture texts
public export
getAllGroups : MatchResult -> List String
getAllGroups result = map text result.captures

||| Get named captures (if groups were named)
public export
getNamedCaptures : MatchResult -> List (Nat, String)
getNamedCaptures result = map (\c => (c.groupId, c.text)) result.captures
