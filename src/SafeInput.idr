-- SPDX-License-Identifier: PMPL-1.0
-- SPDX-FileCopyrightText: 2025 Hyperpolymath

||| SafeInput - Safe input handling for interactive applications
|||
||| Inspired by TUI input handling where user input must be validated
||| character by character and expressions parsed safely.

module SafeInput

import Data.String
import Data.List
import Data.List1

%default total

-- ============================================================================
-- CORE TYPES
-- ============================================================================

||| Input validation result
public export
data InputResult : Type -> Type where
  Valid : a -> InputResult a
  Invalid : (reason : String) -> InputResult a
  Partial : (hint : String) -> InputResult a  -- Valid so far, needs more input

||| Character class for input filtering
public export
data CharClass : Type where
  Alphanumeric : CharClass
  Alphabetic : CharClass
  Numeric : CharClass
  Printable : CharClass
  Identifier : CharClass  -- Letters, digits, underscore
  HexDigit : CharClass
  Custom : (Char -> Bool) -> CharClass

||| Input buffer with validation
public export
record InputBuffer where
  constructor MkBuffer
  content : String
  maxLength : Nat
  cursorPos : Nat
  charFilter : Maybe CharClass

||| Keystroke representation
public export
data Keystroke : Type where
  CharKey : Char -> Keystroke
  Backspace : Keystroke
  Delete : Keystroke
  Enter : Keystroke
  Escape : Keystroke
  Left : Keystroke
  Right : Keystroke
  Home : Keystroke
  End : Keystroke
  Tab : Keystroke

-- ============================================================================
-- CHARACTER CLASSIFICATION
-- ============================================================================

||| Check if character matches a class
export
matchesClass : Char -> CharClass -> Bool
matchesClass c Alphanumeric = isAlphaNum c
matchesClass c Alphabetic = isAlpha c
matchesClass c Numeric = isDigit c
matchesClass c Printable = c >= ' ' && c <= '~'
matchesClass c Identifier = isAlphaNum c || c == '_'
matchesClass c HexDigit = isHexDigit c
matchesClass c (Custom f) = f c

-- ============================================================================
-- INPUT BUFFER OPERATIONS
-- ============================================================================

||| Create an empty input buffer
export
emptyBuffer : (maxLen : Nat) -> InputBuffer
emptyBuffer maxLen = MkBuffer "" maxLen 0 Nothing

||| Create a buffer with character filtering
export
filteredBuffer : (maxLen : Nat) -> CharClass -> InputBuffer
filteredBuffer maxLen cls = MkBuffer "" maxLen 0 (Just cls)

||| Get buffer content
export
getContent : InputBuffer -> String
getContent = content

||| Get buffer length
export
getLength : InputBuffer -> Nat
getLength buf = length buf.content

||| Check if buffer is empty
export
bufferIsEmpty : InputBuffer -> Bool
bufferIsEmpty buf = buf.content == ""

||| Check if buffer is at max length
export
bufferIsFull : InputBuffer -> Bool
bufferIsFull buf = length buf.content >= buf.maxLength

||| Handle a keystroke
export
handleKey : Keystroke -> InputBuffer -> InputBuffer
handleKey key buf = case key of
  CharKey c => insertChar c buf
  Backspace => deleteBackward buf
  Delete => deleteForward buf
  Left => moveCursorLeft buf
  Right => moveCursorRight buf
  Home => { cursorPos := 0 } buf
  End => { cursorPos := length buf.content } buf
  _ => buf  -- Enter, Escape, Tab handled at higher level
  where
    insertChar : Char -> InputBuffer -> InputBuffer
    insertChar c b =
      -- Check length limit
      if length b.content >= b.maxLength then b
      -- Check character filter
      else case b.charFilter of
        Nothing => doInsert c b
        Just cls => if matchesClass c cls then doInsert c b else b

    doInsert : Char -> InputBuffer -> InputBuffer
    doInsert c b =
      let chars = unpack b.content
          (before, after) = splitAt b.cursorPos chars
          newContent = pack (before ++ [c] ++ after)
      in { content := newContent, cursorPos := b.cursorPos + 1 } b

    deleteBackward : InputBuffer -> InputBuffer
    deleteBackward b =
      if b.cursorPos == 0 then b
      else let chars = unpack b.content
               (before, after) = splitAt b.cursorPos chars
               newBefore = take (length before `minus` 1) before
               newContent = pack (newBefore ++ after)
           in { content := newContent, cursorPos := b.cursorPos `minus` 1 } b

    deleteForward : InputBuffer -> InputBuffer
    deleteForward b =
      let chars = unpack b.content
          (before, after) = splitAt b.cursorPos chars
          newAfter = drop 1 after
          newContent = pack (before ++ newAfter)
      in { content := newContent } b

    moveCursorLeft : InputBuffer -> InputBuffer
    moveCursorLeft b =
      { cursorPos := b.cursorPos `minus` 1 } b

    moveCursorRight : InputBuffer -> InputBuffer
    moveCursorRight b =
      { cursorPos := min (b.cursorPos + 1) (length b.content) } b

||| Clear the buffer
export
clearBuffer : InputBuffer -> InputBuffer
clearBuffer buf = { content := "", cursorPos := 0 } buf

||| Set buffer content (respects max length and filter)
export
setContent : String -> InputBuffer -> InputBuffer
setContent s buf =
  let filtered = case buf.charFilter of
        Nothing => s
        Just cls => pack (filter (\c => matchesClass c cls) (unpack s))
      truncated = substr 0 buf.maxLength filtered
  in { content := truncated, cursorPos := length truncated } buf

-- ============================================================================
-- INPUT VALIDATION
-- ============================================================================

||| Validate as integer
export
validateInt : String -> InputResult Integer
validateInt s =
  if s == "" then Partial "Enter a number"
  else if s == "-" then Partial "Enter digits after minus"
  else case parseInteger s of
    Nothing => Invalid "Not a valid integer"
    Just n => Valid n

||| Validate as natural number
export
validateNat : String -> InputResult Nat
validateNat s =
  if s == "" then Partial "Enter a positive number"
  else case parsePositive s of
    Nothing => Invalid "Not a valid positive number"
    Just n => Valid n

||| Validate as decimal number
export
validateDecimal : String -> InputResult Double
validateDecimal s =
  if s == "" then Partial "Enter a number"
  else if s == "-" || s == "." || s == "-." then Partial "Continue entering number"
  else case parseDouble s of
    Nothing => Invalid "Not a valid number"
    Just d => Valid d

||| Validate non-empty string
export
validateNonEmpty : String -> InputResult String
validateNonEmpty s =
  if s == "" then Partial "Enter some text"
  else Valid s

||| Validate with length bounds
export
validateLength : (min : Nat) -> (max : Nat) -> String -> InputResult String
validateLength minLen maxLen s =
  let len = length s
  in if len < minLen then Partial ("Need at least " ++ show minLen ++ " characters")
     else if len > maxLen then Invalid ("Maximum " ++ show maxLen ++ " characters")
     else Valid s

||| Validate identifier (variable name style)
export
validateIdentifier : String -> InputResult String
validateIdentifier s =
  if s == "" then Partial "Enter an identifier"
  else let chars = unpack s
       in case chars of
            [] => Partial "Enter an identifier"
            (c :: cs) =>
              if not (isAlpha c || c == '_')
                then Invalid "Must start with letter or underscore"
                else if all (\x => isAlphaNum x || x == '_') cs
                  then Valid s
                  else Invalid "Only letters, digits, and underscores allowed"

-- ============================================================================
-- EXPRESSION PARSING (for calculator-style input)
-- ============================================================================

||| Simple expression token
public export
data ExprToken : Type where
  TokNum : Integer -> ExprToken
  TokPlus : ExprToken
  TokMinus : ExprToken
  TokMul : ExprToken
  TokDiv : ExprToken
  TokMod : ExprToken
  TokLParen : ExprToken
  TokRParen : ExprToken

||| Tokenize a simple arithmetic expression
export
tokenize : String -> InputResult (List ExprToken)
tokenize s = go (unpack (trim s)) []
  where
    go : List Char -> List ExprToken -> InputResult (List ExprToken)
    go [] acc = Valid (reverse acc)
    go (' ' :: cs) acc = go cs acc
    go ('+' :: cs) acc = go cs (TokPlus :: acc)
    go ('-' :: cs) acc = go cs (TokMinus :: acc)
    go ('*' :: cs) acc = go cs (TokMul :: acc)
    go ('/' :: cs) acc = go cs (TokDiv :: acc)
    go ('%' :: cs) acc = go cs (TokMod :: acc)
    go ('(' :: cs) acc = go cs (TokLParen :: acc)
    go (')' :: cs) acc = go cs (TokRParen :: acc)
    go cs@(c :: _) acc =
      if isDigit c
        then let (numChars, rest) = span isDigit cs
             in case parseInteger (pack numChars) of
                  Nothing => Invalid "Invalid number"
                  Just n => go rest (TokNum n :: acc)
        else Invalid ("Unexpected character: " ++ singleton c)

-- ============================================================================
-- CONVENIENCE PRESETS
-- ============================================================================

||| Buffer for numeric input only
export
numericBuffer : (maxLen : Nat) -> InputBuffer
numericBuffer maxLen = filteredBuffer maxLen Numeric

||| Buffer for identifier input
export
identifierBuffer : (maxLen : Nat) -> InputBuffer
identifierBuffer maxLen = filteredBuffer maxLen Identifier

||| Buffer for hex input
export
hexBuffer : (maxLen : Nat) -> InputBuffer
hexBuffer maxLen = filteredBuffer maxLen HexDigit

||| Buffer for general text with printable chars only
export
textBuffer : (maxLen : Nat) -> InputBuffer
textBuffer maxLen = filteredBuffer maxLen Printable
