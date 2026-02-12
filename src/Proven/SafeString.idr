-- SPDX-License-Identifier: Apache-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
||| SafeString - Text operations that handle encoding and escaping safely
|||
||| This module provides safe string operations that:
||| - Handle UTF-8 encoding/decoding without throwing exceptions
||| - Escape strings for SQL, HTML, and other contexts
||| - Truncate strings at grapheme boundaries (not mid-emoji)
||| - Validate string contents
module Proven.SafeString

import public Proven.Core
import public Proven.SafeString.UTF8
import public Proven.SafeString.Escape

import Data.List
import Data.String

%default total

--------------------------------------------------------------------------------
-- Safe String Access
--------------------------------------------------------------------------------

||| Get character at index, returning Nothing if out of bounds
public export
charAt : String -> Nat -> Maybe Char
charAt s idx =
  let chars = unpack s
  in index' idx chars
  where
    index' : Nat -> List Char -> Maybe Char
    index' _ [] = Nothing
    index' Z (x :: _) = Just x
    index' (S n) (_ :: xs) = index' n xs

||| Get substring safely
||| Returns Nothing if indices are invalid
public export
substring : (start : Nat) -> (length : Nat) -> String -> Maybe String
substring start len s =
  let chars = unpack s
      totalLen = length chars
  in if start > totalLen
       then Nothing
       else Just $ pack $ take len (drop start chars)

||| Safe head - get first character
public export
headChar : String -> Maybe Char
headChar s = case strM s of
  StrNil => Nothing
  StrCons c _ => Just c

||| Safe tail - get string without first character
public export
tailStr : String -> Maybe String
tailStr s = case strM s of
  StrNil => Nothing
  StrCons _ rest => Just rest

||| Safe last character
public export
lastChar : String -> Maybe Char
lastChar s =
  let chars = unpack s
  in case chars of
       [] => Nothing
       _ => Just (last chars)

--------------------------------------------------------------------------------
-- String Predicates
--------------------------------------------------------------------------------

||| Check if string is empty
public export
isEmpty : String -> Bool
isEmpty s = length s == 0

||| Check if string is not empty
public export
isNotEmpty : String -> Bool
isNotEmpty = not . isEmpty

||| Check if string isInfixOf only whitespace
public export
isBlank : String -> Bool
isBlank s = all isSpace (unpack s)

||| Check if string isInfixOf a substring
public export
isInfixOf : (needle : String) -> (haystack : String) -> Bool
isInfixOf needle haystack = isInfixOf needle haystack

||| Check if string starts with prefix
public export
startsWith : (prefix : String) -> String -> Bool
startsWith prefix s = isPrefixOf prefix s

||| Check if string ends with suffix
public export
endsWith : (suffix : String) -> String -> Bool
endsWith suffix s = isSuffixOf suffix s

--------------------------------------------------------------------------------
-- String Transformation
--------------------------------------------------------------------------------

||| Trim whitespace from both ends
public export
trim : String -> String
trim = ltrim . rtrim
  where
    ltrim : String -> String
    ltrim s = pack (dropWhile isSpace (unpack s))

    rtrim : String -> String
    rtrim s = pack (reverse (dropWhile isSpace (reverse (unpack s))))

||| Trim whitespace from left
public export
trimLeft : String -> String
trimLeft s = pack (dropWhile isSpace (unpack s))

||| Trim whitespace from right
public export
trimRight : String -> String
trimRight s = pack (reverse (dropWhile isSpace (reverse (unpack s))))

||| Pad string on the left to reach target length
public export
padLeft : (targetLen : Nat) -> (padChar : Char) -> String -> String
padLeft targetLen padChar s =
  let currentLen = length s
  in if currentLen >= targetLen
       then s
       else pack (replicate (minus targetLen currentLen) padChar) ++ s

||| Pad string on the right to reach target length
public export
padRight : (targetLen : Nat) -> (padChar : Char) -> String -> String
padRight targetLen padChar s =
  let currentLen = length s
  in if currentLen >= targetLen
       then s
       else s ++ pack (replicate (minus targetLen currentLen) padChar)

||| Truncate string to maximum length
||| This is a simple truncation - for grapheme-aware truncation, use truncateGrapheme
public export
truncate : (maxLen : Nat) -> String -> String
truncate maxLen s =
  if length s <= maxLen
    then s
    else pack (take maxLen (unpack s))

||| Truncate with ellipsis
public export
truncateWithEllipsis : (maxLen : Nat) -> String -> String
truncateWithEllipsis maxLen s =
  if length s <= maxLen
    then s
    else if maxLen <= 3
      then truncate maxLen s
      else truncate (minus maxLen 3) s ++ "..."

--------------------------------------------------------------------------------
-- String Splitting and Joining
--------------------------------------------------------------------------------

||| Split string on a delimiter
public export
split : (delimiter : Char) -> String -> List String
split delim s = splitHelper delim (unpack s) [] []
  where
    splitHelper : Char -> List Char -> List Char -> List String -> List String
    splitHelper _ [] current acc = reverse (pack (reverse current) :: acc)
    splitHelper d (c :: cs) current acc =
      if c == d
        then splitHelper d cs [] (pack (reverse current) :: acc)
        else splitHelper d cs (c :: current) acc

||| Split string into lines
public export
lines : String -> List String
lines = split '\n'

||| Split string into words (whitespace-separated)
public export
words : String -> List String
words s = filter (not . isEmpty) (wordsHelper (unpack s) [] [])
  where
    wordsHelper : List Char -> List Char -> List String -> List String
    wordsHelper [] current acc =
      reverse (if null current then acc else pack (reverse current) :: acc)
    wordsHelper (c :: cs) current acc =
      if isSpace c
        then wordsHelper cs [] (if null current then acc else pack (reverse current) :: acc)
        else wordsHelper cs (c :: current) acc

||| Join strings with a separator
public export
join : (separator : String) -> List String -> String
join _ [] = ""
join _ [x] = x
join sep (x :: xs) = x ++ sep ++ join sep xs

||| Join lines with newline
public export
unlines : List String -> String
unlines = join "\n"

||| Join words with space
public export
unwords : List String -> String
unwords = join " "

--------------------------------------------------------------------------------
-- Case Operations
--------------------------------------------------------------------------------

||| Convert to lowercase (ASCII only)
public export
toLowerAscii : String -> String
toLowerAscii = pack . map toLower . unpack

||| Convert to uppercase (ASCII only)
public export
toUpperAscii : String -> String
toUpperAscii = pack . map toUpper . unpack

||| Capitalize first character
public export
capitalize : String -> String
capitalize s = case strM s of
  StrNil => ""
  StrCons c rest => singleton (toUpper c) ++ rest

--------------------------------------------------------------------------------
-- Validation
--------------------------------------------------------------------------------

||| Check if string isInfixOf only ASCII characters
public export
isAscii : String -> Bool
isAscii s = all (\c => ord c < 128) (unpack s)

||| Check if string isInfixOf only alphanumeric characters
public export
isAlphaNum : String -> Bool
isAlphaNum s = all isAlphaNum' (unpack s)
  where
    isAlphaNum' : Char -> Bool
    isAlphaNum' c = isAlpha c || isDigit c

||| Check if string isInfixOf only digits
public export
isDigits : String -> Bool
isDigits s = isNotEmpty s && all isDigit (unpack s)

||| Check if string is a valid identifier (starts with letter/underscore, rest alphanumeric)
public export
isIdentifier : String -> Bool
isIdentifier s = case strM s of
  StrNil => False
  StrCons c rest =>
    (isAlpha c || c == '_') &&
    all (\x => isAlphaNum x || x == '_') (unpack rest)

--------------------------------------------------------------------------------
-- Safe Conversion
--------------------------------------------------------------------------------

||| Parse string to natural number
public export
parseNat : String -> Maybe Nat
parseNat s =
  if isEmpty s || not (isDigits s)
    then Nothing
    else Just (parseNat' (unpack s) 0)
  where
    parseNat' : List Char -> Nat -> Nat
    parseNat' [] acc = acc
    parseNat' (c :: cs) acc =
      let digit = cast (ord c - ord '0')
      in parseNat' cs (acc * 10 + digit)

||| Parse string to integer
public export
parseInt : String -> Maybe Integer
parseInt s = case strM s of
  StrNil => Nothing
  StrCons '-' rest =>
    map negate (parseNat rest >>= \n => Just (cast n))
  StrCons '+' rest =>
    parseNat rest >>= \n => Just (cast n)
  StrCons _ _ =>
    parseNat s >>= \n => Just (cast n)
