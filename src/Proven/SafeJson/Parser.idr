-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
||| Safe JSON Parser
|||
||| A total JSON parser that returns Maybe/Result instead of throwing exceptions.
||| Handles malformed JSON gracefully.
|||
||| Totality is ensured via fuel-based termination: every recursive function
||| receives a `Nat` fuel parameter that strictly decreases on each call.
||| The initial fuel is set to the length of the input string, which is an
||| upper bound on the number of recursive steps (each step consumes at least
||| one character or the fuel runs out).
module Proven.SafeJson.Parser

import Proven.Core
import Data.List
import Data.String
import Data.Maybe
import Data.String

%default total

--------------------------------------------------------------------------------
-- Parser Types
--------------------------------------------------------------------------------

||| JSON Value - redefined here to avoid circular imports
||| The main SafeJson module re-exports this
public export
data JsonValue : Type where
  JsonNull   : JsonValue
  JsonBool   : Bool -> JsonValue
  JsonNumber : Double -> JsonValue
  JsonString : String -> JsonValue
  JsonArray  : List JsonValue -> JsonValue
  JsonObject : List (String, JsonValue) -> JsonValue

public export covering
Show JsonValue where
  show JsonNull = "null"
  show (JsonBool True) = "true"
  show (JsonBool False) = "false"
  show (JsonNumber n) = show n
  show (JsonString s) = show s
  show (JsonArray xs) = "[" ++ joinBy ", " (map show xs) ++ "]"
  show (JsonObject kvs) = "{" ++ joinBy ", " (map showKV kvs) ++ "}"
    where
      showKV : (String, JsonValue) -> String
      showKV (k, v) = show k ++ ": " ++ show v

||| Parse error with location info
public export
data ParseError
  = UnexpectedChar Nat Char
  | UnexpectedEnd Nat
  | InvalidNumber Nat String
  | InvalidEscape Nat Char
  | InvalidUnicode Nat String
  | ExpectedColon Nat
  | ExpectedCommaOrEnd Nat
  | TrailingContent Nat
  | NestingTooDeep Nat
  | OutOfFuel Nat

public export
Show ParseError where
  show (UnexpectedChar pos c) = "Unexpected character '" ++ singleton c ++ "' at position " ++ show pos
  show (UnexpectedEnd pos) = "Unexpected end of input at position " ++ show pos
  show (InvalidNumber pos s) = "Invalid number '" ++ s ++ "' at position " ++ show pos
  show (InvalidEscape pos c) = "Invalid escape sequence '\\" ++ singleton c ++ "' at position " ++ show pos
  show (InvalidUnicode pos s) = "Invalid unicode escape '" ++ s ++ "' at position " ++ show pos
  show (ExpectedColon pos) = "Expected ':' at position " ++ show pos
  show (ExpectedCommaOrEnd pos) = "Expected ',' or closing bracket at position " ++ show pos
  show (TrailingContent pos) = "Trailing content after JSON value at position " ++ show pos
  show (NestingTooDeep pos) = "Nesting too deep at position " ++ show pos
  show (OutOfFuel pos) = "Parser fuel exhausted at position " ++ show pos

||| Parser state
record ParserState where
  constructor MkParserState
  input : List Char
  position : Nat
  depth : Nat

||| Parser result
ParseResult : Type -> Type
ParseResult a = Either ParseError (a, ParserState)

--------------------------------------------------------------------------------
-- Parser Combinators
--------------------------------------------------------------------------------

||| Maximum nesting depth to prevent stack overflow
maxDepth : Nat
maxDepth = 1000

||| Skip whitespace characters from the input.
||| Fuel decreases by one for each whitespace character consumed, ensuring totality.
skipWhitespace : (fuel : Nat) -> ParserState -> ParserState
skipWhitespace Z st = st
skipWhitespace (S k) st = case st.input of
  [] => st
  (c :: cs) =>
    if c == ' ' || c == '\n' || c == '\r' || c == '\t'
      then skipWhitespace k (MkParserState cs (S st.position) st.depth)
      else st

||| Peek at current character without consuming it
peek : ParserState -> Maybe Char
peek st = case st.input of
  [] => Nothing
  (c :: _) => Just c

||| Consume one character from the input
consume : ParserState -> Maybe (Char, ParserState)
consume st = case st.input of
  [] => Nothing
  (c :: cs) => Just (c, MkParserState cs (S st.position) st.depth)

||| Expect a specific character, consuming it on success
expectChar : Char -> ParserState -> ParseResult ()
expectChar expected st = case st.input of
  [] => Left (UnexpectedEnd st.position)
  (c :: cs) =>
    if c == expected
      then Right ((), MkParserState cs (S st.position) st.depth)
      else Left (UnexpectedChar st.position c)

--------------------------------------------------------------------------------
-- Parsing Primitives
--------------------------------------------------------------------------------

||| Parse the literal "null"
parseNull : ParserState -> ParseResult JsonValue
parseNull st = case st.input of
  ('n' :: 'u' :: 'l' :: 'l' :: rest) =>
    Right (JsonNull, MkParserState rest (st.position + 4) st.depth)
  _ => Left (UnexpectedChar st.position (maybe '?' id (peek st)))

||| Parse the literal "true"
parseTrue : ParserState -> ParseResult JsonValue
parseTrue st = case st.input of
  ('t' :: 'r' :: 'u' :: 'e' :: rest) =>
    Right (JsonBool True, MkParserState rest (st.position + 4) st.depth)
  _ => Left (UnexpectedChar st.position (maybe '?' id (peek st)))

||| Parse the literal "false"
parseFalse : ParserState -> ParseResult JsonValue
parseFalse st = case st.input of
  ('f' :: 'a' :: 'l' :: 's' :: 'e' :: rest) =>
    Right (JsonBool False, MkParserState rest (st.position + 5) st.depth)
  _ => Left (UnexpectedChar st.position (maybe '?' id (peek st)))

||| Check if a character is an ASCII digit
isDigitChar : Char -> Bool
isDigitChar c = c >= '0' && c <= '9'

||| Parse a run of digit characters.
||| Fuel decreases per character, ensuring totality.
parseDigits : (fuel : Nat) -> ParserState -> (List Char, ParserState)
parseDigits Z st = ([], st)
parseDigits (S k) st = case st.input of
  [] => ([], st)
  (c :: cs) =>
    if isDigitChar c
      then let (rest, st') = parseDigits k (MkParserState cs (S st.position) st.depth)
           in (c :: rest, st')
      else ([], st)

||| Parse a JSON number (integer, decimal, or scientific notation)
parseNumber : (fuel : Nat) -> ParserState -> ParseResult JsonValue
parseNumber fuel st =
  let (sign, st1) = case st.input of
        ('-' :: cs) => (the (List Char) ['-'], MkParserState cs (S st.position) st.depth)
        _ => (the (List Char) [], st)
      (intPart, st2) = parseDigits fuel st1
      (fracPart, st3) = case st2.input of
        ('.' :: cs) =>
          let (digits, st') = parseDigits fuel (MkParserState cs (S st2.position) st2.depth)
          in ('.' :: digits, st')
        _ => ([], st2)
      (expPart, st4) = case st3.input of
        ('e' :: cs) => parseExp cs st3.position st3.depth
        ('E' :: cs) => parseExp cs st3.position st3.depth
        _ => ([], st3)
      numStr = pack (sign ++ intPart ++ fracPart ++ expPart)
  in if null intPart
       then Left (InvalidNumber st.position numStr)
       else case parseDouble numStr of
              Just n => Right (JsonNumber n, st4)
              Nothing => Left (InvalidNumber st.position numStr)
  where
    ||| Parse the exponent part of a number (e.g., e+10, E-3)
    parseExp : List Char -> Nat -> Nat -> (List Char, ParserState)
    parseExp cs pos depth =
      let (signPart, cs', pos') = case cs of
            ('+' :: rest) => (the (List Char) ['+'], rest, S (S pos))
            ('-' :: rest) => (the (List Char) ['-'], rest, S (S pos))
            _ => (the (List Char) [], cs, S pos)
          (digits, st') = parseDigits fuel (MkParserState cs' pos' depth)
      in ('e' :: signPart ++ digits, st')

    ||| Attempt to parse a string as a Double
    parseDouble : String -> Maybe Double
    parseDouble s = Just (cast {to=Double} s)

||| Parse an escape sequence inside a JSON string (after the backslash)
parseEscape : ParserState -> ParseResult Char
parseEscape st = case st.input of
  ('"' :: cs) => Right ('"', MkParserState cs (S st.position) st.depth)
  ('\\' :: cs) => Right ('\\', MkParserState cs (S st.position) st.depth)
  ('/' :: cs) => Right ('/', MkParserState cs (S st.position) st.depth)
  ('b' :: cs) => Right ('\b', MkParserState cs (S st.position) st.depth)
  ('f' :: cs) => Right ('\f', MkParserState cs (S st.position) st.depth)
  ('n' :: cs) => Right ('\n', MkParserState cs (S st.position) st.depth)
  ('r' :: cs) => Right ('\r', MkParserState cs (S st.position) st.depth)
  ('t' :: cs) => Right ('\t', MkParserState cs (S st.position) st.depth)
  ('u' :: h1 :: h2 :: h3 :: h4 :: cs) =>
    case parseHex [h1, h2, h3, h4] of
      Just cp => Right (chr (cast cp), MkParserState cs (st.position + 5) st.depth)
      Nothing => Left (InvalidUnicode st.position (pack ['u', h1, h2, h3, h4]))
  (c :: _) => Left (InvalidEscape st.position c)
  [] => Left (UnexpectedEnd st.position)
  where
    ||| Convert a hex digit character to its numeric value
    hexValue : Char -> Maybe Nat
    hexValue c =
      if c >= '0' && c <= '9' then Just (cast (ord c - ord '0'))
      else if c >= 'a' && c <= 'f' then Just (cast (ord c - ord 'a' + 10))
      else if c >= 'A' && c <= 'F' then Just (cast (ord c - ord 'A' + 10))
      else Nothing

    ||| Parse exactly 4 hex digits into a codepoint value
    parseHex : List Char -> Maybe Nat
    parseHex [a, b, c, d] = do
      va <- hexValue a
      vb <- hexValue b
      vc <- hexValue c
      vd <- hexValue d
      Just (va * 4096 + vb * 256 + vc * 16 + vd)
    parseHex _ = Nothing

||| Parse characters inside a JSON string until the closing quote.
||| Fuel decreases per character consumed, ensuring totality.
parseStringChars : (fuel : Nat) -> ParserState -> List Char -> ParseResult (List Char)
parseStringChars Z st acc = Left (OutOfFuel st.position)
parseStringChars (S k) st acc = case st.input of
  [] => Left (UnexpectedEnd st.position)
  ('"' :: cs) => Right (reverse acc, MkParserState cs (S st.position) st.depth)
  ('\\' :: cs) =>
    case parseEscape (MkParserState cs (S st.position) st.depth) of
      Right (c, st') => parseStringChars k st' (c :: acc)
      Left err => Left err
  (c :: cs) =>
    if ord c < 0x20
      then Left (UnexpectedChar st.position c)  -- Control characters not allowed
      else parseStringChars k (MkParserState cs (S st.position) st.depth) (c :: acc)

||| Parse a JSON string value (including the surrounding quotes)
parseString : (fuel : Nat) -> ParserState -> ParseResult JsonValue
parseString fuel st = case st.input of
  ('"' :: cs) =>
    case parseStringChars fuel (MkParserState cs (S st.position) st.depth) [] of
      Right (chars, st') => Right (JsonString (pack chars), st')
      Left err => Left err
  _ => Left (UnexpectedChar st.position (maybe '?' id (peek st)))

||| Parse a raw string (for object keys), returning the string content
parseRawString : (fuel : Nat) -> ParserState -> ParseResult String
parseRawString fuel st = case st.input of
  ('"' :: cs) =>
    case parseStringChars fuel (MkParserState cs (S st.position) st.depth) [] of
      Right (chars, st') => Right (pack chars, st')
      Left err => Left err
  _ => Left (UnexpectedChar st.position (maybe '?' id (peek st)))

--------------------------------------------------------------------------------
-- Composite Parsers
--------------------------------------------------------------------------------

mutual
  ||| Parse a JSON value.
  ||| Fuel decreases on each recursive call to composite structures.
  parseValue : (fuel : Nat) -> ParserState -> ParseResult JsonValue
  parseValue Z st = Left (OutOfFuel st.position)
  parseValue (S k) st =
    if st.depth > maxDepth
      then Left (NestingTooDeep st.position)
      else let st' = skipWhitespace k st
           in case peek st' of
                Nothing => Left (UnexpectedEnd st'.position)
                Just 'n' => parseNull st'
                Just 't' => parseTrue st'
                Just 'f' => parseFalse st'
                Just '"' => parseString k st'
                Just '[' => parseArray k st'
                Just '{' => parseObject k st'
                Just c => if c == '-' || isDigitChar c
                            then parseNumber k st'
                            else Left (UnexpectedChar st'.position c)

  ||| Parse array elements (comma-separated JSON values inside []).
  ||| Fuel decreases on each element parsed, ensuring totality.
  parseArrayElems : (fuel : Nat) -> ParserState -> List JsonValue -> ParseResult (List JsonValue)
  parseArrayElems Z st acc = Left (OutOfFuel st.position)
  parseArrayElems (S k) st acc =
    let st' = skipWhitespace k st
    in case peek st' of
         Nothing => Left (UnexpectedEnd st'.position)
         Just ']' => Right (reverse acc, MkParserState (drop 1 st'.input) (S st'.position) st'.depth)
         Just _ =>
           case parseValue k st' of
             Left err => Left err
             Right (val, st'') =>
               let st''' = skipWhitespace k st''
               in case peek st''' of
                    Nothing => Left (UnexpectedEnd st'''.position)
                    Just ']' => Right (reverse (val :: acc), MkParserState (drop 1 st'''.input) (S st'''.position) st'''.depth)
                    Just ',' => parseArrayElems k (MkParserState (drop 1 st'''.input) (S st'''.position) st'''.depth) (val :: acc)
                    Just _ => Left (ExpectedCommaOrEnd st'''.position)

  ||| Parse a JSON array value (including the surrounding brackets)
  parseArray : (fuel : Nat) -> ParserState -> ParseResult JsonValue
  parseArray fuel st = case st.input of
    ('[' :: cs) =>
      let st' = skipWhitespace fuel (MkParserState cs (S st.position) (S st.depth))
      in case peek st' of
           Just ']' => Right (JsonArray [], MkParserState (drop 1 st'.input) (S st'.position) st'.depth)
           _ => case parseArrayElems fuel st' [] of
                  Right (elems, st'') => Right (JsonArray elems, { depth $= minus 1 } st'')
                  Left err => Left err
    _ => Left (UnexpectedChar st.position (maybe '?' id (peek st)))

  ||| Parse object members (comma-separated key:value pairs inside {}).
  ||| Fuel decreases on each member parsed, ensuring totality.
  parseObjectMembers : (fuel : Nat) -> ParserState -> List (String, JsonValue) -> ParseResult (List (String, JsonValue))
  parseObjectMembers Z st acc = Left (OutOfFuel st.position)
  parseObjectMembers (S k) st acc =
    let st' = skipWhitespace k st
    in case peek st' of
         Nothing => Left (UnexpectedEnd st'.position)
         Just '}' => Right (reverse acc, MkParserState (drop 1 st'.input) (S st'.position) st'.depth)
         Just '"' =>
           case parseRawString k st' of
             Left err => Left err
             Right (key, st'') =>
               let st''' = skipWhitespace k st''
               in case expectChar ':' st''' of
                    Left err => Left err
                    Right ((), st4) =>
                      case parseValue k st4 of
                        Left err => Left err
                        Right (val, st5) =>
                          let st6 = skipWhitespace k st5
                          in case peek st6 of
                               Nothing => Left (UnexpectedEnd st6.position)
                               Just '}' => Right (reverse ((key, val) :: acc), MkParserState (drop 1 st6.input) (S st6.position) st6.depth)
                               Just ',' => parseObjectMembers k (MkParserState (drop 1 st6.input) (S st6.position) st6.depth) ((key, val) :: acc)
                               Just _ => Left (ExpectedCommaOrEnd st6.position)
         Just c => Left (UnexpectedChar st'.position c)

  ||| Parse a JSON object value (including the surrounding braces)
  parseObject : (fuel : Nat) -> ParserState -> ParseResult JsonValue
  parseObject fuel st = case st.input of
    ('{' :: cs) =>
      let st' = skipWhitespace fuel (MkParserState cs (S st.position) (S st.depth))
      in case peek st' of
           Just '}' => Right (JsonObject [], MkParserState (drop 1 st'.input) (S st'.position) st'.depth)
           _ => case parseObjectMembers fuel st' [] of
                  Right (members, st'') => Right (JsonObject members, { depth $= minus 1 } st'')
                  Left err => Left err
    _ => Left (UnexpectedChar st.position (maybe '?' id (peek st)))

--------------------------------------------------------------------------------
-- Public API
--------------------------------------------------------------------------------

||| Parse a JSON string.
||| Returns Either ParseError JsonValue.
||| Fuel is derived from the input string length (upper bound on parse steps).
public export
parse : String -> Either ParseError JsonValue
parse s =
  let chars = unpack s
      fuel = length chars
      st = MkParserState chars 0 0
  in case parseValue fuel st of
       Left err => Left err
       Right (val, st') =>
         let st'' = skipWhitespace fuel st'
         in if null st''.input
              then Right val
              else Left (TrailingContent st''.position)

||| Parse JSON, returning Maybe instead of Either
public export
parseJson : String -> Maybe JsonValue
parseJson s = case parse s of
  Right val => Just val
  Left _ => Nothing

||| Parse JSON with a default value on error
public export
parseJsonOr : JsonValue -> String -> JsonValue
parseJsonOr def s = case parse s of
  Right val => val
  Left _ => def

||| Check if a string is valid JSON
public export
isValidJson : String -> Bool
isValidJson s = isJust (parseJson s)
