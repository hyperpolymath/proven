-- SPDX-License-Identifier: Apache-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
||| Safe JSON Parser
|||
||| A total JSON parser that returns Maybe/Result instead of throwing exceptions.
||| Handles malformed JSON gracefully.
module Proven.SafeJson.Parser

import Proven.Core
import Data.List
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

||| Skip whitespace
skipWhitespace : ParserState -> ParserState
skipWhitespace st = case st.input of
  [] => st
  (c :: cs) =>
    if c == ' ' || c == '\n' || c == '\r' || c == '\t'
      then skipWhitespace (MkParserState cs (S st.position) st.depth)
      else st

||| Peek at current character
peek : ParserState -> Maybe Char
peek st = case st.input of
  [] => Nothing
  (c :: _) => Just c

||| Consume one character
consume : ParserState -> Maybe (Char, ParserState)
consume st = case st.input of
  [] => Nothing
  (c :: cs) => Just (c, MkParserState cs (S st.position) st.depth)

||| Expect a specific character
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

||| Parse null
parseNull : ParserState -> ParseResult JsonValue
parseNull st = case st.input of
  ('n' :: 'u' :: 'l' :: 'l' :: rest) =>
    Right (JsonNull, MkParserState rest (st.position + 4) st.depth)
  _ => Left (UnexpectedChar st.position (maybe '?' id (peek st)))

||| Parse true
parseTrue : ParserState -> ParseResult JsonValue
parseTrue st = case st.input of
  ('t' :: 'r' :: 'u' :: 'e' :: rest) =>
    Right (JsonBool True, MkParserState rest (st.position + 4) st.depth)
  _ => Left (UnexpectedChar st.position (maybe '?' id (peek st)))

||| Parse false
parseFalse : ParserState -> ParseResult JsonValue
parseFalse st = case st.input of
  ('f' :: 'a' :: 'l' :: 's' :: 'e' :: rest) =>
    Right (JsonBool False, MkParserState rest (st.position + 5) st.depth)
  _ => Left (UnexpectedChar st.position (maybe '?' id (peek st)))

||| Check if character is a digit
isDigitChar : Char -> Bool
isDigitChar c = c >= '0' && c <= '9'

||| Parse digits
parseDigits : ParserState -> (List Char, ParserState)
parseDigits st = case st.input of
  [] => ([], st)
  (c :: cs) =>
    if isDigitChar c
      then let (rest, st') = parseDigits (MkParserState cs (S st.position) st.depth)
           in (c :: rest, st')
      else ([], st)

||| Parse a JSON number
parseNumber : ParserState -> ParseResult JsonValue
parseNumber st =
  let (sign, st1) = case st.input of
        ('-' :: cs) => (['-'], MkParserState cs (S st.position) st.depth)
        _ => ([], st)
      (intPart, st2) = parseDigits st1
      (fracPart, st3) = case st2.input of
        ('.' :: cs) =>
          let (digits, st') = parseDigits (MkParserState cs (S st2.position) st2.depth)
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
    parseExp : List Char -> Nat -> Nat -> (List Char, ParserState)
    parseExp cs pos depth =
      let (signPart, cs', pos') = case cs of
            ('+' :: rest) => (['+'], rest, S (S pos))
            ('-' :: rest) => (['-'], rest, S (S pos))
            _ => ([], cs, S pos)
          (digits, st') = parseDigits (MkParserState cs' pos' depth)
      in ('e' :: signPart ++ digits, st')

    parseDouble : String -> Maybe Double
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
    parseDouble s = Just (believe_me (cast {to=Double} s))  -- Runtime conversion

||| Parse escape sequence in string
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
    hexValue : Char -> Maybe Nat
    hexValue c =
      if c >= '0' && c <= '9' then Just (cast (ord c - ord '0'))
      else if c >= 'a' && c <= 'f' then Just (cast (ord c - ord 'a' + 10))
      else if c >= 'A' && c <= 'F' then Just (cast (ord c - ord 'A' + 10))
      else Nothing

    parseHex : List Char -> Maybe Nat
    parseHex [a, b, c, d] = do
      va <- hexValue a
      vb <- hexValue b
      vc <- hexValue c
      vd <- hexValue d
      Just (va * 4096 + vb * 256 + vc * 16 + vd)
    parseHex _ = Nothing

||| Parse string characters
parseStringChars : ParserState -> List Char -> ParseResult (List Char)
parseStringChars st acc = case st.input of
  [] => Left (UnexpectedEnd st.position)
  ('"' :: cs) => Right (reverse acc, MkParserState cs (S st.position) st.depth)
  ('\\' :: cs) =>
    case parseEscape (MkParserState cs (S st.position) st.depth) of
      Right (c, st') => parseStringChars st' (c :: acc)
      Left err => Left err
  (c :: cs) =>
    if ord c < 0x20
      then Left (UnexpectedChar st.position c)  -- Control characters not allowed
      else parseStringChars (MkParserState cs (S st.position) st.depth) (c :: acc)

||| Parse a JSON string
parseString : ParserState -> ParseResult JsonValue
parseString st = case st.input of
  ('"' :: cs) =>
    case parseStringChars (MkParserState cs (S st.position) st.depth) [] of
      Right (chars, st') => Right (JsonString (pack chars), st')
      Left err => Left err
  _ => Left (UnexpectedChar st.position (maybe '?' id (peek st)))

||| Parse raw string (for object keys)
parseRawString : ParserState -> ParseResult String
parseRawString st = case st.input of
  ('"' :: cs) =>
    case parseStringChars (MkParserState cs (S st.position) st.depth) [] of
      Right (chars, st') => Right (pack chars, st')
      Left err => Left err
  _ => Left (UnexpectedChar st.position (maybe '?' id (peek st)))

--------------------------------------------------------------------------------
-- Composite Parsers
--------------------------------------------------------------------------------

mutual
  ||| Parse a JSON value
  parseValue : ParserState -> ParseResult JsonValue
  parseValue st =
    if st.depth > maxDepth
      then Left (NestingTooDeep st.position)
      else let st' = skipWhitespace st
           in case peek st' of
                Nothing => Left (UnexpectedEnd st'.position)
                Just 'n' => parseNull st'
                Just 't' => parseTrue st'
                Just 'f' => parseFalse st'
                Just '"' => parseString st'
                Just '[' => parseArray st'
                Just '{' => parseObject st'
                Just c => if c == '-' || isDigitChar c
                            then parseNumber st'
                            else Left (UnexpectedChar st'.position c)

  ||| Parse array elements
  parseArrayElems : ParserState -> List JsonValue -> ParseResult (List JsonValue)
  parseArrayElems st acc =
    let st' = skipWhitespace st
    in case peek st' of
         Nothing => Left (UnexpectedEnd st'.position)
         Just ']' => Right (reverse acc, MkParserState (drop 1 st'.input) (S st'.position) st'.depth)
         Just _ =>
           case parseValue st' of
             Left err => Left err
             Right (val, st'') =>
               let st''' = skipWhitespace st''
               in case peek st''' of
                    Nothing => Left (UnexpectedEnd st'''.position)
                    Just ']' => Right (reverse (val :: acc), MkParserState (drop 1 st'''.input) (S st'''.position) st'''.depth)
                    Just ',' => parseArrayElems (MkParserState (drop 1 st'''.input) (S st'''.position) st'''.depth) (val :: acc)
                    Just _ => Left (ExpectedCommaOrEnd st'''.position)

  ||| Parse a JSON array
  parseArray : ParserState -> ParseResult JsonValue
  parseArray st = case st.input of
    ('[' :: cs) =>
      let st' = skipWhitespace (MkParserState cs (S st.position) (S st.depth))
      in case peek st' of
           Just ']' => Right (JsonArray [], MkParserState (drop 1 st'.input) (S st'.position) st'.depth)
           _ => case parseArrayElems st' [] of
                  Right (elems, st'') => Right (JsonArray elems, record { depth $= minus 1 } st'')
                  Left err => Left err
    _ => Left (UnexpectedChar st.position (maybe '?' id (peek st)))

  ||| Parse object members
  parseObjectMembers : ParserState -> List (String, JsonValue) -> ParseResult (List (String, JsonValue))
  parseObjectMembers st acc =
    let st' = skipWhitespace st
    in case peek st' of
         Nothing => Left (UnexpectedEnd st'.position)
         Just '}' => Right (reverse acc, MkParserState (drop 1 st'.input) (S st'.position) st'.depth)
         Just '"' =>
           case parseRawString st' of
             Left err => Left err
             Right (key, st'') =>
               let st''' = skipWhitespace st''
               in case expectChar ':' st''' of
                    Left err => Left err
                    Right ((), st4) =>
                      case parseValue st4 of
                        Left err => Left err
                        Right (val, st5) =>
                          let st6 = skipWhitespace st5
                          in case peek st6 of
                               Nothing => Left (UnexpectedEnd st6.position)
                               Just '}' => Right (reverse ((key, val) :: acc), MkParserState (drop 1 st6.input) (S st6.position) st6.depth)
                               Just ',' => parseObjectMembers (MkParserState (drop 1 st6.input) (S st6.position) st6.depth) ((key, val) :: acc)
                               Just _ => Left (ExpectedCommaOrEnd st6.position)
         Just c => Left (UnexpectedChar st'.position c)

  ||| Parse a JSON object
  parseObject : ParserState -> ParseResult JsonValue
  parseObject st = case st.input of
    ('{' :: cs) =>
      let st' = skipWhitespace (MkParserState cs (S st.position) (S st.depth))
      in case peek st' of
           Just '}' => Right (JsonObject [], MkParserState (drop 1 st'.input) (S st'.position) st'.depth)
           _ => case parseObjectMembers st' [] of
                  Right (members, st'') => Right (JsonObject members, record { depth $= minus 1 } st'')
                  Left err => Left err
    _ => Left (UnexpectedChar st.position (maybe '?' id (peek st)))

--------------------------------------------------------------------------------
-- Public API
--------------------------------------------------------------------------------

||| Parse a JSON string
||| Returns Either ParseError JsonValue
public export
parse : String -> Either ParseError JsonValue
parse s =
  let st = MkParserState (unpack s) 0 0
  in case parseValue st of
       Left err => Left err
       Right (val, st') =>
         let st'' = skipWhitespace st'
         in if null st''.input
              then Right val
              else Left (TrailingContent st''.position)

||| Parse JSON, returning Maybe instead of Either
public export
parseJson : String -> Maybe JsonValue
parseJson s = case parse s of
  Right val => Just val
  Left _ => Nothing

||| Parse JSON with default value on error
public export
parseJsonOr : JsonValue -> String -> JsonValue
parseJsonOr def s = case parse s of
  Right val => val
  Left _ => def

||| Check if string is valid JSON
public export
isValidJson : String -> Bool
isValidJson s = isJust (parseJson s)
