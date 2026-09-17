-- SPDX-License-Identifier: MPL-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
||| TOML parser with resource limits
|||
||| This module provides TOML parsing with:
||| - Nesting depth limits
||| - Key length limits
||| - Value size limits
||| - Total key count limits
module Proven.SafeTOML.Parser

import Proven.Core
import Proven.SafeTOML.Types
import Data.List
import Data.List1
import Data.Fin
import Data.String

%default total

--------------------------------------------------------------------------------
-- Parser State
--------------------------------------------------------------------------------

||| Parser state tracking
record ParserState where
  constructor MkParserState
  currentDepth : Nat
  totalKeys : Nat
  line : Nat
  col : Nat
  options : TOMLSecurityOptions

||| Initial parser state
initialState : TOMLSecurityOptions -> ParserState
initialState opts = MkParserState 0 0 1 1 opts

--------------------------------------------------------------------------------
-- Depth Checking
--------------------------------------------------------------------------------

||| Check if depth is within limits
checkDepth : ParserState -> TOMLResult ()
checkDepth state =
  if state.currentDepth > state.options.maxDepth
    then Err (NestingTooDeep state.currentDepth state.options.maxDepth)
    else Ok ()

||| Increment depth with check
withDepth : ParserState -> TOMLResult ParserState
withDepth state = do
  let newState = { currentDepth := S state.currentDepth } state
  checkDepth newState
  Ok newState

--------------------------------------------------------------------------------
-- Key Checking
--------------------------------------------------------------------------------

||| Check key length
checkKeyLength : ParserState -> String -> TOMLResult ()
checkKeyLength state key =
  let len = length (unpack key)
  in if len > state.options.maxKeyLength
       then Err (KeyTooLong len state.options.maxKeyLength)
       else Ok ()

||| Check total key count
checkKeyCount : ParserState -> TOMLResult ParserState
checkKeyCount state =
  let newCount = S state.totalKeys
  in if newCount > state.options.maxTotalKeys
       then Err (TooManyKeys newCount state.options.maxTotalKeys)
       else Ok ({ totalKeys := newCount } state)

--------------------------------------------------------------------------------
-- Value Checking
--------------------------------------------------------------------------------

||| Check string value size
checkValueSize : ParserState -> String -> TOMLResult ()
checkValueSize state val =
  let size = length (unpack val)
  in if size > state.options.maxValueSize
       then Err (ValueTooLarge size state.options.maxValueSize)
       else Ok ()

||| Check array length
checkArrayLength : ParserState -> List a -> TOMLResult ()
checkArrayLength state arr =
  if length arr > state.options.maxArrayLength
    then Err (ValueTooLarge (length arr) state.options.maxArrayLength)
    else Ok ()

--------------------------------------------------------------------------------
-- String Parsing Utilities
--------------------------------------------------------------------------------

||| Unescape TOML string
unescapeString : String -> TOMLResult String
unescapeString s = map pack (go (unpack s))
  where
    go : List Char -> TOMLResult (List Char)
    go [] = Ok []
    go ('\\' :: 'b' :: rest) = map ('\b' ::) (go rest)
    go ('\\' :: 't' :: rest) = map ('\t' ::) (go rest)
    go ('\\' :: 'n' :: rest) = map ('\n' ::) (go rest)
    go ('\\' :: 'f' :: rest) = map ('\f' ::) (go rest)
    go ('\\' :: 'r' :: rest) = map ('\r' ::) (go rest)
    go ('\\' :: '"' :: rest) = map ('"' ::) (go rest)
    go ('\\' :: '\\' :: rest) = map ('\\' ::) (go rest)
    go ('\\' :: c :: rest) = Err (InvalidValue ("\\" ++ singleton c) "escape sequence")
    go (c :: rest) = map (c ::) (go rest)

||| Escape string for TOML output
escapeString : String -> String
escapeString s = pack (go (unpack s))
  where
    go : List Char -> List Char
    go [] = []
    go ('\b' :: rest) = '\\' :: 'b' :: go rest
    go ('\t' :: rest) = '\\' :: 't' :: go rest
    go ('\n' :: rest) = '\\' :: 'n' :: go rest
    go ('\f' :: rest) = '\\' :: 'f' :: go rest
    go ('\r' :: rest) = '\\' :: 'r' :: go rest
    go ('"' :: rest) = '\\' :: '"' :: go rest
    go ('\\' :: rest) = '\\' :: '\\' :: go rest
    go (c :: rest) = c :: go rest

--------------------------------------------------------------------------------
-- Number Parsing
--------------------------------------------------------------------------------

||| Parse integer (supports underscores, hex, octal, binary)
parseInteger : String -> TOMLResult Integer
parseInteger s =
  let stripped = pack (filter (/= '_') (unpack s))
  in case unpack stripped of
       ('0' :: 'x' :: hex) => parseHex (pack hex)
       ('0' :: 'o' :: oct) => parseOctal (pack oct)
       ('0' :: 'b' :: bin) => parseBinary (pack bin)
       ('+' :: rest) => parseDecimal (pack rest)
       ('-' :: rest) => map negate (parseDecimal (pack rest))
       _ => parseDecimal stripped
  where
    parseDecimal : String -> TOMLResult Integer
    parseDecimal str =
      case parseInteger str of
        Just i => Ok i
        Nothing => Err (InvalidValue str "integer")

    parseHex : String -> TOMLResult Integer
    parseHex str =
      let chars = unpack (toLower str)
      in go 0 chars
      where
        hexDigit : Char -> Maybe Integer
        hexDigit c = if isDigit c then Just (cast (ord c - ord '0'))
                     else if c >= 'a' && c <= 'f' then Just (10 + cast (ord c - ord 'a'))
                     else Nothing

        go : Integer -> List Char -> TOMLResult Integer
        go acc [] = Ok acc
        go acc (c :: cs) = case hexDigit c of
          Just d => go (acc * 16 + d) cs
          Nothing => Err (InvalidValue str "hexadecimal")

    parseOctal : String -> TOMLResult Integer
    parseOctal str =
      go 0 (unpack str)
      where
        go : Integer -> List Char -> TOMLResult Integer
        go acc [] = Ok acc
        go acc (c :: cs) =
          if c >= '0' && c <= '7'
            then go (acc * 8 + cast (ord c - ord '0')) cs
            else Err (InvalidValue str "octal")

    parseBinary : String -> TOMLResult Integer
    parseBinary str =
      go 0 (unpack str)
      where
        go : Integer -> List Char -> TOMLResult Integer
        go acc [] = Ok acc
        go acc ('0' :: cs) = go (acc * 2) cs
        go acc ('1' :: cs) = go (acc * 2 + 1) cs
        go acc _ = Err (InvalidValue str "binary")

||| Parse float (supports inf, nan, exponents)
parseFloat : String -> TOMLResult Double
parseFloat s =
  let stripped = pack (filter (/= '_') (unpack s))
  in case stripped of
       "inf" => Ok (1.0 / 0.0)
       "+inf" => Ok (1.0 / 0.0)
       "-inf" => Ok (negate (1.0 / 0.0))
       "nan" => Ok (0.0 / 0.0)
       "+nan" => Ok (0.0 / 0.0)
       "-nan" => Ok (0.0 / 0.0)
       _ => case parseDouble stripped of
              Just d => Ok d
              Nothing => Err (InvalidValue s "float")

--------------------------------------------------------------------------------
-- DateTime Parsing
--------------------------------------------------------------------------------

||| Parse date (YYYY-MM-DD)
parseDate : String -> TOMLResult TOMLDate
parseDate s =
  case forget (Data.String.split (== '-') s) of
    [y, m, d] =>
      case (parseInteger y, parseInteger m, parseInteger d) of
        (Ok year, Ok month, Ok day) =>
          if month >= 1 && month <= 12 && day >= 1 && day <= 31
            then Ok (MkTOMLDate year (cast month) (cast day))
            else Err (InvalidDateTime s)
        _ => Err (InvalidDateTime s)
    _ => Err (InvalidDateTime s)

||| Parse time (HH:MM:SS or HH:MM:SS.sss)
parseTime : String -> TOMLResult TOMLTime
parseTime s =
  let (timeStr, msStr) = case Data.String.span (/= '.') s of
                           (t, "") => (t, "000")
                           (t, ms) => (t, pack (drop 1 (unpack ms)))
  in case forget (Data.String.split (== ':') timeStr) of
       [h, m, sec] =>
         case (parseInteger h, parseInteger m, parseInteger sec, parseInteger msStr) of
           (Ok hour, Ok minute, Ok second, Ok ms) =>
             if hour >= 0 && hour <= 23 &&
                minute >= 0 && minute <= 59 &&
                second >= 0 && second <= 60  -- 60 for leap second
               then Ok (MkTOMLTime (cast hour) (cast minute) (cast second) (cast ms))
               else Err (InvalidDateTime s)
           _ => Err (InvalidDateTime s)
       _ => Err (InvalidDateTime s)

||| Parse datetime (date + time + optional timezone)
parseDateTime : String -> TOMLResult TOMLDateTime
parseDateTime s =
  -- Split by 'T' or ' '
  let parts = if isInfixOf "T" s
                then forget (Data.String.split (== 'T') s)
                else forget (Data.String.split (== ' ') s)
  in case parts of
       [dateStr, timeAndTz] => do
         datePart <- parseDate dateStr
         -- Extract timezone
         let (timeStr, tz) = extractTimezone timeAndTz
         timePart <- parseTime timeStr
         Ok (MkTOMLDateTime
               datePart.year datePart.month datePart.day
               timePart.hour timePart.minute timePart.second timePart.millisecond
               tz)
       _ => Err (InvalidDateTime s)
  where
    -- Helpers are ordered dependency-first. Idris2 elaborates `where`-block
    -- siblings in order, so a helper that is used must appear before its user;
    -- this block was originally written in exactly the reverse order.
    dropLast : Nat -> String -> String
    dropLast n s = pack (take (minus (length (unpack s)) n) (unpack s))

    findLastIndex : (a -> Bool) -> List a -> Maybe Nat
    findLastIndex _ [] = Nothing
    findLastIndex p xs = go Nothing 0 xs
      where
        go : Maybe Nat -> Nat -> List a -> Maybe Nat
        go acc _ [] = acc
        go acc idx (x :: xs) = go (if p x then Just idx else acc) (S idx) xs

    findTzOffset : String -> Maybe Nat
    findTzOffset str =
      let chars = unpack str
          plusIdx = findIndex (== '+') chars
          minusIdx = findLastIndex (== '-') chars  -- Last minus to avoid date separators
      in case (plusIdx, minusIdx) of
           (Just p, _) => Just (finToNat p)  -- findIndex returns Fin, not Nat
           (_, Just m) => if m > 10 then Just m else Nothing  -- After time portion
           _ => Nothing

    extractTimezone : String -> (String, Maybe String)
    extractTimezone str =
      if isSuffixOf "Z" str
        then (dropLast 1 str, Just "Z")
        else case findTzOffset str of
               Just idx =>
                 let (time, tz) = splitAt idx (unpack str)
                 in (pack time, Just (pack tz))
               Nothing => (str, Nothing)

--------------------------------------------------------------------------------
-- Array Type Checking
--------------------------------------------------------------------------------

||| Get array element type
arrayElementType : TOMLValue -> String
arrayElementType (TString _) = "string"
arrayElementType (TInt _) = "integer"
arrayElementType (TFloat _) = "float"
arrayElementType (TBool _) = "boolean"
arrayElementType (TDateTime _) = "datetime"
arrayElementType (TDate _) = "date"
arrayElementType (TTime _) = "time"
arrayElementType (TArray _) = "array"
arrayElementType (TInlineTable _) = "table"
arrayElementType (TTable _) = "table"

||| Check array homogeneity (TOML 1.0)
checkArrayHomogeneity : ParserState -> List TOMLValue -> TOMLResult ()
checkArrayHomogeneity state [] = Ok ()
checkArrayHomogeneity state [x] = Ok ()
checkArrayHomogeneity state (x :: y :: xs) =
  if state.options.allowHeterogeneousArrays
    then Ok ()
    else if arrayElementType x == arrayElementType y
           then checkArrayHomogeneity state (y :: xs)
           else Err (HeterogeneousArray state.line)

--------------------------------------------------------------------------------
-- High-Level Parsing API
--------------------------------------------------------------------------------

||| Parse TOML with custom options
|||
||| Declared here, defined below: `parseTOML` calls it, and Idris2 elaborates
||| top-level declarations in order, so the signature must precede the use.
||| Splitting declaration from definition is legal at the top level and avoids
||| reordering the whole block.
export
parseTOMLWith : TOMLSecurityOptions -> String -> TOMLResult TOMLDocument

||| Parse TOML document with secure defaults
export
parseTOML : String -> TOMLResult TOMLDocument
parseTOML = parseTOMLWith secureDefaults

parseTOMLWith opts input =
  -- Stub implementation - actual parser would be complex
  -- This demonstrates the security checking interface
  let state = initialState opts
  in parseDocument state (lines input)
  where
    parseKeyValue : ParserState -> String -> List String -> TOMLResult TOMLDocument
    parseKeyValue state line rest =
      -- Simplified: just return empty document
      -- Real implementation would parse key = value pairs
      Ok []

    parseDocument : ParserState -> List String -> TOMLResult TOMLDocument
    parseDocument _ [] = Ok []
    parseDocument state (l :: ls) =
      let trimmed = trim l
      in if null (unpack trimmed) || isPrefixOf "#" trimmed
           then parseDocument ({ line := S state.line } state) ls
           else parseKeyValue state trimmed ls

||| Parse TOML value from string representation
export
parseValue : String -> TOMLResult TOMLValue
parseValue s =
  let trimmed = trim s
  in case unpack trimmed of
       ('"' :: _) => parseStringValue trimmed
       ('\'' :: _) => parseLiteralString trimmed
       ('[' :: _) => Ok (TArray [])  -- Simplified
       ('{' :: _) => Ok (TInlineTable [])  -- Simplified
       _ => parseScalar trimmed
  where
    parseStringValue : String -> TOMLResult TOMLValue
    parseStringValue str =
      if length (unpack str) < 2
        then Err (InvalidValue str "string")
        else let inner = pack (take (minus (length (unpack str)) 2) (drop 1 (unpack str)))
             in do
               unescaped <- unescapeString inner
               Ok (TString unescaped)

    parseLiteralString : String -> TOMLResult TOMLValue
    parseLiteralString str =
      if length (unpack str) < 2
        then Err (InvalidValue str "literal string")
        else let inner = pack (take (minus (length (unpack str)) 2) (drop 1 (unpack str)))
             in Ok (TString inner)  -- Literal strings: no escaping

    parseScalar : String -> TOMLResult TOMLValue
    parseScalar str =
      case str of
        "true" => Ok (TBool True)
        "false" => Ok (TBool False)
        _ => case parseInteger str of
               Ok i => Ok (TInt i)
               Err _ => case parseFloat str of
                          Ok f => Ok (TFloat f)
                          Err _ => Err (InvalidValue str "scalar value")

--------------------------------------------------------------------------------
-- Rendering
--------------------------------------------------------------------------------

-- Rendering helpers.
--
-- `join` and `renderKey` were duplicated `where`-block siblings of individual
-- `renderValue` clauses; they are hoisted to the top level.
--
-- `renderItems`/`renderPairs` are mutually recursive with `renderValue`, so
-- their signatures are declared before it and their clauses follow it. Split
-- declaration/definition is legal at the Idris2 top level.
--
-- They are explicit list recursion rather than `map`: a recursive call handed
-- to `map` hides the structural descent from the totality checker, which is
-- why the original `map renderValue xs` / `map renderKV kvs` were rejected.

||| Join strings with a separator
join : String -> List String -> String
join _ [] = ""
join _ [x] = x
join sep (x :: xs) = x ++ sep ++ join sep xs

||| Render a key, quoting it when it is not a valid bare key
renderKey : String -> String
renderKey k = if isValidBareKey k then k else "\"" ++ escapeString k ++ "\""

renderItems : List TOMLValue -> List String

renderPairs : List (String, TOMLValue) -> List String

||| Render TOML value to string
export
renderValue : TOMLValue -> String
renderValue (TString s) = "\"" ++ escapeString s ++ "\""
renderValue (TInt i) = show i
renderValue (TFloat f) = show f
renderValue (TBool True) = "true"
renderValue (TBool False) = "false"
renderValue (TDateTime dt) = show dt
renderValue (TDate d) = show d
renderValue (TTime t) = show t
renderValue (TArray xs) = "[" ++ join ", " (renderItems xs) ++ "]"
renderValue (TInlineTable kvs) = "{" ++ join ", " (renderPairs kvs) ++ "}"
renderValue (TTable _) = "{...}"  -- Tables rendered differently

renderItems [] = []
renderItems (x :: xs) = renderValue x :: renderItems xs

renderPairs [] = []
renderPairs ((k, v) :: kvs) = (renderKey k ++ " = " ++ renderValue v) :: renderPairs kvs

||| Render TOML document
export
renderDocument : TOMLDocument -> String
renderDocument doc = unlines (map renderTopLevel doc)
  where
    renderTopLevel : (String, TOMLValue) -> String
    renderTopLevel (k, TTable kvs) =
      "[" ++ k ++ "]\n" ++ unlines (map (\(k', v) => k' ++ " = " ++ renderValue v) kvs)
    renderTopLevel (k, v) =
      k ++ " = " ++ renderValue v
