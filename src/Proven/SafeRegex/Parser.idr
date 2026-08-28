-- SPDX-License-Identifier: Palimpsest-MPL-1.0
||| SafeRegex.Parser - Parse regex patterns from strings
|||
||| This module provides a safe regex parser that:
||| - Parses standard regex syntax (PCRE-like)
||| - Validates patterns during parsing
||| - Rejects patterns that would cause ReDoS
||| - Returns structured errors on invalid input
module Proven.SafeRegex.Parser

import Proven.Core
import Proven.SafeRegex.Types
import Proven.SafeRegex.Safety
import Data.List
import Data.String
import Data.Maybe

%default total

--------------------------------------------------------------------------------
-- Parser State
--------------------------------------------------------------------------------

||| Parser state
record ParserState where
  constructor MkParserState
  ||| Remaining input
  input : List Char
  ||| Current position (for error reporting)
  pos : Nat
  ||| Number of open groups
  openGroups : Nat
  ||| Next group ID to assign
  nextGroupId : Nat
  ||| Parsing flags
  flags : RegexFlags

||| Initial parser state
initState : String -> RegexFlags -> ParserState
initState s flags = MkParserState (unpack s) 0 0 1 flags

||| Parser result
Parser : Type -> Type
Parser a = ParserState -> Either RegexError (a, ParserState)

--------------------------------------------------------------------------------
-- Parser Combinators
--------------------------------------------------------------------------------

||| Run a parser
runParser : Parser a -> ParserState -> Either RegexError (a, ParserState)
runParser p st = p st

||| Pure value
pure : a -> Parser a
pure x st = Right (x, st)

||| Map over parser result
map : (a -> b) -> Parser a -> Parser b
map f p st = case p st of
  Left err => Left err
  Right (x, st') => Right (f x, st')

||| Sequence parsers
bind : Parser a -> (a -> Parser b) -> Parser b
bind p f st = case p st of
  Left err => Left err
  Right (x, st') => f x st'

||| Fail with error
fail : RegexError -> Parser a
fail err _ = Left err

||| Try a parser, return Nothing on failure
optional : Parser a -> Parser (Maybe a)
optional p st = case p st of
  Left _ => Right (Nothing, st)
  Right (x, st') => Right (Just x, st')

||| Parse zero or more
many : Parser a -> Parser (List a)
many p st = go (length st.input) [] st
  where
    -- The budget is the number of input characters remaining. Every accepted
    -- item must consume at least one character, so the budget is a strict
    -- over-approximation of the iteration count and can never cut a
    -- well-behaved parse short. A parser that succeeds WITHOUT consuming
    -- input terminates the loop instead of spinning, which is the only
    -- behavioural difference from the previous non-total definition -- and in
    -- that case the previous definition did not terminate at all.
    go : Nat -> List a -> Parser (List a)
    go Z acc st' = Right (reverse acc, st')
    go (S k) acc st' = case p st' of
      Left _ => Right (reverse acc, st')
      Right (x, st'') =>
        if length st''.input < length st'.input
          then go k (x :: acc) st''
          else Right (reverse (x :: acc), st'')

||| Parse one or more
some : Parser a -> Parser (List a)
some p st = case p st of
  Left err => Left err
  Right (x, st') => case many p st' of
    Left err => Left err
    Right (xs, st'') => Right (x :: xs, st'')

||| Alternative
alt : Parser a -> Parser a -> Parser a
alt p1 p2 st = case p1 st of
  Right res => Right res
  Left _ => p2 st

--------------------------------------------------------------------------------
-- Character Parsers
--------------------------------------------------------------------------------

||| Peek at current character
peek : Parser (Maybe Char)
peek st = case st.input of
  [] => Right (Nothing, st)
  (c :: _) => Right (Just c, st)

||| Consume any character
anyChar : Parser Char
anyChar st = case st.input of
  [] => Left $ ParseError st.pos "Unexpected end of input"
  (c :: rest) => Right (c, { input := rest, pos := S st.pos } st)

||| Consume specific character
char : Char -> Parser Char
char expected st = case st.input of
  [] => Left $ ParseError st.pos ("Expected '" ++ singleton expected ++ "'")
  (c :: rest) =>
    if c == expected
      then Right (c, { input := rest, pos := S st.pos } st)
      else Left $ ParseError st.pos ("Expected '" ++ singleton expected ++ "', got '" ++ singleton c ++ "'")

||| Consume character if predicate holds
satisfy : (Char -> Bool) -> Parser Char
satisfy pred st = case st.input of
  [] => Left $ ParseError st.pos "Unexpected end of input"
  (c :: rest) =>
    if pred c
      then Right (c, { input := rest, pos := S st.pos } st)
      else Left $ ParseError st.pos ("Unexpected character '" ++ singleton c ++ "'")

||| Check if at end of input
atEnd : Parser Bool
atEnd st = Right (isNil st.input, st)

||| Parse a digit
digit : Parser Char
digit = satisfy isDigit

||| Parse a natural number
natural : Parser Nat
natural = map stringToNat (map pack (some digit))
  where
    stringToNat : String -> Nat
    stringToNat s = cast (cast {to=Integer} s)

--------------------------------------------------------------------------------
-- Escape Sequence Parsing
--------------------------------------------------------------------------------

||| Parse escape sequence
parseEscape : Parser CharClass
parseEscape st = case st.input of
  [] => Left $ ParseError st.pos "Unexpected end after backslash"
  (c :: rest) =>
    let st' : ParserState = { input := rest, pos := S st.pos } st
    in case c of
         'd' => Right (Digit, st')
         'D' => Right (Negate Digit, st')
         'w' => Right (Word, st')
         'W' => Right (Negate Word, st')
         's' => Right (Space, st')
         'S' => Right (Negate Space, st')
         'n' => Right (SingleChar '\n', st')
         'r' => Right (SingleChar '\r', st')
         't' => Right (SingleChar '\t', st')
         'f' => Right (SingleChar '\x0C', st')  -- Form feed
         'v' => Right (SingleChar '\x0B', st')  -- Vertical tab
         '0' => Right (SingleChar '\0', st')
         '\\' => Right (SingleChar '\\', st')
         '.' => Right (SingleChar '.', st')
         '*' => Right (SingleChar '*', st')
         '+' => Right (SingleChar '+', st')
         '?' => Right (SingleChar '?', st')
         '^' => Right (SingleChar '^', st')
         '$' => Right (SingleChar '$', st')
         '|' => Right (SingleChar '|', st')
         '[' => Right (SingleChar '[', st')
         ']' => Right (SingleChar ']', st')
         '(' => Right (SingleChar '(', st')
         ')' => Right (SingleChar ')', st')
         '{' => Right (SingleChar '{', st')
         '}' => Right (SingleChar '}', st')
         _ => Left $ InvalidEscape st.pos c

--------------------------------------------------------------------------------
-- Character Class Parsing
--------------------------------------------------------------------------------

||| Parse character class item (inside [...])
parseClassItem : Parser CharClass
parseClassItem st = case st.input of
  [] => Left $ UnclosedCharClass st.pos
  ('\\' :: rest) => parseEscape ({ input := rest, pos := S st.pos } st)
  (']' :: _) => Left $ ParseError st.pos "Empty character class"
  (c :: '-' :: ']' :: rest) =>
    -- Trailing dash: treat as literal
    Right (Union (SingleChar c) (SingleChar '-'), { input := '-' :: ']' :: rest, pos := S st.pos } st)
  (c1 :: '-' :: c2 :: rest) =>
    if c2 == ']'
      then Right (SingleChar c1, { input := '-' :: ']' :: rest, pos := S st.pos } st)
      else Right (Range c1 c2, { input := rest, pos := st.pos + 3 } st)
  (c :: rest) =>
    Right (SingleChar c, { input := rest, pos := S st.pos } st)

||| Parse character class contents
parseClassContents : Parser CharClass
parseClassContents st = go (length st.input) Nothing st
  where
    -- Budget is the remaining input length; each iteration consumes at least
    -- one character via parseClassItem, so exhaustion is unreachable for
    -- well-formed input. Exhaustion is reported as an unclosed class, which
    -- is what running off the end of the input means here anyway.
    go : Nat -> Maybe CharClass -> Parser CharClass
    go Z _ st' = Left $ UnclosedCharClass st'.pos
    go (S k) acc st' = case st'.input of
      [] => Left $ UnclosedCharClass st'.pos
      (']' :: rest) =>
        case acc of
          Nothing => Left $ ParseError st'.pos "Empty character class"
          Just cls => Right (cls, { input := rest, pos := S st'.pos } st')
      _ => case parseClassItem st' of
             Left err => Left err
             Right (item, st'') =>
               go k (Just $ maybe item (\a => Union a item) acc) st''

||| Parse a character class [...] or [^...]
parseCharClass : Parser CharClass
parseCharClass st =
  case bind (char '[') (\_ => peek) st of
    Left err => Left err
    Right (mc, st') =>
      case mc of
        Just '^' => case bind anyChar (\_ => parseClassContents) st' of
                      Left err => Left err
                      Right (cls, st'') => Right (Negate cls, st'')
        _ => parseClassContents st'

--------------------------------------------------------------------------------
-- Quantifier Parsing
--------------------------------------------------------------------------------

||| Parse quantifier suffix
parseQuantifier : Parser (Maybe Quantifier)
parseQuantifier st = case st.input of
  ('*' :: '?' :: rest) =>
    Right (Just (lazy zeroOrMore), { input := rest, pos := st.pos + 2 } st)
  ('*' :: rest) =>
    Right (Just zeroOrMore, { input := rest, pos := S st.pos } st)
  ('+' :: '?' :: rest) =>
    Right (Just (lazy oneOrMore), { input := rest, pos := st.pos + 2 } st)
  ('+' :: rest) =>
    Right (Just oneOrMore, { input := rest, pos := S st.pos } st)
  ('?' :: '?' :: rest) =>
    Right (Just (lazy zeroOrOne), { input := rest, pos := st.pos + 2 } st)
  ('?' :: rest) =>
    Right (Just zeroOrOne, { input := rest, pos := S st.pos } st)
  ('{' :: rest) =>
    parseBraceQuantifier ({ input := rest, pos := S st.pos } st)
  _ => Right (Nothing, st)
  where
    parseBraceQuantifier : Parser (Maybe Quantifier)
    parseBraceQuantifier st = case natural st of
      Left _ => Left $ InvalidQuantifier st.pos "Expected number in quantifier"
      Right (n, st') => case st'.input of
        ('}' :: rest) =>
          Right (Just (exactly n), { input := rest, pos := S st'.pos } st')
        (',' :: '}' :: rest) =>
          Right (Just (atLeast n), { input := rest, pos := st'.pos + 2 } st')
        (',' :: rest) =>
          case natural ({ input := rest, pos := S st'.pos } st') of
            Left _ => Left $ InvalidQuantifier st.pos "Expected number after comma"
            Right (m, st'') => case st''.input of
              ('}' :: rest') =>
                if m < n
                  then Left $ InvalidQuantifier st.pos "Max less than min"
                  else Right (Just (between n m), { input := rest', pos := S st''.pos } st'')
              _ => Left $ InvalidQuantifier st.pos "Expected '}'"
        _ => Left $ InvalidQuantifier st.pos "Invalid quantifier syntax"

--------------------------------------------------------------------------------
-- Main Regex Parser
--------------------------------------------------------------------------------

mutual
  ||| Parse a single regex atom (character, group, class, etc.)
  |||
  ||| The leading `Nat` is a structural recursion budget. This group recurses
  ||| through a `ParserState` record, which Idris2 cannot measure, so the
  ||| budget supplies the decreasing argument instead. `parseRegex` seeds it
  ||| with `16 * length (unpack pattern) + 16`: one cycle through the group is
  ||| at most eight hops and consumes at least one input character, so the seed
  ||| is a strict over-approximation and no valid pattern can exhaust it.
  ||| Exhaustion is therefore unreachable for well-formed input, and is
  ||| reported as an explicit complexity error rather than silently truncating
  ||| the parse.
  parseAtom : Nat -> Parser Regex
  parseAtom Z st = Left $ ParseError st.pos "Regex complexity budget exhausted"
  parseAtom (S k) st = case st.input of
    [] => Right (Empty, st)
    ('(' :: rest) => parseGroup k ({ input := rest, pos := S st.pos } st)
    ('[' :: _) => map Match parseCharClass st
    ('.' :: rest) => Right (Match Any, { input := rest, pos := S st.pos } st)
    ('^' :: rest) => Right (StartAnchor, { input := rest, pos := S st.pos } st)
    ('$' :: rest) => Right (EndAnchor, { input := rest, pos := S st.pos } st)
    ('\\' :: 'b' :: rest) => Right (WordBoundary, { input := rest, pos := st.pos + 2 } st)
    ('\\' :: c :: rest) =>
      if isDigit c && c /= '0'
        then Right (BackRef (cast (ord c - ord '0')), { input := rest, pos := st.pos + 2 } st)
        else case parseEscape ({ input := c :: rest, pos := S st.pos } st) of
               Left err => Left err
               Right (cls, st') => Right (Match cls, st')
    (c :: rest) =>
      if c `elem` [')', '|', '*', '+', '?', '{', '}']
        then Right (Empty, st)  -- Let caller handle these
        else Right (Match (SingleChar c), { input := rest, pos := S st.pos } st)

  ||| Parse a group (...) or (?:...) etc.
  parseGroup : Nat -> Parser Regex
  parseGroup Z st = Left $ ParseError st.pos "Regex complexity budget exhausted"
  parseGroup (S k) st = case st.input of
    ('?' :: ':' :: rest) =>
      -- Non-capturing group
      case parseAlternation k ({ input := rest, pos := st.pos + 2, openGroups := S st.openGroups } st) of
        Left err => Left err
        Right (r, st') => case st'.input of
          (')' :: rest') => Right (NCGroup r, { input := rest', pos := S st'.pos, openGroups := pred st'.openGroups } st')
          _ => Left $ UnclosedGroup st.pos
    ('?' :: '=' :: rest) =>
      -- Positive lookahead
      case parseAlternation k ({ input := rest, pos := st.pos + 2, openGroups := S st.openGroups } st) of
        Left err => Left err
        Right (r, st') => case st'.input of
          (')' :: rest') => Right (Lookahead True r, { input := rest', pos := S st'.pos, openGroups := pred st'.openGroups } st')
          _ => Left $ UnclosedGroup st.pos
    ('?' :: '!' :: rest) =>
      -- Negative lookahead
      case parseAlternation k ({ input := rest, pos := st.pos + 2, openGroups := S st.openGroups } st) of
        Left err => Left err
        Right (r, st') => case st'.input of
          (')' :: rest') => Right (Lookahead False r, { input := rest', pos := S st'.pos, openGroups := pred st'.openGroups } st')
          _ => Left $ UnclosedGroup st.pos
    _ =>
      -- Capturing group
      let gid = st.nextGroupId
          st' = { openGroups := S st.openGroups, nextGroupId := S st.nextGroupId } st
      in case parseAlternation k st' of
           Left err => Left err
           Right (r, st'') => case st''.input of
             (')' :: rest) => Right (Group gid r, { input := rest, pos := S st''.pos, openGroups := pred st''.openGroups } st'')
             _ => Left $ UnclosedGroup st.pos

  ||| Parse an atom with optional quantifier
  parseQuantified : Nat -> Parser Regex
  parseQuantified Z st = Left $ ParseError st.pos "Regex complexity budget exhausted"
  parseQuantified (S k) st = case parseAtom k st of
    Left err => Left err
    Right (Empty, st') => Right (Empty, st')
    Right (r, st') => case parseQuantifier st' of
      Left err => Left err
      Right (Nothing, st'') => Right (r, st'')
      Right (Just q, st'') => Right (Quant r q, st'')

  ||| Parse a sequence of quantified atoms
  parseSequence : Nat -> Parser Regex
  parseSequence Z st = Left $ ParseError st.pos "Regex complexity budget exhausted"
  parseSequence (S k) st = parseSequenceGo k Empty st

  ||| Accumulator loop for `parseSequence`.
  |||
  ||| Hoisted out of a `where` block: `parseSequence` now has two clauses, and
  ||| an Idris2 `where` attaches to a single clause only, so the helper has to
  ||| live in the mutual group alongside its caller.
  parseSequenceGo : Nat -> Regex -> Parser Regex
  parseSequenceGo Z acc st = Right (acc, st)
  parseSequenceGo (S k) acc st = case st.input of
    [] => Right (acc, st)
    ('|' :: _) => Right (acc, st)
    (')' :: _) => Right (acc, st)
    _ => case parseQuantified k st of
           Left err => Left err
           Right (Empty, st') => Right (acc, st')
           Right (r, st') =>
             let combined = case acc of
                              Empty => r
                              _ => Seq acc r
             in parseSequenceGo k combined st'

  ||| Parse alternation (a|b|c)
  parseAlternation : Nat -> Parser Regex
  parseAlternation Z st = Left $ ParseError st.pos "Regex complexity budget exhausted"
  parseAlternation (S k) st = case parseSequence k st of
    Left err => Left err
    Right (r1, st') => case st'.input of
      ('|' :: rest) =>
        case parseAlternation k ({ input := rest, pos := S st'.pos } st') of
          Left err => Left err
          Right (r2, st'') => Right (Alt r1 r2, st'')
      _ => Right (r1, st')

--------------------------------------------------------------------------------
-- Public API
--------------------------------------------------------------------------------

||| Parse a regex pattern string
public export
parseRegex : String -> Either RegexError Regex
parseRegex pattern =
  case parseAlternation (16 * length (unpack pattern) + 16) (initState pattern defaultFlags) of
    Left err => Left err
    Right (r, st) =>
      if isNil st.input
        then Right r
        else Left $ ParseError st.pos ("Unexpected character: " ++ pack st.input)

||| Parse a regex pattern with flags
public export
parseRegexWithFlags : String -> RegexFlags -> Either RegexError Regex
parseRegexWithFlags pattern flags =
  case parseAlternation (16 * length (unpack pattern) + 16) (initState pattern flags) of
    Left err => Left err
    Right (r, st) =>
      if isNil st.input
        then Right r
        else Left $ ParseError st.pos ("Unexpected character: " ++ pack st.input)

||| Parse and create a safe regex
public export
parseSafe : String -> Either RegexError SafeRegex
parseSafe pattern = do
  r <- parseRegex pattern
  safe r

||| Parse and create a strictly safe regex
public export
parseSafeStrict : String -> Either RegexError SafeRegex
parseSafeStrict pattern = do
  r <- parseRegex pattern
  safeStrict r

||| Common pre-built safe patterns
public export
emailPattern : SafeRegex
emailPattern = case parseSafe "^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}$" of
  Right sr => sr
  Left _ => MkSafeRegex Empty (MkComplexityAnalysis Linear 0 0 0 False False []) 1000

public export
urlPattern : SafeRegex
urlPattern = case parseSafe "^https?://[a-zA-Z0-9.-]+(/[a-zA-Z0-9._~:/?#@!$&'()*+,;=-]*)?$" of
  Right sr => sr
  Left _ => MkSafeRegex Empty (MkComplexityAnalysis Linear 0 0 0 False False []) 1000

public export
ipv4Pattern : SafeRegex
ipv4Pattern = case parseSafe "^([0-9]{1,3}\\.){3}[0-9]{1,3}$" of
  Right sr => sr
  Left _ => MkSafeRegex Empty (MkComplexityAnalysis Linear 0 0 0 False False []) 1000

public export
uuidPattern : SafeRegex
uuidPattern = case parseSafe "^[0-9a-fA-F]{8}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{12}$" of
  Right sr => sr
  Left _ => MkSafeRegex Empty (MkComplexityAnalysis Linear 0 0 0 False False []) 1000
