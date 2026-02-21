-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
||| Safe Email Address Parser
|||
||| Parses email addresses according to RFC 5321 (SMTP) and RFC 5322 (IMF).
||| Returns Maybe instead of throwing exceptions.
module Proven.SafeEmail.Parser

import Proven.Core
import Data.List
import Data.String

%default total

--------------------------------------------------------------------------------
-- Parser Types
--------------------------------------------------------------------------------

||| Parsed email components
public export
record ParsedEmail where
  constructor MkParsedEmail
  localPart : String
  domain : String

||| Parse error types
public export
data EmailParseError
  = EmptyInput
  | NoAtSign
  | MultipleAtSigns
  | EmptyLocalPart
  | EmptyDomain
  | InvalidLocalPart String
  | InvalidDomain String
  | LocalPartTooLong
  | DomainTooLong
  | TotalLengthTooLong

public export
Show EmailParseError where
  show EmptyInput = "Empty input"
  show NoAtSign = "No @ sign found"
  show MultipleAtSigns = "Multiple @ signs found"
  show EmptyLocalPart = "Empty local part"
  show EmptyDomain = "Empty domain"
  show (InvalidLocalPart s) = "Invalid local part: " ++ s
  show (InvalidDomain s) = "Invalid domain: " ++ s
  show LocalPartTooLong = "Local part exceeds 64 characters"
  show DomainTooLong = "Domain exceeds 253 characters"
  show TotalLengthTooLong = "Total length exceeds 254 characters"

--------------------------------------------------------------------------------
-- Character Validation
--------------------------------------------------------------------------------

||| Characters allowed in local part (atom characters)
public export
isAtomChar : Char -> Bool
isAtomChar c =
  isAlphaNum c ||
  c `elem` unpack "!#$%&'*+/=?^_`{|}~-"

||| Characters allowed in quoted local part
public export
isQuotedChar : Char -> Bool
isQuotedChar c =
  (ord c >= 32 && ord c <= 126 && c /= '\\' && c /= '"') ||
  c == ' ' || c == '\t'

||| Characters allowed in domain label
public export
isDomainChar : Char -> Bool
isDomainChar c = isAlphaNum c || c == '-'

--------------------------------------------------------------------------------
-- Parsing Functions
--------------------------------------------------------------------------------

||| Split on last occurrence of a character
splitOnLast : Char -> String -> Maybe (String, String)
splitOnLast c s =
  let chars = unpack s
      idx = findLastIndex c chars 0 Nothing
  in case idx of
       Nothing => Nothing
       Just i =>
         let (before, after) = splitAt i chars
         in Just (pack before, pack (drop 1 after))
  where
    findLastIndex : Char -> List Char -> Nat -> Maybe Nat -> Maybe Nat
    findLastIndex _ [] _ found = found
    findLastIndex target (x :: xs) i found =
      if x == target
        then findLastIndex target xs (S i) (Just i)
        else findLastIndex target xs (S i) found

||| Parse local part (supports both atom and quoted strings)
parseLocalPart : String -> Either EmailParseError String
parseLocalPart "" = Left EmptyLocalPart
parseLocalPart s =
  if length s > 64
    then Left LocalPartTooLong
    else if isPrefixOf "\"" s && isSuffixOf "\"" s
      then parseQuoted (drop 1 (take (minus (length s) 1) s))
      else parseAtom s
  where
    parseAtom : String -> Either EmailParseError String
    parseAtom str =
      let chars = unpack str
      in if all isAtomChar chars &&
            not (isPrefixOf "." str) &&
            not (isSuffixOf "." str) &&
            not (isInfixOf ".." str)
           then Right str
           else Left (InvalidLocalPart str)

    parseQuoted : String -> Either EmailParseError String
    parseQuoted str = validateQuoted (unpack str) []
      where
        validateQuoted : List Char -> List Char -> Either EmailParseError String
        validateQuoted [] acc = Right (pack (reverse acc))
        validateQuoted ('\\' :: c :: rest) acc =
          if ord c >= 32 && ord c <= 126
            then validateQuoted rest (c :: acc)
            else Left (InvalidLocalPart str)
        validateQuoted (c :: rest) acc =
          if isQuotedChar c
            then validateQuoted rest (c :: acc)
            else Left (InvalidLocalPart str)

||| Parse domain part
parseDomain : String -> Either EmailParseError String
parseDomain "" = Left EmptyDomain
parseDomain s =
  if length s > 253
    then Left DomainTooLong
    else if isPrefixOf "[" s && isSuffixOf "]" s
      then parseIPLiteral s
      else parseDomainName s
  where
    parseIPLiteral : String -> Either EmailParseError String
    parseIPLiteral str = Right str  -- Accept IP literals as-is for now

    parseDomainName : String -> Either EmailParseError String
    parseDomainName str =
      let labels = split '.' str
      in if all isValidLabel labels && not (null labels)
           then Right str
           else Left (InvalidDomain str)

    isValidLabel : String -> Bool
    isValidLabel "" = False
    isValidLabel label =
      length label <= 63 &&
      all isDomainChar (unpack label) &&
      not (isPrefixOf "-" label) &&
      not (isSuffixOf "-" label)

--------------------------------------------------------------------------------
-- Public API
--------------------------------------------------------------------------------

||| Parse email address string
public export
parseEmail : String -> Maybe ParsedEmail
parseEmail "" = Nothing
parseEmail s = case parseEmailEither s of
  Right email => Just email
  Left _ => Nothing

||| Parse email with detailed error
public export
parseEmailEither : String -> Either EmailParseError ParsedEmail
parseEmailEither "" = Left EmptyInput
parseEmailEither s =
  if length s > 254
    then Left TotalLengthTooLong
    else case splitOnLast '@' s of
      Nothing => Left NoAtSign
      Just (local, domain) =>
        if isInfixOf "@" local
          then Left MultipleAtSigns
          else do
            validLocal <- parseLocalPart local
            validDomain <- parseDomain domain
            Right (MkParsedEmail validLocal validDomain)

||| Parse email from "Name <email>" format
public export
parseNamedEmail : String -> Maybe (Maybe String, ParsedEmail)
parseNamedEmail s =
  let trimmed = trim s
  in if isSuffixOf ">" trimmed
       then parseAngleBracket trimmed
       else map (\email => (Nothing, email)) (parseEmail trimmed)
  where
    trim : String -> String
    trim = pack . dropWhile isSpace . reverse . dropWhile isSpace . reverse . unpack

    parseAngleBracket : String -> Maybe (Maybe String, ParsedEmail)
    parseAngleBracket str =
      case findIndex (== '<') (unpack str) of
        Nothing => Nothing
        Just idx =>
          let namePart = trim (take idx str)
              emailPart = take (minus (length str) 1) (drop (S idx) str)
              name = if namePart == "" then Nothing
                     else Just (unquote namePart)
          in map (\email => (name, email)) (parseEmail emailPart)

    findIndex : (a -> Bool) -> List a -> Maybe Nat
    findIndex _ [] = Nothing
    findIndex p (x :: xs) =
      if p x then Just 0 else map S (findIndex p xs)

    unquote : String -> String
    unquote str =
      if isPrefixOf "\"" str && isSuffixOf "\"" str
        then drop 1 (take (minus (length str) 1) str)
        else str

||| Quick check if string looks like email
public export
looksLikeEmail : String -> Bool
looksLikeEmail s =
  let atCount = length (filter (== '@') (unpack s))
  in atCount == 1 && length s >= 3

--------------------------------------------------------------------------------
-- Batch Parsing
--------------------------------------------------------------------------------

||| Parse comma-separated email list
public export
parseEmailList : String -> List ParsedEmail
parseEmailList s =
  let parts = concatMap (split ';') (split ',' s)
      trimmed = map trim parts
  in mapMaybe parseEmail trimmed
  where
    trim : String -> String
    trim = pack . dropWhile isSpace . reverse . dropWhile isSpace . reverse . unpack

||| Parse and collect errors
public export
parseEmailListWithErrors : String -> (List ParsedEmail, List (String, EmailParseError))
parseEmailListWithErrors s =
  let parts = concatMap (split ';') (split ',' s)
      trimmed = map trim parts
      results = map (\p => (p, parseEmailEither p)) trimmed
  in partitionResults results ([], [])
  where
    trim : String -> String
    trim = pack . dropWhile isSpace . reverse . dropWhile isSpace . reverse . unpack

    partitionResults : List (String, Either EmailParseError ParsedEmail) ->
                       (List ParsedEmail, List (String, EmailParseError)) ->
                       (List ParsedEmail, List (String, EmailParseError))
    partitionResults [] (ok, err) = (reverse ok, reverse err)
    partitionResults ((_, Right email) :: rest) (ok, err) =
      partitionResults rest (email :: ok, err)
    partitionResults ((s', Left e) :: rest) (ok, err) =
      partitionResults rest (ok, (s', e) :: err)
