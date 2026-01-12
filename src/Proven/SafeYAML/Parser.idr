-- SPDX-License-Identifier: Palimpsest-MPL-1.0
||| Safe YAML parsing
|||
||| This module provides secure YAML parsing that:
||| - Prevents alias bomb attacks
||| - Blocks dangerous language-specific tags
||| - Limits nesting depth and value sizes
||| - Validates input structure
module Proven.SafeYAML.Parser

import Proven.Core
import Proven.SafeYAML.Types
import Data.List
import Data.String

%default total

--------------------------------------------------------------------------------
-- Parser State
--------------------------------------------------------------------------------

||| Parser state
record ParserState where
  constructor MkParserState
  input : List Char
  line : Nat
  col : Nat
  depth : Nat
  anchors : List (String, YAMLValue)
  aliasDepth : Nat
  options : YAMLSecurityOptions

||| Initialize parser state
initParser : YAMLSecurityOptions -> String -> ParserState
initParser opts s = MkParserState (unpack s) 1 1 0 [] 0 opts

--------------------------------------------------------------------------------
-- Basic Parsing Utilities
--------------------------------------------------------------------------------

||| Peek at current character
peek : ParserState -> Maybe Char
peek st = head' st.input

||| Peek at next n characters
peekN : Nat -> ParserState -> List Char
peekN n st = take n st.input

||| Consume one character
consume : ParserState -> (Maybe Char, ParserState)
consume st = case st.input of
  [] => (Nothing, st)
  ('\n' :: cs) => (Just '\n', { input := cs, line := S st.line, col := 1 } st)
  (c :: cs) => (Just c, { input := cs, col := S st.col } st)

||| Consume while predicate is true
consumeWhile : (Char -> Bool) -> ParserState -> (String, ParserState)
consumeWhile pred st = go [] st
  where
    go : List Char -> ParserState -> (String, ParserState)
    go acc s = case peek s of
      Nothing => (pack (reverse acc), s)
      Just c => if pred c
                  then let (_, s') = consume s in go (c :: acc) s'
                  else (pack (reverse acc), s)

||| Skip whitespace (space and tab only)
skipSpaces : ParserState -> ParserState
skipSpaces st = snd (consumeWhile (\c => c == ' ' || c == '\t') st)

||| Skip line content (including newline)
skipLine : ParserState -> ParserState
skipLine st =
  let (_, st') = consumeWhile (/= '\n') st
  in case consume st' of
       (Just '\n', st'') => st''
       _ => st'

||| Skip comments
skipComment : ParserState -> ParserState
skipComment st = case peek st of
  Just '#' => skipLine st
  _ => st

||| Check if at end
isEOF : ParserState -> Bool
isEOF st = null st.input

||| Current position for errors
currentPos : ParserState -> (Nat, Nat)
currentPos st = (st.line, st.col)

--------------------------------------------------------------------------------
-- Security Checks
--------------------------------------------------------------------------------

||| Check depth limit
checkDepth : ParserState -> YAMLResult ()
checkDepth st =
  if st.depth > st.options.maxDepth
    then Err (NestingTooDeep st.depth st.options.maxDepth)
    else Ok ()

||| Check key length
checkKeyLength : String -> ParserState -> YAMLResult ()
checkKeyLength key st =
  let len = length (unpack key)
  in if len > st.options.maxKeyLength
       then Err (KeyTooLong len st.options.maxKeyLength)
       else Ok ()

||| Check value size
checkValueSize : String -> ParserState -> YAMLResult ()
checkValueSize val st =
  let size = length (unpack val)
  in if size > st.options.maxValueSize
       then Err (ValueTooLarge size st.options.maxValueSize)
       else Ok ()

||| Check alias depth
checkAliasDepth : ParserState -> YAMLResult ()
checkAliasDepth st =
  if st.aliasDepth > st.options.maxAliasDepth
    then Err (AliasDepthExceeded st.aliasDepth st.options.maxAliasDepth)
    else Ok ()

||| Check tag safety
checkTag : String -> ParserState -> YAMLResult ()
checkTag tag st =
  if isDangerousTag tag || isBlockedTag st.options tag
    then Err (DangerousTag tag)
    else if not st.options.allowCustomTags && isCustomTag tag
      then Err (DangerousTag tag)
      else Ok ()
  where
    isCustomTag : String -> Bool
    isCustomTag t = isPrefixOf "!!" t && not (t `elem` standardTags)

    standardTags : List String
    standardTags = ["!!null", "!!bool", "!!int", "!!float", "!!str", "!!seq", "!!map", "!!binary", "!!timestamp"]

--------------------------------------------------------------------------------
-- Value Parsing
--------------------------------------------------------------------------------

||| Parse null value
parseNull : ParserState -> YAMLResult (YAMLValue, ParserState)
parseNull st =
  let (word, st') = consumeWhile isAlphaNum st
  in if word `elem` ["null", "~", "Null", "NULL"]
       then Ok (YNull, st')
       else Err (SyntaxError ("Expected null, got: " ++ word) st.line st.col)

||| Parse boolean
parseBool : ParserState -> YAMLResult (YAMLValue, ParserState)
parseBool st =
  let (word, st') = consumeWhile isAlphaNum st
  in if word `elem` ["true", "True", "TRUE", "yes", "Yes", "YES", "on", "On", "ON"]
       then Ok (YBool True, st')
       else if word `elem` ["false", "False", "FALSE", "no", "No", "NO", "off", "Off", "OFF"]
         then Ok (YBool False, st')
         else Err (SyntaxError ("Expected boolean, got: " ++ word) st.line st.col)

||| Parse integer
parseInt : ParserState -> YAMLResult (YAMLValue, ParserState)
parseInt st =
  let (sign, st1) = case peek st of
                      Just '-' => ((-1), snd (consume st))
                      Just '+' => (1, snd (consume st))
                      _ => (1, st)
      (digits, st2) = consumeWhile isDigit st1
  in if null (unpack digits)
       then Err (SyntaxError "Expected integer" st.line st.col)
       else Ok (YInt (sign * cast (parseInteger digits)), st2)
  where
    parseInteger : String -> Integer
    parseInteger s = foldl (\acc, c => acc * 10 + cast (ord c - ord '0')) 0 (unpack s)

||| Parse float
parseFloat : ParserState -> YAMLResult (YAMLValue, ParserState)
parseFloat st =
  let (numStr, st') = consumeWhile (\c => isDigit c || c == '.' || c == 'e' || c == 'E' || c == '-' || c == '+') st
  in case parseDoubleMaybe numStr of
       Just d => Ok (YFloat d, st')
       Nothing => Err (SyntaxError ("Invalid float: " ++ numStr) st.line st.col)
  where
    parseDoubleMaybe : String -> Maybe Double
    parseDoubleMaybe s = Just 0.0  -- Simplified; real impl would parse

||| Parse quoted string
parseQuotedString : Char -> ParserState -> YAMLResult (String, ParserState)
parseQuotedString quote st =
  case consume st of
    (Just c, st1) =>
      if c /= quote
        then Err (SyntaxError ("Expected " ++ singleton quote) st.line st.col)
        else parseContent [] st1
    (Nothing, _) => Err (SyntaxError "Unexpected end of input" st.line st.col)
  where
    parseContent : List Char -> ParserState -> YAMLResult (String, ParserState)
    parseContent acc st =
      case consume st of
        (Nothing, _) => Err (SyntaxError "Unclosed string" st.line st.col)
        (Just c, st') =>
          if c == quote
            then Ok (pack (reverse acc), st')
            else if c == '\\'
              then case consume st' of
                     (Just escaped, st'') =>
                       let unescaped = case escaped of
                                         'n' => '\n'
                                         't' => '\t'
                                         'r' => '\r'
                                         '\\' => '\\'
                                         '"' => '"'
                                         '\'' => '\''
                                         _ => escaped
                       in parseContent (unescaped :: acc) st''
                     (Nothing, _) => Err (SyntaxError "Unclosed escape" st.line st.col)
              else parseContent (c :: acc) st'

||| Parse unquoted string (plain scalar)
parsePlainScalar : ParserState -> YAMLResult (String, ParserState)
parsePlainScalar st =
  let (content, st') = consumeWhile isPlainChar st
  in if null (unpack content)
       then Err (SyntaxError "Empty scalar" st.line st.col)
       else Ok (trim content, st')
  where
    isPlainChar : Char -> Bool
    isPlainChar c = c /= ':' && c /= '\n' && c /= '#' && c /= '[' && c /= ']' && c /= '{' && c /= '}' && c /= ','

    trim : String -> String
    trim s = pack (reverse (dropWhile isSpace (reverse (dropWhile isSpace (unpack s)))))

--------------------------------------------------------------------------------
-- Collection Parsing
--------------------------------------------------------------------------------

||| Increase depth
pushDepth : ParserState -> ParserState
pushDepth st = { depth := S st.depth } st

||| Decrease depth
popDepth : ParserState -> ParserState
popDepth st = { depth := pred st.depth } st

||| Parse flow sequence [item, item, ...]
parseFlowSequence : ParserState -> YAMLResult (YAMLValue, ParserState)
parseFlowSequence st = do
  checkDepth st
  case consume st of
    (Just '[', st1) =>
      let st2 = pushDepth (skipSpaces st1)
      in parseItems [] st2
    _ => Err (SyntaxError "Expected '['" st.line st.col)
  where
    parseItems : List YAMLValue -> ParserState -> YAMLResult (YAMLValue, ParserState)
    parseItems acc st =
      let st' = skipSpaces (skipComment st)
      in case peek st' of
           Just ']' => Ok (YArray (reverse acc), popDepth (snd (consume st')))
           _ => do
             (val, st1) <- parseValue st'
             let st2 = skipSpaces st1
             case peek st2 of
               Just ',' => parseItems (val :: acc) (skipSpaces (snd (consume st2)))
               Just ']' => Ok (YArray (reverse (val :: acc)), popDepth (snd (consume st2)))
               _ => Err (SyntaxError "Expected ',' or ']'" st2.line st2.col)

||| Parse flow mapping {key: value, ...}
parseFlowMapping : ParserState -> YAMLResult (YAMLValue, ParserState)
parseFlowMapping st = do
  checkDepth st
  case consume st of
    (Just '{', st1) =>
      let st2 = pushDepth (skipSpaces st1)
      in parsePairs [] st2
    _ => Err (SyntaxError "Expected '{'" st.line st.col)
  where
    parsePairs : List (String, YAMLValue) -> ParserState -> YAMLResult (YAMLValue, ParserState)
    parsePairs acc st =
      let st' = skipSpaces (skipComment st)
      in case peek st' of
           Just '}' => Ok (YObject (reverse acc), popDepth (snd (consume st')))
           _ => do
             (key, st1) <- parseKey st'
             checkKeyLength key st
             let st2 = skipSpaces st1
             case consume st2 of
               (Just ':', st3) => do
                 (val, st4) <- parseValue (skipSpaces st3)
                 let st5 = skipSpaces st4
                 case peek st5 of
                   Just ',' => parsePairs ((key, val) :: acc) (skipSpaces (snd (consume st5)))
                   Just '}' => Ok (YObject (reverse ((key, val) :: acc)), popDepth (snd (consume st5)))
                   _ => Err (SyntaxError "Expected ',' or '}'" st5.line st5.col)
               _ => Err (SyntaxError "Expected ':'" st2.line st2.col)

    parseKey : ParserState -> YAMLResult (String, ParserState)
    parseKey st = case peek st of
      Just '"' => parseQuotedString '"' st
      Just '\'' => parseQuotedString '\'' st
      _ => parsePlainScalar st

||| Parse any value
parseValue : ParserState -> YAMLResult (YAMLValue, ParserState)
parseValue st =
  let st' = skipSpaces (skipComment st)
  in case peek st' of
       Nothing => Err (SyntaxError "Unexpected end of input" st'.line st'.col)
       Just '[' => parseFlowSequence st'
       Just '{' => parseFlowMapping st'
       Just '"' => do
         (s, st1) <- parseQuotedString '"' st'
         checkValueSize s st
         Ok (YString s, st1)
       Just '\'' => do
         (s, st1) <- parseQuotedString '\'' st'
         checkValueSize s st
         Ok (YString s, st1)
       Just '&' => parseAnchor st'
       Just '*' => parseAlias st'
       Just '!' => parseTaggedValue st'
       Just c =>
         if isDigit c || c == '-' || c == '+'
           then parseNumeric st'
           else do
             (s, st1) <- parsePlainScalar st'
             -- Try to interpret as null, bool, etc.
             if s `elem` ["null", "~", "Null", "NULL"]
               then Ok (YNull, st1)
               else if s `elem` ["true", "True", "TRUE", "yes", "Yes", "YES"]
                 then Ok (YBool True, st1)
                 else if s `elem` ["false", "False", "FALSE", "no", "No", "NO"]
                   then Ok (YBool False, st1)
                   else do
                     checkValueSize s st
                     Ok (YString s, st1)

||| Parse numeric value (int or float)
parseNumeric : ParserState -> YAMLResult (YAMLValue, ParserState)
parseNumeric st =
  let (numStr, st') = consumeWhile isNumChar st
  in if isInfixOf "." numStr || isInfixOf "e" numStr || isInfixOf "E" numStr
       then Ok (YFloat 0.0, st')  -- Simplified
       else Ok (YInt (parseInteger numStr), st')
  where
    isNumChar : Char -> Bool
    isNumChar c = isDigit c || c == '.' || c == 'e' || c == 'E' || c == '-' || c == '+'

    parseInteger : String -> Integer
    parseInteger s =
      let chars = unpack s
          (sign, digits) = case chars of
                             ('-' :: ds) => (-1, ds)
                             ('+' :: ds) => (1, ds)
                             ds => (1, ds)
      in sign * foldl (\acc, c => acc * 10 + cast (ord c - ord '0')) 0 digits

||| Parse anchor definition &name
parseAnchor : ParserState -> YAMLResult (YAMLValue, ParserState)
parseAnchor st =
  if not st.options.allowAnchors
    then Err (DangerousTag "anchor (disabled)")
    else do
      case consume st of
        (Just '&', st1) =>
          let (name, st2) = consumeWhile isAnchorChar st1
              st3 = skipSpaces st2
          in do
            (val, st4) <- parseValue st3
            let st5 = { anchors := (name, val) :: st4.anchors } st4
            Ok (val, st5)
        _ => Err (SyntaxError "Expected '&'" st.line st.col)
  where
    isAnchorChar : Char -> Bool
    isAnchorChar c = isAlphaNum c || c == '_' || c == '-'

||| Parse alias reference *name
parseAlias : ParserState -> YAMLResult (YAMLValue, ParserState)
parseAlias st =
  if not st.options.allowAnchors
    then Err (DangerousTag "alias (disabled)")
    else do
      checkAliasDepth st
      case consume st of
        (Just '*', st1) =>
          let (name, st2) = consumeWhile isAnchorChar st1
          in case lookup name st2.anchors of
               Just val =>
                 let st3 = { aliasDepth := S st2.aliasDepth } st2
                 in Ok (val, st3)
               Nothing => Err (AnchorNotFound name)
        _ => Err (SyntaxError "Expected '*'" st.line st.col)
  where
    isAnchorChar : Char -> Bool
    isAnchorChar c = isAlphaNum c || c == '_' || c == '-'

||| Parse tagged value !tag value
parseTaggedValue : ParserState -> YAMLResult (YAMLValue, ParserState)
parseTaggedValue st = do
  case consume st of
    (Just '!', st1) =>
      let (tag, st2) = consumeWhile isTagChar st1
          fullTag = "!" ++ tag
      in do
        checkTag fullTag st
        let st3 = skipSpaces st2
        (val, st4) <- parseValue st3
        -- Apply tag transformation
        Ok (applyTag fullTag val, st4)
    _ => Err (SyntaxError "Expected '!'" st.line st.col)
  where
    isTagChar : Char -> Bool
    isTagChar c = isAlphaNum c || c == '!' || c == '/' || c == ':' || c == '.' || c == '-' || c == '_'

    applyTag : String -> YAMLValue -> YAMLValue
    applyTag "!!null" _ = YNull
    applyTag "!!bool" (YString "true") = YBool True
    applyTag "!!bool" (YString "false") = YBool False
    applyTag "!!int" (YString s) = YInt (cast (length (unpack s)))  -- Simplified
    applyTag "!!float" (YString s) = YFloat 0.0  -- Simplified
    applyTag "!!str" v = case v of
                           YString s => v
                           YInt i => YString (show i)
                           YFloat f => YString (show f)
                           YBool b => YString (if b then "true" else "false")
                           _ => v
    applyTag "!!binary" (YString s) = YBinary []  -- Would decode base64
    applyTag "!!timestamp" (YString s) = YTimestamp s
    applyTag _ v = v

--------------------------------------------------------------------------------
-- Document Parsing
--------------------------------------------------------------------------------

||| Parse document separator ---
parseDocStart : ParserState -> ParserState
parseDocStart st =
  case peekN 3 st of
    ['-', '-', '-'] => snd (consumeWhile (/= '\n') (advance 3 st))
    _ => st
  where
    advance : Nat -> ParserState -> ParserState
    advance Z s = s
    advance (S k) s = advance k (snd (consume s))

||| Parse document end ...
parseDocEnd : ParserState -> ParserState
parseDocEnd st =
  case peekN 3 st of
    ['.', '.', '.'] => snd (consumeWhile (/= '\n') (advance 3 st))
    _ => st
  where
    advance : Nat -> ParserState -> ParserState
    advance Z s = s
    advance (S k) s = advance k (snd (consume s))

||| Parse YAML directive
parseDirective : ParserState -> YAMLResult (Maybe String, ParserState)
parseDirective st =
  case peek st of
    Just '%' =>
      let (_, st1) = consume st
          (directive, st2) = consumeWhile (/= '\n') st1
          st3 = skipLine st2
      in if isPrefixOf "YAML" directive
           then let version = trim (strSubstr 4 (cast (length directive - 4)) directive)
                in if version `elem` ["1.0", "1.1", "1.2"]
                     then Ok (Just version, st3)
                     else Err (UnsupportedVersion version)
           else Ok (Nothing, st3)  -- Ignore other directives
    _ => Ok (Nothing, st)
  where
    trim : String -> String
    trim s = pack (reverse (dropWhile isSpace (reverse (dropWhile isSpace (unpack s)))))

||| Parse single document
parseDocument : ParserState -> YAMLResult (YAMLDocument, ParserState)
parseDocument st = do
  -- Parse optional directives
  (version, st1) <- parseDirective (skipSpaces st)
  -- Skip document start marker
  let st2 = parseDocStart (skipSpaces st1)
  -- Parse value
  (val, st3) <- parseValue (skipSpaces st2)
  -- Skip document end marker
  let st4 = parseDocEnd (skipSpaces st3)
  Ok (MkYAMLDocument version [] val, st4)

||| Parse YAML stream (multiple documents)
public export
parseStream : YAMLSecurityOptions -> String -> YAMLResult YAMLStream
parseStream opts input = do
  let st = initParser opts input
  go 0 [] st
  where
    go : Nat -> List YAMLDocument -> ParserState -> YAMLResult YAMLStream
    go count acc st =
      let st' = skipSpaces (skipComment st)
      in if isEOF st'
           then Ok (reverse acc)
           else do
             if count >= opts.maxDocuments
               then Err (TooManyDocuments count opts.maxDocuments)
               else do
                 (doc, st1) <- parseDocument st'
                 go (S count) (doc :: acc) st1

--------------------------------------------------------------------------------
-- Public API
--------------------------------------------------------------------------------

||| Parse YAML with secure defaults
public export
parseYAML : String -> YAMLResult YAMLValue
parseYAML input = do
  docs <- parseStream secureDefaults input
  case docs of
    [] => Ok YNull
    [doc] => Ok doc.value
    _ => Ok (YArray (map (.value) docs))

||| Parse YAML with custom options
public export
parseYAMLWith : YAMLSecurityOptions -> String -> YAMLResult YAMLValue
parseYAMLWith opts input = do
  docs <- parseStream opts input
  case docs of
    [] => Ok YNull
    [doc] => Ok doc.value
    _ => Ok (YArray (map (.value) docs))

||| Parse single YAML document
public export
parseYAMLDocument : String -> YAMLResult YAMLDocument
parseYAMLDocument input = do
  docs <- parseStream secureDefaults input
  case docs of
    [] => Err (SyntaxError "Empty document" 1 1)
    [doc] => Ok doc
    _ => Err (TooManyDocuments (length docs) 1)
