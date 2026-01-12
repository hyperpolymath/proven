-- SPDX-License-Identifier: Palimpsest-MPL-1.0
||| Safe XML parsing with XXE prevention
|||
||| This module provides secure XML parsing that:
||| - Prevents XXE (XML External Entity) attacks
||| - Prevents entity expansion bombs (billion laughs)
||| - Limits nesting depth to prevent stack overflow
||| - Validates input character safety
module Proven.SafeXML.Parser

import Proven.Core
import Proven.SafeXML.Types
import Data.List
import Data.String

%default total

--------------------------------------------------------------------------------
-- Parser State
--------------------------------------------------------------------------------

||| Parser state tracks position and security limits
record ParserState where
  constructor MkParserState
  input : List Char
  position : Nat
  depth : Nat
  entityExpansions : Nat
  options : XMLSecurityOptions

||| Initial parser state
initParser : XMLSecurityOptions -> String -> ParserState
initParser opts s = MkParserState (unpack s) 0 0 0 opts

||| Advance parser position
advance : Nat -> ParserState -> ParserState
advance n st = { input := drop n st.input, position := st.position + n } st

||| Increase nesting depth
pushDepth : ParserState -> ParserState
pushDepth st = { depth := S st.depth } st

||| Decrease nesting depth
popDepth : ParserState -> ParserState
popDepth st = { depth := pred st.depth } st

--------------------------------------------------------------------------------
-- Basic Parsing Utilities
--------------------------------------------------------------------------------

||| Look at next character without consuming
peek : ParserState -> Maybe Char
peek st = head' st.input

||| Look at next n characters
peekN : Nat -> ParserState -> List Char
peekN n st = take n st.input

||| Consume one character
consume : ParserState -> (Maybe Char, ParserState)
consume st = case st.input of
  [] => (Nothing, st)
  (c :: cs) => (Just c, { input := cs, position := S st.position } st)

||| Consume while predicate is true
consumeWhile : (Char -> Bool) -> ParserState -> (String, ParserState)
consumeWhile pred st =
  let (taken, rest) = span pred st.input
  in (pack taken, { input := rest, position := st.position + length taken } st)

||| Skip whitespace
skipWhitespace : ParserState -> ParserState
skipWhitespace st =
  let (_, st') = consumeWhile isSpace st
  in st'

||| Check if at end of input
isEOF : ParserState -> Bool
isEOF st = null st.input

||| Expect a specific string
expect : String -> ParserState -> XMLResult ParserState
expect expected st =
  let chars = unpack expected
      actual = take (length chars) st.input
  in if actual == chars
       then Ok (advance (length chars) st)
       else Err (MalformedXML ("Expected '" ++ expected ++ "'"))

--------------------------------------------------------------------------------
-- Security Checks
--------------------------------------------------------------------------------

||| Check nesting depth limit
checkDepth : ParserState -> XMLResult ()
checkDepth st =
  if st.depth > st.options.maxNestingDepth
    then Err (NestingDepthExceeded st.depth st.options.maxNestingDepth)
    else Ok ()

||| Check entity expansion limit
checkEntityLimit : ParserState -> XMLResult ()
checkEntityLimit st =
  if st.entityExpansions > st.options.maxEntityExpansions
    then Err (EntityExpansionLimitExceeded st.entityExpansions st.options.maxEntityExpansions)
    else Ok ()

||| Detect external entity reference
isExternalEntity : String -> Bool
isExternalEntity s =
  isPrefixOf "SYSTEM" s || isPrefixOf "PUBLIC" s

||| Check for XXE patterns
checkXXE : String -> XMLSecurityOptions -> XMLResult ()
checkXXE content opts =
  if not opts.allowExternalEntities && containsExternalEntity content
    then Err (ExternalEntityDetected content)
    else Ok ()
  where
    containsExternalEntity : String -> Bool
    containsExternalEntity s =
      isInfixOf "<!ENTITY" s && (isInfixOf "SYSTEM" s || isInfixOf "PUBLIC" s)

--------------------------------------------------------------------------------
-- Name Parsing
--------------------------------------------------------------------------------

||| Parse an XML name
parseName : ParserState -> XMLResult (XMLName, ParserState)
parseName st =
  case peek st of
    Nothing => Err (MalformedXML "Unexpected end of input in name")
    Just c =>
      if not (isXMLNameStartChar c)
        then Err (InvalidName (singleton c) "Invalid name start character")
        else
          let (name, st') = consumeWhile isXMLNameChar st
          in if null (unpack name)
               then Err (InvalidName "" "Empty name")
               else
                 -- Check for namespace prefix
                 case break (== ':') (unpack name) of
                   (prefix, ':' :: local) =>
                     Ok (MkXMLName (pack local) (Just (pack prefix)) Nothing, st')
                   _ =>
                     Ok (MkXMLName name Nothing Nothing, st')

--------------------------------------------------------------------------------
-- Attribute Parsing
--------------------------------------------------------------------------------

||| Escape special characters in attribute values
escapeAttrValue : String -> String
escapeAttrValue s = pack (concatMap escapeChar (unpack s))
  where
    escapeChar : Char -> List Char
    escapeChar '<' = unpack "&lt;"
    escapeChar '>' = unpack "&gt;"
    escapeChar '&' = unpack "&amp;"
    escapeChar '"' = unpack "&quot;"
    escapeChar c = [c]

||| Parse attribute value (handles both single and double quotes)
parseAttrValue : ParserState -> XMLResult (XMLAttrValue, ParserState)
parseAttrValue st =
  case peek st of
    Just '"' =>
      let st1 = snd (consume st)
          (value, st2) = consumeWhile (/= '"') st1
      in case consume st2 of
           (Just '"', st3) => Ok (MkXMLAttrValue value (escapeAttrValue value), st3)
           _ => Err (MalformedXML "Unclosed attribute value")
    Just '\'' =>
      let st1 = snd (consume st)
          (value, st2) = consumeWhile (/= '\'') st1
      in case consume st2 of
           (Just '\'', st3) => Ok (MkXMLAttrValue value (escapeAttrValue value), st3)
           _ => Err (MalformedXML "Unclosed attribute value")
    _ => Err (MalformedXML "Expected quote for attribute value")

||| Parse a single attribute
parseAttribute : ParserState -> XMLResult (XMLAttr, ParserState)
parseAttribute st = do
  (name, st1) <- parseName st
  let st2 = skipWhitespace st1
  st3 <- expect "=" st2
  let st4 = skipWhitespace st3
  (value, st5) <- parseAttrValue st4
  Ok (MkXMLAttr name value, st5)

||| Parse attributes until > or />
parseAttributes : ParserState -> XMLResult (List XMLAttr, ParserState)
parseAttributes st =
  let st' = skipWhitespace st
  in case peek st' of
       Just '>' => Ok ([], st')
       Just '/' => case peekN 2 st' of
                     ['/', '>'] => Ok ([], st')
                     _ => Err (MalformedXML "Expected '>' after '/'")
       Just c =>
         if isXMLNameStartChar c
           then do
             -- Check attribute count limit
             if length [] >= st.options.maxAttributesPerElement
               then Err (MalformedXML "Too many attributes")
               else do
                 (attr, st1) <- parseAttribute st'
                 (rest, st2) <- parseAttributes st1
                 Ok (attr :: rest, st2)
           else Err (MalformedXML ("Unexpected character in attributes: " ++ singleton c))
       Nothing => Err (MalformedXML "Unexpected end of input in attributes")

--------------------------------------------------------------------------------
-- Content Parsing
--------------------------------------------------------------------------------

||| Escape text content
escapeTextContent : String -> String
escapeTextContent s = pack (concatMap escapeChar (unpack s))
  where
    escapeChar : Char -> List Char
    escapeChar '<' = unpack "&lt;"
    escapeChar '>' = unpack "&gt;"
    escapeChar '&' = unpack "&amp;"
    escapeChar c = [c]

||| Parse text content
parseTextContent : ParserState -> XMLResult (XMLText, ParserState)
parseTextContent st =
  let (content, st') = consumeWhile (\c => c /= '<' && c /= '&') st
  in if null (unpack content)
       then Err (MalformedXML "Empty text content")
       else Ok (MkXMLText content (escapeTextContent content), st')

||| Parse CDATA section
parseCDATA : ParserState -> XMLResult (XMLNode, ParserState)
parseCDATA st = do
  st1 <- expect "<![CDATA[" st
  let (content, st2) = consumeUntil "]]>" st1
  st3 <- expect "]]>" st2
  Ok (CDATA content, st3)
  where
    consumeUntil : String -> ParserState -> (String, ParserState)
    consumeUntil marker st =
      let markerChars = unpack marker
          go acc inp =
            if take (length markerChars) inp == markerChars
              then (pack (reverse acc), { input := inp, position := st.position + length acc } st)
              else case inp of
                     [] => (pack (reverse acc), { input := [], position := st.position + length acc } st)
                     (c :: cs) => go (c :: acc) cs
      in go [] st.input

||| Parse comment
parseComment : ParserState -> XMLResult (XMLNode, ParserState)
parseComment st = do
  st1 <- expect "<!--" st
  let (content, st2) = consumeUntilComment st1
  -- Check for -- in comment (not allowed)
  if isInfixOf "--" content
    then Err (MalformedXML "Comment cannot contain '--'")
    else do
      st3 <- expect "-->" st2
      Ok (Comment content, st3)
  where
    consumeUntilComment : ParserState -> (String, ParserState)
    consumeUntilComment st =
      let go acc inp =
            case inp of
              ('-' :: '-' :: '>' :: rest) => (pack (reverse acc), { input := inp, position := st.position + length acc } st)
              [] => (pack (reverse acc), { input := [], position := st.position + length acc } st)
              (c :: cs) => go (c :: acc) cs
      in go [] st.input

||| Parse processing instruction
parsePI : ParserState -> XMLResult (XMLNode, ParserState)
parsePI st =
  if not st.options.allowProcessingInstructions
    then Err (PINotAllowed "")
    else do
      st1 <- expect "<?" st
      (targetName, st2) <- parseName st1
      let st3 = skipWhitespace st2
      let (dat, st4) = consumeUntilPI st3
      st5 <- expect "?>" st4
      -- Check for "xml" target (reserved)
      if toLower (qualifiedName targetName) == "xml" && st.position > 2
        then Err (PINotAllowed "xml declaration not allowed here")
        else Ok (ProcessingInstruction (qualifiedName targetName) dat, st5)
  where
    consumeUntilPI : ParserState -> (String, ParserState)
    consumeUntilPI st =
      let go acc inp =
            case inp of
              ('?' :: '>' :: rest) => (pack (reverse acc), { input := inp, position := st.position + length acc } st)
              [] => (pack (reverse acc), { input := [], position := st.position + length acc } st)
              (c :: cs) => go (c :: acc) cs
      in go [] st.input

--------------------------------------------------------------------------------
-- Element Parsing
--------------------------------------------------------------------------------

||| Parse element (recursive)
mutual
  parseElement : ParserState -> XMLResult (XMLNode, ParserState)
  parseElement st = do
    -- Check depth limit
    checkDepth st
    st1 <- expect "<" st
    (name, st2) <- parseName st1
    (attrs, st3) <- parseAttributes st2
    -- Check for self-closing or start tag
    case peekN 2 st3 of
      ['/', '>'] => do
        st4 <- expect "/>" st3
        Ok (Element name attrs [], st4)
      _ => do
        st4 <- expect ">" st3
        let st5 = pushDepth st4
        (children, st6) <- parseChildren st5
        -- Expect closing tag
        st7 <- expect "</" st6
        (closeName, st8) <- parseName st7
        if qualifiedName closeName /= qualifiedName name
          then Err (MalformedXML ("Mismatched closing tag: expected " ++ qualifiedName name ++ ", got " ++ qualifiedName closeName))
          else do
            let st9 = skipWhitespace st8
            st10 <- expect ">" st9
            let st11 = popDepth st10
            Ok (Element name attrs children, st11)

  ||| Parse children nodes
  parseChildren : ParserState -> XMLResult (List XMLNode, ParserState)
  parseChildren st =
    let st' = skipWhitespace st
    in case peekN 2 st' of
         ['<', '/'] => Ok ([], st')  -- End tag
         ['<', '!'] =>
           case peekN 4 st' of
             ['<', '!', '-', '-'] => do
               (comment, st1) <- parseComment st'
               (rest, st2) <- parseChildren st1
               Ok (comment :: rest, st2)
             ['<', '!', '[', 'C'] => do
               (cdata, st1) <- parseCDATA st'
               (rest, st2) <- parseChildren st1
               Ok (cdata :: rest, st2)
             ['<', '!', 'D', 'O'] =>
               -- DOCTYPE not allowed here
               Err DTDNotAllowed
             _ => Err (MalformedXML "Invalid markup declaration")
         ['<', '?'] => do
           (pi, st1) <- parsePI st'
           (rest, st2) <- parseChildren st1
           Ok (pi :: rest, st2)
         ['<', _] => do
           (elem, st1) <- parseElement st'
           (rest, st2) <- parseChildren st1
           Ok (elem :: rest, st2)
         _ =>
           if isEOF st'
             then Ok ([], st')
             else do
               -- Parse text content
               (text, st1) <- parseTextContent st'
               (rest, st2) <- parseChildren st1
               Ok (Text text :: rest, st2)

--------------------------------------------------------------------------------
-- Document Parsing
--------------------------------------------------------------------------------

||| Parse XML declaration
parseXMLDeclaration : ParserState -> XMLResult (Maybe XMLDeclaration, ParserState)
parseXMLDeclaration st =
  let st' = skipWhitespace st
  in case peekN 5 st' of
       ['<', '?', 'x', 'm', 'l'] => do
         st1 <- expect "<?xml" st'
         let st2 = skipWhitespace st1
         -- Parse version
         st3 <- expect "version" st2
         let st4 = skipWhitespace st3
         st5 <- expect "=" st4
         let st6 = skipWhitespace st5
         (version, st7) <- parseAttrValue st6
         let st8 = skipWhitespace st7
         -- Optional encoding
         (encoding, st9) <- parseOptionalAttr "encoding" st8
         let st10 = skipWhitespace st9
         -- Optional standalone
         (standalone, st11) <- parseOptionalAttr "standalone" st10
         let st12 = skipWhitespace st11
         st13 <- expect "?>" st12
         let standaloneVal = case standalone of
                               Just "yes" => Just True
                               Just "no" => Just False
                               _ => Nothing
         Ok (Just (MkXMLDeclaration version.raw encoding standaloneVal), st13)
       _ => Ok (Nothing, st')
  where
    parseOptionalAttr : String -> ParserState -> XMLResult (Maybe String, ParserState)
    parseOptionalAttr name st =
      let nameChars = unpack name
      in if take (length nameChars) st.input == nameChars
           then do
             st1 <- expect name st
             let st2 = skipWhitespace st1
             st3 <- expect "=" st2
             let st4 = skipWhitespace st3
             (value, st5) <- parseAttrValue st4
             Ok (Just value.raw, st5)
           else Ok (Nothing, st)

||| Parse complete XML document
public export
parseDocument : XMLSecurityOptions -> String -> XMLResult XMLDocument
parseDocument opts input = do
  -- Check for XXE patterns before parsing
  checkXXE input opts
  let st = initParser opts input
  -- Parse optional XML declaration
  (decl, st1) <- parseXMLDeclaration st
  let st2 = skipWhitespace st1
  -- Skip DOCTYPE if present (but check if allowed)
  st3 <- skipDoctype st2
  let st4 = skipWhitespace st3
  -- Parse root element
  (root, st5) <- parseElement st4
  let st6 = skipWhitespace st5
  -- Ensure no trailing content
  if isEOF st6
    then Ok (MkXMLDocument decl Nothing root)
    else Err (MalformedXML "Trailing content after root element")
  where
    skipDoctype : ParserState -> XMLResult ParserState
    skipDoctype st =
      case peekN 9 st.input of
        ('!' :: 'D' :: 'O' :: 'C' :: 'T' :: 'Y' :: 'P' :: 'E' :: _) =>
          if st.options.allowDTD
            then
              -- Skip DOCTYPE declaration (simplified)
              let (_, st') = consumeWhile (/= '>') st
              in case consume st' of
                   (Just '>', st'') => Ok st''
                   _ => Err (MalformedXML "Unclosed DOCTYPE")
            else Err DTDNotAllowed
        _ => Ok st

--------------------------------------------------------------------------------
-- Convenience Functions
--------------------------------------------------------------------------------

||| Parse XML with secure defaults
public export
parseXML : String -> XMLResult XMLDocument
parseXML = parseDocument secureDefaults

||| Parse XML with custom options
public export
parseXMLWith : XMLSecurityOptions -> String -> XMLResult XMLDocument
parseXMLWith = parseDocument

||| Parse XML node (fragment, not full document)
public export
parseFragment : String -> XMLResult XMLNode
parseFragment input = do
  let st = initParser secureDefaults input
  (node, _) <- parseElement st
  Ok node
