-- SPDX-License-Identifier: Apache-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
||| SafeXML - XML processing that prevents injection attacks
|||
||| This module provides safe XML operations including:
||| - XXE (XML External Entity) prevention
||| - Entity expansion bomb protection
||| - Safe XML building with automatic escaping
||| - Secure parsing with configurable limits
|||
||| Example usage:
||| ```idris
||| -- Parse XML safely
||| case parseXML xmlString of
|||   Ok doc => processDocument doc
|||   Err (ExternalEntityDetected _) => handleXXEAttempt
|||   Err e => handleError e
|||
||| -- Build XML safely
||| elem <- element "user"
||| elem' <- withAttr "id" "123" elem
||| let node = build (withText "John Doe" elem')
||| let xml = renderNode node  -- "<user id=\"123\">John Doe</user>"
||| ```
module Proven.SafeXML

import public Proven.Core
import public Proven.SafeXML.Types
import public Proven.SafeXML.Parser
import public Proven.SafeXML.Builder
import public Proven.SafeXML.Proofs

import Data.List
import Data.String

%default total

--------------------------------------------------------------------------------
-- High-Level API
--------------------------------------------------------------------------------

||| Parse XML with secure defaults (XXE protection enabled)
|||
||| This is the recommended way to parse untrusted XML.
public export
parse : String -> XMLResult XMLDocument
parse = parseXML

||| Parse XML with custom security options
|||
||| Use this when you need to relax security constraints (trusted input only).
public export
parseWith : XMLSecurityOptions -> String -> XMLResult XMLDocument
parseWith = parseXMLWith

||| Build an XML element
public export
elem : String -> XMLResult ElementBuilder
elem = element

||| Render XML node to string
public export
render : XMLNode -> String
render = renderNode

||| Render XML document to string
public export
renderDoc : XMLDocument -> String
renderDoc = renderDocument

||| Render with pretty printing
public export
renderDocPretty : XMLDocument -> String
renderDocPretty = renderDocumentPretty

--------------------------------------------------------------------------------
-- Safe Text Helpers
--------------------------------------------------------------------------------

||| Create safe text content (automatically escaped)
public export
text : String -> XMLNode
text s = Text (xmlText s)

||| Create CDATA section (for raw content)
public export
cdata : String -> XMLNode
cdata = CDATA

||| Create XML comment (validates no "--" inside)
public export
comment : String -> XMLResult XMLNode
comment content =
  if isInfixOf "--" content
    then Err (MalformedXML "Comment cannot contain '--'")
    else Ok (Comment content)

--------------------------------------------------------------------------------
-- Element Shortcuts
--------------------------------------------------------------------------------

||| Create a simple element with text content
public export
textElem : String -> String -> XMLResult XMLNode
textElem = textElement

||| Create an empty element
public export
emptyElem : String -> XMLResult XMLNode
emptyElem = emptyElement

||| Create a wrapper element with children
public export
wrap : String -> List XMLNode -> XMLResult XMLNode
wrap = wrapper

--------------------------------------------------------------------------------
-- Document Building
--------------------------------------------------------------------------------

||| Create a new document
public export
newDocument : DocumentBuilder
newDocument = document

||| Create a document with default declaration
public export
newDocumentWithDecl : DocumentBuilder
newDocumentWithDecl = withDefaultDeclaration document

||| Build a complete document
public export
buildDoc : DocumentBuilder -> XMLResult XMLDocument
buildDoc = buildDocument

||| Create a simple document with just a root element
public export
simpleDocument : XMLNode -> XMLDocument
simpleDocument root = MkXMLDocument (Just defaultDeclaration) Nothing root

--------------------------------------------------------------------------------
-- Query Functions
--------------------------------------------------------------------------------

||| Get element by name (first match)
public export
findElement : String -> XMLNode -> Maybe XMLNode
findElement name node = case node of
  Element n _ children =>
    if n.localName == name
      then Just node
      else findFirst (findElement name) children
  _ => Nothing
  where
    findFirst : (a -> Maybe b) -> List a -> Maybe b
    findFirst f [] = Nothing
    findFirst f (x :: xs) = case f x of
                              Just y => Just y
                              Nothing => findFirst f xs

||| Get all elements by name
public export
findElements : String -> XMLNode -> List XMLNode
findElements name node = case node of
  Element n _ children =>
    let found = if n.localName == name then [node] else []
    in found ++ concatMap (findElements name) children
  _ => []

||| Get element attribute value
public export
getAttribute : String -> XMLNode -> Maybe String
getAttribute attrName node = case node of
  Element _ attrs _ =>
    case find (\a => a.name.localName == attrName) attrs of
      Just attr => Just attr.value.raw
      Nothing => Nothing
  _ => Nothing

||| Get text content of element
public export
getTextContent : XMLNode -> String
getTextContent node = case node of
  Text t => t.raw
  CDATA c => c
  Element _ _ children => concatMap getTextContent children
  _ => ""

||| Get all child elements (excluding text/comments)
public export
getChildElements : XMLNode -> List XMLNode
getChildElements node = case node of
  Element _ _ children => filter isElement children
  _ => []
  where
    isElement : XMLNode -> Bool
    isElement (Element _ _ _) = True
    isElement _ = False

--------------------------------------------------------------------------------
-- Transformation Functions
--------------------------------------------------------------------------------

||| Map over all elements
public export
mapElements : (XMLNode -> XMLNode) -> XMLNode -> XMLNode
mapElements f node = case node of
  Element name attrs children =>
    f (Element name attrs (map (mapElements f) children))
  other => other

||| Filter child elements
public export
filterChildren : (XMLNode -> Bool) -> XMLNode -> XMLNode
filterChildren pred node = case node of
  Element name attrs children =>
    Element name attrs (filter pred (map (filterChildren pred) children))
  other => other

||| Add attribute to element
public export
addAttribute : String -> String -> XMLNode -> XMLResult XMLNode
addAttribute name value node = case node of
  Element eName attrs children => do
    a <- attr name value
    Ok (Element eName (attrs ++ [a]) children)
  other => Ok other

||| Remove attribute from element
public export
removeAttribute : String -> XMLNode -> XMLNode
removeAttribute name node = case node of
  Element eName attrs children =>
    Element eName (filter (\a => a.name.localName /= name) attrs) children
  other => other

--------------------------------------------------------------------------------
-- Validation Functions
--------------------------------------------------------------------------------

||| Check if XML is safe to process
public export
isSafe : String -> Bool
isSafe xml = isOk (parse xml)

||| Validate XML structure
public export
validate : String -> XMLResult ()
validate xml = do
  _ <- parse xml
  Ok ()

||| Check for XXE patterns without full parsing
public export
hasXXEPatterns : String -> Bool
hasXXEPatterns xml =
  isInfixOf "<!ENTITY" xml && (isInfixOf "SYSTEM" xml || isInfixOf "PUBLIC" xml)

||| Check for DOCTYPE
public export
hasDTD : String -> Bool
hasDTD xml = isInfixOf "<!DOCTYPE" xml

--------------------------------------------------------------------------------
-- Security Presets
--------------------------------------------------------------------------------

||| Maximum security (strictest settings)
public export
maxSecurity : XMLSecurityOptions
maxSecurity = secureDefaults

||| Standard security (allows PIs and comments)
public export
standardSecurity : XMLSecurityOptions
standardSecurity = { allowProcessingInstructions := True } secureDefaults

||| Relaxed security (for trusted input only)
public export
relaxedSecurity : XMLSecurityOptions
relaxedSecurity =
  { allowInternalEntities := True
  , maxEntityExpansions := 100
  , maxNestingDepth := 500
  , allowProcessingInstructions := True
  } secureDefaults

--------------------------------------------------------------------------------
-- Common XML Patterns
--------------------------------------------------------------------------------

||| Create XML declaration
public export
xmlDecl : String -> Maybe String -> XMLDeclaration
xmlDecl version encoding = MkXMLDeclaration version encoding Nothing

||| Standard XML 1.0 UTF-8 declaration
public export
xml10UTF8 : XMLDeclaration
xml10UTF8 = defaultDeclaration

||| Create namespace declaration
public export
nsDecl : String -> String -> XMLResult XMLAttr
nsDecl prefix uri =
  if null (unpack prefix)
    then xmlns uri
    else xmlnsPrefix prefix uri

--------------------------------------------------------------------------------
-- Error Helpers
--------------------------------------------------------------------------------

||| Check if error is XXE-related
public export
isXXEError : XMLError -> Bool
isXXEError (ExternalEntityDetected _) = True
isXXEError DTDNotAllowed = True
isXXEError _ = False

||| Check if error is entity expansion related
public export
isEntityBombError : XMLError -> Bool
isEntityBombError (EntityExpansionLimitExceeded _ _) = True
isEntityBombError _ = False

||| Check if error is depth related
public export
isDepthError : XMLError -> Bool
isDepthError (NestingDepthExceeded _ _) = True
isDepthError _ = False

||| Get user-friendly error message
public export
friendlyError : XMLError -> String
friendlyError (ExternalEntityDetected _) =
  "XML isInfixOf external entity references which are not allowed for security reasons."
friendlyError (EntityExpansionLimitExceeded _ _) =
  "XML isInfixOf too many entity expansions (possible entity bomb attack)."
friendlyError (NestingDepthExceeded _ _) =
  "XML nesting is too deep (possible attack or malformed document)."
friendlyError DTDNotAllowed =
  "DOCTYPE declarations are not allowed for security reasons."
friendlyError (PINotAllowed target) =
  "Processing instruction '" ++ target ++ "' is not allowed."
friendlyError (InvalidName name reason) =
  "Invalid XML name '" ++ name ++ "': " ++ reason
friendlyError (InvalidXMLCharacter c pos) =
  "Invalid character at position " ++ show pos
friendlyError (MalformedXML msg) =
  "Malformed XML: " ++ msg

--------------------------------------------------------------------------------
-- Escaping Utilities
--------------------------------------------------------------------------------

||| Escape string for XML text content
public export
escapeXML : String -> String
escapeXML s = (xmlText s).escaped

||| Escape string for XML attribute value
public export
escapeXMLAttr : String -> String
escapeXMLAttr s = (xmlAttrValue s).escaped

||| Unescape XML entities (predefined only)
public export
unescapeXML : String -> String
unescapeXML s = pack (go (unpack s))
  where
    go : List Char -> List Char
    go [] = []
    go ('&' :: 'l' :: 't' :: ';' :: rest) = '<' :: go rest
    go ('&' :: 'g' :: 't' :: ';' :: rest) = '>' :: go rest
    go ('&' :: 'a' :: 'm' :: 'p' :: ';' :: rest) = '&' :: go rest
    go ('&' :: 'q' :: 'u' :: 'o' :: 't' :: ';' :: rest) = '"' :: go rest
    go ('&' :: 'a' :: 'p' :: 'o' :: 's' :: ';' :: rest) = '\'' :: go rest
    go (c :: rest) = c :: go rest

--------------------------------------------------------------------------------
-- Debugging
--------------------------------------------------------------------------------

||| Pretty print XML node for debugging
public export
debugNode : XMLNode -> String
debugNode node = case node of
  Element name attrs children =>
    "Element(" ++ qualifiedName name ++
    ", attrs=" ++ show (map (\a => qualifiedName a.name ++ "=" ++ a.value.raw) attrs) ++
    ", children=" ++ show (length children) ++ ")"
  Text t => "Text(\"" ++ t.raw ++ "\")"
  CDATA c => "CDATA(" ++ show (length (unpack c)) ++ " chars)"
  Comment c => "Comment(\"" ++ c ++ "\")"
  ProcessingInstruction target dat => "PI(" ++ target ++ ")"

||| Get document structure summary
public export
documentInfo : XMLDocument -> String
documentInfo doc =
  let hasDecl = maybe "no" (const "yes") doc.declaration
      hasDtd = maybe "no" (const "yes") doc.doctype
      rootName = case doc.root of
                   Element n _ _ => qualifiedName n
                   _ => "<not element>"
  in "XMLDocument(decl=" ++ hasDecl ++ ", dtd=" ++ hasDtd ++ ", root=" ++ rootName ++ ")"
