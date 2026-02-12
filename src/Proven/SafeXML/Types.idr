-- SPDX-License-Identifier: Apache-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
||| XML types and security constraints
|||
||| This module defines types for safe XML processing including:
||| - XXE (XML External Entity) prevention
||| - Entity expansion bomb protection
||| - Safe element/attribute construction
module Proven.SafeXML.Types

import Proven.Core
import Data.List
import Data.String

%default total

--------------------------------------------------------------------------------
-- XML Value Types
--------------------------------------------------------------------------------

||| Represents a safe XML attribute value (escaped)
public export
record XMLAttrValue where
  constructor MkXMLAttrValue
  raw : String
  escaped : String

public export
Show XMLAttrValue where
  show v = "XMLAttrValue(\"" ++ v.escaped ++ "\")"

public export
Eq XMLAttrValue where
  v1 == v2 = v1.raw == v2.raw

||| Represents a safe XML text content (escaped)
public export
record XMLText where
  constructor MkXMLText
  raw : String
  escaped : String

public export
Show XMLText where
  show t = "XMLText(\"" ++ t.escaped ++ "\")"

public export
Eq XMLText where
  t1 == t2 = t1.raw == t2.raw

||| Safe XML element name (validated)
public export
record XMLName where
  constructor MkXMLName
  localName : String
  prefix : Maybe String
  namespaceURI : Maybe String

public export
Show XMLName where
  show n = case n.prefix of
             Just p => p ++ ":" ++ n.localName
             Nothing => n.localName

public export
Eq XMLName where
  n1 == n2 = n1.localName == n2.localName &&
             n1.prefix == n2.prefix &&
             n1.namespaceURI == n2.namespaceURI

||| Qualified name string
public export
qualifiedName : XMLName -> String
qualifiedName n = case n.prefix of
                    Just p => p ++ ":" ++ n.localName
                    Nothing => n.localName

--------------------------------------------------------------------------------
-- XML Attribute
--------------------------------------------------------------------------------

||| Safe XML attribute (validated name and escaped value)
public export
record XMLAttr where
  constructor MkXMLAttr
  name : XMLName
  value : XMLAttrValue

public export
Show XMLAttr where
  show a = qualifiedName a.name ++ "=\"" ++ a.value.escaped ++ "\""

public export
Eq XMLAttr where
  a1 == a2 = a1.name == a2.name && a1.value == a2.value

--------------------------------------------------------------------------------
-- XML Nodes
--------------------------------------------------------------------------------

||| Safe XML node types
public export
data XMLNode : Type where
  ||| Element node with attributes and children
  Element : (name : XMLName) -> (attrs : List XMLAttr) -> (children : List XMLNode) -> XMLNode

  ||| Text content (escaped)
  Text : (content : XMLText) -> XMLNode

  ||| CDATA section (raw content)
  CDATA : (content : String) -> XMLNode

  ||| Comment (validated to not contain --)
  Comment : (content : String) -> XMLNode

  ||| Processing instruction
  ProcessingInstruction : (target : String) -> (data : String) -> XMLNode

public export
Show XMLNode where
  show (Element name attrs children) =
    "<" ++ qualifiedName name ++
    (if null attrs then "" else " " ++ unwords (map show attrs)) ++
    (if null children then "/>" else ">" ++ concatMap show children ++ "</" ++ qualifiedName name ++ ">")
  show (Text content) = content.escaped
  show (CDATA content) = "<![CDATA[" ++ content ++ "]]>"
  show (Comment content) = "<!--" ++ content ++ "-->"
  show (ProcessingInstruction target dat) = "<?" ++ target ++ " " ++ dat ++ "?>"

--------------------------------------------------------------------------------
-- XML Document
--------------------------------------------------------------------------------

||| XML declaration/prolog
public export
record XMLDeclaration where
  constructor MkXMLDeclaration
  version : String
  encoding : Maybe String
  standalone : Maybe Bool

public export
Show XMLDeclaration where
  show d =
    let enc = case d.encoding of
                Just e => " encoding=\"" ++ e ++ "\""
                Nothing => ""
        std = case d.standalone of
                Just True => " standalone=\"yes\""
                Just False => " standalone=\"no\""
                Nothing => ""
    in "<?xml version=\"" ++ d.version ++ "\"" ++ enc ++ std ++ "?>"

||| Default XML 1.0 UTF-8 declaration
public export
defaultDeclaration : XMLDeclaration
defaultDeclaration = MkXMLDeclaration "1.0" (Just "UTF-8") Nothing

||| XML Document Type Declaration (DOCTYPE)
public export
record XMLDoctype where
  constructor MkXMLDoctype
  rootElement : String
  publicId : Maybe String
  systemId : Maybe String
  -- Internal subset is NOT supported for security

public export
Show XMLDoctype where
  show d =
    let pubSys = case (d.publicId, d.systemId) of
                   (Just pub, Just sys) => " PUBLIC \"" ++ pub ++ "\" \"" ++ sys ++ "\""
                   (Nothing, Just sys) => " SYSTEM \"" ++ sys ++ "\""
                   _ => ""
    in "<!DOCTYPE " ++ d.rootElement ++ pubSys ++ ">"

||| Safe XML document (no external entities)
public export
record XMLDocument where
  constructor MkXMLDocument
  declaration : Maybe XMLDeclaration
  doctype : Maybe XMLDoctype
  root : XMLNode

public export
Show XMLDocument where
  show doc =
    let declStr = maybe "" (++ "\n") (map show doc.declaration)
        dtdStr = maybe "" (++ "\n") (map show doc.doctype)
    in declStr ++ dtdStr ++ show doc.root

--------------------------------------------------------------------------------
-- XML Errors
--------------------------------------------------------------------------------

||| XML processing errors
public export
data XMLError : Type where
  ||| Invalid element/attribute name
  InvalidName : (name : String) -> (reason : String) -> XMLError

  ||| External entity detected (XXE)
  ExternalEntityDetected : (entity : String) -> XMLError

  ||| Entity expansion limit exceeded
  EntityExpansionLimitExceeded : (count : Nat) -> (limit : Nat) -> XMLError

  ||| Nested element depth exceeded
  NestingDepthExceeded : (depth : Nat) -> (limit : Nat) -> XMLError

  ||| Invalid character in content
  InvalidXMLCharacter : (char : Char) -> (position : Nat) -> XMLError

  ||| Malformed XML structure
  MalformedXML : (message : String) -> XMLError

  ||| DTD not allowed
  DTDNotAllowed : XMLError

  ||| Processing instruction not allowed
  PINotAllowed : (target : String) -> XMLError

public export
Show XMLError where
  show (InvalidName name reason) = "InvalidName: '" ++ name ++ "' - " ++ reason
  show (ExternalEntityDetected entity) = "ExternalEntityDetected: " ++ entity
  show (EntityExpansionLimitExceeded count limit) =
    "EntityExpansionLimitExceeded: " ++ show count ++ " > " ++ show limit
  show (NestingDepthExceeded depth limit) =
    "NestingDepthExceeded: " ++ show depth ++ " > " ++ show limit
  show (InvalidXMLCharacter char pos) =
    "InvalidXMLCharacter: '" ++ singleton char ++ "' at position " ++ show pos
  show (MalformedXML msg) = "MalformedXML: " ++ msg
  show DTDNotAllowed = "DTDNotAllowed: DTD declarations are not permitted"
  show (PINotAllowed target) = "PINotAllowed: processing instruction '" ++ target ++ "' is not permitted"

public export
Eq XMLError where
  InvalidName n1 r1 == InvalidName n2 r2 = n1 == n2 && r1 == r2
  ExternalEntityDetected e1 == ExternalEntityDetected e2 = e1 == e2
  EntityExpansionLimitExceeded c1 l1 == EntityExpansionLimitExceeded c2 l2 = c1 == c2 && l1 == l2
  NestingDepthExceeded d1 l1 == NestingDepthExceeded d2 l2 = d1 == d2 && l1 == l2
  InvalidXMLCharacter c1 p1 == InvalidXMLCharacter c2 p2 = c1 == c2 && p1 == p2
  MalformedXML m1 == MalformedXML m2 = m1 == m2
  DTDNotAllowed == DTDNotAllowed = True
  PINotAllowed t1 == PINotAllowed t2 = t1 == t2
  _ == _ = False

--------------------------------------------------------------------------------
-- Result Type
--------------------------------------------------------------------------------

||| Result type for XML operations
public export
XMLResult : Type -> Type
XMLResult = Result XMLError

--------------------------------------------------------------------------------
-- Parser Options
--------------------------------------------------------------------------------

||| Security options for XML parsing
public export
record XMLSecurityOptions where
  constructor MkXMLSecurityOptions
  ||| Allow external entities (default: False for XXE prevention)
  allowExternalEntities : Bool
  ||| Allow internal entities (default: False for entity bomb prevention)
  allowInternalEntities : Bool
  ||| Maximum entity expansion count
  maxEntityExpansions : Nat
  ||| Maximum nesting depth
  maxNestingDepth : Nat
  ||| Allow DTD declarations
  allowDTD : Bool
  ||| Allow processing instructions
  allowProcessingInstructions : Bool
  ||| Maximum attribute count per element
  maxAttributesPerElement : Nat
  ||| Maximum text node size
  maxTextNodeSize : Nat

||| Default secure options (strictest settings)
public export
secureDefaults : XMLSecurityOptions
secureDefaults = MkXMLSecurityOptions
  { allowExternalEntities = False
  , allowInternalEntities = False
  , maxEntityExpansions = 0
  , maxNestingDepth = 100
  , allowDTD = False
  , allowProcessingInstructions = False
  , maxAttributesPerElement = 100
  , maxTextNodeSize = 10000000  -- 10MB
  }

||| Permissive options (for trusted input only!)
public export
permissiveOptions : XMLSecurityOptions
permissiveOptions = MkXMLSecurityOptions
  { allowExternalEntities = False  -- Still no XXE!
  , allowInternalEntities = True
  , maxEntityExpansions = 10000
  , maxNestingDepth = 1000
  , allowDTD = True
  , allowProcessingInstructions = True
  , maxAttributesPerElement = 1000
  , maxTextNodeSize = 100000000  -- 100MB
  }

--------------------------------------------------------------------------------
-- Name Validation
--------------------------------------------------------------------------------

||| Check if character is valid XML name start character
public export
isXMLNameStartChar : Char -> Bool
isXMLNameStartChar c =
  c == ':' || c == '_' ||
  (c >= 'A' && c <= 'Z') ||
  (c >= 'a' && c <= 'z') ||
  (c >= '\xC0' && c <= '\xD6') ||
  (c >= '\xD8' && c <= '\xF6') ||
  (c >= '\xF8' && c <= '\x2FF')
  -- Additional ranges omitted for simplicity

||| Check if character is valid XML name character
public export
isXMLNameChar : Char -> Bool
isXMLNameChar c =
  isXMLNameStartChar c ||
  c == '-' || c == '.' ||
  (c >= '0' && c <= '9') ||
  c == '\xB7'

||| Check if string is valid XML name
public export
isValidXMLName : String -> Bool
isValidXMLName s =
  case unpack s of
    [] => False
    (c :: cs) => isXMLNameStartChar c && all isXMLNameChar cs

--------------------------------------------------------------------------------
-- Character Validation
--------------------------------------------------------------------------------

||| Check if character is valid XML character (XML 1.0)
public export
isValidXMLChar : Char -> Bool
isValidXMLChar c =
  c == '\x09' || c == '\x0A' || c == '\x0D' ||
  (c >= '\x20' && c <= '\xD7FF') ||
  (c >= '\xE000' && c <= '\xFFFD')
  -- Surrogate pairs handled separately

||| Check if string contains only valid XML characters
public export
hasValidXMLChars : String -> Bool
hasValidXMLChars s = all isValidXMLChar (unpack s)

--------------------------------------------------------------------------------
-- Predefined Entities
--------------------------------------------------------------------------------

||| Standard XML predefined entities
public export
predefinedEntities : List (String, String)
predefinedEntities =
  [ ("lt", "<")
  , ("gt", ">")
  , ("amp", "&")
  , ("apos", "'")
  , ("quot", "\"")
  ]

||| Look up predefined entity
public export
lookupPredefinedEntity : String -> Maybe String
lookupPredefinedEntity name = lookup name predefinedEntities
