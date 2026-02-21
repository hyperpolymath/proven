-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
||| Safe XML builder DSL
|||
||| This module provides a fluent API for building XML documents
||| that are guaranteed to be well-formed and safe from injection.
module Proven.SafeXML.Builder

import Proven.Core
import Proven.SafeXML.Types
import Data.List
import Data.String

%default total

--------------------------------------------------------------------------------
-- Safe Name Construction
--------------------------------------------------------------------------------

||| Validate and create an XML name
public export
xmlName : String -> XMLResult XMLName
xmlName s =
  if null (unpack s)
    then Err (InvalidName s "Name cannot be empty")
    else if not (isValidXMLName s)
      then Err (InvalidName s "Invalid XML name characters")
      else Ok (MkXMLName s Nothing Nothing)

||| Create a qualified name with namespace prefix
public export
xmlQName : String -> String -> XMLResult XMLName
xmlQName prefix local =
  if null (unpack prefix) || null (unpack local)
    then Err (InvalidName (prefix ++ ":" ++ local) "Prefix and local name cannot be empty")
    else if not (isValidXMLName prefix) || not (isValidXMLName local)
      then Err (InvalidName (prefix ++ ":" ++ local) "Invalid XML name characters")
      else Ok (MkXMLName local (Just prefix) Nothing)

||| Create a name with namespace URI
public export
xmlNameNS : String -> String -> XMLResult XMLName
xmlNameNS local nsURI =
  if null (unpack local)
    then Err (InvalidName local "Name cannot be empty")
    else if not (isValidXMLName local)
      then Err (InvalidName local "Invalid XML name characters")
      else Ok (MkXMLName local Nothing (Just nsURI))

--------------------------------------------------------------------------------
-- Safe Text Construction
--------------------------------------------------------------------------------

||| Escape special XML characters in text content
escapeText : String -> String
escapeText s = pack (concatMap escapeChar (unpack s))
  where
    escapeChar : Char -> List Char
    escapeChar '<' = unpack "&lt;"
    escapeChar '>' = unpack "&gt;"
    escapeChar '&' = unpack "&amp;"
    escapeChar c = [c]

||| Create safe text content
public export
xmlText : String -> XMLText
xmlText s = MkXMLText s (escapeText s)

||| Create safe attribute value
escapeAttr : String -> String
escapeAttr s = pack (concatMap escapeChar (unpack s))
  where
    escapeChar : Char -> List Char
    escapeChar '<' = unpack "&lt;"
    escapeChar '>' = unpack "&gt;"
    escapeChar '&' = unpack "&amp;"
    escapeChar '"' = unpack "&quot;"
    escapeChar '\'' = unpack "&#39;"
    escapeChar c = [c]

||| Create safe attribute value
public export
xmlAttrValue : String -> XMLAttrValue
xmlAttrValue s = MkXMLAttrValue s (escapeAttr s)

--------------------------------------------------------------------------------
-- Attribute Builder
--------------------------------------------------------------------------------

||| Create an attribute with validation
public export
attr : String -> String -> XMLResult XMLAttr
attr name value = do
  n <- xmlName name
  Ok (MkXMLAttr n (xmlAttrValue value))

||| Create a qualified attribute
public export
qattr : String -> String -> String -> XMLResult XMLAttr
qattr prefix local value = do
  n <- xmlQName prefix local
  Ok (MkXMLAttr n (xmlAttrValue value))

||| Create xmlns attribute
public export
xmlns : String -> XMLResult XMLAttr
xmlns uri = do
  n <- xmlName "xmlns"
  Ok (MkXMLAttr n (xmlAttrValue uri))

||| Create xmlns:prefix attribute
public export
xmlnsPrefix : String -> String -> XMLResult XMLAttr
xmlnsPrefix prefix uri = do
  n <- xmlQName "xmlns" prefix
  Ok (MkXMLAttr n (xmlAttrValue uri))

--------------------------------------------------------------------------------
-- Element Builder
--------------------------------------------------------------------------------

||| Element builder type for fluent API
public export
record ElementBuilder where
  constructor MkElementBuilder
  elemName : XMLName
  elemAttrs : List XMLAttr
  elemChildren : List XMLNode

||| Start building an element
public export
element : String -> XMLResult ElementBuilder
element name = do
  n <- xmlName name
  Ok (MkElementBuilder n [] [])

||| Start building a qualified element
public export
qelement : String -> String -> XMLResult ElementBuilder
qelement prefix local = do
  n <- xmlQName prefix local
  Ok (MkElementBuilder n [] [])

||| Add an attribute to element
public export
withAttr : String -> String -> ElementBuilder -> XMLResult ElementBuilder
withAttr name value builder = do
  a <- attr name value
  Ok ({ elemAttrs := builder.elemAttrs ++ [a] } builder)

||| Add a qualified attribute
public export
withQAttr : String -> String -> String -> ElementBuilder -> XMLResult ElementBuilder
withQAttr prefix local value builder = do
  a <- qattr prefix local value
  Ok ({ elemAttrs := builder.elemAttrs ++ [a] } builder)

||| Add multiple attributes
public export
withAttrs : List (String, String) -> ElementBuilder -> XMLResult ElementBuilder
withAttrs [] builder = Ok builder
withAttrs ((n, v) :: rest) builder = do
  builder' <- withAttr n v builder
  withAttrs rest builder'

||| Add a child element
public export
withChild : XMLNode -> ElementBuilder -> ElementBuilder
withChild child builder = { elemChildren := builder.elemChildren ++ [child] } builder

||| Add multiple children
public export
withChildren : List XMLNode -> ElementBuilder -> ElementBuilder
withChildren children builder =
  { elemChildren := builder.elemChildren ++ children } builder

||| Add text content
public export
withText : String -> ElementBuilder -> ElementBuilder
withText s builder =
  withChild (Text (xmlText s)) builder

||| Add CDATA content
public export
withCDATA : String -> ElementBuilder -> ElementBuilder
withCDATA s builder =
  withChild (CDATA s) builder

||| Build the element
public export
build : ElementBuilder -> XMLNode
build builder = Element builder.elemName builder.elemAttrs builder.elemChildren

--------------------------------------------------------------------------------
-- Document Builder
--------------------------------------------------------------------------------

||| Document builder type
public export
record DocumentBuilder where
  constructor MkDocumentBuilder
  docDecl : Maybe XMLDeclaration
  docDoctype : Maybe XMLDoctype
  docRoot : Maybe XMLNode

||| Start building a document
public export
document : DocumentBuilder
document = MkDocumentBuilder Nothing Nothing Nothing

||| Add XML declaration
public export
withDeclaration : XMLDeclaration -> DocumentBuilder -> DocumentBuilder
withDeclaration decl doc = { docDecl := Just decl } doc

||| Add default declaration (XML 1.0 UTF-8)
public export
withDefaultDeclaration : DocumentBuilder -> DocumentBuilder
withDefaultDeclaration = withDeclaration defaultDeclaration

||| Set root element
public export
withRoot : XMLNode -> DocumentBuilder -> DocumentBuilder
withRoot root doc = { docRoot := Just root } doc

||| Build the document
public export
buildDocument : DocumentBuilder -> XMLResult XMLDocument
buildDocument doc = case doc.docRoot of
  Nothing => Err (MalformedXML "Document requires a root element")
  Just root => Ok (MkXMLDocument doc.docDecl doc.docDoctype root)

--------------------------------------------------------------------------------
-- Rendering
--------------------------------------------------------------------------------

||| Render declaration to string
renderDeclaration : XMLDeclaration -> String
renderDeclaration d =
  let enc = case d.encoding of
              Just e => " encoding=\"" ++ e ++ "\""
              Nothing => ""
      std = case d.standalone of
              Just True => " standalone=\"yes\""
              Just False => " standalone=\"no\""
              Nothing => ""
  in "<?xml version=\"" ++ d.version ++ "\"" ++ enc ++ std ++ "?>"

||| Render attribute to string
renderAttr : XMLAttr -> String
renderAttr a = qualifiedName a.name ++ "=\"" ++ a.value.escaped ++ "\""

||| Render node to string
mutual
  public export
  renderNode : XMLNode -> String
  renderNode (Element name attrs children) =
    let attrStr = if null attrs then "" else " " ++ unwords (map renderAttr attrs)
    in if null children
         then "<" ++ qualifiedName name ++ attrStr ++ "/>"
         else "<" ++ qualifiedName name ++ attrStr ++ ">" ++
              concatMap renderNode children ++
              "</" ++ qualifiedName name ++ ">"
  renderNode (Text content) = content.escaped
  renderNode (CDATA content) = "<![CDATA[" ++ content ++ "]]>"
  renderNode (Comment content) = "<!--" ++ content ++ "-->"
  renderNode (ProcessingInstruction target dat) = "<?" ++ target ++ " " ++ dat ++ "?>"

||| Render document to string
public export
renderDocument : XMLDocument -> String
renderDocument doc =
  let declStr = maybe "" (\d => renderDeclaration d ++ "\n") doc.declaration
  in declStr ++ renderNode doc.root

--------------------------------------------------------------------------------
-- Pretty Printing
--------------------------------------------------------------------------------

||| Render with indentation
mutual
  public export
  renderPretty : Nat -> XMLNode -> String
  renderPretty indent (Element name attrs children) =
    let ind = replicate indent ' '
        attrStr = if null attrs then "" else " " ++ unwords (map renderAttr attrs)
    in if null children
         then pack ind ++ "<" ++ qualifiedName name ++ attrStr ++ "/>\n"
         else pack ind ++ "<" ++ qualifiedName name ++ attrStr ++ ">\n" ++
              concatMap (renderPretty (indent + 2)) children ++
              pack ind ++ "</" ++ qualifiedName name ++ ">\n"
  renderPretty indent (Text content) = content.escaped
  renderPretty indent (CDATA content) = pack (replicate indent ' ') ++ "<![CDATA[" ++ content ++ "]]>\n"
  renderPretty indent (Comment content) = pack (replicate indent ' ') ++ "<!--" ++ content ++ "-->\n"
  renderPretty indent (ProcessingInstruction target dat) =
    pack (replicate indent ' ') ++ "<?" ++ target ++ " " ++ dat ++ "?>\n"

||| Pretty print document
public export
renderDocumentPretty : XMLDocument -> String
renderDocumentPretty doc =
  let declStr = maybe "" (\d => renderDeclaration d ++ "\n") doc.declaration
  in declStr ++ renderPretty 0 doc.root

--------------------------------------------------------------------------------
-- Convenience Functions
--------------------------------------------------------------------------------

||| Create a simple element with text content
public export
textElement : String -> String -> XMLResult XMLNode
textElement name text = do
  builder <- element name
  Ok (build (withText text builder))

||| Create an element with attributes and text
public export
attrTextElement : String -> List (String, String) -> String -> XMLResult XMLNode
attrTextElement name attrs text = do
  builder <- element name
  builder' <- withAttrs attrs builder
  Ok (build (withText text builder'))

||| Create a simple wrapper element
public export
wrapper : String -> List XMLNode -> XMLResult XMLNode
wrapper name children = do
  builder <- element name
  Ok (build (withChildren children builder))

||| Create an empty element
public export
emptyElement : String -> XMLResult XMLNode
emptyElement name = do
  builder <- element name
  Ok (build builder)

||| Create an empty element with attributes
public export
emptyElementWithAttrs : String -> List (String, String) -> XMLResult XMLNode
emptyElementWithAttrs name attrs = do
  builder <- element name
  builder' <- withAttrs attrs builder
  Ok (build builder')

--------------------------------------------------------------------------------
-- Common Elements
--------------------------------------------------------------------------------

||| Create HTML-style div element
public export
div : List XMLNode -> XMLResult XMLNode
div = wrapper "div"

||| Create HTML-style span element
public export
span : String -> XMLResult XMLNode
span = textElement "span"

||| Create HTML-style paragraph
public export
para : String -> XMLResult XMLNode
para = textElement "p"

||| Create HTML-style link
public export
link : String -> String -> XMLResult XMLNode
link href text = attrTextElement "a" [("href", href)] text

||| Create HTML-style image
public export
img : String -> String -> XMLResult XMLNode
img src alt = emptyElementWithAttrs "img" [("src", src), ("alt", alt)]
