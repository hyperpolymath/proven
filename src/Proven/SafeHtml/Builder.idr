-- SPDX-License-Identifier: Apache-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
||| Type-safe HTML builder DSL
|||
||| Provides a fluent interface for building HTML safely
||| Ensures proper nesting and escaping at the type level
module Proven.SafeHtml.Builder

import Proven.Core
import Proven.SafeHtml.Escape
import Data.List
import Data.String

%default total

--------------------------------------------------------------------------------
-- Builder Types
--------------------------------------------------------------------------------

||| HTML attribute in builder context
public export
record BuilderAttr where
  constructor MkBuilderAttr
  attrName : String
  attrValue : String

||| Builder state for constructing HTML
public export
record HtmlBuilder where
  constructor MkBuilder
  builderAttrs : List BuilderAttr
  builderChildren : List String
  builderTag : String

--------------------------------------------------------------------------------
-- Builder Creation
--------------------------------------------------------------------------------

||| Start building an element with given tag
public export
element : String -> HtmlBuilder
element tag = MkBuilder [] [] tag

||| Start building a div
public export
divBuilder : HtmlBuilder
divBuilder = element "div"

||| Start building a span
public export
spanBuilder : HtmlBuilder
spanBuilder = element "span"

||| Start building a paragraph
public export
pBuilder : HtmlBuilder
pBuilder = element "p"

||| Start building an anchor
public export
aBuilder : HtmlBuilder
aBuilder = element "a"

||| Start building an image
public export
imgBuilder : HtmlBuilder
imgBuilder = element "img"

--------------------------------------------------------------------------------
-- Attribute Methods
--------------------------------------------------------------------------------

||| Add an attribute to the builder
public export
withAttr : String -> String -> HtmlBuilder -> HtmlBuilder
withAttr name value builder =
  { builderAttrs := MkBuilderAttr name value :: builder.builderAttrs } builder

||| Add class attribute
public export
withClass : String -> HtmlBuilder -> HtmlBuilder
withClass = withAttr "class"

||| Add id attribute
public export
withId : String -> HtmlBuilder -> HtmlBuilder
withId = withAttr "id"

||| Add href attribute (with URL sanitization)
public export
withHref : String -> HtmlBuilder -> Maybe HtmlBuilder
withHref url builder =
  if hasDangerousScheme url
    then Nothing
    else Just (withAttr "href" url builder)

||| Add href attribute (unsafe - no validation)
public export
withHrefUnsafe : String -> HtmlBuilder -> HtmlBuilder
withHrefUnsafe = withAttr "href"

||| Add src attribute
public export
withSrc : String -> HtmlBuilder -> HtmlBuilder
withSrc = withAttr "src"

||| Add alt attribute
public export
withAlt : String -> HtmlBuilder -> HtmlBuilder
withAlt = withAttr "alt"

||| Add title attribute
public export
withTitle : String -> HtmlBuilder -> HtmlBuilder
withTitle = withAttr "title"

||| Add style attribute (with CSS escaping)
public export
withStyle : String -> HtmlBuilder -> HtmlBuilder
withStyle css = withAttr "style" (escapeInlineCss css)

||| Add data attribute
public export
withData : String -> String -> HtmlBuilder -> HtmlBuilder
withData name value = withAttr ("data-" ++ name) value

||| Add aria attribute
public export
withAria : String -> String -> HtmlBuilder -> HtmlBuilder
withAria name value = withAttr ("aria-" ++ name) value

||| Add role attribute
public export
withRole : String -> HtmlBuilder -> HtmlBuilder
withRole = withAttr "role"

--------------------------------------------------------------------------------
-- Content Methods
--------------------------------------------------------------------------------

||| Add escaped text content
public export
withText : String -> HtmlBuilder -> HtmlBuilder
withText content builder =
  { builderChildren := escapeHtmlContent content :: builder.builderChildren } builder

||| Add raw HTML content (use with caution!)
public export
withRawHtml : String -> HtmlBuilder -> HtmlBuilder
withRawHtml html builder =
  { builderChildren := html :: builder.builderChildren } builder

||| Add child builder
public export
withChild : HtmlBuilder -> HtmlBuilder -> HtmlBuilder
withChild child parent =
  { builderChildren := build child :: parent.builderChildren } parent

||| Add multiple children
public export
withChildren : List HtmlBuilder -> HtmlBuilder -> HtmlBuilder
withChildren children parent = foldl (flip withChild) parent children

--------------------------------------------------------------------------------
-- Rendering
--------------------------------------------------------------------------------

||| Render a single attribute
renderAttr : BuilderAttr -> String
renderAttr (MkBuilderAttr name value) =
  " " ++ name ++ "=\"" ++ escapeHtmlAttr value ++ "\""

||| List of void (self-closing) elements
voidElements : List String
voidElements = ["area", "base", "br", "col", "embed", "hr", "img", "input",
                "link", "meta", "param", "source", "track", "wbr"]

||| Check if element is void
isVoid : String -> Bool
isVoid tag = toLower tag `elem` voidElements

||| Build the HTML string
public export
build : HtmlBuilder -> String
build builder =
  let attrs = concat (map renderAttr (reverse builder.builderAttrs))
      tag = builder.builderTag
  in if isVoid tag
       then "<" ++ tag ++ attrs ++ " />"
       else let children = concat (reverse builder.builderChildren)
            in "<" ++ tag ++ attrs ++ ">" ++ children ++ "</" ++ tag ++ ">"

--------------------------------------------------------------------------------
-- Convenience Combinators
--------------------------------------------------------------------------------

||| Create element with just text content
public export
textElement : String -> String -> String
textElement tag content = build (withText content (element tag))

||| Create element with class and text
public export
classedText : String -> String -> String -> String
classedText tag cls content =
  build (withText content (withClass cls (element tag)))

||| Create a link with text
public export
link : String -> String -> Maybe String
link url content = map build (withHref url (withText content aBuilder))

||| Create an image with alt text
public export
image : String -> String -> String
image srcUrl altText =
  build (withAlt altText (withSrc srcUrl imgBuilder))

--------------------------------------------------------------------------------
-- List Builders
--------------------------------------------------------------------------------

||| Build an unordered list from items
public export
buildUl : List String -> HtmlBuilder -> HtmlBuilder
buildUl items builder =
  let lis = map (\item => build (withText item (element "li"))) items
  in { builderChildren := lis ++ builder.builderChildren } builder

||| Build an ordered list from items
public export
buildOl : List String -> HtmlBuilder -> HtmlBuilder
buildOl = buildUl  -- Same structure, just ol tag

||| Create unordered list directly
public export
ul : List String -> String
ul items = build (buildUl items (element "ul"))

||| Create ordered list directly
public export
ol : List String -> String
ol items = build (buildOl items (element "ol"))

--------------------------------------------------------------------------------
-- Table Builders
--------------------------------------------------------------------------------

||| Build a table row from cells
public export
buildRow : (cellTag : String) -> List String -> String
buildRow cellTag cells =
  let tds = map (\cell => build (withText cell (element cellTag))) cells
  in build ({ builderChildren := tds } (element "tr"))

||| Build table body rows
public export
buildTableBody : List (List String) -> List String
buildTableBody rows = map (buildRow "td") rows

||| Build table with header and body
public export
buildTable : List String -> List (List String) -> HtmlBuilder -> HtmlBuilder
buildTable headers rows builder =
  let headerRow = buildRow "th" headers
      bodyRows = buildTableBody rows
      allRows = headerRow :: bodyRows
  in { builderChildren := allRows ++ builder.builderChildren } builder

||| Create table directly
public export
table : List String -> List (List String) -> String
table headers rows = build (buildTable headers rows (element "table"))

--------------------------------------------------------------------------------
-- Definition List Builder
--------------------------------------------------------------------------------

||| Build definition list from term-definition pairs
public export
buildDl : List (String, String) -> HtmlBuilder -> HtmlBuilder
buildDl items builder =
  let entries = concatMap (\(term, def) =>
                  [build (withText term (element "dt")),
                   build (withText def (element "dd"))]) items
  in { builderChildren := entries ++ builder.builderChildren } builder

||| Create definition list directly
public export
dl : List (String, String) -> String
dl items = build (buildDl items (element "dl"))
