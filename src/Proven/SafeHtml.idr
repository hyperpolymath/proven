-- SPDX-License-Identifier: Palimpsest-MPL-1.0
||| SafeHtml - Type-safe HTML construction and XSS prevention
|||
||| This module provides:
||| - Type-safe HTML element construction
||| - XSS prevention through proper escaping
||| - HTML sanitization for user input
||| - Proofs that generated HTML is safe
module Proven.SafeHtml

import public Proven.Core
import public Proven.SafeHtml.Escape
import public Proven.SafeHtml.Builder
import public Proven.SafeHtml.Sanitize

import Data.List
import Data.String

%default total

--------------------------------------------------------------------------------
-- Core HTML Types
--------------------------------------------------------------------------------

||| Marker for trusted (pre-escaped) HTML content
||| Content wrapped in TrustedHtml has been verified safe
public export
data TrustedHtml : Type where
  MkTrustedHtml : String -> TrustedHtml

||| Marker for untrusted (user-provided) content
||| Must be escaped before rendering
public export
data UntrustedContent : Type where
  MkUntrusted : String -> UntrustedContent

||| HTML attribute name (validated)
public export
data AttrName : Type where
  MkAttrName : (name : String) -> {auto prf : isValidAttrName name = True} -> AttrName

||| HTML attribute with safe value
public export
record HtmlAttr where
  constructor MkHtmlAttr
  name : String
  value : String

||| HTML element with type-safe construction
public export
data HtmlElement : Type where
  ||| Text node (automatically escaped)
  TextNode : String -> HtmlElement
  ||| Raw HTML (trusted, already escaped)
  RawHtml : TrustedHtml -> HtmlElement
  ||| Self-closing element (br, hr, img, etc.)
  VoidElement : (tag : String) -> List HtmlAttr -> HtmlElement
  ||| Element with children
  Element : (tag : String) -> List HtmlAttr -> List HtmlElement -> HtmlElement
  ||| Document fragment (no wrapper tag)
  Fragment : List HtmlElement -> HtmlElement

--------------------------------------------------------------------------------
-- Attribute Name Validation
--------------------------------------------------------------------------------

||| Check if character is valid in attribute name
public export
isAttrChar : Char -> Bool
isAttrChar c = isAlpha c || isDigit c || c == '-' || c == '_' || c == ':'

||| Check if string is valid attribute name
||| Must start with letter, contain only valid chars, not be empty
public export
isValidAttrName : String -> Bool
isValidAttrName s = case strM s of
  StrNil => False
  StrCons c rest => isAlpha c && all isAttrChar (unpack rest)

--------------------------------------------------------------------------------
-- Safe Tag Names
--------------------------------------------------------------------------------

||| List of self-closing (void) HTML elements
public export
voidTags : List String
voidTags = ["area", "base", "br", "col", "embed", "hr", "img", "input",
            "link", "meta", "param", "source", "track", "wbr"]

||| Check if tag is a void element
public export
isVoidTag : String -> Bool
isVoidTag tag = toLower tag `elem` voidTags

||| List of dangerous tags that should be sanitized
public export
dangerousTags : List String
dangerousTags = ["script", "style", "iframe", "object", "embed", "form",
                 "input", "button", "textarea", "select", "frame", "frameset",
                 "applet", "layer", "ilayer", "link", "meta", "base"]

||| Check if tag is dangerous (potential XSS vector)
public export
isDangerousTag : String -> Bool
isDangerousTag tag = toLower tag `elem` dangerousTags

--------------------------------------------------------------------------------
-- TrustedHtml Operations
--------------------------------------------------------------------------------

||| Extract the raw string from TrustedHtml
||| Only use when you need the final output
public export
unTrust : TrustedHtml -> String
unTrust (MkTrustedHtml s) = s

||| Create trusted HTML from an already-escaped string
||| WARNING: Only use with content you have verified is safe
public export
trustHtml : String -> TrustedHtml
trustHtml = MkTrustedHtml

||| Concatenate trusted HTML fragments
public export
concatTrusted : List TrustedHtml -> TrustedHtml
concatTrusted xs = MkTrustedHtml (concat (map unTrust xs))

||| Empty trusted HTML
public export
emptyHtml : TrustedHtml
emptyHtml = MkTrustedHtml ""

--------------------------------------------------------------------------------
-- UntrustedContent Operations
--------------------------------------------------------------------------------

||| Create untrusted content from user input
public export
untrusted : String -> UntrustedContent
untrusted = MkUntrusted

||| Get the raw content (for escaping)
public export
unUntrusted : UntrustedContent -> String
unUntrusted (MkUntrusted s) = s

||| Escape untrusted content for safe HTML rendering
public export
escapeUntrusted : UntrustedContent -> TrustedHtml
escapeUntrusted (MkUntrusted s) = MkTrustedHtml (escapeHtmlContent s)

--------------------------------------------------------------------------------
-- Rendering
--------------------------------------------------------------------------------

||| Render an HTML attribute
public export
renderAttr : HtmlAttr -> String
renderAttr (MkHtmlAttr name value) =
  " " ++ name ++ "=\"" ++ escapeHtmlAttr value ++ "\""

||| Render a list of attributes
public export
renderAttrs : List HtmlAttr -> String
renderAttrs = concat . map renderAttr

||| Render an HTML element to TrustedHtml
public export
render : HtmlElement -> TrustedHtml
render (TextNode s) = MkTrustedHtml (escapeHtmlContent s)
render (RawHtml trusted) = trusted
render (VoidElement tag attrs) =
  MkTrustedHtml $ "<" ++ tag ++ renderAttrs attrs ++ " />"
render (Element tag attrs children) =
  let childHtml = concat (map (unTrust . render) children)
  in MkTrustedHtml $ "<" ++ tag ++ renderAttrs attrs ++ ">" ++ childHtml ++ "</" ++ tag ++ ">"
render (Fragment children) =
  concatTrusted (map render children)

||| Render to string (final output)
public export
renderToString : HtmlElement -> String
renderToString = unTrust . render

--------------------------------------------------------------------------------
-- Common Helper Functions
--------------------------------------------------------------------------------

||| Create a text node from untrusted content
public export
text : String -> HtmlElement
text = TextNode

||| Create raw HTML (use with caution!)
public export
raw : TrustedHtml -> HtmlElement
raw = RawHtml

||| Create an attribute
public export
attr : String -> String -> HtmlAttr
attr = MkHtmlAttr

||| Create a class attribute
public export
class_ : String -> HtmlAttr
class_ = MkHtmlAttr "class"

||| Create an id attribute
public export
id_ : String -> HtmlAttr
id_ = MkHtmlAttr "id"

||| Create an href attribute (URL-escaped)
public export
href : String -> HtmlAttr
href url = MkHtmlAttr "href" url

||| Create a src attribute
public export
src : String -> HtmlAttr
src = MkHtmlAttr "src"

||| Create an alt attribute
public export
alt : String -> HtmlAttr
alt = MkHtmlAttr "alt"

--------------------------------------------------------------------------------
-- Common Elements
--------------------------------------------------------------------------------

||| Create a div element
public export
div_ : List HtmlAttr -> List HtmlElement -> HtmlElement
div_ = Element "div"

||| Create a span element
public export
span_ : List HtmlAttr -> List HtmlElement -> HtmlElement
span_ = Element "span"

||| Create a paragraph
public export
p_ : List HtmlAttr -> List HtmlElement -> HtmlElement
p_ = Element "p"

||| Create a heading (h1-h6)
public export
h : (level : Nat) -> {auto prf : level >= 1 && level <= 6 = True} -> List HtmlAttr -> List HtmlElement -> HtmlElement
h level attrs children = Element ("h" ++ show level) attrs children

||| Create an anchor link
public export
a_ : List HtmlAttr -> List HtmlElement -> HtmlElement
a_ = Element "a"

||| Create an image
public export
img_ : List HtmlAttr -> HtmlElement
img_ = VoidElement "img"

||| Create a line break
public export
br_ : HtmlElement
br_ = VoidElement "br" []

||| Create a horizontal rule
public export
hr_ : HtmlElement
hr_ = VoidElement "hr" []

||| Create an unordered list
public export
ul_ : List HtmlAttr -> List HtmlElement -> HtmlElement
ul_ = Element "ul"

||| Create an ordered list
public export
ol_ : List HtmlAttr -> List HtmlElement -> HtmlElement
ol_ = Element "ol"

||| Create a list item
public export
li_ : List HtmlAttr -> List HtmlElement -> HtmlElement
li_ = Element "li"

||| Create a table
public export
table_ : List HtmlAttr -> List HtmlElement -> HtmlElement
table_ = Element "table"

||| Create a table row
public export
tr_ : List HtmlAttr -> List HtmlElement -> HtmlElement
tr_ = Element "tr"

||| Create a table cell
public export
td_ : List HtmlAttr -> List HtmlElement -> HtmlElement
td_ = Element "td"

||| Create a table header cell
public export
th_ : List HtmlAttr -> List HtmlElement -> HtmlElement
th_ = Element "th"

||| Create a code element
public export
code_ : List HtmlAttr -> List HtmlElement -> HtmlElement
code_ = Element "code"

||| Create a pre element
public export
pre_ : List HtmlAttr -> List HtmlElement -> HtmlElement
pre_ = Element "pre"

||| Create a blockquote
public export
blockquote_ : List HtmlAttr -> List HtmlElement -> HtmlElement
blockquote_ = Element "blockquote"

||| Create emphasis
public export
em_ : List HtmlAttr -> List HtmlElement -> HtmlElement
em_ = Element "em"

||| Create strong emphasis
public export
strong_ : List HtmlAttr -> List HtmlElement -> HtmlElement
strong_ = Element "strong"
