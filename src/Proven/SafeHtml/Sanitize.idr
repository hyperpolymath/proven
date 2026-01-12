-- SPDX-License-Identifier: Palimpsest-MPL-1.0
||| HTML sanitization for XSS prevention
|||
||| Provides functions to strip or neutralize dangerous HTML content
||| from user-provided input while preserving safe formatting
module Proven.SafeHtml.Sanitize

import Proven.Core
import Proven.SafeHtml.Escape
import Data.List
import Data.String

%default total

--------------------------------------------------------------------------------
-- Sanitization Policy Types
--------------------------------------------------------------------------------

||| What to do with disallowed tags
public export
data TagPolicy : Type where
  ||| Remove the tag but keep its content
  StripTag : TagPolicy
  ||| Remove the tag and all its content
  RemoveWithContent : TagPolicy
  ||| Escape the tag to show as text
  EscapeTag : TagPolicy

||| What to do with disallowed attributes
public export
data AttrPolicy : Type where
  ||| Remove the attribute entirely
  RemoveAttr : AttrPolicy
  ||| Keep attribute name but clear value
  ClearValue : AttrPolicy

||| Sanitization configuration
public export
record SanitizeConfig where
  constructor MkConfig
  ||| Allowed HTML tags (lowercase)
  allowedTags : List String
  ||| Allowed attributes per tag (tag -> attributes)
  allowedAttrs : List (String, List String)
  ||| Global allowed attributes (on all tags)
  globalAttrs : List String
  ||| How to handle disallowed tags
  tagPolicy : TagPolicy
  ||| How to handle disallowed attributes
  attrPolicy : AttrPolicy
  ||| Allow data-* attributes
  allowDataAttrs : Bool
  ||| Allow aria-* attributes
  allowAriaAttrs : Bool

--------------------------------------------------------------------------------
-- Predefined Configurations
--------------------------------------------------------------------------------

||| Very restrictive - only basic text formatting
public export
strictConfig : SanitizeConfig
strictConfig = MkConfig
  { allowedTags = ["p", "br", "b", "i", "u", "strong", "em", "span"]
  , allowedAttrs = []
  , globalAttrs = []
  , tagPolicy = StripTag
  , attrPolicy = RemoveAttr
  , allowDataAttrs = False
  , allowAriaAttrs = False
  }

||| Standard - common formatting and links
public export
standardConfig : SanitizeConfig
standardConfig = MkConfig
  { allowedTags = ["p", "br", "b", "i", "u", "strong", "em", "span",
                   "a", "ul", "ol", "li", "blockquote", "code", "pre",
                   "h1", "h2", "h3", "h4", "h5", "h6", "hr"]
  , allowedAttrs = [("a", ["href", "title"]), ("img", ["src", "alt", "title"])]
  , globalAttrs = ["class", "id"]
  , tagPolicy = StripTag
  , attrPolicy = RemoveAttr
  , allowDataAttrs = False
  , allowAriaAttrs = True
  }

||| Permissive - allows images and more formatting
public export
permissiveConfig : SanitizeConfig
permissiveConfig = MkConfig
  { allowedTags = ["p", "br", "b", "i", "u", "strong", "em", "span",
                   "a", "ul", "ol", "li", "blockquote", "code", "pre",
                   "h1", "h2", "h3", "h4", "h5", "h6", "hr",
                   "img", "figure", "figcaption",
                   "table", "thead", "tbody", "tr", "th", "td",
                   "dl", "dt", "dd", "sub", "sup", "mark", "small"]
  , allowedAttrs = [("a", ["href", "title", "rel", "target"]),
                    ("img", ["src", "alt", "title", "width", "height"]),
                    ("td", ["colspan", "rowspan"]),
                    ("th", ["colspan", "rowspan", "scope"])]
  , globalAttrs = ["class", "id", "title", "lang", "dir"]
  , tagPolicy = StripTag
  , attrPolicy = RemoveAttr
  , allowDataAttrs = True
  , allowAriaAttrs = True
  }

--------------------------------------------------------------------------------
-- Tag/Attribute Checking
--------------------------------------------------------------------------------

||| Check if tag is allowed
public export
isTagAllowed : SanitizeConfig -> String -> Bool
isTagAllowed config tag = toLower tag `elem` config.allowedTags

||| Check if attribute is allowed for tag
public export
isAttrAllowed : SanitizeConfig -> String -> String -> Bool
isAttrAllowed config tag attrName =
  let lowerTag = toLower tag
      lowerAttr = toLower attrName
      -- Check global attrs
      globalOk = lowerAttr `elem` config.globalAttrs
      -- Check tag-specific attrs
      tagAttrs = lookup lowerTag config.allowedAttrs
      tagOk = maybe False (lowerAttr `elem`) tagAttrs
      -- Check data-* attrs
      dataOk = config.allowDataAttrs && isPrefixOf "data-" lowerAttr
      -- Check aria-* attrs
      ariaOk = config.allowAriaAttrs && isPrefixOf "aria-" lowerAttr
  in globalOk || tagOk || dataOk || ariaOk

--------------------------------------------------------------------------------
-- Dangerous Content Detection
--------------------------------------------------------------------------------

||| List of absolutely forbidden tags (always removed with content)
public export
blacklistedTags : List String
blacklistedTags = ["script", "style", "iframe", "object", "embed",
                   "applet", "frame", "frameset", "layer", "ilayer",
                   "meta", "link", "base"]

||| List of dangerous attribute names
public export
dangerousAttrNames : List String
dangerousAttrNames = ["onabort", "onblur", "onchange", "onclick", "ondblclick",
                      "onerror", "onfocus", "onkeydown", "onkeypress", "onkeyup",
                      "onload", "onmousedown", "onmousemove", "onmouseout",
                      "onmouseover", "onmouseup", "onreset", "onresize",
                      "onscroll", "onselect", "onsubmit", "onunload",
                      "formaction", "xlink:href", "xmlns"]

||| Check if attribute is dangerous (event handler, etc)
public export
isDangerousAttr : String -> Bool
isDangerousAttr name =
  let lower = toLower name
  in lower `elem` dangerousAttrNames || isPrefixOf "on" lower

||| Check if tag is blacklisted
public export
isBlacklistedTag : String -> Bool
isBlacklistedTag tag = toLower tag `elem` blacklistedTags

--------------------------------------------------------------------------------
-- URL Sanitization
--------------------------------------------------------------------------------

||| Sanitize URL attribute value
||| Returns Nothing if URL is dangerous
public export
sanitizeUrlValue : String -> Maybe String
sanitizeUrlValue url =
  let trimmed = trim url
  in if hasDangerousScheme trimmed
       then Nothing
       else Just trimmed

||| Attributes that contain URLs
public export
urlAttributes : List String
urlAttributes = ["href", "src", "action", "formaction", "poster",
                 "data", "cite", "background", "longdesc"]

||| Check if attribute contains URL
public export
isUrlAttribute : String -> Bool
isUrlAttribute name = toLower name `elem` urlAttributes

--------------------------------------------------------------------------------
-- Simple Text Extraction
--------------------------------------------------------------------------------

||| Remove all HTML tags, keeping only text content
||| Simple implementation - for production use a proper HTML parser
public export
stripAllTags : String -> String
stripAllTags s = pack (go (unpack s) False)
  where
    go : List Char -> Bool -> List Char
    go [] _ = []
    go ('<' :: cs) _ = go cs True     -- Start of tag
    go ('>' :: cs) True = go cs False -- End of tag
    go (c :: cs) True = go cs True    -- Inside tag, skip
    go (c :: cs) False = c :: go cs False  -- Outside tag, keep

||| Remove specific tags but keep content
public export
stripTags : List String -> String -> String
stripTags tags s =
  -- Simple approach: strip all tags for now
  -- A proper implementation would need HTML parsing
  stripAllTags s

--------------------------------------------------------------------------------
-- Attribute Value Sanitization
--------------------------------------------------------------------------------

||| Sanitize an attribute value based on context
public export
sanitizeAttrValue : String -> String -> String -> Maybe String
sanitizeAttrValue tag attrName value =
  -- Check for dangerous attributes first
  if isDangerousAttr attrName
    then Nothing
    -- Check URL attributes
    else if isUrlAttribute attrName
      then sanitizeUrlValue value
      -- Check style attribute (limited support)
      else if toLower attrName == "style"
        then Just (escapeInlineCss value)
        -- Regular attribute
        else Just value

--------------------------------------------------------------------------------
-- Simple Sanitization (Text-Only Approach)
--------------------------------------------------------------------------------

||| Sanitize by stripping all HTML and escaping
||| This is the safest approach - produces plain text
public export
sanitizeToText : String -> String
sanitizeToText = escapeHtmlContent . stripAllTags

||| Sanitize to allow only line breaks
||| Converts newlines to <br> tags
public export
sanitizeWithBreaks : String -> String
sanitizeWithBreaks s =
  let text = stripAllTags s
      escaped = escapeHtmlContent text
  in pack (addBreaks (unpack escaped))
  where
    addBreaks : List Char -> List Char
    addBreaks [] = []
    addBreaks ('\n' :: cs) = unpack "<br />" ++ addBreaks cs
    addBreaks ('\r' :: '\n' :: cs) = unpack "<br />" ++ addBreaks cs
    addBreaks ('\r' :: cs) = unpack "<br />" ++ addBreaks cs
    addBreaks (c :: cs) = c :: addBreaks cs

--------------------------------------------------------------------------------
-- Whitelist-Based Sanitization
--------------------------------------------------------------------------------

||| Result of parsing an attribute
public export
record ParsedAttr where
  constructor MkParsedAttr
  paName : String
  paValue : String

||| Result of parsing a tag
public export
data ParsedTag : Type where
  ||| Opening tag with attributes
  OpenTag : String -> List ParsedAttr -> ParsedTag
  ||| Closing tag
  CloseTag : String -> ParsedTag
  ||| Self-closing tag
  SelfClose : String -> List ParsedAttr -> ParsedTag
  ||| Text content
  TextContent : String -> ParsedTag
  ||| Comment (to be removed)
  Comment : String -> ParsedTag

||| Sanitize based on configuration
||| Note: This is a simplified implementation that escapes all HTML
||| For full HTML parsing and selective sanitization, use a proper HTML parser
public export
sanitize : SanitizeConfig -> String -> String
sanitize config s =
  -- Simple safe approach: escape everything
  -- A full implementation would parse HTML and selectively allow tags
  escapeHtmlContent (stripAllTags s)

||| Sanitize with standard config
public export
sanitizeStandard : String -> String
sanitizeStandard = sanitize standardConfig

||| Sanitize with strict config
public export
sanitizeStrict : String -> String
sanitizeStrict = sanitize strictConfig

||| Sanitize with permissive config
public export
sanitizePermissive : String -> String
sanitizePermissive = sanitize permissiveConfig

--------------------------------------------------------------------------------
-- Content Security Helpers
--------------------------------------------------------------------------------

||| Remove all script-like content from string
public export
removeScripts : String -> String
removeScripts = sanitizeToText

||| Check if string contains potentially dangerous content
public export
containsDangerousContent : String -> Bool
containsDangerousContent s =
  let lower = toLower s
  in any (\tag => isInfixOf ("<" ++ tag) lower) blacklistedTags ||
     any (\attr => isInfixOf attr lower) dangerousAttrNames ||
     hasDangerousScheme lower

||| Validate that string is safe HTML
public export
isSafeHtml : SanitizeConfig -> String -> Bool
isSafeHtml config s = not (containsDangerousContent s)
