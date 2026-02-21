-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
||| HTML-specific escaping functions
|||
||| Provides escaping for HTML content, attributes, and special contexts
||| Builds on SafeString.Escape with HTML-specific additions
module Proven.SafeHtml.Escape

import Proven.Core
import Proven.SafeString.Escape
import Data.List
import Data.String

%default total

--------------------------------------------------------------------------------
-- HTML Content Escaping
--------------------------------------------------------------------------------

||| Escape string for safe HTML content (prevents XSS)
||| Escapes: & < > " '
public export
escapeHtmlContent : String -> String
escapeHtmlContent = escapeHTML

||| Escape string for HTML attribute values
||| More restrictive than content escaping
||| Escapes: & < > " ' / ` =
public export
escapeHtmlAttr : String -> String
escapeHtmlAttr = escapeHTMLAttr

--------------------------------------------------------------------------------
-- URL in HTML Context
--------------------------------------------------------------------------------

||| Dangerous URL schemes that can execute code
public export
dangerousSchemes : List String
dangerousSchemes = ["javascript", "vbscript", "data", "about"]

||| Check if URL starts with a dangerous scheme
public export
hasDangerousScheme : String -> Bool
hasDangerousScheme url =
  let lower = toLower (pack (take 20 (unpack url)))
  in any (\scheme => isPrefixOf (scheme ++ ":") lower) dangerousSchemes

||| Sanitize URL for href attribute
||| Removes javascript: and other dangerous schemes
public export
sanitizeUrl : String -> Maybe String
sanitizeUrl url =
  if hasDangerousScheme (trim url)
    then Nothing
    else Just url

||| Escape URL for use in HTML attribute
||| Returns Nothing if URL has dangerous scheme
public export
escapeUrlAttr : String -> Maybe String
escapeUrlAttr url = map escapeHtmlAttr (sanitizeUrl url)

--------------------------------------------------------------------------------
-- Script Context Escaping
--------------------------------------------------------------------------------

||| Escape string for embedding in inline JavaScript within HTML
||| Prevents breaking out of script context
public export
escapeInlineScript : String -> String
escapeInlineScript s = pack (go (unpack s))
  where
    go : List Char -> List Char
    go [] = []
    go ('\\' :: cs) = '\\' :: '\\' :: go cs
    go ('"' :: cs) = '\\' :: '"' :: go cs
    go ('\'' :: cs) = '\\' :: '\'' :: go cs
    go ('\n' :: cs) = '\\' :: 'n' :: go cs
    go ('\r' :: cs) = '\\' :: 'r' :: go cs
    -- Prevent </script> injection
    go ('<' :: '/' :: cs) = '\\' :: 'x' :: '3' :: 'C' :: '/' :: go cs
    go ('<' :: cs) = '\\' :: 'x' :: '3' :: 'C' :: go cs
    go ('>' :: cs) = '\\' :: 'x' :: '3' :: 'E' :: go cs
    -- Prevent HTML comment injection
    go ('-' :: '-' :: '>' :: cs) = '\\' :: 'x' :: '2' :: 'D' :: '\\' :: 'x' :: '2' :: 'D' :: '>' :: go cs
    go (c :: cs) = c :: go cs

--------------------------------------------------------------------------------
-- CSS Context Escaping
--------------------------------------------------------------------------------

||| Escape string for embedding in inline CSS within HTML
||| Prevents CSS injection attacks
public export
escapeInlineCss : String -> String
escapeInlineCss s = pack (go (unpack s))
  where
    go : List Char -> List Char
    go [] = []
    go ('\\' :: cs) = '\\' :: '\\' :: go cs
    go ('"' :: cs) = '\\' :: '"' :: go cs
    go ('\'' :: cs) = '\\' :: '\'' :: go cs
    go ('<' :: cs) = '\\' :: '3' :: 'C' :: ' ' :: go cs
    go ('>' :: cs) = '\\' :: '3' :: 'E' :: ' ' :: go cs
    go ('/' :: cs) = '\\' :: '2' :: 'F' :: ' ' :: go cs
    -- Prevent url() injection
    go ('(' :: cs) = '\\' :: '2' :: '8' :: ' ' :: go cs
    go (')' :: cs) = '\\' :: '2' :: '9' :: ' ' :: go cs
    go (c :: cs) = c :: go cs

--------------------------------------------------------------------------------
-- Comment Escaping
--------------------------------------------------------------------------------

||| Escape string for HTML comment
||| Prevents breaking out of comment context
public export
escapeHtmlComment : String -> String
escapeHtmlComment s = pack (go (unpack s))
  where
    go : List Char -> List Char
    go [] = []
    -- Escape -- sequence which could close comment
    go ('-' :: '-' :: cs) = '-' :: ' ' :: '-' :: go cs
    -- Escape > after -- (already handled above but extra safety)
    go ('>' :: cs) = ' ' :: '>' :: go cs
    go (c :: cs) = c :: go cs

--------------------------------------------------------------------------------
-- Numeric Character References
--------------------------------------------------------------------------------

||| Convert character to HTML numeric entity
public export
toNumericEntity : Char -> String
toNumericEntity c = "&#" ++ show (ord c) ++ ";"

||| Convert character to hex entity
public export
partial
toHexEntity : Char -> String
toHexEntity c = "&#x" ++ toHex (ord c) ++ ";"
  where
    hexDigit : Int -> Char
    hexDigit d = if d < 10 then chr (ord '0' + d) else chr (ord 'A' + d - 10)

    toHex : Int -> String
    toHex 0 = "0"
    toHex n = pack (toHexHelper n [])
      where
        toHexHelper : Int -> List Char -> List Char
        toHexHelper 0 acc = acc
        toHexHelper m acc = toHexHelper (m `div` 16) (hexDigit (m `mod` 16) :: acc)

||| Escape all non-ASCII characters as numeric entities
||| Useful for ensuring ASCII-only output
public export
escapeNonAscii : String -> String
escapeNonAscii s = concat (map escape (unpack s))
  where
    escape : Char -> String
    escape c = if ord c < 128 then singleton c else toNumericEntity c

--------------------------------------------------------------------------------
-- Event Handler Escaping
--------------------------------------------------------------------------------

||| List of event handler attribute prefixes
public export
eventHandlerPrefixes : List String
eventHandlerPrefixes = ["on"]

||| Check if attribute name is an event handler
public export
isEventHandler : String -> Bool
isEventHandler name = isPrefixOf "on" (toLower name)

||| Escape value for event handler attribute
||| Same as inline script escaping but also entity-encoded
public export
escapeEventHandler : String -> String
escapeEventHandler = escapeHtmlAttr . escapeInlineScript
