-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
||| Safe escaping functions for various contexts
|||
||| This module provides escaping functions to prevent injection attacks
||| in SQL, HTML, JavaScript, and other contexts.
module Proven.SafeString.Escape

import Proven.Core
import Data.List
import Data.String

%default total

--------------------------------------------------------------------------------
-- SQL Escaping
--------------------------------------------------------------------------------

||| Escape a string for safe use in SQL queries
||| This escapes single quotes by doubling them
||| Note: Parameterized queries are still preferred over escaping
public export
escapeSQL : String -> String
escapeSQL s = pack (go (unpack s))
  where
    go : List Char -> List Char
    go [] = []
    go ('\'' :: cs) = '\'' :: '\'' :: go cs  -- Double single quotes
    go ('\\' :: cs) = '\\' :: '\\' :: go cs  -- Double backslashes
    go ('\x00' :: cs) = go cs                -- Remove null bytes
    go (c :: cs) = c :: go cs

||| Escape for SQL LIKE patterns (escapes %, _, and \)
public export
escapeSQLLike : String -> String
escapeSQLLike s = pack (go (unpack s))
  where
    go : List Char -> List Char
    go [] = []
    go ('%' :: cs) = '\\' :: '%' :: go cs
    go ('_' :: cs) = '\\' :: '_' :: go cs
    go ('\\' :: cs) = '\\' :: '\\' :: go cs
    go (c :: cs) = c :: go cs

--------------------------------------------------------------------------------
-- HTML Escaping
--------------------------------------------------------------------------------

||| Escape string for safe HTML content (prevents XSS)
public export
escapeHTML : String -> String
escapeHTML s = pack (go (unpack s))
  where
    go : List Char -> List Char
    go [] = []
    go ('&' :: cs) = unpack "&amp;" ++ go cs
    go ('<' :: cs) = unpack "&lt;" ++ go cs
    go ('>' :: cs) = unpack "&gt;" ++ go cs
    go ('"' :: cs) = unpack "&quot;" ++ go cs
    go ('\'' :: cs) = unpack "&#x27;" ++ go cs
    go (c :: cs) = c :: go cs

||| Escape for HTML attribute values (more restrictive)
public export
escapeHTMLAttr : String -> String
escapeHTMLAttr s = pack (go (unpack s))
  where
    go : List Char -> List Char
    go [] = []
    go ('&' :: cs) = unpack "&amp;" ++ go cs
    go ('<' :: cs) = unpack "&lt;" ++ go cs
    go ('>' :: cs) = unpack "&gt;" ++ go cs
    go ('"' :: cs) = unpack "&quot;" ++ go cs
    go ('\'' :: cs) = unpack "&#x27;" ++ go cs
    go ('/' :: cs) = unpack "&#x2F;" ++ go cs
    go ('`' :: cs) = unpack "&#x60;" ++ go cs
    go ('=' :: cs) = unpack "&#x3D;" ++ go cs
    go (c :: cs) = c :: go cs

--------------------------------------------------------------------------------
-- JavaScript Escaping
--------------------------------------------------------------------------------

||| Escape string for safe use in JavaScript string literals
public export
escapeJS : String -> String
escapeJS s = pack (go (unpack s))
  where
    go : List Char -> List Char
    go [] = []
    go ('\\' :: cs) = '\\' :: '\\' :: go cs
    go ('"' :: cs) = '\\' :: '"' :: go cs
    go ('\'' :: cs) = '\\' :: '\'' :: go cs
    go ('\n' :: cs) = '\\' :: 'n' :: go cs
    go ('\r' :: cs) = '\\' :: 'r' :: go cs
    go ('\t' :: cs) = '\\' :: 't' :: go cs
    go ('<' :: cs) = '\\' :: 'x' :: '3' :: 'C' :: go cs  -- Prevent </script> injection
    go ('>' :: cs) = '\\' :: 'x' :: '3' :: 'E' :: go cs
    go (c :: cs) = c :: go cs

padLeft : Nat -> Char -> String -> String
padLeft n pad s =
  let len = length s in
  if len >= n
    then s
    else pack (unpack (replicate (minus n len) pad) ++ unpack s)

partial
unicodeEscape : Char -> String
unicodeEscape c = "\\u" ++ Proven.SafeString.Escape.padLeft 4 '0' (toHex (cast (ord c)))
  where
    partial toHex : Nat -> String
    toHex n = pack (toHexHelper n [])
      where
        hexDigit : Nat -> Char
        hexDigit d = if d < 10 then chr (ord '0' + cast d) else chr (ord 'a' + cast d - 10)

        toHexHelper : Nat -> List Char -> List Char
        toHexHelper 0 [] = ['0']
        toHexHelper 0 acc = acc
        toHexHelper m acc = toHexHelper (m `div` 16) (hexDigit (m `mod` 16) :: acc)

||| Escape for JSON string values
public export
partial
escapeJSON : String -> String
escapeJSON s = pack (go (unpack s))
  where
    go : List Char -> List Char
    go [] = []
    go ('\\' :: cs) = '\\' :: '\\' :: go cs
    go ('"' :: cs) = '\\' :: '"' :: go cs
    go ('\n' :: cs) = '\\' :: 'n' :: go cs
    go ('\r' :: cs) = '\\' :: 'r' :: go cs
    go ('\t' :: cs) = '\\' :: 't' :: go cs
    go ('\x08' :: cs) = '\\' :: 'b' :: go cs  -- Backspace
    go ('\x0C' :: cs) = '\\' :: 'f' :: go cs  -- Form feed
    go (c :: cs) =
      if ord c < 0x20
        then unpack (unicodeEscape c) ++ go cs
        else c :: go cs

--------------------------------------------------------------------------------
-- URL Escaping
--------------------------------------------------------------------------------

||| Check if character is safe in URL (unreserved characters per RFC 3986)
public export
isURLSafe : Char -> Bool
isURLSafe c =
  isAlphaNum c || c == '-' || c == '_' || c == '.' || c == '~'

||| Percent-encode a single character
public export
percentEncode : Char -> String
percentEncode c =
  if isURLSafe c
    then singleton c
    else "%" ++ toHex (ord c)
  where
    hexDigit : Int -> Char
    hexDigit d = if d < 10 then chr (ord '0' + d) else chr (ord 'A' + d - 10)

    toHex : Int -> String
    toHex n =
      let hi = n `div` 16
          lo = n `mod` 16
      in pack [hexDigit hi, hexDigit lo]

||| URL-encode a string (for query parameters)
public export
urlEncode : String -> String
urlEncode s = concat (map percentEncode (unpack s))

||| URL-encode preserving slashes (for paths)
public export
urlEncodePath : String -> String
urlEncodePath s = concat (map encodePart (unpack s))
  where
    encodePart : Char -> String
    encodePart '/' = "/"
    encodePart c = percentEncode c

--------------------------------------------------------------------------------
-- Shell Escaping
--------------------------------------------------------------------------------

||| Escape string for safe use in shell commands
||| Uses single quotes which prevent all interpretation
public export
escapeShell : String -> String
escapeShell s = "'" ++ escape (unpack s) ++ "'"
  where
    escape : List Char -> String
    escape [] = ""
    escape ('\'' :: cs) = "'\\''" ++ escape cs  -- End quote, escaped quote, start quote
    escape (c :: cs) = singleton c ++ escape cs

||| Escape for shell double quotes (allows variable expansion)
public export
escapeShellDouble : String -> String
escapeShellDouble s = "\"" ++ pack (go (unpack s)) ++ "\""
  where
    go : List Char -> List Char
    go [] = []
    go ('$' :: cs) = '\\' :: '$' :: go cs
    go ('`' :: cs) = '\\' :: '`' :: go cs
    go ('\\' :: cs) = '\\' :: '\\' :: go cs
    go ('"' :: cs) = '\\' :: '"' :: go cs
    go ('!' :: cs) = '\\' :: '!' :: go cs
    go (c :: cs) = c :: go cs

--------------------------------------------------------------------------------
-- Regex Escaping
--------------------------------------------------------------------------------

||| Escape string for use in regular expressions
public export
escapeRegex : String -> String
escapeRegex s = pack (go (unpack s))
  where
    metaChars : List Char
    metaChars = unpack "\\^$.|?*+()[]{}"

    go : List Char -> List Char
    go [] = []
    go (c :: cs) =
      if c `elem` metaChars
        then '\\' :: c :: go cs
        else c :: go cs

--------------------------------------------------------------------------------
-- XML Escaping
--------------------------------------------------------------------------------

||| Escape string for XML content
public export
escapeXML : String -> String
escapeXML = escapeHTML  -- Same as HTML for content

||| Escape for XML CDATA section
public export
escapeCDATA : String -> String
escapeCDATA s = pack (go (unpack s))
  where
    -- CDATA cannot contain "]]>"
    go : List Char -> List Char
    go [] = []
    go (']' :: ']' :: '>' :: cs) = unpack "]]]]><![CDATA[>" ++ go cs
    go (c :: cs) = c :: go cs

--------------------------------------------------------------------------------
-- CSV Escaping
--------------------------------------------------------------------------------

||| Escape string for CSV (RFC 4180)
public export
escapeCSV : String -> String
escapeCSV s =
  if needsQuoting s
    then "\"" ++ pack (escapeQuotes (unpack s)) ++ "\""
    else s
  where
    needsQuoting : String -> Bool
    needsQuoting str = any (\c => c == ',' || c == '"' || c == '\n' || c == '\r') (unpack str)

    escapeQuotes : List Char -> List Char
    escapeQuotes [] = []
    escapeQuotes ('"' :: cs) = '"' :: '"' :: escapeQuotes cs
    escapeQuotes (c :: cs) = c :: escapeQuotes cs
