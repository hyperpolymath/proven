-- SPDX-License-Identifier: Apache-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell <jonathan.jewell@open.ac.uk>
||| FFI exports for SafeMarkdown operations
|||
||| This module exports markdown generation with XSS prevention to the C ABI
||| via Idris2's RefC backend. All functions are proven total and escape special chars.
|||
||| Return conventions:
||| - Markdown generation → String (escaped and formatted)
||| - Heading level → Int (1-6)
||| - List style → Int (0=Unordered, 1=Ordered)
|||
||| CRITICAL: All text content is escaped to prevent markdown injection.
|||           URLs are separately escaped for security.
|||
||| Escaping: \, `, *, _, {, }, [, ], (, ), #, +, -, ., !, |
module Proven.FFI.SafeMarkdown

import Proven.SafeMarkdown
import Proven.Core
import Data.String

%default total

--------------------------------------------------------------------------------
-- FFI Result Encoding
--------------------------------------------------------------------------------

||| Encode HeadingLevel as Int
encodeHeadingLevel : HeadingLevel -> Int
encodeHeadingLevel H1 = 1
encodeHeadingLevel H2 = 2
encodeHeadingLevel H3 = 3
encodeHeadingLevel H4 = 4
encodeHeadingLevel H5 = 5
encodeHeadingLevel H6 = 6

||| Decode Int to HeadingLevel
decodeHeadingLevel : Int -> Maybe HeadingLevel
decodeHeadingLevel 1 = Just H1
decodeHeadingLevel 2 = Just H2
decodeHeadingLevel 3 = Just H3
decodeHeadingLevel 4 = Just H4
decodeHeadingLevel 5 = Just H5
decodeHeadingLevel 6 = Just H6
decodeHeadingLevel _ = Nothing

||| Encode ListStyle as Int
encodeListStyle : ListStyle -> Int
encodeListStyle Unordered = 0
encodeListStyle Ordered = 1

||| Decode Int to ListStyle
decodeListStyle : Int -> Maybe ListStyle
decodeListStyle 0 = Just Unordered
decodeListStyle 1 = Just Ordered
decodeListStyle _ = Nothing

||| Parse list from newline-separated string
parseLines : String -> List String
parseLines s = if s == "" then [] else split (== '\n') s

--------------------------------------------------------------------------------
-- Escaping
--------------------------------------------------------------------------------

%export
proven_idris_md_escape : String -> String
proven_idris_md_escape s = escape s

%export
proven_idris_md_escape_url : String -> String
proven_idris_md_escape_url s = escapeUrl s

--------------------------------------------------------------------------------
-- Inline Elements
--------------------------------------------------------------------------------

%export
proven_idris_md_bold : String -> String
proven_idris_md_bold s = bold s

%export
proven_idris_md_italic : String -> String
proven_idris_md_italic s = italic s

%export
proven_idris_md_bold_italic : String -> String
proven_idris_md_bold_italic s = boldItalic s

%export
proven_idris_md_strikethrough : String -> String
proven_idris_md_strikethrough s = strikethrough s

%export
proven_idris_md_code : String -> String
proven_idris_md_code s = code s

%export
proven_idris_md_link : String -> String -> String
proven_idris_md_link text url = link text url

%export
proven_idris_md_link_with_title : String -> String -> String -> String
proven_idris_md_link_with_title text url title = linkWithTitle text url title

%export
proven_idris_md_image : String -> String -> String
proven_idris_md_image alt url = image alt url

%export
proven_idris_md_image_with_title : String -> String -> String -> String
proven_idris_md_image_with_title alt url title = imageWithTitle alt url title

--------------------------------------------------------------------------------
-- Block Elements
--------------------------------------------------------------------------------

%export
proven_idris_md_heading : Int -> String -> String
proven_idris_md_heading level text =
  case decodeHeadingLevel level of
    Nothing => escape text  -- Invalid level, just escape
    Just lvl => heading lvl text

%export
proven_idris_md_paragraph : String -> String
proven_idris_md_paragraph s = paragraph s

%export
proven_idris_md_blockquote : String -> String
proven_idris_md_blockquote s = blockquote s

%export
proven_idris_md_blockquote_lines : String -> String
proven_idris_md_blockquote_lines lines =
  blockquoteLines (parseLines lines)

%export
proven_idris_md_code_block : String -> String -> String
proven_idris_md_code_block lang content =
  let language = if lang == "" then Plain else Lang lang
  in codeBlock language content

%export
proven_idris_md_horizontal_rule : String
proven_idris_md_horizontal_rule = horizontalRule

--------------------------------------------------------------------------------
-- Lists
--------------------------------------------------------------------------------

%export
proven_idris_md_list : Int -> String -> String
proven_idris_md_list styleCode items =
  case decodeListStyle styleCode of
    Nothing => ""  -- Invalid style
    Just style => list style (parseLines items)

%export
proven_idris_md_unordered_list : String -> String
proven_idris_md_unordered_list items = list Unordered (parseLines items)

%export
proven_idris_md_ordered_list : String -> String
proven_idris_md_ordered_list items = list Ordered (parseLines items)

--------------------------------------------------------------------------------
-- Heading Level Helpers
--------------------------------------------------------------------------------

%export
proven_idris_md_is_valid_heading_level : Int -> Int
proven_idris_md_is_valid_heading_level level =
  case decodeHeadingLevel level of
    Nothing => 0
    Just _ => 1

%export
proven_idris_md_encode_heading_level : String -> Int
proven_idris_md_encode_heading_level s =
  case toLower s of
    "h1" => 1
    "h2" => 2
    "h3" => 3
    "h4" => 4
    "h5" => 5
    "h6" => 6
    _ => (-1)

--------------------------------------------------------------------------------
-- List Style Helpers
--------------------------------------------------------------------------------

%export
proven_idris_md_is_valid_list_style : Int -> Int
proven_idris_md_is_valid_list_style styleCode =
  case decodeListStyle styleCode of
    Nothing => 0
    Just _ => 1

%export
proven_idris_md_encode_list_style : String -> Int
proven_idris_md_encode_list_style s =
  case toLower s of
    "unordered" => 0
    "ul" => 0
    "ordered" => 1
    "ol" => 1
    _ => (-1)

--------------------------------------------------------------------------------
-- Content Validation
--------------------------------------------------------------------------------

%export
proven_idris_md_is_safe_url : String -> Int
proven_idris_md_is_safe_url url =
  let lower = toLower url
  in if isPrefixOf "http://" lower || isPrefixOf "https://" lower
       then 1
       else if isPrefixOf "javascript:" lower || isPrefixOf "data:" lower
         then 0  -- Potentially unsafe
         else 1  -- Relative URLs okay

%export
proven_idris_md_has_markdown_special : String -> Int
proven_idris_md_has_markdown_special s =
  let specials = ['\\', '`', '*', '_', '{', '}', '[', ']', '(', ')', '#', '+', '-', '.', '!', '|']
  in if any (\c => elem c specials) (unpack s)
       then 1
       else 0

--------------------------------------------------------------------------------
-- Error Messages
--------------------------------------------------------------------------------

%export
proven_idris_md_friendly_error : String -> String
proven_idris_md_friendly_error errorMsg =
  if isInfixOf "injection" (toLower errorMsg) || isInfixOf "xss" (toLower errorMsg)
    then "Markdown injection attempt detected (special chars not escaped)"
  else if isInfixOf "heading" (toLower errorMsg)
    then "Invalid heading level (must be 1-6)"
  else if isInfixOf "list" (toLower errorMsg)
    then "Invalid list style"
  else if isInfixOf "url" (toLower errorMsg)
    then "Unsafe URL scheme (javascript:, data: not allowed)"
  else
    "Markdown generation error"
