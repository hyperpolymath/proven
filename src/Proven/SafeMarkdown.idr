-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
||| SafeMarkdown - Safe markdown generation and escaping
|||
||| This module provides safe markdown generation with
||| proper escaping to prevent injection attacks.
module Proven.SafeMarkdown
import Data.String
import Data.List

import public Proven.Core

%default total

--------------------------------------------------------------------------------
-- Markdown Elements
--------------------------------------------------------------------------------

||| Heading level (1-6)
public export
data HeadingLevel : Type where
  H1 : HeadingLevel
  H2 : HeadingLevel
  H3 : HeadingLevel
  H4 : HeadingLevel
  H5 : HeadingLevel
  H6 : HeadingLevel

||| Convert heading level to number
public export
headingNum : HeadingLevel -> Nat
headingNum H1 = 1
headingNum H2 = 2
headingNum H3 = 3
headingNum H4 = 4
headingNum H5 = 5
headingNum H6 = 6

||| List style
public export
data ListStyle : Type where
  Unordered : ListStyle
  Ordered : ListStyle

||| Code block language
public export
data CodeLanguage : Type where
  Plain : CodeLanguage
  Lang : String -> CodeLanguage

--------------------------------------------------------------------------------
-- Safe Escaping
--------------------------------------------------------------------------------

||| Characters that need escaping in markdown
markdownSpecial : List Char
markdownSpecial = ['\\', '`', '*', '_', '{', '}', '[', ']', '(', ')', '#', '+', '-', '.', '!', '|']

||| Escape a single character
escapeChar : Char -> String
escapeChar c =
  if any (== c) markdownSpecial
    then pack ['\\', c]
    else singleton c

||| Escape markdown special characters in text
public export
escape : String -> String
escape s = concat (map escapeChar (unpack s))

||| Escape for use in URLs (encode special chars)
public export
escapeUrl : String -> String
escapeUrl s = concat (map encodeChar (unpack s))
  where
    encodeChar : Char -> String
    encodeChar ' ' = "%20"
    encodeChar '<' = "%3C"
    encodeChar '>' = "%3E"
    encodeChar '"' = "%22"
    encodeChar c = singleton c

--------------------------------------------------------------------------------
-- Inline Elements
--------------------------------------------------------------------------------

||| Create bold text
public export
bold : String -> String
bold s = "**" ++ escape s ++ "**"

||| Create italic text
public export
italic : String -> String
italic s = "_" ++ escape s ++ "_"

||| Create bold italic text
public export
boldItalic : String -> String
boldItalic s = "***" ++ escape s ++ "***"

||| Create strikethrough text
public export
strikethrough : String -> String
strikethrough s = "~~" ++ escape s ++ "~~"

||| Create inline code
public export
code : String -> String
code s = "`" ++ s ++ "`"  -- No escaping inside code

||| Create a link
public export
link : (text : String) -> (url : String) -> String
link text url = "[" ++ escape text ++ "](" ++ escapeUrl url ++ ")"

||| Create a link with title
public export
linkWithTitle : (text : String) -> (url : String) -> (title : String) -> String
linkWithTitle text url title =
  "[" ++ escape text ++ "](" ++ escapeUrl url ++ " \"" ++ escape title ++ "\")"

||| Create an image
public export
image : (alt : String) -> (url : String) -> String
image alt url = "!" ++ link alt url

||| Create an image with title
public export
imageWithTitle : (alt : String) -> (url : String) -> (title : String) -> String
imageWithTitle alt url title = "!" ++ linkWithTitle alt url title

--------------------------------------------------------------------------------
-- Block Elements
--------------------------------------------------------------------------------

||| Create a heading
public export
heading : HeadingLevel -> String -> String
heading level text =
  replicate (headingNum level) '#' ++ " " ++ escape text
  where
    replicate : Nat -> Char -> String
    replicate Z _ = ""
    replicate (S n) c = pack [c] ++ replicate n c

||| Create a paragraph
public export
paragraph : String -> String
paragraph s = escape s ++ "\n\n"

||| Create a blockquote
public export
blockquote : String -> String
blockquote s = "> " ++ escape s

||| Create a multi-line blockquote
public export
blockquoteLines : List String -> String
blockquoteLines lines = unlines (map (\l => "> " ++ escape l) lines)
  where
    unlines : List String -> String
    unlines [] = ""
    unlines [x] = x
    unlines (x :: xs) = x ++ "\n" ++ unlines xs

||| Create a code block
public export
codeBlock : CodeLanguage -> String -> String
codeBlock lang content =
  let langTag = case lang of
                  Plain => ""
                  Lang l => l
  in "```" ++ langTag ++ "\n" ++ content ++ "\n```"

||| Create a horizontal rule
public export
horizontalRule : String
horizontalRule = "---\n"

--------------------------------------------------------------------------------
-- Lists
--------------------------------------------------------------------------------

||| Create a list item
listItem : ListStyle -> Nat -> String -> String
listItem Unordered _ text = "- " ++ escape text
listItem Ordered num text = show (S num) ++ ". " ++ escape text

||| Create a list
public export
list : ListStyle -> List String -> String
list style items = unlines (zipWithIndex (\i, item => listItem style i item) items)
  where
    zipWithIndex : (Nat -> a -> b) -> List a -> List b
    zipWithIndex f xs = go 0 xs
      where
        go : Nat -> List a -> List b
        go _ [] = []
        go n (x :: rest) = f n x :: go (S n) rest
    
    unlines : List String -> String
    unlines [] = ""
    unlines [x] = x
    unlines (x :: xs) = x ++ "\n" ++ unlines xs

||| Create an unordered list
public export
bulletList : List String -> String
bulletList = list Unordered

||| Create an ordered list
public export
numberedList : List String -> String
numberedList = list Ordered

||| Create a task list
public export
taskList : List (Bool, String) -> String
taskList items = unlines (map mkTask items)
  where
    mkTask : (Bool, String) -> String
    mkTask (done, text) = "- [" ++ (if done then "x" else " ") ++ "] " ++ escape text
    
    unlines : List String -> String
    unlines [] = ""
    unlines [x] = x
    unlines (x :: xs) = x ++ "\n" ++ unlines xs

--------------------------------------------------------------------------------
-- Tables
--------------------------------------------------------------------------------

||| Table alignment
public export
data Alignment : Type where
  AlignLeft : Alignment
  AlignCenter : Alignment
  AlignRight : Alignment
  AlignDefault : Alignment

||| Create a table
public export
table : (headers : List String) -> (alignments : List Alignment) -> (rows : List (List String)) -> String
table headers aligns rows =
  let headerRow = mkRow headers
      separatorRow = mkSeparator aligns (length headers)
      dataRows = map mkRow rows
  in headerRow ++ "\n" ++ separatorRow ++ "\n" ++ unlines dataRows
  where
    mkCell : String -> String
    mkCell s = " " ++ escape s ++ " "
    
    mkRow : List String -> String
    mkRow cells = "|" ++ concat (intersperse "|" (map mkCell cells)) ++ "|"
    
    intersperse : a -> List a -> List a
    intersperse _ [] = []
    intersperse _ [x] = [x]
    intersperse sep (x :: xs) = x :: sep :: intersperse sep xs
    
    alignSep : Alignment -> String
    alignSep AlignLeft = ":---"
    alignSep AlignCenter = ":---:"
    alignSep AlignRight = "---:"
    alignSep AlignDefault = "---"
    
    mkSeparator : List Alignment -> Nat -> String
    mkSeparator as n =
      let aligns = take n (as ++ repeat AlignDefault)
      in "|" ++ concat (intersperse "|" (map alignSep aligns)) ++ "|"
    
    repeat : a -> List a
    repeat x = x :: repeat x  -- Infinite, but `take` limits it
    
    unlines : List String -> String
    unlines [] = ""
    unlines [x] = x
    unlines (x :: xs) = x ++ "\n" ++ unlines xs

--------------------------------------------------------------------------------
-- Document Builder
--------------------------------------------------------------------------------

||| A markdown document (list of blocks)
public export
record MarkdownDoc where
  constructor MkDoc
  blocks : List String

||| Create an empty document
public export
emptyDoc : MarkdownDoc
emptyDoc = MkDoc []

||| Add a block to the document
public export
addBlock : String -> MarkdownDoc -> MarkdownDoc
addBlock block doc = MkDoc (doc.blocks ++ [block])

||| Render the document
public export
render : MarkdownDoc -> String
render doc = concat (intersperse "\n\n" doc.blocks)
  where
    intersperse : a -> List a -> List a
    intersperse _ [] = []
    intersperse _ [x] = [x]
    intersperse sep (x :: xs) = x :: sep :: intersperse sep xs

public export
Show MarkdownDoc where
  show = render

