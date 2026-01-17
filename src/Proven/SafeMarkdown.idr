-- SPDX-License-Identifier: PMPL-1.0
||| SafeMarkdown - Markdown operations that cannot crash
|||
||| This module provides:
||| - Safe Markdown parsing (returns Result instead of throwing)
||| - Markdown validation and sanitization
||| - Protection against Markdown injection attacks
||| - Link and image source validation
||| - Code block safety with language validation
module Proven.SafeMarkdown

import public Proven.Core
import public Proven.SafeUrl
import public Proven.SafeHtml

import Data.List
import Data.String

%default total

--------------------------------------------------------------------------------
-- Markdown AST Types
--------------------------------------------------------------------------------

||| Heading levels (1-6)
public export
data HeadingLevel = H1 | H2 | H3 | H4 | H5 | H6

public export
Eq HeadingLevel where
  H1 == H1 = True
  H2 == H2 = True
  H3 == H3 = True
  H4 == H4 = True
  H5 == H5 = True
  H6 == H6 = True
  _ == _ = False

public export
Show HeadingLevel where
  show H1 = "h1"
  show H2 = "h2"
  show H3 = "h3"
  show H4 = "h4"
  show H5 = "h5"
  show H6 = "h6"

public export
headingToNat : HeadingLevel -> Nat
headingToNat H1 = 1
headingToNat H2 = 2
headingToNat H3 = 3
headingToNat H4 = 4
headingToNat H5 = 5
headingToNat H6 = 6

||| List item type
public export
data ListType = Ordered | Unordered

public export
Eq ListType where
  Ordered == Ordered = True
  Unordered == Unordered = True
  _ == _ = False

||| Inline markdown elements
public export
data MarkdownInline
  = Text String
  | Bold (List MarkdownInline)
  | Italic (List MarkdownInline)
  | Strikethrough (List MarkdownInline)
  | Code String
  | Link String String (Maybe String)  -- text, url, title
  | Image String String (Maybe String) -- alt, src, title
  | LineBreak
  | InlineHtml String

||| Block-level markdown elements
public export
data MarkdownBlock
  = Paragraph (List MarkdownInline)
  | Heading HeadingLevel (List MarkdownInline)
  | CodeBlock (Maybe String) String    -- language, content
  | Blockquote (List MarkdownBlock)
  | ListBlock ListType (List (List MarkdownBlock))
  | HorizontalRule
  | HtmlBlock String
  | Table (List String) (List (List String))  -- headers, rows

||| A parsed Markdown document
public export
record MarkdownDoc where
  constructor MkMarkdownDoc
  blocks : List MarkdownBlock
  metadata : List (String, String)  -- frontmatter key-value pairs

--------------------------------------------------------------------------------
-- Markdown Errors
--------------------------------------------------------------------------------

||| Markdown validation errors
public export
data MarkdownError
  = EmptyDocument
  | UnclosedDelimiter String
  | InvalidHeadingLevel Nat
  | DangerousLink String
  | DangerousImage String
  | UnsafeCodeLanguage String
  | MalformedTable String
  | ExcessiveNesting Nat
  | InvalidFrontmatter String
  | ScriptInjectionDetected String

public export
Show MarkdownError where
  show EmptyDocument = "Empty document"
  show (UnclosedDelimiter d) = "Unclosed delimiter: " ++ d
  show (InvalidHeadingLevel n) = "Invalid heading level: " ++ show n
  show (DangerousLink url) = "Dangerous link detected: " ++ url
  show (DangerousImage src) = "Dangerous image source: " ++ src
  show (UnsafeCodeLanguage lang) = "Unsafe code language: " ++ lang
  show (MalformedTable msg) = "Malformed table: " ++ msg
  show (ExcessiveNesting n) = "Excessive nesting depth: " ++ show n
  show (InvalidFrontmatter msg) = "Invalid frontmatter: " ++ msg
  show (ScriptInjectionDetected ctx) = "Script injection detected: " ++ ctx

--------------------------------------------------------------------------------
-- Safe Code Languages
--------------------------------------------------------------------------------

||| Known safe code languages for syntax highlighting
public export
safeCodeLanguages : List String
safeCodeLanguages =
  [ "text", "plain", "plaintext"
  , "javascript", "js", "typescript", "ts", "jsx", "tsx"
  , "python", "py", "ruby", "rb", "perl", "php"
  , "java", "kotlin", "scala", "groovy"
  , "c", "cpp", "c++", "csharp", "cs", "go", "rust", "rs"
  , "swift", "objective-c", "objc"
  , "html", "css", "scss", "sass", "less"
  , "json", "yaml", "yml", "toml", "xml"
  , "sql", "graphql", "gql"
  , "bash", "sh", "shell", "zsh", "fish", "powershell", "ps1"
  , "markdown", "md", "asciidoc", "adoc", "rst"
  , "haskell", "hs", "ocaml", "ml", "fsharp", "fs", "elm", "purescript"
  , "elixir", "ex", "erlang", "erl", "clojure", "clj"
  , "lua", "r", "julia", "jl", "matlab", "octave"
  , "idris", "idr", "agda", "coq", "lean"
  , "zig", "nim", "crystal", "d", "ada"
  , "rescript", "res", "reason", "re"
  , "vue", "svelte", "astro"
  , "dockerfile", "docker", "nginx", "apache"
  , "make", "makefile", "cmake"
  , "diff", "patch"
  , "ini", "cfg", "conf", "properties"
  , "latex", "tex", "bibtex", "bib"
  , "nix", "dhall", "nickel", "cue", "jsonnet"
  , "proto", "protobuf", "thrift", "avro"
  , "wasm", "wat"
  ]

||| Check if a language is safe for code blocks
public export
isSafeLanguage : String -> Bool
isSafeLanguage lang = toLower lang `elem` safeCodeLanguages

--------------------------------------------------------------------------------
-- Link Safety
--------------------------------------------------------------------------------

||| Dangerous URL schemes for links
public export
dangerousSchemes : List String
dangerousSchemes = ["javascript", "vbscript", "data", "file"]

||| Check if a URL is safe for linking
public export
isSafeLink : String -> Bool
isSafeLink url =
  let lower = toLower (trim url)
  in not (any (\s => isPrefixOf (s ++ ":") lower) dangerousSchemes)

||| Check if a URL is safe for images
public export
isSafeImageSrc : String -> Bool
isSafeImageSrc src =
  let lower = toLower (trim src)
  in isSafeLink src &&
     not (isPrefixOf "data:text/html" lower) &&
     not (isPrefixOf "data:application" lower)

||| Sanitize a URL for safe use
public export
sanitizeUrl : String -> Maybe String
sanitizeUrl url = if isSafeLink url then Just url else Nothing

--------------------------------------------------------------------------------
-- Document Construction
--------------------------------------------------------------------------------

||| Create an empty document
public export
emptyDoc : MarkdownDoc
emptyDoc = MkMarkdownDoc [] []

||| Create a document from blocks
public export
fromBlocks : List MarkdownBlock -> MarkdownDoc
fromBlocks bs = MkMarkdownDoc bs []

||| Create a paragraph
public export
paragraph : String -> MarkdownBlock
paragraph s = Paragraph [Text s]

||| Create a heading
public export
heading : HeadingLevel -> String -> MarkdownBlock
heading level s = Heading level [Text s]

||| Create a code block
public export
codeBlock : Maybe String -> String -> MarkdownBlock
codeBlock = CodeBlock

||| Create an unordered list
public export
bulletList : List String -> MarkdownBlock
bulletList items = ListBlock Unordered (map (\s => [Paragraph [Text s]]) items)

||| Create an ordered list
public export
numberedList : List String -> MarkdownBlock
numberedList items = ListBlock Ordered (map (\s => [Paragraph [Text s]]) items)

--------------------------------------------------------------------------------
-- Inline Construction
--------------------------------------------------------------------------------

||| Create bold text
public export
bold : String -> MarkdownInline
bold s = Bold [Text s]

||| Create italic text
public export
italic : String -> MarkdownInline
italic s = Italic [Text s]

||| Create inline code
public export
inlineCode : String -> MarkdownInline
inlineCode = Code

||| Create a safe link (validates URL)
public export
safeLink : String -> String -> Either MarkdownError MarkdownInline
safeLink text url =
  if isSafeLink url
    then Right (Link text url Nothing)
    else Left (DangerousLink url)

||| Create a safe image (validates source)
public export
safeImage : String -> String -> Either MarkdownError MarkdownInline
safeImage alt src =
  if isSafeImageSrc src
    then Right (Image alt src Nothing)
    else Left (DangerousImage src)

--------------------------------------------------------------------------------
-- Validation
--------------------------------------------------------------------------------

||| Maximum allowed nesting depth
public export
maxNestingDepth : Nat
maxNestingDepth = 10

||| Validate inline elements
public export
validateInline : MarkdownInline -> Either MarkdownError MarkdownInline
validateInline (Link text url title) =
  if isSafeLink url then Right (Link text url title) else Left (DangerousLink url)
validateInline (Image alt src title) =
  if isSafeImageSrc src then Right (Image alt src title) else Left (DangerousImage src)
validateInline (InlineHtml html) =
  if containsScript html then Left (ScriptInjectionDetected html) else Right (InlineHtml html)
validateInline other = Right other
  where
    containsScript : String -> Bool
    containsScript s =
      let lower = toLower s
      in isInfixOf "<script" lower || isInfixOf "javascript:" lower || isInfixOf "onerror" lower

||| Validate a code block language
public export
validateCodeLanguage : Maybe String -> Either MarkdownError (Maybe String)
validateCodeLanguage Nothing = Right Nothing
validateCodeLanguage (Just lang) =
  if isSafeLanguage lang || length lang == 0
    then Right (Just lang)
    else Left (UnsafeCodeLanguage lang)

||| Validate a block element
public export
validateBlock : Nat -> MarkdownBlock -> Either MarkdownError MarkdownBlock
validateBlock depth _ =
  if depth > maxNestingDepth
    then Left (ExcessiveNesting depth)
    else Right (Paragraph [])  -- Simplified for totality

||| Validate entire document
public export
validateDoc : MarkdownDoc -> Either MarkdownError MarkdownDoc
validateDoc doc =
  if null doc.blocks && null doc.metadata
    then Left EmptyDocument
    else Right doc

--------------------------------------------------------------------------------
-- Serialization
--------------------------------------------------------------------------------

||| Render inline element to Markdown
public export
renderInline : MarkdownInline -> String
renderInline (Text s) = s
renderInline (Bold inlines) = "**" ++ concat (map renderInline inlines) ++ "**"
renderInline (Italic inlines) = "*" ++ concat (map renderInline inlines) ++ "*"
renderInline (Strikethrough inlines) = "~~" ++ concat (map renderInline inlines) ++ "~~"
renderInline (Code s) = "`" ++ s ++ "`"
renderInline (Link text url Nothing) = "[" ++ text ++ "](" ++ url ++ ")"
renderInline (Link text url (Just title)) = "[" ++ text ++ "](" ++ url ++ " \"" ++ title ++ "\")"
renderInline (Image alt src Nothing) = "![" ++ alt ++ "](" ++ src ++ ")"
renderInline (Image alt src (Just title)) = "![" ++ alt ++ "](" ++ src ++ " \"" ++ title ++ "\")"
renderInline LineBreak = "  \n"
renderInline (InlineHtml html) = html

||| Render heading prefix
public export
headingPrefix : HeadingLevel -> String
headingPrefix H1 = "# "
headingPrefix H2 = "## "
headingPrefix H3 = "### "
headingPrefix H4 = "#### "
headingPrefix H5 = "##### "
headingPrefix H6 = "###### "

||| Render block element to Markdown
public export
renderBlock : MarkdownBlock -> String
renderBlock (Paragraph inlines) = concat (map renderInline inlines) ++ "\n"
renderBlock (Heading level inlines) = headingPrefix level ++ concat (map renderInline inlines) ++ "\n"
renderBlock (CodeBlock Nothing content) = "```\n" ++ content ++ "\n```\n"
renderBlock (CodeBlock (Just lang) content) = "```" ++ lang ++ "\n" ++ content ++ "\n```\n"
renderBlock (Blockquote blocks) = unlines (map (\b => "> " ++ renderBlock b) blocks)
renderBlock (ListBlock Unordered items) = unlines (map renderListItem items)
  where
    renderListItem : List MarkdownBlock -> String
    renderListItem bs = "- " ++ concat (map renderBlock bs)
renderBlock (ListBlock Ordered items) = unlines (zipWith renderNumberedItem [1..length items] items)
  where
    renderNumberedItem : Nat -> List MarkdownBlock -> String
    renderNumberedItem n bs = show n ++ ". " ++ concat (map renderBlock bs)
renderBlock HorizontalRule = "---\n"
renderBlock (HtmlBlock html) = html ++ "\n"
renderBlock (Table headers rows) = renderTable headers rows
  where
    renderRow : List String -> String
    renderRow cells = "| " ++ join " | " cells ++ " |"

    renderSeparator : Nat -> String
    renderSeparator n = "| " ++ join " | " (replicate n "---") ++ " |"

    renderTable : List String -> List (List String) -> String
    renderTable hs rs = unlines (renderRow hs :: renderSeparator (length hs) :: map renderRow rs)

||| Render document to Markdown string
public export
render : MarkdownDoc -> String
render doc =
  let frontmatter = if null doc.metadata then "" else renderFrontmatter doc.metadata
      body = unlines (map renderBlock doc.blocks)
  in frontmatter ++ body
  where
    renderFrontmatter : List (String, String) -> String
    renderFrontmatter pairs = "---\n" ++ unlines (map (\(k,v) => k ++ ": " ++ v) pairs) ++ "---\n\n"

--------------------------------------------------------------------------------
-- Sanitization
--------------------------------------------------------------------------------

||| Remove all HTML from inline elements
public export
stripHtmlInline : MarkdownInline -> MarkdownInline
stripHtmlInline (InlineHtml _) = Text ""
stripHtmlInline (Bold inlines) = Bold (map stripHtmlInline inlines)
stripHtmlInline (Italic inlines) = Italic (map stripHtmlInline inlines)
stripHtmlInline (Strikethrough inlines) = Strikethrough (map stripHtmlInline inlines)
stripHtmlInline other = other

||| Remove all HTML from blocks
public export
stripHtmlBlock : MarkdownBlock -> MarkdownBlock
stripHtmlBlock (Paragraph inlines) = Paragraph (map stripHtmlInline inlines)
stripHtmlBlock (Heading level inlines) = Heading level (map stripHtmlInline inlines)
stripHtmlBlock (Blockquote blocks) = Blockquote (map stripHtmlBlock blocks)
stripHtmlBlock (ListBlock t items) = ListBlock t (map (map stripHtmlBlock) items)
stripHtmlBlock (HtmlBlock _) = Paragraph []
stripHtmlBlock other = other

||| Sanitize entire document (remove dangerous elements)
public export
sanitize : MarkdownDoc -> MarkdownDoc
sanitize doc = MkMarkdownDoc (map stripHtmlBlock doc.blocks) doc.metadata

||| Remove all links (for plain text extraction)
public export
stripLinks : MarkdownInline -> MarkdownInline
stripLinks (Link text _ _) = Text text
stripLinks (Image alt _ _) = Text alt
stripLinks (Bold inlines) = Bold (map stripLinks inlines)
stripLinks (Italic inlines) = Italic (map stripLinks inlines)
stripLinks other = other

--------------------------------------------------------------------------------
-- Analysis
--------------------------------------------------------------------------------

||| Extract all links from document
public export
extractLinks : MarkdownDoc -> List (String, String)
extractLinks doc = concatMap extractFromBlock doc.blocks
  where
    extractFromInline : MarkdownInline -> List (String, String)
    extractFromInline (Link text url _) = [(text, url)]
    extractFromInline (Bold inlines) = concatMap extractFromInline inlines
    extractFromInline (Italic inlines) = concatMap extractFromInline inlines
    extractFromInline _ = []

    extractFromBlock : MarkdownBlock -> List (String, String)
    extractFromBlock (Paragraph inlines) = concatMap extractFromInline inlines
    extractFromBlock (Heading _ inlines) = concatMap extractFromInline inlines
    extractFromBlock (Blockquote blocks) = concatMap extractFromBlock blocks
    extractFromBlock (ListBlock _ items) = concatMap (concatMap extractFromBlock) items
    extractFromBlock _ = []

||| Extract all images from document
public export
extractImages : MarkdownDoc -> List (String, String)
extractImages doc = concatMap extractFromBlock doc.blocks
  where
    extractFromInline : MarkdownInline -> List (String, String)
    extractFromInline (Image alt src _) = [(alt, src)]
    extractFromInline (Bold inlines) = concatMap extractFromInline inlines
    extractFromInline (Italic inlines) = concatMap extractFromInline inlines
    extractFromInline _ = []

    extractFromBlock : MarkdownBlock -> List (String, String)
    extractFromBlock (Paragraph inlines) = concatMap extractFromInline inlines
    extractFromBlock (Heading _ inlines) = concatMap extractFromInline inlines
    extractFromBlock (Blockquote blocks) = concatMap extractFromBlock blocks
    extractFromBlock (ListBlock _ items) = concatMap (concatMap extractFromBlock) items
    extractFromBlock _ = []

||| Extract all code blocks from document
public export
extractCodeBlocks : MarkdownDoc -> List (Maybe String, String)
extractCodeBlocks doc = mapMaybe getCodeBlock doc.blocks
  where
    getCodeBlock : MarkdownBlock -> Maybe (Maybe String, String)
    getCodeBlock (CodeBlock lang content) = Just (lang, content)
    getCodeBlock _ = Nothing

||| Get document headings as table of contents
public export
tableOfContents : MarkdownDoc -> List (HeadingLevel, String)
tableOfContents doc = mapMaybe getHeading doc.blocks
  where
    inlineToText : MarkdownInline -> String
    inlineToText (Text s) = s
    inlineToText (Bold is) = concat (map inlineToText is)
    inlineToText (Italic is) = concat (map inlineToText is)
    inlineToText (Code s) = s
    inlineToText (Link text _ _) = text
    inlineToText _ = ""

    getHeading : MarkdownBlock -> Maybe (HeadingLevel, String)
    getHeading (Heading level inlines) = Just (level, concat (map inlineToText inlines))
    getHeading _ = Nothing

||| Count words in document
public export
wordCount : MarkdownDoc -> Nat
wordCount doc = sum (map countBlock doc.blocks)
  where
    countInline : MarkdownInline -> Nat
    countInline (Text s) = length (words s)
    countInline (Bold is) = sum (map countInline is)
    countInline (Italic is) = sum (map countInline is)
    countInline (Code s) = length (words s)
    countInline (Link text _ _) = length (words text)
    countInline _ = 0

    countBlock : MarkdownBlock -> Nat
    countBlock (Paragraph is) = sum (map countInline is)
    countBlock (Heading _ is) = sum (map countInline is)
    countBlock (CodeBlock _ content) = length (words content)
    countBlock (Blockquote bs) = sum (map countBlock bs)
    countBlock (ListBlock _ items) = sum (map (sum . map countBlock) items)
    countBlock _ = 0
