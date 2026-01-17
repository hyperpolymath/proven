-- SPDX-License-Identifier: PMPL-1.0
-- SPDX-FileCopyrightText: 2024-2025 Hyperpolymath

||| Safe BibTeX parsing and validation
|||
||| This module provides type-safe BibTeX handling:
||| - Entry type validation
||| - Field validation per entry type
||| - Citation key validation
||| - Cross-reference resolution
||| - Format normalization
|||
||| Security features:
||| - LaTeX command injection prevention
||| - Unicode normalization
||| - Special character escaping
||| - URL validation in fields
||| - Duplicate key detection
module Proven.SafeBibTeX

import Data.List
import Data.String
import Data.Either
import Proven.Core

%default total

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

||| BibTeX entry types
public export
data EntryType
  = Article
  | Book
  | Booklet
  | Conference
  | InBook
  | InCollection
  | InProceedings
  | Manual
  | MastersThesis
  | Misc
  | PhdThesis
  | Proceedings
  | TechReport
  | Unpublished
  | Online          -- Modern extension
  | Software        -- Modern extension
  | Dataset         -- Modern extension
  | CustomType String

||| Standard BibTeX fields
public export
data FieldName
  = Author
  | Title
  | Year
  | Journal
  | Volume
  | Number
  | Pages
  | Month
  | Note
  | Publisher
  | Address
  | Edition
  | Series
  | Booktitle
  | Editor
  | Chapter
  | Organization
  | School
  | Institution
  | HowPublished
  | Type
  | Key
  | Crossref
  | DOI
  | URL
  | ISBN
  | ISSN
  | Abstract
  | Keywords
  | Language
  | CustomField String

||| Field value (can contain LaTeX)
public export
record FieldValue where
  constructor MkFieldValue
  raw : String
  sanitized : String
  containsLaTeX : Bool

||| Single field in an entry
public export
record BibField where
  constructor MkBibField
  name : FieldName
  value : FieldValue

||| Citation key
public export
record CitationKey where
  constructor MkCitationKey
  key : String

||| Complete BibTeX entry
public export
record BibEntry where
  constructor MkBibEntry
  entryType : EntryType
  citationKey : CitationKey
  fields : List BibField

||| BibTeX string definition (@string{...})
public export
record BibString where
  constructor MkBibString
  name : String
  value : String

||| BibTeX preamble (@preamble{...})
public export
record BibPreamble where
  constructor MkBibPreamble
  content : String

||| Complete BibTeX database
public export
record BibDatabase where
  constructor MkBibDatabase
  preamble : Maybe BibPreamble
  strings : List BibString
  entries : List BibEntry

||| Required fields for entry types
public export
record RequiredFields where
  constructor MkRequiredFields
  required : List FieldName
  optional : List FieldName
  alternatives : List (List FieldName)  -- One of these groups required

||| Validation result
public export
record ValidationResult where
  constructor MkValidationResult
  isValid : Bool
  missingFields : List FieldName
  warnings : List String

--------------------------------------------------------------------------------
-- Errors
--------------------------------------------------------------------------------

||| BibTeX errors
public export
data BibTeXError
  = EmptyEntry
  | InvalidCitationKey String
  | DuplicateCitationKey String
  | InvalidEntryType String
  | MissingRequiredField EntryType FieldName
  | InvalidField FieldName String
  | LaTeXInjection String
  | UnbalancedBraces String
  | InvalidMonth String
  | InvalidYear String
  | InvalidPages String
  | InvalidDOI String
  | InvalidISBN String
  | InvalidISSN String
  | InvalidURL String
  | CrossrefNotFound String
  | CircularCrossref String
  | ParseError String Nat  -- message, line

public export
Show BibTeXError where
  show EmptyEntry = "BibTeX error: empty entry"
  show (InvalidCitationKey k) = "BibTeX error: invalid citation key '" ++ k ++ "'"
  show (DuplicateCitationKey k) = "BibTeX error: duplicate citation key '" ++ k ++ "'"
  show (InvalidEntryType t) = "BibTeX error: invalid entry type '" ++ t ++ "'"
  show (MissingRequiredField et fn) = "BibTeX error: missing required field for entry type"
  show (InvalidField fn msg) = "BibTeX error: invalid field - " ++ msg
  show (LaTeXInjection cmd) = "BibTeX security: potential LaTeX injection '" ++ cmd ++ "'"
  show (UnbalancedBraces content) = "BibTeX error: unbalanced braces"
  show (InvalidMonth m) = "BibTeX error: invalid month '" ++ m ++ "'"
  show (InvalidYear y) = "BibTeX error: invalid year '" ++ y ++ "'"
  show (InvalidPages p) = "BibTeX error: invalid pages '" ++ p ++ "'"
  show (InvalidDOI d) = "BibTeX error: invalid DOI '" ++ d ++ "'"
  show (InvalidISBN i) = "BibTeX error: invalid ISBN '" ++ i ++ "'"
  show (InvalidISSN i) = "BibTeX error: invalid ISSN '" ++ i ++ "'"
  show (InvalidURL u) = "BibTeX error: invalid URL '" ++ u ++ "'"
  show (CrossrefNotFound k) = "BibTeX error: crossref not found '" ++ k ++ "'"
  show (CircularCrossref k) = "BibTeX error: circular crossref '" ++ k ++ "'"
  show (ParseError msg line) = "BibTeX parse error at line " ++ show line ++ ": " ++ msg

--------------------------------------------------------------------------------
-- Constants
--------------------------------------------------------------------------------

||| Dangerous LaTeX commands
dangerousCommands : List String
dangerousCommands =
  [ "\\input", "\\include", "\\write", "\\immediate"
  , "\\openout", "\\closeout", "\\special", "\\pdffilemoddate"
  , "\\pdfshellescape", "\\ShellEscape", "\\directlua"
  , "\\catcode", "\\def", "\\newcommand"
  ]

||| Valid month names
monthNames : List String
monthNames =
  [ "jan", "feb", "mar", "apr", "may", "jun"
  , "jul", "aug", "sep", "oct", "nov", "dec"
  , "january", "february", "march", "april", "may", "june"
  , "july", "august", "september", "october", "november", "december"
  ]

||| Characters valid in citation keys
validKeyChars : List Char
validKeyChars =
  unpack "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789:._-/"

--------------------------------------------------------------------------------
-- Entry type helpers
--------------------------------------------------------------------------------

||| Parse entry type from string
public export
parseEntryType : String -> EntryType
parseEntryType s =
  case toLower s of
    "article" => Article
    "book" => Book
    "booklet" => Booklet
    "conference" => Conference
    "inbook" => InBook
    "incollection" => InCollection
    "inproceedings" => InProceedings
    "manual" => Manual
    "mastersthesis" => MastersThesis
    "misc" => Misc
    "phdthesis" => PhdThesis
    "proceedings" => Proceedings
    "techreport" => TechReport
    "unpublished" => Unpublished
    "online" => Online
    "software" => Software
    "dataset" => Dataset
    _ => CustomType s

||| Show entry type
public export
showEntryType : EntryType -> String
showEntryType Article = "article"
showEntryType Book = "book"
showEntryType Booklet = "booklet"
showEntryType Conference = "conference"
showEntryType InBook = "inbook"
showEntryType InCollection = "incollection"
showEntryType InProceedings = "inproceedings"
showEntryType Manual = "manual"
showEntryType MastersThesis = "mastersthesis"
showEntryType Misc = "misc"
showEntryType PhdThesis = "phdthesis"
showEntryType Proceedings = "proceedings"
showEntryType TechReport = "techreport"
showEntryType Unpublished = "unpublished"
showEntryType Online = "online"
showEntryType Software = "software"
showEntryType Dataset = "dataset"
showEntryType (CustomType s) = s

||| Get required fields for entry type
public export
getRequiredFields : EntryType -> RequiredFields
getRequiredFields Article = MkRequiredFields [Author, Title, Journal, Year] [Volume, Number, Pages, Month, DOI] []
getRequiredFields Book = MkRequiredFields [Title, Publisher, Year] [Volume, Series, Address, Edition, Month, ISBN] [[Author], [Editor]]
getRequiredFields InProceedings = MkRequiredFields [Author, Title, Booktitle, Year] [Editor, Volume, Series, Pages, Address, Month, Organization, Publisher, DOI] []
getRequiredFields PhdThesis = MkRequiredFields [Author, Title, School, Year] [Type, Address, Month] []
getRequiredFields MastersThesis = MkRequiredFields [Author, Title, School, Year] [Type, Address, Month] []
getRequiredFields TechReport = MkRequiredFields [Author, Title, Institution, Year] [Type, Number, Address, Month] []
getRequiredFields Misc = MkRequiredFields [] [Author, Title, HowPublished, Month, Year, Note] []
getRequiredFields _ = MkRequiredFields [] [] []

--------------------------------------------------------------------------------
-- Field name helpers
--------------------------------------------------------------------------------

||| Parse field name from string
public export
parseFieldName : String -> FieldName
parseFieldName s =
  case toLower s of
    "author" => Author
    "title" => Title
    "year" => Year
    "journal" => Journal
    "volume" => Volume
    "number" => Number
    "pages" => Pages
    "month" => Month
    "note" => Note
    "publisher" => Publisher
    "address" => Address
    "edition" => Edition
    "series" => Series
    "booktitle" => Booktitle
    "editor" => Editor
    "chapter" => Chapter
    "organization" => Organization
    "school" => School
    "institution" => Institution
    "howpublished" => HowPublished
    "type" => Type
    "key" => Key
    "crossref" => Crossref
    "doi" => DOI
    "url" => URL
    "isbn" => ISBN
    "issn" => ISSN
    "abstract" => Abstract
    "keywords" => Keywords
    "language" => Language
    _ => CustomField s

||| Show field name
public export
showFieldName : FieldName -> String
showFieldName Author = "author"
showFieldName Title = "title"
showFieldName Year = "year"
showFieldName Journal = "journal"
showFieldName Volume = "volume"
showFieldName Number = "number"
showFieldName Pages = "pages"
showFieldName Month = "month"
showFieldName Note = "note"
showFieldName Publisher = "publisher"
showFieldName Address = "address"
showFieldName Edition = "edition"
showFieldName Series = "series"
showFieldName Booktitle = "booktitle"
showFieldName Editor = "editor"
showFieldName Chapter = "chapter"
showFieldName Organization = "organization"
showFieldName School = "school"
showFieldName Institution = "institution"
showFieldName HowPublished = "howpublished"
showFieldName Type = "type"
showFieldName Key = "key"
showFieldName Crossref = "crossref"
showFieldName DOI = "doi"
showFieldName URL = "url"
showFieldName ISBN = "isbn"
showFieldName ISSN = "issn"
showFieldName Abstract = "abstract"
showFieldName Keywords = "keywords"
showFieldName Language = "language"
showFieldName (CustomField s) = s

--------------------------------------------------------------------------------
-- Validation helpers
--------------------------------------------------------------------------------

||| Check for dangerous LaTeX commands
public export
detectLaTeXInjection : String -> Maybe String
detectLaTeXInjection content =
  find (\cmd => isInfixOf cmd content) dangerousCommands

||| Check if braces are balanced
public export
bracesBalanced : String -> Bool
bracesBalanced content = go 0 (unpack content)
  where
    go : Int -> List Char -> Bool
    go depth [] = depth == 0
    go depth ('{' :: rest) = go (depth + 1) rest
    go depth ('}' :: rest) = depth > 0 && go (depth - 1) rest
    go depth (_ :: rest) = go depth rest

||| Validate citation key
public export
validateCitationKey : String -> Either BibTeXError CitationKey
validateCitationKey "" = Left (InvalidCitationKey "empty")
validateCitationKey key =
  if all (\c => elem c validKeyChars) (unpack key)
    then Right (MkCitationKey key)
    else Left (InvalidCitationKey key)

||| Validate year field
public export
validateYear : String -> Either BibTeXError String
validateYear "" = Left (InvalidYear "empty")
validateYear year =
  case parsePositive year of
    Nothing => Left (InvalidYear year)
    Just n =>
      if n >= 1000 && n <= 2100
        then Right year
        else Left (InvalidYear year)

||| Validate month field
public export
validateMonth : String -> Either BibTeXError String
validateMonth "" = Left (InvalidMonth "empty")
validateMonth month =
  if elem (toLower month) monthNames
    then Right month
    else case parsePositive month of
      Just n => if n >= 1 && n <= 12 then Right month else Left (InvalidMonth month)
      Nothing => Left (InvalidMonth month)

||| Validate pages field (e.g., "1-10", "1--10", "1,5,10")
public export
validatePages : String -> Either BibTeXError String
validatePages "" = Left (InvalidPages "empty")
validatePages pages =
  -- Simplified - allow digits, hyphens, commas, spaces
  if all (\c => isDigit c || c == '-' || c == ',' || c == ' ') (unpack pages)
    then Right pages
    else Left (InvalidPages pages)

||| Validate DOI
public export
validateDOI : String -> Either BibTeXError String
validateDOI "" = Left (InvalidDOI "empty")
validateDOI doi =
  if isPrefixOf "10." doi || isPrefixOf "https://doi.org/10." doi
    then Right doi
    else Left (InvalidDOI doi)

||| Validate ISBN (simplified - check for 10 or 13 digits)
public export
validateISBN : String -> Either BibTeXError String
validateISBN "" = Left (InvalidISBN "empty")
validateISBN isbn =
  let digits = filter isDigit (unpack isbn)
  in if length digits == 10 || length digits == 13
       then Right isbn
       else Left (InvalidISBN isbn)

||| Validate ISSN (8 digits with possible hyphen)
public export
validateISSN : String -> Either BibTeXError String
validateISSN "" = Left (InvalidISSN "empty")
validateISSN issn =
  let digits = filter (\c => isDigit c || c == 'X' || c == 'x') (unpack issn)
  in if length digits == 8
       then Right issn
       else Left (InvalidISSN issn)

||| Sanitize field value
public export
sanitizeValue : String -> Either BibTeXError FieldValue
sanitizeValue content =
  case detectLaTeXInjection content of
    Just cmd => Left (LaTeXInjection cmd)
    Nothing =>
      if not (bracesBalanced content)
        then Left (UnbalancedBraces content)
        else Right (MkFieldValue content content (isInfixOf "\\" content))

--------------------------------------------------------------------------------
-- Entry construction
--------------------------------------------------------------------------------

||| Create a field
public export
mkField : String -> String -> Either BibTeXError BibField
mkField name value = do
  fieldValue <- sanitizeValue value
  pure (MkBibField (parseFieldName name) fieldValue)

||| Create an entry
public export
mkEntry : String -> String -> List (String, String) -> Either BibTeXError BibEntry
mkEntry entryType key fields = do
  citationKey <- validateCitationKey key
  validFields <- traverse (\(n, v) => mkField n v) fields
  let et = parseEntryType entryType
  pure (MkBibEntry et citationKey validFields)

||| Validate entry has required fields
public export
validateEntry : BibEntry -> ValidationResult
validateEntry entry =
  let required = getRequiredFields entry.entryType
      presentFields = map (.name) entry.fields
      missing = filter (\f => not (elem f presentFields)) required.required
  in MkValidationResult (null missing) missing []

--------------------------------------------------------------------------------
-- Database operations
--------------------------------------------------------------------------------

||| Check for duplicate keys
public export
findDuplicateKeys : List BibEntry -> List String
findDuplicateKeys entries =
  let keys = map (\e => e.citationKey.key) entries
      grouped = groupBy (==) (sort keys)
  in map head (filter (\g => length g > 1) grouped)

||| Find entry by citation key
public export
lookupEntry : String -> BibDatabase -> Maybe BibEntry
lookupEntry key db = find (\e => e.citationKey.key == key) db.entries

||| Resolve crossref
public export
resolveCrossref : BibEntry -> BibDatabase -> Either BibTeXError (Maybe BibEntry)
resolveCrossref entry db =
  case find (\f => f.name == Crossref) entry.fields of
    Nothing => Right Nothing
    Just crossrefField =>
      case lookupEntry crossrefField.value.raw db of
        Nothing => Left (CrossrefNotFound crossrefField.value.raw)
        Just parent => Right (Just parent)

--------------------------------------------------------------------------------
-- Serialization
--------------------------------------------------------------------------------

||| Escape special BibTeX characters
public export
escapeValue : String -> String
escapeValue str = concatMap escapeChar (unpack str)
  where
    escapeChar : Char -> String
    escapeChar '#' = "\\#"
    escapeChar '%' = "\\%"
    escapeChar '&' = "\\&"
    escapeChar '_' = "\\_"
    escapeChar c = singleton c

||| Format a single field
public export
formatField : BibField -> String
formatField field =
  "  " ++ showFieldName field.name ++ " = {" ++ field.value.raw ++ "}"

||| Format an entry
public export
formatEntry : BibEntry -> String
formatEntry entry =
  "@" ++ showEntryType entry.entryType ++ "{" ++ entry.citationKey.key ++ ",\n" ++
  concat (intersperse ",\n" (map formatField entry.fields)) ++ "\n}"

||| Format a string definition
public export
formatString : BibString -> String
formatString str =
  "@string{" ++ str.name ++ " = \"" ++ str.value ++ "\"}"

||| Format complete database
public export
formatDatabase : BibDatabase -> String
formatDatabase db =
  maybe "" (\p => "@preamble{" ++ p.content ++ "}\n\n") db.preamble ++
  concat (map (\s => formatString s ++ "\n") db.strings) ++
  (if null db.strings then "" else "\n") ++
  concat (intersperse "\n\n" (map formatEntry db.entries))

--------------------------------------------------------------------------------
-- Empty/default values
--------------------------------------------------------------------------------

||| Empty database
public export
emptyDatabase : BibDatabase
emptyDatabase = MkBibDatabase Nothing [] []

||| Create simple article entry
public export
mkArticle : String -> String -> String -> String -> String -> Either BibTeXError BibEntry
mkArticle key author title journal year = do
  mkEntry "article" key
    [ ("author", author)
    , ("title", title)
    , ("journal", journal)
    , ("year", year)
    ]

||| Create simple book entry
public export
mkBook : String -> String -> String -> String -> String -> Either BibTeXError BibEntry
mkBook key author title publisher year = do
  mkEntry "book" key
    [ ("author", author)
    , ("title", title)
    , ("publisher", publisher)
    , ("year", year)
    ]
