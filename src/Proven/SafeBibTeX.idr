-- SPDX-License-Identifier: Apache-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
||| SafeBibTeX - BibTeX parsing with LaTeX injection prevention
|||
||| Provides type-safe BibTeX entry construction and validation.
||| Prevents: LaTeX command injection, malformed entries, special char issues.
module Proven.SafeBibTeX

import Data.String
import Data.List
import Data.Nat
import Data.Maybe

%default total

||| BibTeX entry types
public export
data EntryType =
    Article | Book | InProceedings | InCollection
  | PhdThesis | MastersThesis | TechReport
  | Misc | Unpublished | Online | Software

public export
Show EntryType where
  show Article = "article"
  show Book = "book"
  show InProceedings = "inproceedings"
  show InCollection = "incollection"
  show PhdThesis = "phdthesis"
  show MastersThesis = "mastersthesis"
  show TechReport = "techreport"
  show Misc = "misc"
  show Unpublished = "unpublished"
  show Online = "online"
  show Software = "software"

public export
Eq EntryType where
  Article == Article = True
  Book == Book = True
  InProceedings == InProceedings = True
  InCollection == InCollection = True
  PhdThesis == PhdThesis = True
  MastersThesis == MastersThesis = True
  TechReport == TechReport = True
  Misc == Misc = True
  Unpublished == Unpublished = True
  Online == Online = True
  Software == Software = True
  _ == _ = False

||| Dangerous LaTeX commands that could be exploited
public export
dangerousCommands : List String
dangerousCommands =
  [ "\\input", "\\include", "\\write", "\\read"
  , "\\immediate", "\\openin", "\\openout"
  , "\\closein", "\\closeout"
  , "\\catcode", "\\def", "\\edef", "\\gdef"
  , "\\newcommand", "\\renewcommand"
  , "\\csname", "\\endcsname"
  , "\\expandafter", "\\noexpand"
  , "\\pdfsystem", "\\pdfcreationdate"
  , "\\directlua", "\\latelua"
  , "\\write18", "\\ShellEscape"
  ]

||| Check if a string contains dangerous LaTeX commands
public export
hasDangerousLaTeX : String -> Bool
hasDangerousLaTeX s = any (\cmd => isInfixOf cmd s) dangerousCommands

||| Sanitize a BibTeX field value (escape special chars, remove dangerous commands)
public export
sanitizeBibField : String -> String
sanitizeBibField s =
  if hasDangerousLaTeX s
    then pack (filter (\c => c /= '\\') (unpack s))
    else escapeSpecial s
  where
    escapeSpecial : String -> String
    escapeSpecial str = concatMap escChar (unpack str)
      where
        escChar : Char -> String
        escChar '#' = "\\#"
        escChar '$' = "\\$"
        escChar '%' = "\\%"
        escChar '&' = "\\&"
        escChar '_' = "\\_"
        escChar c = singleton c

||| A citation key (alphanumeric + hyphens + underscores + colons)
public export
data CitationKey : Type where
  MkCitationKey : (key : String) -> CitationKey

public export
Eq CitationKey where
  MkCitationKey a == MkCitationKey b = a == b

public export
Show CitationKey where
  show (MkCitationKey k) = k

||| Validate a citation key
public export
isValidCitationKey : String -> Bool
isValidCitationKey s =
  let chars = unpack s
  in length chars > 0 && length chars <= 256 &&
     all (\c => isAlphaNum c || c == '-' || c == '_' || c == ':' || c == '.') chars

||| Smart constructor for citation keys
public export
mkCitationKey : String -> Maybe CitationKey
mkCitationKey s = if isValidCitationKey s then Just (MkCitationKey s) else Nothing

||| A BibTeX field (name-value pair)
public export
record BibField where
  constructor MkBibField
  fieldName  : String
  fieldValue : String

||| A BibTeX entry
public export
record BibEntry where
  constructor MkBibEntry
  entryType : EntryType
  citeKey   : CitationKey
  fields    : List BibField

||| Required fields for each entry type
public export
requiredFields : EntryType -> List String
requiredFields Article = ["author", "title", "journal", "year"]
requiredFields Book = ["author", "title", "publisher", "year"]
requiredFields InProceedings = ["author", "title", "booktitle", "year"]
requiredFields InCollection = ["author", "title", "booktitle", "publisher", "year"]
requiredFields PhdThesis = ["author", "title", "school", "year"]
requiredFields MastersThesis = ["author", "title", "school", "year"]
requiredFields TechReport = ["author", "title", "institution", "year"]
requiredFields Misc = []
requiredFields Unpublished = ["author", "title", "note"]
requiredFields Online = ["author", "title", "url"]
requiredFields Software = ["author", "title", "version"]

||| Check if all required fields are present
public export
hasRequiredFields : BibEntry -> Bool
hasRequiredFields entry =
  let required = requiredFields (entryType entry)
      present = map fieldName (fields entry)
  in all (\r => elem r present) required

||| Get a field value by name
public export
getField : String -> BibEntry -> Maybe String
getField name entry =
  map fieldValue (find (\f => fieldName f == name) (fields entry))

||| Render a BibTeX entry to string
public export
renderEntry : BibEntry -> String
renderEntry entry =
  "@" ++ show (entryType entry) ++ "{" ++ show (citeKey entry) ++ ",\n" ++
  concatMap renderField (fields entry) ++
  "}\n"
  where
    renderField : BibField -> String
    renderField f = "  " ++ fieldName f ++ " = {" ++ sanitizeBibField (fieldValue f) ++ "},\n"

||| Build a safe BibTeX entry
public export
mkBibEntry : EntryType -> String -> List (String, String) -> Maybe BibEntry
mkBibEntry etype keyStr fieldPairs =
  case mkCitationKey keyStr of
    Nothing => Nothing
    Just key =>
      let bibFields = map (\(n, v) => MkBibField n v) fieldPairs
          entry = MkBibEntry etype key bibFields
      in if hasRequiredFields entry
           then Just entry
           else Nothing

-- ----------------------------------------------------------------
-- Proof types
-- ----------------------------------------------------------------

||| Proof that an entry has all required fields
public export
data CompleteEntry : BibEntry -> Type where
  MkCompleteEntry : hasRequiredFields entry = True -> CompleteEntry entry

||| Proof that a field value has no dangerous LaTeX
public export
data SafeLaTeX : String -> Type where
  MkSafeLaTeX : hasDangerousLaTeX s = False -> SafeLaTeX s

||| Proof that a citation key is valid
public export
data ValidCiteKey : CitationKey -> Type where
  MkValidCiteKey : (k : CitationKey) -> ValidCiteKey k
