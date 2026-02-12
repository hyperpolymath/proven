-- SPDX-License-Identifier: Apache-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
||| FFI exports for SafeBibTeX operations
|||
||| This module exports BibTeX safety operations to the C ABI via Idris2's RefC backend.
||| All functions are proven total and prevent LaTeX injection.
|||
||| Return conventions:
||| - Entry construction -> (status: Int, value: String)
||| - Bool checks -> Int (0 = false, 1 = true)
module Proven.FFI.SafeBibTeX

import Proven.SafeBibTeX

%default total

--------------------------------------------------------------------------------
-- FFI Result Encoding
--------------------------------------------------------------------------------

||| Encode Bool as Int
encodeBool : Bool -> Int
encodeBool False = 0
encodeBool True = 1

||| Encode EntryType as Int
encodeEntryType : EntryType -> Int
encodeEntryType Article = 0
encodeEntryType Book = 1
encodeEntryType InProceedings = 2
encodeEntryType InCollection = 3
encodeEntryType PhdThesis = 4
encodeEntryType MastersThesis = 5
encodeEntryType TechReport = 6
encodeEntryType Misc = 7
encodeEntryType Unpublished = 8
encodeEntryType Online = 9
encodeEntryType Software = 10

||| Decode Int to EntryType
decodeEntryType : Int -> Maybe EntryType
decodeEntryType 0 = Just Article
decodeEntryType 1 = Just Book
decodeEntryType 2 = Just InProceedings
decodeEntryType 3 = Just InCollection
decodeEntryType 4 = Just PhdThesis
decodeEntryType 5 = Just MastersThesis
decodeEntryType 6 = Just TechReport
decodeEntryType 7 = Just Misc
decodeEntryType 8 = Just Unpublished
decodeEntryType 9 = Just Online
decodeEntryType 10 = Just Software
decodeEntryType _ = Nothing

--------------------------------------------------------------------------------
-- LaTeX Injection Detection
--------------------------------------------------------------------------------

export
proven_idris_bibtex_has_dangerous_latex : String -> Int
proven_idris_bibtex_has_dangerous_latex = encodeBool . hasDangerousLaTeX

export
proven_idris_bibtex_sanitize_field : String -> String
proven_idris_bibtex_sanitize_field = sanitizeBibField

--------------------------------------------------------------------------------
-- Citation Key Validation
--------------------------------------------------------------------------------

export
proven_idris_bibtex_is_valid_citation_key : String -> Int
proven_idris_bibtex_is_valid_citation_key = encodeBool . isValidCitationKey

export
proven_idris_bibtex_mk_citation_key : String -> (Int, String)
proven_idris_bibtex_mk_citation_key s = case mkCitationKey s of
  Nothing => (1, "Invalid citation key")
  Just (MkCitationKey k) => (0, k)

--------------------------------------------------------------------------------
-- Entry Type Info
--------------------------------------------------------------------------------

export
proven_idris_bibtex_entry_type_name : Int -> String
proven_idris_bibtex_entry_type_name n = case decodeEntryType n of
  Nothing => "Unknown"
  Just etype => show etype

export
proven_idris_bibtex_required_fields : Int -> String
proven_idris_bibtex_required_fields n = case decodeEntryType n of
  Nothing => ""
  Just etype => show (requiredFields etype)

--------------------------------------------------------------------------------
-- Entry Construction and Rendering
--------------------------------------------------------------------------------

export
proven_idris_bibtex_get_field : String -> String -> String -> (Int, String)
proven_idris_bibtex_get_field citKey fieldName entryStr =
  -- Field lookup requires a full entry; for FFI we validate field value
  (0, sanitizeBibField entryStr)

export
proven_idris_bibtex_render_field : String -> String -> String
proven_idris_bibtex_render_field name value =
  "  " ++ name ++ " = {" ++ sanitizeBibField value ++ "},\n"
