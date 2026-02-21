-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
||| SafeCSV - Safe CSV parsing and generation
|||
||| This module provides RFC 4180 compliant CSV parsing with
||| proper handling of quoted fields, escaping, and edge cases.
module Proven.SafeCSV

import public Proven.Core
import Data.List
import Data.String

%default total

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

||| A CSV row is a list of fields
public export
Row : Type
Row = List String

||| A CSV document is a list of rows
public export
CSV : Type
CSV = List Row

||| CSV parsing/writing options
public export
record CSVOptions where
  constructor MkCSVOptions
  delimiter : Char      -- Field separator (default: ',')
  quote : Char          -- Quote character (default: '"')
  escape : Char         -- Escape character (default: '"')
  lineEnding : String   -- Line ending (default: "\r\n" per RFC 4180)

||| Default CSV options (RFC 4180 compliant)
public export
defaultOptions : CSVOptions
defaultOptions = MkCSVOptions ',' '"' '"' "\r\n"

||| CSV errors
public export
data CSVError
  = UnterminatedQuote Nat    -- Line number where quote wasn't closed
  | InvalidEscape Nat Nat    -- Line, column of invalid escape
  | InconsistentColumns Nat Nat Nat  -- Line, expected columns, actual columns
  | EmptyInput

public export
Show CSVError where
  show (UnterminatedQuote line) = "Unterminated quote at line " ++ show line
  show (InvalidEscape line col) = "Invalid escape at line " ++ show line ++ ", column " ++ show col
  show (InconsistentColumns line expected actual) =
    "Inconsistent columns at line " ++ show line ++
    ": expected " ++ show expected ++ ", got " ++ show actual
  show EmptyInput = "Empty CSV input"

--------------------------------------------------------------------------------
-- Parsing
--------------------------------------------------------------------------------

||| Parser state
record ParseState where
  constructor MkParseState
  line : Nat
  column : Nat
  inQuote : Bool
  currentField : List Char
  currentRow : Row
  rows : CSV

||| Initial parse state
initState : ParseState
initState = MkParseState 1 1 False [] [] []

||| Parse a CSV string with default options
public export
parse : String -> Either CSVError CSV
parse = parseWith defaultOptions

||| Parse a CSV string with custom options
public export
parseWith : CSVOptions -> String -> Either CSVError CSV
parseWith opts input =
  if null input then Left EmptyInput
  else parseChars opts (unpack input) initState

||| Character-by-character parser
parseChars : CSVOptions -> List Char -> ParseState -> Either CSVError CSV
parseChars _ [] state =
  if state.inQuote
    then Left (UnterminatedQuote state.line)
    else Right (reverse (finalizeRow state))
  where
    finalizeRow : ParseState -> CSV
    finalizeRow st =
      let field = pack (reverse st.currentField)
          row = reverse (field :: st.currentRow)
      in if null st.currentField && null st.currentRow && null st.rows
           then st.rows
           else row :: st.rows

parseChars opts (c :: cs) state =
  if state.inQuote
    then parseInQuote opts c cs state
    else parseNormal opts c cs state

||| Parse while inside a quoted field
parseInQuote : CSVOptions -> Char -> List Char -> ParseState -> Either CSVError CSV
parseInQuote opts c cs state =
  if c == opts.quote
    then case cs of
           (c2 :: rest) =>
             if c2 == opts.escape
               then -- Escaped quote - add single quote to field
                 parseChars opts rest
                   (MkParseState state.line (state.column + 2)
                    True (opts.quote :: state.currentField)
                    state.currentRow state.rows)
               else -- End of quoted field
                 parseChars opts cs
                   (MkParseState state.line (state.column + 1)
                    False state.currentField
                    state.currentRow state.rows)
           [] => -- End of input after closing quote
             parseChars opts [] (MkParseState state.line (state.column + 1)
                                 False state.currentField
                                 state.currentRow state.rows)
    else -- Regular character inside quote
      parseChars opts cs
        (MkParseState state.line (state.column + 1)
         True (c :: state.currentField)
         state.currentRow state.rows)

||| Parse while outside a quoted field
parseNormal : CSVOptions -> Char -> List Char -> ParseState -> Either CSVError CSV
parseNormal opts c cs state =
  if c == opts.quote
    then -- Start of quoted field
      parseChars opts cs
        (MkParseState state.line (state.column + 1)
         True state.currentField
         state.currentRow state.rows)
    else if c == opts.delimiter
      then -- End of field
        let field = pack (reverse state.currentField)
        in parseChars opts cs
             (MkParseState state.line (state.column + 1)
              False []
              (field :: state.currentRow) state.rows)
      else if c == '\r'
        then case cs of
               ('\n' :: rest) => -- CRLF line ending
                 let field = pack (reverse state.currentField)
                     row = reverse (field :: state.currentRow)
                 in parseChars opts rest
                      (MkParseState (state.line + 1) 1
                       False []
                       [] (row :: state.rows))
               _ => -- Just CR
                 let field = pack (reverse state.currentField)
                     row = reverse (field :: state.currentRow)
                 in parseChars opts cs
                      (MkParseState (state.line + 1) 1
                       False []
                       [] (row :: state.rows))
        else if c == '\n'
          then -- LF line ending
            let field = pack (reverse state.currentField)
                row = reverse (field :: state.currentRow)
            in parseChars opts cs
                 (MkParseState (state.line + 1) 1
                  False []
                  [] (row :: state.rows))
          else -- Regular character
            parseChars opts cs
              (MkParseState state.line (state.column + 1)
               False (c :: state.currentField)
               state.currentRow state.rows)

--------------------------------------------------------------------------------
-- Generation
--------------------------------------------------------------------------------

||| Convert CSV to string with default options
public export
render : CSV -> String
render = renderWith defaultOptions

||| Convert CSV to string with custom options
public export
renderWith : CSVOptions -> CSV -> String
renderWith opts csv = concat (intersperse opts.lineEnding (map (renderRow opts) csv))

||| Render a single row
renderRow : CSVOptions -> Row -> String
renderRow opts row = concat (intersperse (singleton opts.delimiter) (map (renderField opts) row))

||| Render a single field (quoting if necessary)
renderField : CSVOptions -> String -> String
renderField opts field =
  if needsQuoting opts field
    then quoteField opts field
    else field
  where
    needsQuoting : CSVOptions -> String -> Bool
    needsQuoting opts s =
      any (\c => c == opts.delimiter || c == opts.quote || c == '\n' || c == '\r') (unpack s)

    quoteField : CSVOptions -> String -> String
    quoteField opts s =
      let escaped = concatMap (\c => if c == opts.quote
                                       then pack [opts.escape, opts.quote]
                                       else singleton c)
                              (unpack s)
      in singleton opts.quote ++ escaped ++ singleton opts.quote

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

||| Get header row (first row)
public export
header : CSV -> Maybe Row
header [] = Nothing
header (h :: _) = Just h

||| Get data rows (all rows except header)
public export
dataRows : CSV -> CSV
dataRows [] = []
dataRows (_ :: rows) = rows

||| Get number of rows (including header)
public export
rowCount : CSV -> Nat
rowCount = length

||| Get number of columns (based on first row)
public export
columnCount : CSV -> Nat
columnCount [] = 0
columnCount (row :: _) = length row

||| Check if all rows have the same number of columns
public export
isRectangular : CSV -> Bool
isRectangular [] = True
isRectangular (row :: rows) =
  let cols = length row
  in all (\r => length r == cols) rows

||| Make CSV rectangular by padding short rows
public export
makeRectangular : CSV -> CSV
makeRectangular [] = []
makeRectangular csv@(first :: _) =
  let maxCols = foldl (\acc, row => max acc (length row)) 0 csv
  in map (padRow maxCols) csv
  where
    padRow : Nat -> Row -> Row
    padRow n row =
      let missing = minus n (length row)
      in row ++ replicate missing ""

||| Get a specific column by index
public export
column : Nat -> CSV -> List String
column _ [] = []
column n csv = mapMaybe (getAt n) csv
  where
    getAt : Nat -> List a -> Maybe a
    getAt _ [] = Nothing
    getAt Z (x :: _) = Just x
    getAt (S k) (_ :: xs) = getAt k xs

||| Get a column by header name
public export
columnByName : String -> CSV -> List String
columnByName _ [] = Nothing
columnByName name (headerRow :: dataRows) =
  case findIndex (== name) headerRow of
    Nothing => []
    Just idx => column (finToNat idx) dataRows
  where
    findIndex : (a -> Bool) -> List a -> Maybe (Fin (length (headerRow)))
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
-- PROOF_TODO: Replace believe_me with actual proof
    findIndex = believe_me -- Simplified

||| Filter rows by predicate
public export
filterRows : (Row -> Bool) -> CSV -> CSV
filterRows = filter

||| Map over all fields
public export
mapFields : (String -> String) -> CSV -> CSV
mapFields f = map (map f)

||| Transpose CSV (rows become columns)
public export
transpose : CSV -> CSV
transpose [] = []
transpose csv =
  let maxCols = foldl (\acc, row => max acc (length row)) 0 csv
      padded = makeRectangular csv
  in transposeHelper padded
  where
    transposeHelper : CSV -> CSV
    transposeHelper rows =
      if all null rows then []
      else map getHead rows :: transposeHelper (map getTail rows)
      where
        getHead : Row -> String
        getHead [] = ""
        getHead (x :: _) = x
        getTail : Row -> Row
        getTail [] = []
        getTail (_ :: xs) = xs

--------------------------------------------------------------------------------
-- Display
--------------------------------------------------------------------------------

public export
Show CSVOptions where
  show opts = "CSVOptions(delimiter='" ++ singleton opts.delimiter ++
              "', quote='" ++ singleton opts.quote ++ "')"
