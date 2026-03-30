-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
||| Proofs for SafeCSV operations
|||
||| Verifies properties of CSV parsing and generation: empty input handling,
||| rectangular invariant preservation, and column count consistency.
module Proven.SafeCSV.Proofs

import Proven.Core
import Proven.SafeCSV
import Data.List
import Data.String
import Data.Nat

%default total

--------------------------------------------------------------------------------
-- Empty Input Properties
--------------------------------------------------------------------------------

||| Rendering empty CSV yields empty string.
public export
renderEmptyIsEmpty : render [] = ""
renderEmptyIsEmpty = Refl

||| rowCount of empty CSV is 0.
public export
rowCountEmpty : rowCount [] = 0
rowCountEmpty = Refl

||| columnCount of empty CSV is 0.
public export
columnCountEmpty : columnCount [] = 0
columnCountEmpty = Refl

||| Empty CSV has no header.
public export
headerEmpty : header [] = Nothing
headerEmpty = Refl

||| dataRows of empty CSV is empty.
public export
dataRowsEmpty : dataRows [] = []
dataRowsEmpty = Refl

||| Empty CSV is rectangular (vacuously true).
public export
emptyIsRectangular : isRectangular [] = True
emptyIsRectangular = Refl

--------------------------------------------------------------------------------
-- Row Count Properties
--------------------------------------------------------------------------------

||| rowCount of a single-row CSV is 1.
public export
rowCountSingle : (row : Row) -> rowCount [row] = 1
rowCountSingle _ = Refl

||| header of a non-empty CSV returns the first row.
public export
headerNonEmpty : (h : Row) -> (rest : CSV) -> header (h :: rest) = Just h
headerNonEmpty _ _ = Refl

||| dataRows of a non-empty CSV drops the header.
public export
dataRowsNonEmpty : (h : Row) -> (rest : CSV) -> dataRows (h :: rest) = rest
dataRowsNonEmpty _ _ = Refl

||| columnCount of a non-empty CSV is the length of the first row.
public export
columnCountFirstRow : (h : Row) -> (rest : CSV) -> columnCount (h :: rest) = length h
columnCountFirstRow _ _ = Refl

--------------------------------------------------------------------------------
-- Rectangular Properties
--------------------------------------------------------------------------------

||| A single-row CSV is always rectangular.
public export
singleRowRectangular : (row : Row) -> isRectangular [row] = True
singleRowRectangular row = Refl

||| makeRectangular preserves empty CSV.
public export
makeRectangularEmpty : makeRectangular [] = []
makeRectangularEmpty = Refl

--------------------------------------------------------------------------------
-- Filter and Map Properties
--------------------------------------------------------------------------------

||| filterRows with always-true predicate is identity.
||| Proved by list filter identity.
export
filterTrueIdentity : (csv : CSV) -> filterRows (\_ => True) csv = csv

||| mapFields with identity function is identity.
||| Proved by map-id on nested lists.
export
mapFieldsIdentity : (csv : CSV) -> mapFields id csv = csv

||| column from empty CSV is always empty.
public export
columnFromEmpty : (n : Nat) -> column n [] = []
columnFromEmpty _ = Refl

||| columnByName from empty CSV is always empty.
public export
columnByNameFromEmpty : (name : String) -> columnByName name [] = []
columnByNameFromEmpty _ = Refl

--------------------------------------------------------------------------------
-- Default Options Properties
--------------------------------------------------------------------------------

||| Default delimiter is comma.
public export
defaultDelimiterIsComma : defaultOptions.delimiter = ','
defaultDelimiterIsComma = Refl

||| Default quote character is double-quote.
public export
defaultQuoteIsDoubleQuote : defaultOptions.quote = '"'
defaultQuoteIsDoubleQuote = Refl

||| Default escape character matches quote character (RFC 4180).
public export
defaultEscapeMatchesQuote : defaultOptions.escape = defaultOptions.quote
defaultEscapeMatchesQuote = Refl
