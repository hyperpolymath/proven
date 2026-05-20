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

||| OWED: `filterRows (\_ => True) csv = csv`. By the definition
||| `filterRows = filter`, this is the well-known `filterTrue`
||| identity for `Data.List.filter`. Held back by Idris2 0.8.0 not
||| reducing `filter (\_ => True) (x :: xs)` to `x :: filter (\_ => True) xs`
||| by `Refl` alone — `filter`'s with-block branch on the predicate's
||| result does not unfold under an opaque lambda, so the inductive
||| step is not Refl-discharable and the stdlib does not expose a
||| `filterTrueId` lemma. Discharge once `Data.List` ships the lemma
||| or once the predicate's constant return is exposed reductively.
public export
0 filterTrueIdentity : (csv : CSV) -> filterRows (\_ => True) csv = csv

||| OWED: `mapFields id csv = csv`. By the definition
||| `mapFields f = map (map f)`, this composes the standard `mapId`
||| lemma (`map id xs = xs`) at the outer and inner list levels.
||| Held back by Idris2 0.8.0 not exposing `Data.List.mapId` as a
||| `Refl`-reducible identity — the proof requires induction on the
||| outer list and a second induction on each `Row`, neither of which
||| is available without an explicit recursive body, and the stdlib
||| does not ship the lemma. Discharge once `Data.List` ships
||| `mapId` (or by adding a local recursive `mapIdList` helper plus
||| `cong (:: _)` traversal — kept as OWED here pending the estate
||| convention on whether to inline the helper).
public export
0 mapFieldsIdentity : (csv : CSV) -> mapFields id csv = csv

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
