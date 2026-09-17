-- SPDX-License-Identifier: MPL-2.0
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

||| DISCHARGED: `filterRows (\_ => True) csv = csv`. By the definition
||| `filterRows = filter`, the `(x :: xs)` case reduces to
||| `if (\_ => True) x then x :: filter ... xs else filter ... xs`,
||| then to `if True then x :: ... else ...`, then to `x :: ...`.
||| Inductive step closed by `cong (x ::)` applied to the recursive
||| call. The OWED comment claimed the lambda doesn't unfold inside
||| `if`, but empirically (Idris2 0.8.0, `/tmp/charrefl` harness) it
||| does — the cons-arm reduces by Refl after lambda beta-reduction.
public export
filterTrueIdentity : (csv : CSV) -> filterRows (\_ => True) csv = csv
filterTrueIdentity [] = Refl
filterTrueIdentity (x :: xs) = cong (x ::) (filterTrueIdentity xs)

||| Helper: `map id xs = xs` for any `List`. Used to discharge
||| `mapFieldsIdentity` below at both the outer (rows) and inner
||| (fields) list levels. Idris2 0.8.0's `Data.List` does not ship
||| this as a `Refl`-reducible lemma, so it lives here.
mapIdHelper : (xs : List a) -> map Prelude.id xs = xs
mapIdHelper [] = Refl
mapIdHelper (x :: xs) = cong (x ::) (mapIdHelper xs)

||| DISCHARGED: `mapFields id csv = csv`. By the definition
||| `mapFields f = map (map f)`, the `(x :: xs)` case reduces to
||| `map id x :: map (map id) xs`. Outer cons rewritten via
||| `mapIdHelper x` on the head (giving `x ::`), then inductive
||| step on the tail via `mapFieldsIdentity xs`. Composed with
||| `trans` to match `(map id x) :: (map (map id) xs) = x :: xs`.
public export
mapFieldsIdentity : (csv : CSV) -> mapFields Prelude.id csv = csv
mapFieldsIdentity [] = Refl
mapFieldsIdentity (x :: xs) =
  trans (cong (:: map (map Prelude.id) xs) (mapIdHelper x))
        (cong (x ::) (mapFieldsIdentity xs))

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
--
-- The three claims below state that `defaultOptions` has comma /
-- double-quote / matching-escape. Operationally true by the
-- `defaultOptions = MkCSVOptions ',' '"' '"' "\r\n"` body in
-- `Proven.SafeCSV`, BUT Idris2 0.8.0 does NOT reduce a `public
-- export` top-level definition through `.fieldName` projection at
-- type-check time — `defaultOptions.delimiter` does not normalise to
-- `(MkCSVOptions ',' '"' '"' "\r\n").delimiter` by Refl alone, even
-- though `(MkCSVOptions ',' '"' '"' "\r\n").delimiter = ','` IS
-- Refl-discharable. Empirically verified at `/tmp/charrefl/src/
-- TestRecField.idr`. Same blocker family as the pre-fix SafeFile
-- `updateAfterRead`/`updateAfterWrite` issue (proven#136 fix
-- inlined the constructor on a function with an argument; here the
-- record field is a top-level constant with no argument to attach a
-- visibility-bumped function to).
--
-- Discharge once Idris2 fixes the projection-through-top-level-def
-- reduction, OR by re-stating each lemma at the constructor form
-- `(MkCSVOptions ',' '"' '"' "\r\n").delimiter = ','` and leaving
-- the abstract `defaultOptions.delimiter = ','` form as a bridge
-- once a `defaultOptionsIsCtor` lemma is admitted (which itself
-- requires the same reduction we're missing).
--
-- Stated as OWED postulates parallel to SafeBuffer's I6/I7 pattern.
--------------------------------------------------------------------------------

||| OWED: Default delimiter is comma.
public export
0 defaultDelimiterIsComma : defaultOptions.delimiter = ','

||| OWED: Default quote character is double-quote.
public export
0 defaultQuoteIsDoubleQuote : defaultOptions.quote = '"'

||| OWED: Default escape character matches quote character (RFC 4180).
public export
0 defaultEscapeMatchesQuote : defaultOptions.escape = defaultOptions.quote

--------------------------------------------------------------------------------
-- Constructor-form witnesses of the same facts
--
-- These three are the same theorems as the OWED above but stated at
-- the explicit-constructor LHS, so the `.fieldName` projection
-- reduces by Refl. They are the proof-debt-free witnesses callers
-- should prefer when they have flexibility in how to reference the
-- default options shape.
--------------------------------------------------------------------------------

||| Constructor-form: the explicit `MkCSVOptions ',' '"' '"' "\r\n"`
||| (which `defaultOptions` is judgmentally equal to at runtime) has
||| delimiter `','`.
public export
defaultDelimiterIsCommaCtor : (MkCSVOptions ',' '"' '"' "\r\n").delimiter = ','
defaultDelimiterIsCommaCtor = Refl

||| Constructor-form: quote character is `'"'`.
public export
defaultQuoteIsDoubleQuoteCtor : (MkCSVOptions ',' '"' '"' "\r\n").quote = '"'
defaultQuoteIsDoubleQuoteCtor = Refl

||| Constructor-form: escape matches quote.
public export
defaultEscapeMatchesQuoteCtor :
  (MkCSVOptions ',' '"' '"' "\r\n").escape = (MkCSVOptions ',' '"' '"' "\r\n").quote
defaultEscapeMatchesQuoteCtor = Refl
