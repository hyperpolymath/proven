-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
||| Safety proofs for SafeDateTime.
|||
||| This module discharges the SafeDateTime safety contract with genuine,
||| machine-checked theorems. There are no `believe_me`, `idris_crash`,
||| `assert_*`, holes, or `%default partial` — `%default total` holds and
||| `idris2 0.8.0 --check` returns exit 0.
|||
||| It replaces what was a 5-line header-only stub (a "proof absence
||| disguised as presence", flagged CRITICAL in PROOF-NEEDS.md). The
||| theorems below are about the module's actual exported surface:
|||
|||  * `daysInMonth` always returns a value in the calendar-valid band
|||    28..31 (so it is non-zero — no division/indexing hazard, and the
|||    "overflow protection" claim has a concrete lower/upper bound).
|||  * `makeDate` is *sound*: any `Date` it returns satisfies the full
|||    validation guard (year 1..9999, day 1..daysInMonth) — i.e. the
|||    smart constructor cannot manufacture an out-of-range date. This
|||    is the real "validated via smart constructors" / "safe parsing
|||    without exceptions" guarantee.
module Proven.SafeDateTime.Proofs

import Proven.SafeDateTime

%default total

--------------------------------------------------------------------------------
-- daysInMonth is always within the Gregorian-calendar band 28..31
--------------------------------------------------------------------------------

||| Every month has at least 28 days. For February this holds in both the
||| leap (29) and common (28) cases, so the result is never zero — there
||| is no degenerate month that could underflow day arithmetic or an
||| indexer.
export
daysInMonthLowerBound : (y : Nat) -> (m : Month) -> (daysInMonth y m >= 28) = True
daysInMonthLowerBound y February with (isLeapYear y)
  daysInMonthLowerBound y February | True  = Refl
  daysInMonthLowerBound y February | False = Refl
daysInMonthLowerBound _ January   = Refl
daysInMonthLowerBound _ March     = Refl
daysInMonthLowerBound _ April     = Refl
daysInMonthLowerBound _ May       = Refl
daysInMonthLowerBound _ June      = Refl
daysInMonthLowerBound _ July      = Refl
daysInMonthLowerBound _ August    = Refl
daysInMonthLowerBound _ September = Refl
daysInMonthLowerBound _ October   = Refl
daysInMonthLowerBound _ November  = Refl
daysInMonthLowerBound _ December  = Refl

||| Every month has at most 31 days — an upper bound on the day
||| component, pairing with the lower bound to pin `daysInMonth` to the
||| valid band.
export
daysInMonthUpperBound : (y : Nat) -> (m : Month) -> (daysInMonth y m <= 31) = True
daysInMonthUpperBound y February with (isLeapYear y)
  daysInMonthUpperBound y February | True  = Refl
  daysInMonthUpperBound y February | False = Refl
daysInMonthUpperBound _ January   = Refl
daysInMonthUpperBound _ March     = Refl
daysInMonthUpperBound _ April     = Refl
daysInMonthUpperBound _ May       = Refl
daysInMonthUpperBound _ June      = Refl
daysInMonthUpperBound _ July      = Refl
daysInMonthUpperBound _ August    = Refl
daysInMonthUpperBound _ September = Refl
daysInMonthUpperBound _ October   = Refl
daysInMonthUpperBound _ November  = Refl
daysInMonthUpperBound _ December  = Refl

||| Corollary: no month has zero days (immediate from the lower bound; a
||| convenient form for callers that only need non-degeneracy).
export
daysInMonthNonZero : (y : Nat) -> (m : Month) -> (daysInMonth y m >= 1) = True
daysInMonthNonZero y February with (isLeapYear y)
  daysInMonthNonZero y February | True  = Refl
  daysInMonthNonZero y February | False = Refl
daysInMonthNonZero _ January   = Refl
daysInMonthNonZero _ March     = Refl
daysInMonthNonZero _ April     = Refl
daysInMonthNonZero _ May       = Refl
daysInMonthNonZero _ June      = Refl
daysInMonthNonZero _ July      = Refl
daysInMonthNonZero _ August    = Refl
daysInMonthNonZero _ September = Refl
daysInMonthNonZero _ October   = Refl
daysInMonthNonZero _ November  = Refl
daysInMonthNonZero _ December  = Refl

--------------------------------------------------------------------------------
-- makeDate soundness: a returned Date always satisfies the validation guard
--------------------------------------------------------------------------------

||| The exact boolean guard `makeDate` checks before it will return a
||| `Date`. Naming the predicate makes the soundness theorem read as
||| "any returned Date satisfies the guard", which is its meaning.
public export
dateGuard : (year : Nat) -> (month : Month) -> (day : Nat) -> Bool
dateGuard year month day =
  year >= 1 && year <= 9999 && day >= 1 && day <= daysInMonth year month

||| Soundness of the `makeDate` smart constructor: if it returns
||| `Just dt`, then `dt` satisfies the full validation guard. Hence
||| `makeDate` can never manufacture an out-of-range date — the genuine
||| content of "validated via smart constructors" / "safe parsing
||| without exceptions".
|||
||| Proof outline: invert the `Maybe` do-block. `natToMonth mn` is
||| `Nothing` (then `makeDate = Nothing`, contradicting `Just dt`) or
||| `Just m`; then split the `if` guard. The `True` branch fixes
||| `dt = MkDate y m d` by injectivity of `Just`, so the guard proof
||| transports onto `dt`'s own fields. The `False` branch is again
||| `Nothing = Just dt`, absurd.
export
makeDateSound : (y, mn, d : Nat) -> (dt : Date) ->
                makeDate y mn d = Just dt ->
                dateGuard dt.year dt.month dt.day = True
makeDateSound y mn d dt prf with (natToMonth mn)
  makeDateSound y mn d dt prf | Nothing = absurd prf
  makeDateSound y mn d dt prf | (Just m) with (dateGuard y m d) proof gEq
    makeDateSound y mn d dt prf | (Just m) | True =
      case prf of
        Refl => gEq
    makeDateSound y mn d dt prf | (Just m) | False = absurd prf
