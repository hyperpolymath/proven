-- SPDX-License-Identifier: MPL-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
||| Safety proofs for SafeCron cron-expression evaluation.
|||
||| Discharges Show wire-format anchors for `CronField` and structural
||| anchors for common cron-expression presets.
|||
||| Zero `believe_me` / `idris_crash`.
module Proven.SafeCron.Proofs

import Proven.SafeCron

%default total

--------------------------------------------------------------------------------
-- CronField Show anchors
--------------------------------------------------------------------------------

public export
anyShow : show Proven.SafeCron.Any = "*"
anyShow = Refl

--------------------------------------------------------------------------------
-- Common-expression structural anchors (smart constructors that
-- bottom out in direct `MkCron` invocation reduce by Refl)
--------------------------------------------------------------------------------

||| DISCHARGED: `everyMinute = MkCron Any Any Any Any Any`. The smart
||| constructor is `public export` with a concrete body in SafeCron.idr
||| L166-168, so the elaborator unfolds it for `Refl`. Same pattern as
||| `anyShow = Refl` above.
public export
everyMinuteDef : everyMinute = MkCron Proven.SafeCron.Any Proven.SafeCron.Any Proven.SafeCron.Any Proven.SafeCron.Any Proven.SafeCron.Any
everyMinuteDef = Refl

||| DISCHARGED: same shape — `hourly` is `public export` (SafeCron.idr L171-173).
public export
hourlyDef : hourly = MkCron (Single 0) Proven.SafeCron.Any Proven.SafeCron.Any Proven.SafeCron.Any Proven.SafeCron.Any
hourlyDef = Refl

||| DISCHARGED: same shape — `daily` is `public export` (SafeCron.idr L176-178).
public export
dailyDef : daily = MkCron (Single 0) (Single 0) Proven.SafeCron.Any Proven.SafeCron.Any Proven.SafeCron.Any
dailyDef = Refl

||| DISCHARGED: same shape — `weekly` is `public export` (SafeCron.idr L186-188).
public export
weeklyDef : weekly = MkCron (Single 0) (Single 0) Proven.SafeCron.Any Proven.SafeCron.Any (Single 0)
weeklyDef = Refl

||| DISCHARGED: same shape — `monthly` is `public export` (SafeCron.idr L191-193).
public export
monthlyDef : monthly = MkCron (Single 0) (Single 0) (Single 1) Proven.SafeCron.Any Proven.SafeCron.Any
monthlyDef = Refl

||| DISCHARGED: `dailyAt h` is `public export` (SafeCron.idr L181-183) and
||| its body `MkCron (Single 0) (Single h) Any Any Any` reduces for any `h`.
public export
dailyAtExpansion :
  (h : Nat) -> dailyAt h = MkCron (Single 0) (Single h) Proven.SafeCron.Any Proven.SafeCron.Any Proven.SafeCron.Any
dailyAtExpansion _ = Refl

||| DISCHARGED: `everyNMinutes n` is `public export` (SafeCron.idr L196-198).
public export
everyNMinutesExpansion :
  (n : Nat) -> everyNMinutes n = MkCron (Step Proven.SafeCron.Any n) Proven.SafeCron.Any Proven.SafeCron.Any Proven.SafeCron.Any Proven.SafeCron.Any
everyNMinutesExpansion _ = Refl

||| DISCHARGED: `everyNHours n` is `public export` (SafeCron.idr L201-203).
public export
everyNHoursExpansion :
  (n : Nat) -> everyNHours n = MkCron (Single 0) (Step Proven.SafeCron.Any n) Proven.SafeCron.Any Proven.SafeCron.Any Proven.SafeCron.Any
everyNHoursExpansion _ = Refl

||| DISCHARGED: `weekdaysAt h m` is `public export` (SafeCron.idr L206-208).
public export
weekdaysAtExpansion :
  (h, m : Nat)
  -> weekdaysAt h m = MkCron (Single m) (Single h) Proven.SafeCron.Any Proven.SafeCron.Any (Range 1 5)
weekdaysAtExpansion _ _ = Refl

||| DISCHARGED: `weekendsAt h m` is `public export` (SafeCron.idr L211-213).
public export
weekendsAtExpansion :
  (h, m : Nat)
  -> weekendsAt h m = MkCron (Single m) (Single h) Proven.SafeCron.Any Proven.SafeCron.Any (List [0, 6])
weekendsAtExpansion _ _ = Refl

--------------------------------------------------------------------------------
-- `MkTime` record projections
--------------------------------------------------------------------------------

public export
mkTimeMinute : (mi, h, dom, mo, dow : Nat)
            -> (MkTime mi h dom mo dow).minute = mi
mkTimeMinute mi h dom mo dow = Refl

public export
mkTimeHour : (mi, h, dom, mo, dow : Nat)
          -> (MkTime mi h dom mo dow).hour = h
mkTimeHour mi h dom mo dow = Refl
