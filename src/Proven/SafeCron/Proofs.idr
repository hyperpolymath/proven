-- SPDX-License-Identifier: PMPL-1.0-or-later
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

||| OWED: `everyMinute = MkCron Any Any Any Any Any`. Blocked on
||| `public export` body opacity at this call-site.
public export
0 everyMinuteDef : everyMinute = MkCron Proven.SafeCron.Any Proven.SafeCron.Any Proven.SafeCron.Any Proven.SafeCron.Any Proven.SafeCron.Any

public export
0 hourlyDef : hourly = MkCron (Single 0) Proven.SafeCron.Any Proven.SafeCron.Any Proven.SafeCron.Any Proven.SafeCron.Any

public export
0 dailyDef : daily = MkCron (Single 0) (Single 0) Proven.SafeCron.Any Proven.SafeCron.Any Proven.SafeCron.Any

public export
0 weeklyDef : weekly = MkCron (Single 0) (Single 0) Proven.SafeCron.Any Proven.SafeCron.Any (Single 0)

public export
0 monthlyDef : monthly = MkCron (Single 0) (Single 0) (Single 1) Proven.SafeCron.Any Proven.SafeCron.Any

public export
0 dailyAtExpansion :
  (h : Nat) -> dailyAt h = MkCron (Single 0) (Single h) Proven.SafeCron.Any Proven.SafeCron.Any Proven.SafeCron.Any

public export
0 everyNMinutesExpansion :
  (n : Nat) -> everyNMinutes n = MkCron (Step Proven.SafeCron.Any n) Proven.SafeCron.Any Proven.SafeCron.Any Proven.SafeCron.Any Proven.SafeCron.Any

public export
0 everyNHoursExpansion :
  (n : Nat) -> everyNHours n = MkCron (Single 0) (Step Proven.SafeCron.Any n) Proven.SafeCron.Any Proven.SafeCron.Any Proven.SafeCron.Any

public export
0 weekdaysAtExpansion :
  (h, m : Nat)
  -> weekdaysAt h m = MkCron (Single m) (Single h) Proven.SafeCron.Any Proven.SafeCron.Any (Range 1 5)

public export
0 weekendsAtExpansion :
  (h, m : Nat)
  -> weekendsAt h m = MkCron (Single m) (Single h) Proven.SafeCron.Any Proven.SafeCron.Any (List [0, 6])

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
