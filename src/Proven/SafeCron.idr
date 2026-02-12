-- SPDX-License-Identifier: Apache-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
||| SafeCron - Safe cron expression parsing and scheduling
|||
||| This module provides safe parsing and evaluation of cron
||| expressions for task scheduling.
module Proven.SafeCron
import Data.String
import Data.List

import public Proven.Core

%default total

--------------------------------------------------------------------------------
-- Cron Field Types
--------------------------------------------------------------------------------

||| A cron field value
public export
data CronField : Type where
  ||| All values (*)
  Any : CronField
  ||| Single value
  Single : Nat -> CronField
  ||| Range of values (start-end)
  Range : (start : Nat) -> (end : Nat) -> CronField
  ||| Step values (*/step or start/step)
  Step : (base : CronField) -> (step : Nat) -> CronField
  ||| List of values
  List : List Nat -> CronField

public export
Show CronField where
  show Any = "*"
  show (Single n) = show n
  show (Range s e) = show s ++ "-" ++ show e
  show (Step base step) = show base ++ "/" ++ show step
  show (List ns) = showList ns
    where
      showList : List Nat -> String
      showList [] = ""
      showList [x] = show x
      showList (x :: xs) = show x ++ "," ++ showList xs

--------------------------------------------------------------------------------
-- Cron Expression
--------------------------------------------------------------------------------

||| A complete cron expression (minute hour dayOfMonth month dayOfWeek)
public export
record CronExpr where
  constructor MkCron
  minute : CronField       -- 0-59
  hour : CronField         -- 0-23
  dayOfMonth : CronField   -- 1-31
  month : CronField        -- 1-12
  dayOfWeek : CronField    -- 0-6 (Sunday=0)

||| Field bounds for validation
public export
record FieldBounds where
  constructor MkBounds
  minVal : Nat
  maxVal : Nat

minuteBounds : FieldBounds
minuteBounds = MkBounds 0 59

hourBounds : FieldBounds
hourBounds = MkBounds 0 23

dayOfMonthBounds : FieldBounds
dayOfMonthBounds = MkBounds 1 31

monthBounds : FieldBounds
monthBounds = MkBounds 1 12

dayOfWeekBounds : FieldBounds
dayOfWeekBounds = MkBounds 0 6

--------------------------------------------------------------------------------
-- Validation
--------------------------------------------------------------------------------

||| Check if a value is within bounds
inBounds : FieldBounds -> Nat -> Bool
inBounds bounds n = n >= bounds.minVal && n <= bounds.maxVal

||| Validate a cron field against bounds
public export
validateField : FieldBounds -> CronField -> Bool
validateField _ Any = True
validateField bounds (Single n) = inBounds bounds n
validateField bounds (Range s e) = inBounds bounds s && inBounds bounds e && s <= e
validateField bounds (Step base step) = validateField bounds base && step > 0
validateField bounds (List ns) = all (inBounds bounds) ns

||| Validate an entire cron expression
public export
validateCron : CronExpr -> Bool
validateCron cron =
  validateField minuteBounds cron.minute &&
  validateField hourBounds cron.hour &&
  validateField dayOfMonthBounds cron.dayOfMonth &&
  validateField monthBounds cron.month &&
  validateField dayOfWeekBounds cron.dayOfWeek

--------------------------------------------------------------------------------
-- Field Matching
--------------------------------------------------------------------------------

||| Expand a field to all matching values
public export
expandField : FieldBounds -> CronField -> List Nat
expandField bounds Any = [bounds.minVal .. bounds.maxVal]
expandField _ (Single n) = [n]
expandField _ (Range s e) = [s .. e]
expandField bounds (Step base step) =
  let baseValues = expandField bounds base
      filtered = stepFilter step baseValues 0
  in filtered
  where
    stepFilter : Nat -> List Nat -> Nat -> List Nat
    stepFilter _ [] _ = []
    stepFilter s (x :: xs) idx =
      if idx `mod` s == 0
        then x :: stepFilter s xs (S idx)
        else stepFilter s xs (S idx)
expandField _ (List ns) = ns

||| Check if a value matches a cron field
public export
matchesField : FieldBounds -> CronField -> Nat -> Bool
matchesField bounds field val = any (== val) (expandField bounds field)

--------------------------------------------------------------------------------
-- Time Representation
--------------------------------------------------------------------------------

||| A point in time for cron matching
public export
record CronTime where
  constructor MkTime
  minute : Nat       -- 0-59
  hour : Nat         -- 0-23
  dayOfMonth : Nat   -- 1-31
  month : Nat        -- 1-12
  dayOfWeek : Nat    -- 0-6

||| Check if a time matches a cron expression
public export
matches : CronExpr -> CronTime -> Bool
matches cron time =
  matchesField minuteBounds cron.minute time.minute &&
  matchesField hourBounds cron.hour time.hour &&
  matchesField dayOfMonthBounds cron.dayOfMonth time.dayOfMonth &&
  matchesField monthBounds cron.month time.month &&
  matchesField dayOfWeekBounds cron.dayOfWeek time.dayOfWeek

--------------------------------------------------------------------------------
-- Common Expressions
--------------------------------------------------------------------------------

||| Every minute
public export
everyMinute : CronExpr
everyMinute = MkCron Any Any Any Any Any

||| Every hour (at minute 0)
public export
hourly : CronExpr
hourly = MkCron (Single 0) Any Any Any Any

||| Every day at midnight
public export
daily : CronExpr
daily = MkCron (Single 0) (Single 0) Any Any Any

||| Every day at a specific hour
public export
dailyAt : (hour : Nat) -> CronExpr
dailyAt h = MkCron (Single 0) (Single h) Any Any Any

||| Every week on Sunday at midnight
public export
weekly : CronExpr
weekly = MkCron (Single 0) (Single 0) Any Any (Single 0)

||| First day of every month at midnight
public export
monthly : CronExpr
monthly = MkCron (Single 0) (Single 0) (Single 1) Any Any

||| Every N minutes
public export
everyNMinutes : (n : Nat) -> CronExpr
everyNMinutes n = MkCron (Step Any n) Any Any Any Any

||| Every N hours
public export
everyNHours : (n : Nat) -> CronExpr
everyNHours n = MkCron (Single 0) (Step Any n) Any Any Any

||| Weekdays only (Monday-Friday) at a specific time
public export
weekdaysAt : (hour : Nat) -> (minute : Nat) -> CronExpr
weekdaysAt h m = MkCron (Single m) (Single h) Any Any (Range 1 5)

||| Weekends only (Saturday-Sunday) at a specific time
public export
weekendsAt : (hour : Nat) -> (minute : Nat) -> CronExpr
weekendsAt h m = MkCron (Single m) (Single h) Any Any (List [0, 6])

--------------------------------------------------------------------------------
-- Display
--------------------------------------------------------------------------------

public export
Show CronExpr where
  show cron = show cron.minute ++ " " ++
              show cron.hour ++ " " ++
              show cron.dayOfMonth ++ " " ++
              show cron.month ++ " " ++
              show cron.dayOfWeek

public export
Show CronTime where
  show t = show t.minute ++ ":" ++ show t.hour ++ " " ++
           show t.dayOfMonth ++ "/" ++ show t.month ++
           " (day " ++ show t.dayOfWeek ++ ")"

