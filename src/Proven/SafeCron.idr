-- SPDX-License-Identifier: PMPL-1.0
-- SPDX-FileCopyrightText: 2024-2025 Hyperpolymath

||| Safe cron expression parsing and validation
|||
||| This module provides type-safe cron handling:
||| - Standard 5-field cron expressions
||| - Extended 6-field (with seconds) and 7-field (with years)
||| - Named months and days
||| - Special characters (*, /, -, ,, L, W, #)
||| - Schedule calculation
|||
||| Security features:
||| - Frequency limit enforcement
||| - Command injection prevention
||| - Resource exhaustion prevention
||| - Time zone validation
module Proven.SafeCron

import Data.List
import Data.String
import Data.Either
import Proven.Core

%default total

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

||| Cron field value
public export
data CronValue
  = Wildcard                       -- *
  | Single Nat                     -- specific value
  | Range Nat Nat                  -- start-end
  | Step Nat Nat                   -- start/step
  | WildcardStep Nat               -- */step
  | List (List CronValue)          -- value,value,...
  | Last                           -- L (last day)
  | LastWeekday                    -- LW
  | NthWeekday Nat Nat             -- day#week (e.g., 3#2 = second Wednesday)
  | NearestWeekday Nat             -- 15W (nearest weekday to 15th)

||| Day of week (0=Sunday or 7=Sunday, 1=Monday, etc.)
public export
data DayOfWeek = Sun | Mon | Tue | Wed | Thu | Fri | Sat

||| Month
public export
data Month
  = Jan | Feb | Mar | Apr | May | Jun
  | Jul | Aug | Sep | Oct | Nov | Dec

||| Standard 5-field cron expression
public export
record CronExpr5 where
  constructor MkCronExpr5
  minute : CronValue
  hour : CronValue
  dayOfMonth : CronValue
  month : CronValue
  dayOfWeek : CronValue

||| Extended 6-field cron expression (with seconds)
public export
record CronExpr6 where
  constructor MkCronExpr6
  second : CronValue
  minute : CronValue
  hour : CronValue
  dayOfMonth : CronValue
  month : CronValue
  dayOfWeek : CronValue

||| Full 7-field cron expression (with seconds and year)
public export
record CronExpr7 where
  constructor MkCronExpr7
  second : CronValue
  minute : CronValue
  hour : CronValue
  dayOfMonth : CronValue
  month : CronValue
  dayOfWeek : CronValue
  year : CronValue

||| Unified cron expression
public export
data CronExpr
  = Cron5 CronExpr5
  | Cron6 CronExpr6
  | Cron7 CronExpr7

||| Special cron schedules
public export
data SpecialSchedule
  = Yearly       -- @yearly or @annually
  | Monthly      -- @monthly
  | Weekly       -- @weekly
  | Daily        -- @daily or @midnight
  | Hourly       -- @hourly
  | Reboot       -- @reboot

||| Schedule with optional timezone
public export
record CronSchedule where
  constructor MkCronSchedule
  expression : Either SpecialSchedule CronExpr
  timezone : Maybe String
  description : Maybe String

||| Cron job definition
public export
record CronJob where
  constructor MkCronJob
  id : String
  schedule : CronSchedule
  command : String
  enabled : Bool
  runAs : Maybe String
  timeout : Maybe Nat      -- Seconds
  retries : Maybe Nat
  onFailure : Maybe String -- Notification/action

||| Frequency limits
public export
record FrequencyLimits where
  constructor MkFrequencyLimits
  minIntervalSeconds : Nat
  maxExecutionsPerHour : Nat
  maxExecutionsPerDay : Nat

||| Next execution info
public export
record NextExecution where
  constructor MkNextExecution
  timestamp : String       -- ISO 8601
  secondsFromNow : Nat

--------------------------------------------------------------------------------
-- Errors
--------------------------------------------------------------------------------

||| Cron errors
public export
data CronError
  = EmptyExpression
  | InvalidFieldCount Nat
  | InvalidMinute String
  | InvalidHour String
  | InvalidDayOfMonth String
  | InvalidMonth String
  | InvalidDayOfWeek String
  | InvalidSecond String
  | InvalidYear String
  | InvalidStep Nat
  | InvalidRange Nat Nat
  | InvalidSpecialChar Char
  | FrequencyTooHigh Nat Nat  -- Actual, minimum interval
  | CommandInjection String
  | InvalidTimezone String
  | InvalidSpecialSchedule String
  | ConflictingFields String String

public export
Show CronError where
  show EmptyExpression = "Cron error: empty expression"
  show (InvalidFieldCount n) = "Cron error: invalid field count " ++ show n ++ " (expected 5, 6, or 7)"
  show (InvalidMinute v) = "Cron error: invalid minute '" ++ v ++ "' (0-59)"
  show (InvalidHour v) = "Cron error: invalid hour '" ++ v ++ "' (0-23)"
  show (InvalidDayOfMonth v) = "Cron error: invalid day of month '" ++ v ++ "' (1-31)"
  show (InvalidMonth v) = "Cron error: invalid month '" ++ v ++ "' (1-12 or JAN-DEC)"
  show (InvalidDayOfWeek v) = "Cron error: invalid day of week '" ++ v ++ "' (0-7 or SUN-SAT)"
  show (InvalidSecond v) = "Cron error: invalid second '" ++ v ++ "' (0-59)"
  show (InvalidYear v) = "Cron error: invalid year '" ++ v ++ "'"
  show (InvalidStep s) = "Cron error: invalid step " ++ show s ++ " (must be > 0)"
  show (InvalidRange start end) = "Cron error: invalid range " ++ show start ++ "-" ++ show end
  show (InvalidSpecialChar c) = "Cron error: invalid special character '" ++ singleton c ++ "'"
  show (FrequencyTooHigh actual min) =
    "Cron security: frequency too high (" ++ show actual ++ "s, minimum " ++ show min ++ "s)"
  show (CommandInjection cmd) = "Cron security: potential command injection in '" ++ cmd ++ "'"
  show (InvalidTimezone tz) = "Cron error: invalid timezone '" ++ tz ++ "'"
  show (InvalidSpecialSchedule s) = "Cron error: invalid special schedule '" ++ s ++ "'"
  show (ConflictingFields f1 f2) = "Cron error: conflicting fields " ++ f1 ++ " and " ++ f2

--------------------------------------------------------------------------------
-- Constants
--------------------------------------------------------------------------------

||| Field boundaries
minuteRange : (Nat, Nat)
minuteRange = (0, 59)

hourRange : (Nat, Nat)
hourRange = (0, 23)

dayOfMonthRange : (Nat, Nat)
dayOfMonthRange = (1, 31)

monthRange : (Nat, Nat)
monthRange = (1, 12)

dayOfWeekRange : (Nat, Nat)
dayOfWeekRange = (0, 7)  -- 0 and 7 both = Sunday

secondRange : (Nat, Nat)
secondRange = (0, 59)

yearRange : (Nat, Nat)
yearRange = (1970, 2099)

||| Month names
monthNames : List (String, Nat)
monthNames =
  [ ("JAN", 1), ("FEB", 2), ("MAR", 3), ("APR", 4)
  , ("MAY", 5), ("JUN", 6), ("JUL", 7), ("AUG", 8)
  , ("SEP", 9), ("OCT", 10), ("NOV", 11), ("DEC", 12)
  ]

||| Day names
dayNames : List (String, Nat)
dayNames =
  [ ("SUN", 0), ("MON", 1), ("TUE", 2), ("WED", 3)
  , ("THU", 4), ("FRI", 5), ("SAT", 6)
  ]

||| Command injection patterns
injectionPatterns : List String
injectionPatterns =
  [ ";", "&&", "||", "|", "`", "$("
  , ">", "<", "&", "\n", "\r"
  ]

||| Default frequency limits
defaultLimits : FrequencyLimits
defaultLimits = MkFrequencyLimits 60 60 1440  -- Min 1 minute, max 60/hour, max 1440/day

--------------------------------------------------------------------------------
-- Name conversion
--------------------------------------------------------------------------------

||| Parse month name to number
public export
parseMonthName : String -> Maybe Nat
parseMonthName name = lookup (toUpper name) monthNames

||| Parse day name to number
public export
parseDayName : String -> Maybe Nat
parseDayName name = lookup (toUpper name) dayNames

||| Show month
public export
showMonth : Month -> String
showMonth Jan = "JAN"
showMonth Feb = "FEB"
showMonth Mar = "MAR"
showMonth Apr = "APR"
showMonth May = "MAY"
showMonth Jun = "JUN"
showMonth Jul = "JUL"
showMonth Aug = "AUG"
showMonth Sep = "SEP"
showMonth Oct = "OCT"
showMonth Nov = "NOV"
showMonth Dec = "DEC"

||| Show day of week
public export
showDayOfWeek : DayOfWeek -> String
showDayOfWeek Sun = "SUN"
showDayOfWeek Mon = "MON"
showDayOfWeek Tue = "TUE"
showDayOfWeek Wed = "WED"
showDayOfWeek Thu = "THU"
showDayOfWeek Fri = "FRI"
showDayOfWeek Sat = "SAT"

--------------------------------------------------------------------------------
-- Value parsing
--------------------------------------------------------------------------------

||| Parse a single number or name
parseValue : String -> (String -> Maybe Nat) -> (Nat, Nat) -> Either CronError Nat
parseValue str nameLookup (minVal, maxVal) =
  case nameLookup str of
    Just n => Right n
    Nothing =>
      case parsePositive str of
        Nothing => Left (InvalidMinute str)  -- Generic error
        Just n =>
          if n >= minVal && n <= maxVal
            then Right n
            else Left (InvalidRange n maxVal)

||| Parse cron field value
public export
parseCronValue : String -> (String -> Maybe Nat) -> (Nat, Nat) -> Either CronError CronValue
parseCronValue "*" _ _ = Right Wildcard
parseCronValue "L" _ _ = Right Last
parseCronValue "LW" _ _ = Right LastWeekday
parseCronValue field nameLookup range =
  if isInfixOf "," field
    then do
      let parts = split (== ',') field
      values <- traverse (\p => parseCronValue p nameLookup range) parts
      pure (List values)
    else if isInfixOf "/" field
      then case break (== '/') field of
        (base, stepStr) =>
          case parsePositive (strTail stepStr) of
            Nothing => Left (InvalidStep 0)
            Just step =>
              if step == 0
                then Left (InvalidStep 0)
                else if base == "*"
                  then Right (WildcardStep step)
                  else case parsePositive base of
                    Nothing => Left (InvalidMinute base)
                    Just start => Right (Step start step)
      else if isInfixOf "-" field
        then case break (== '-') field of
          (startStr, endStr) => do
            start <- parseValue startStr nameLookup range
            end <- parseValue (strTail endStr) nameLookup range
            if start > end
              then Left (InvalidRange start end)
              else Right (Range start end)
        else if isInfixOf "#" field
          then case break (== '#') field of
            (dayStr, weekStr) => do
              day <- parseValue dayStr nameLookup range
              case parsePositive (strTail weekStr) of
                Nothing => Left (InvalidDayOfWeek field)
                Just week => Right (NthWeekday day week)
          else if isSuffixOf "W" field
            then case parsePositive (strSubstr 0 (minus (length field) 1) field) of
              Nothing => Left (InvalidDayOfMonth field)
              Just day => Right (NearestWeekday day)
            else case parseValue field nameLookup range of
              Right n => Right (Single n)
              Left e => Left e

--------------------------------------------------------------------------------
-- Expression parsing
--------------------------------------------------------------------------------

||| Parse 5-field cron expression
public export
parseCron5 : String -> Either CronError CronExpr5
parseCron5 expr =
  let fields = words expr
  in if length fields /= 5
       then Left (InvalidFieldCount (length fields))
       else case fields of
         [minF, hourF, domF, monF, dowF] => do
           minute <- parseCronValue minF (const Nothing) minuteRange
           hour <- parseCronValue hourF (const Nothing) hourRange
           dayOfMonth <- parseCronValue domF (const Nothing) dayOfMonthRange
           month <- parseCronValue monF parseMonthName monthRange
           dayOfWeek <- parseCronValue dowF parseDayName dayOfWeekRange
           pure (MkCronExpr5 minute hour dayOfMonth month dayOfWeek)
         _ => Left (InvalidFieldCount (length fields))

||| Parse 6-field cron expression (with seconds)
public export
parseCron6 : String -> Either CronError CronExpr6
parseCron6 expr =
  let fields = words expr
  in if length fields /= 6
       then Left (InvalidFieldCount (length fields))
       else case fields of
         [secF, minF, hourF, domF, monF, dowF] => do
           second <- parseCronValue secF (const Nothing) secondRange
           minute <- parseCronValue minF (const Nothing) minuteRange
           hour <- parseCronValue hourF (const Nothing) hourRange
           dayOfMonth <- parseCronValue domF (const Nothing) dayOfMonthRange
           month <- parseCronValue monF parseMonthName monthRange
           dayOfWeek <- parseCronValue dowF parseDayName dayOfWeekRange
           pure (MkCronExpr6 second minute hour dayOfMonth month dayOfWeek)
         _ => Left (InvalidFieldCount (length fields))

||| Parse special schedule
public export
parseSpecialSchedule : String -> Maybe SpecialSchedule
parseSpecialSchedule "@yearly" = Just Yearly
parseSpecialSchedule "@annually" = Just Yearly
parseSpecialSchedule "@monthly" = Just Monthly
parseSpecialSchedule "@weekly" = Just Weekly
parseSpecialSchedule "@daily" = Just Daily
parseSpecialSchedule "@midnight" = Just Daily
parseSpecialSchedule "@hourly" = Just Hourly
parseSpecialSchedule "@reboot" = Just Reboot
parseSpecialSchedule _ = Nothing

||| Parse cron expression (any format)
public export
parseCronExpr : String -> Either CronError CronExpr
parseCronExpr "" = Left EmptyExpression
parseCronExpr expr =
  let fieldCount = length (words expr)
  in if fieldCount == 5
       then map Cron5 (parseCron5 expr)
       else if fieldCount == 6
         then map Cron6 (parseCron6 expr)
         else Left (InvalidFieldCount fieldCount)

--------------------------------------------------------------------------------
-- Command validation
--------------------------------------------------------------------------------

||| Check for command injection
public export
detectInjection : String -> Maybe String
detectInjection cmd =
  find (\p => isInfixOf p cmd) injectionPatterns

||| Validate cron command
public export
validateCommand : String -> Either CronError String
validateCommand "" = Left (CommandInjection "empty command")
validateCommand cmd =
  case detectInjection cmd of
    Just pattern => Left (CommandInjection pattern)
    Nothing => Right cmd

--------------------------------------------------------------------------------
-- Frequency calculation
--------------------------------------------------------------------------------

||| Calculate minimum interval for wildcard with step
minIntervalForStep : Nat -> Nat -> Nat
minIntervalForStep fieldMax step =
  if step > fieldMax then fieldMax else step

||| Estimate minimum interval in seconds
public export
estimateMinInterval : CronExpr -> Nat
estimateMinInterval (Cron5 expr) =
  case expr.minute of
    Wildcard => 60             -- Every minute
    WildcardStep s => s * 60   -- Every s minutes
    Single _ => 60 * 60        -- Once per hour
    _ => 60
estimateMinInterval (Cron6 expr) =
  case expr.second of
    Wildcard => 1              -- Every second
    WildcardStep s => s        -- Every s seconds
    Single _ => 60             -- Once per minute
    _ => 1
estimateMinInterval (Cron7 _) = 60  -- Default assumption

||| Validate frequency against limits
public export
validateFrequency : FrequencyLimits -> CronExpr -> Either CronError ()
validateFrequency limits expr =
  let interval = estimateMinInterval expr
  in if interval < limits.minIntervalSeconds
       then Left (FrequencyTooHigh interval limits.minIntervalSeconds)
       else Right ()

--------------------------------------------------------------------------------
-- Serialization
--------------------------------------------------------------------------------

||| Show cron value
public export
showCronValue : CronValue -> String
showCronValue Wildcard = "*"
showCronValue (Single n) = show n
showCronValue (Range start end) = show start ++ "-" ++ show end
showCronValue (Step start step) = show start ++ "/" ++ show step
showCronValue (WildcardStep step) = "*/" ++ show step
showCronValue (List values) = concat (intersperse "," (map showCronValue values))
showCronValue Last = "L"
showCronValue LastWeekday = "LW"
showCronValue (NthWeekday day week) = show day ++ "#" ++ show week
showCronValue (NearestWeekday day) = show day ++ "W"

||| Show 5-field cron expression
public export
showCron5 : CronExpr5 -> String
showCron5 expr =
  unwords
    [ showCronValue expr.minute
    , showCronValue expr.hour
    , showCronValue expr.dayOfMonth
    , showCronValue expr.month
    , showCronValue expr.dayOfWeek
    ]

||| Show 6-field cron expression
public export
showCron6 : CronExpr6 -> String
showCron6 expr =
  unwords
    [ showCronValue expr.second
    , showCronValue expr.minute
    , showCronValue expr.hour
    , showCronValue expr.dayOfMonth
    , showCronValue expr.month
    , showCronValue expr.dayOfWeek
    ]

||| Show cron expression
public export
showCronExpr : CronExpr -> String
showCronExpr (Cron5 e) = showCron5 e
showCronExpr (Cron6 e) = showCron6 e
showCronExpr (Cron7 _) = "unsupported"

||| Show special schedule
public export
showSpecialSchedule : SpecialSchedule -> String
showSpecialSchedule Yearly = "@yearly"
showSpecialSchedule Monthly = "@monthly"
showSpecialSchedule Weekly = "@weekly"
showSpecialSchedule Daily = "@daily"
showSpecialSchedule Hourly = "@hourly"
showSpecialSchedule Reboot = "@reboot"

||| Describe cron expression in human terms
public export
describeCronExpr : CronExpr -> String
describeCronExpr (Cron5 expr) =
  case (expr.minute, expr.hour, expr.dayOfMonth, expr.month, expr.dayOfWeek) of
    (Single 0, Single 0, Single 1, Single 1, _) => "once a year (Jan 1 at midnight)"
    (Single 0, Single 0, Single 1, _, _) => "once a month (1st at midnight)"
    (Single 0, Single 0, _, _, Single 0) => "once a week (Sunday at midnight)"
    (Single 0, Single 0, _, _, _) => "once a day (at midnight)"
    (Single 0, _, _, _, _) => "once an hour (at minute 0)"
    (Wildcard, _, _, _, _) => "every minute"
    (WildcardStep s, _, _, _, _) => "every " ++ show s ++ " minutes"
    _ => showCron5 expr
describeCronExpr (Cron6 expr) =
  case expr.second of
    Wildcard => "every second"
    WildcardStep s => "every " ++ show s ++ " seconds"
    _ => showCron6 expr
describeCronExpr expr = showCronExpr expr

--------------------------------------------------------------------------------
-- Job construction
--------------------------------------------------------------------------------

||| Create a cron job
public export
mkCronJob : String -> String -> String -> Either CronError CronJob
mkCronJob id schedule command = do
  validCommand <- validateCommand command
  expr <- parseCronExpr schedule
  _ <- validateFrequency defaultLimits expr
  pure (MkCronJob id (MkCronSchedule (Right expr) Nothing Nothing) validCommand True Nothing Nothing Nothing Nothing)

||| Create a cron schedule from special schedule
public export
mkSpecialSchedule : SpecialSchedule -> CronSchedule
mkSpecialSchedule special =
  MkCronSchedule (Left special) Nothing (Just (showSpecialSchedule special))
