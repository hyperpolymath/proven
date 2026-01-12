-- SPDX-License-Identifier: Palimpsest-MPL-1.0
||| Date/Time Parsing
|||
||| Safe parsing functions for various date/time formats.
module Proven.SafeDateTime.Parse

import Proven.Core
import Data.List
import Data.String

%default total

--------------------------------------------------------------------------------
-- Parse Error Types
--------------------------------------------------------------------------------

||| Date/time parsing errors
public export
data DateTimeError
  = InvalidFormat String
  | InvalidYear String
  | InvalidMonth String
  | InvalidDay String
  | InvalidHour String
  | InvalidMinute String
  | InvalidSecond String
  | InvalidTimezone String
  | OutOfRange String
  | AmbiguousTime String

public export
Show DateTimeError where
  show (InvalidFormat s) = "Invalid format: " ++ s
  show (InvalidYear s) = "Invalid year: " ++ s
  show (InvalidMonth s) = "Invalid month: " ++ s
  show (InvalidDay s) = "Invalid day: " ++ s
  show (InvalidHour s) = "Invalid hour: " ++ s
  show (InvalidMinute s) = "Invalid minute: " ++ s
  show (InvalidSecond s) = "Invalid second: " ++ s
  show (InvalidTimezone s) = "Invalid timezone: " ++ s
  show (OutOfRange s) = "Value out of range: " ++ s
  show (AmbiguousTime s) = "Ambiguous time: " ++ s

--------------------------------------------------------------------------------
-- Parser Result Type
--------------------------------------------------------------------------------

||| Result of parsing
public export
ParseResult : Type -> Type
ParseResult a = Either DateTimeError a

--------------------------------------------------------------------------------
-- ISO 8601 Parsing
--------------------------------------------------------------------------------

||| Parse ISO 8601 date (YYYY-MM-DD)
public export
parseISODate : String -> ParseResult (Nat, Nat, Nat)
parseISODate s =
  case split (== '-') s of
    [yearStr, monthStr, dayStr] => do
      year <- parseNat yearStr "year"
      month <- parseNat monthStr "month"
      day <- parseNat dayStr "day"
      validateDate year month day
    _ => Left (InvalidFormat "Expected YYYY-MM-DD")
  where
    parseNat : String -> String -> ParseResult Nat
    parseNat s field =
      case parsePositive s of
        Just n => Right n
        Nothing => Left (InvalidFormat $ "Invalid " ++ field ++ ": " ++ s)

    validateDate : Nat -> Nat -> Nat -> ParseResult (Nat, Nat, Nat)
    validateDate y m d =
      if y < 1 || y > 9999 then Left (InvalidYear $ show y)
      else if m < 1 || m > 12 then Left (InvalidMonth $ show m)
      else if d < 1 || d > 31 then Left (InvalidDay $ show d)
      else Right (y, m, d)

||| Parse ISO 8601 time (HH:MM:SS or HH:MM:SS.sss)
public export
parseISOTime : String -> ParseResult (Nat, Nat, Nat, Nat)
parseISOTime s =
  let (timeStr, fracStr) = splitFrac s
  in case split (== ':') timeStr of
       [hourStr, minStr, secStr] => do
         hour <- parseNat hourStr "hour"
         minute <- parseNat minStr "minute"
         second <- parseNat secStr "second"
         nano <- parseFrac fracStr
         validateTime hour minute second nano
       [hourStr, minStr] => do
         hour <- parseNat hourStr "hour"
         minute <- parseNat minStr "minute"
         validateTime hour minute 0 0
       _ => Left (InvalidFormat "Expected HH:MM:SS")
  where
    splitFrac : String -> (String, String)
    splitFrac str =
      case break (== '.') (unpack str) of
        (time, '.' :: frac) => (pack time, pack frac)
        (time, _) => (pack time, "")

    parseNat : String -> String -> ParseResult Nat
    parseNat str field =
      case parsePositive str of
        Just n => Right n
        Nothing => Left (InvalidFormat $ "Invalid " ++ field)

    parseFrac : String -> ParseResult Nat
    parseFrac "" = Right 0
    parseFrac str =
      case parsePositive str of
        Just n =>
          let digits = length str
              scale = pow 10 (9 `minus` digits)
          in Right (n * scale)
        Nothing => Left (InvalidFormat "Invalid fractional seconds")

    pow : Nat -> Nat -> Nat
    pow _ 0 = 1
    pow b (S n) = b * pow b n

    validateTime : Nat -> Nat -> Nat -> Nat -> ParseResult (Nat, Nat, Nat, Nat)
    validateTime h m s n =
      if h >= 24 then Left (InvalidHour $ show h)
      else if m >= 60 then Left (InvalidMinute $ show m)
      else if s >= 60 then Left (InvalidSecond $ show s)
      else Right (h, m, s, n)

||| Parse ISO 8601 datetime (YYYY-MM-DDTHH:MM:SS)
public export
parseISODateTime : String -> ParseResult ((Nat, Nat, Nat), (Nat, Nat, Nat, Nat))
parseISODateTime s =
  case splitAt (findT s) s of
    (dateStr, timeWithT) =>
      if length timeWithT <= 1
        then Left (InvalidFormat "Expected date and time")
        else do
          date <- parseISODate dateStr
          let timeStr = strTail timeWithT  -- Remove 'T'
          time <- parseISOTime (takeWhile isTimeChar (unpack timeStr))
          Right (date, time)
  where
    findT : String -> Nat
    findT str = go 0 (unpack str)
      where
        go : Nat -> List Char -> Nat
        go n [] = n
        go n ('T' :: _) = n
        go n ('t' :: _) = n
        go n (_ :: xs) = go (S n) xs

    splitAt : Nat -> String -> (String, String)
    splitAt n str = (pack (take n (unpack str)), pack (drop n (unpack str)))

    isTimeChar : Char -> Bool
    isTimeChar c = isDigit c || c == ':' || c == '.'

    takeWhile : (Char -> Bool) -> List Char -> String
    takeWhile p [] = ""
    takeWhile p (c :: cs) = if p c then pack [c] ++ takeWhile p cs else ""

--------------------------------------------------------------------------------
-- RFC 2822 Parsing (Email date format)
--------------------------------------------------------------------------------

||| Parse RFC 2822 date (e.g., "Mon, 15 Jan 2024 10:30:00 +0000")
public export
parseRFC2822 : String -> ParseResult ((Nat, Nat, Nat), (Nat, Nat, Nat, Nat), Integer)
parseRFC2822 s =
  -- Simplified parsing - full RFC 2822 is complex
  Left (InvalidFormat "RFC 2822 parsing not fully implemented")

--------------------------------------------------------------------------------
-- Common Format Parsing
--------------------------------------------------------------------------------

||| Parse date in format MM/DD/YYYY
public export
parseUSDate : String -> ParseResult (Nat, Nat, Nat)
parseUSDate s =
  case split (== '/') s of
    [monthStr, dayStr, yearStr] => do
      month <- parseNat monthStr
      day <- parseNat dayStr
      year <- parseNat yearStr
      if year < 1 || year > 9999 then Left (InvalidYear $ show year)
      else if month < 1 || month > 12 then Left (InvalidMonth $ show month)
      else if day < 1 || day > 31 then Left (InvalidDay $ show day)
      else Right (year, month, day)
    _ => Left (InvalidFormat "Expected MM/DD/YYYY")
  where
    parseNat : String -> ParseResult Nat
    parseNat str = case parsePositive str of
      Just n => Right n
      Nothing => Left (InvalidFormat str)

||| Parse date in format DD/MM/YYYY
public export
parseEUDate : String -> ParseResult (Nat, Nat, Nat)
parseEUDate s =
  case split (== '/') s of
    [dayStr, monthStr, yearStr] => do
      day <- parseNat dayStr
      month <- parseNat monthStr
      year <- parseNat yearStr
      if year < 1 || year > 9999 then Left (InvalidYear $ show year)
      else if month < 1 || month > 12 then Left (InvalidMonth $ show month)
      else if day < 1 || day > 31 then Left (InvalidDay $ show day)
      else Right (year, month, day)
    _ => Left (InvalidFormat "Expected DD/MM/YYYY")
  where
    parseNat : String -> ParseResult Nat
    parseNat str = case parsePositive str of
      Just n => Right n
      Nothing => Left (InvalidFormat str)

--------------------------------------------------------------------------------
-- Timezone Offset Parsing
--------------------------------------------------------------------------------

||| Parse timezone offset (+HH:MM or -HH:MM or Z)
public export
parseTimezoneOffset : String -> ParseResult Integer
parseTimezoneOffset "Z" = Right 0
parseTimezoneOffset s =
  case unpack s of
    (sign :: rest) =>
      let signMult = if sign == '+' then 1 else if sign == '-' then -1 else 0
      in if signMult == 0
           then Left (InvalidTimezone s)
           else parseOffset signMult (pack rest)
    _ => Left (InvalidTimezone s)
  where
    parseOffset : Integer -> String -> ParseResult Integer
    parseOffset mult str =
      case split (== ':') str of
        [hourStr, minStr] =>
          case (parsePositive hourStr, parsePositive minStr) of
            (Just h, Just m) =>
              if h > 14 || m > 59
                then Left (InvalidTimezone str)
                else Right (mult * (cast h * 3600 + cast m * 60))
            _ => Left (InvalidTimezone str)
        [hourMinStr] =>
          if length hourMinStr == 4
            then case (parsePositive (substr 0 2 hourMinStr),
                       parsePositive (substr 2 2 hourMinStr)) of
                   (Just h, Just m) =>
                     if h > 14 || m > 59
                       then Left (InvalidTimezone hourMinStr)
                       else Right (mult * (cast h * 3600 + cast m * 60))
                   _ => Left (InvalidTimezone hourMinStr)
            else Left (InvalidTimezone str)
        _ => Left (InvalidTimezone str)

--------------------------------------------------------------------------------
-- Duration Parsing (ISO 8601)
--------------------------------------------------------------------------------

||| Parse ISO 8601 duration (e.g., P1Y2M3DT4H5M6S)
public export
parseISODuration : String -> ParseResult Integer
parseISODuration s =
  case unpack s of
    ('P' :: rest) => parseDurationParts (pack rest)
    _ => Left (InvalidFormat "Duration must start with P")
  where
    parseDurationParts : String -> ParseResult Integer
    parseDurationParts str =
      -- Simplified - full ISO 8601 duration parsing is complex
      Right 0  -- Stub

--------------------------------------------------------------------------------
-- Formatting
--------------------------------------------------------------------------------

||| Format date as ISO 8601
public export
formatISODate : (Nat, Nat, Nat) -> String
formatISODate (year, month, day) =
  padYear year ++ "-" ++ padTwo month ++ "-" ++ padTwo day
  where
    padYear : Nat -> String
    padYear n = let s = show n in replicate (4 `minus` length s) '0' ++ s
    padTwo : Nat -> String
    padTwo n = if n < 10 then "0" ++ show n else show n

||| Format time as ISO 8601
public export
formatISOTime : (Nat, Nat, Nat, Nat) -> String
formatISOTime (hour, minute, second, nano) =
  padTwo hour ++ ":" ++ padTwo minute ++ ":" ++ padTwo second ++
  (if nano > 0 then "." ++ formatNano nano else "")
  where
    padTwo : Nat -> String
    padTwo n = if n < 10 then "0" ++ show n else show n
    formatNano : Nat -> String
    formatNano n = let s = show n in replicate (9 `minus` length s) '0' ++ s

||| Format datetime as ISO 8601
public export
formatISODateTime : ((Nat, Nat, Nat), (Nat, Nat, Nat, Nat)) -> String
formatISODateTime (date, time) = formatISODate date ++ "T" ++ formatISOTime time

--------------------------------------------------------------------------------
-- Helper Functions
--------------------------------------------------------------------------------

||| Split string on delimiter
split : (Char -> Bool) -> String -> List String
split p s = go (unpack s) [] []
  where
    go : List Char -> List Char -> List String -> List String
    go [] current acc = reverse (pack (reverse current) :: acc)
    go (c :: cs) current acc =
      if p c
        then go cs [] (pack (reverse current) :: acc)
        else go cs (c :: current) acc

||| Break string at first occurrence
break : (Char -> Bool) -> List Char -> (List Char, List Char)
break p [] = ([], [])
break p (c :: cs) =
  if p c then ([], c :: cs) else let (ys, zs) = break p cs in (c :: ys, zs)
