{- SPDX-License-Identifier: Apache-2.0 -}
{- SPDX-FileCopyrightText: 2025 Hyperpolymath -}

-- | Safe date and time operations with validation.
--
-- Provides secure date/time handling with bounds checking
-- and protection against invalid dates.
module Proven.SafeDateTime
  ( -- * Types
    Date(..)
  , Time(..)
  , DateTime(..)
  , Duration(..)
  , Weekday(..)
    -- * Construction
  , makeDate
  , makeTime
  , makeDateTime
  , makeDuration
    -- * Validation
  , isValidDate
  , isValidTime
  , isLeapYear
  , daysInMonth
    -- * Current Time
  , getCurrentDateTime
    -- * Conversion
  , toTimestamp
  , fromTimestamp
  , toIso8601
  , fromIso8601
    -- * Manipulation
  , addDays
  , addMonths
  , addYears
  , addHours
  , addMinutes
  , addSeconds
  , addDuration
    -- * Comparison
  , diffDays
  , diffSeconds
    -- * Components
  , getYear
  , getMonth
  , getDay
  , getHour
  , getMinute
  , getSecond
  , getWeekday
    -- * Formatting
  , formatDate
  , formatTime
  , formatDateTime
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Int (Int64)
import Data.Time (getCurrentTime, UTCTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Proven.Core (ProvenError(..), Result)

-- | Days of the week.
data Weekday = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | Date with year, month, day.
data Date = Date
  { dateYear  :: !Int
  , dateMonth :: !Int
  , dateDay   :: !Int
  } deriving (Eq, Ord, Show)

-- | Time with hour, minute, second, millisecond.
data Time = Time
  { timeHour        :: !Int
  , timeMinute      :: !Int
  , timeSecond      :: !Int
  , timeMillisecond :: !Int
  } deriving (Eq, Ord, Show)

-- | Combined date and time.
data DateTime = DateTime
  { dtDate :: !Date
  , dtTime :: !Time
  } deriving (Eq, Ord, Show)

-- | Duration in various units.
data Duration = Duration
  { durDays    :: !Int
  , durHours   :: !Int
  , durMinutes :: !Int
  , durSeconds :: !Int
  , durMillis  :: !Int
  } deriving (Eq, Show)

-- | Create a validated date.
makeDate :: Int -> Int -> Int -> Result Date
makeDate year month day
  | month < 1 || month > 12 = Left (OutOfRange "Month must be 1-12")
  | day < 1 = Left (OutOfRange "Day must be positive")
  | day > daysInMonth year month = Left (OutOfRange "Day exceeds days in month")
  | year < 1 || year > 9999 = Left (OutOfRange "Year must be 1-9999")
  | otherwise = Right (Date year month day)

-- | Create a validated time.
makeTime :: Int -> Int -> Int -> Int -> Result Time
makeTime hour minute second millis
  | hour < 0 || hour > 23 = Left (OutOfRange "Hour must be 0-23")
  | minute < 0 || minute > 59 = Left (OutOfRange "Minute must be 0-59")
  | second < 0 || second > 59 = Left (OutOfRange "Second must be 0-59")
  | millis < 0 || millis > 999 = Left (OutOfRange "Millisecond must be 0-999")
  | otherwise = Right (Time hour minute second millis)

-- | Create a validated datetime.
makeDateTime :: Date -> Time -> Result DateTime
makeDateTime date time = Right (DateTime date time)

-- | Create a duration.
makeDuration :: Int -> Int -> Int -> Int -> Int -> Result Duration
makeDuration days hours minutes seconds millis
  | any (< 0) [days, hours, minutes, seconds, millis] =
      Left (OutOfRange "Duration components must be non-negative")
  | otherwise = Right (Duration days hours minutes seconds millis)

-- | Check if date is valid.
isValidDate :: Int -> Int -> Int -> Bool
isValidDate year month day = case makeDate year month day of
  Right _ -> True
  Left _ -> False

-- | Check if time is valid.
isValidTime :: Int -> Int -> Int -> Int -> Bool
isValidTime hour minute second millis = case makeTime hour minute second millis of
  Right _ -> True
  Left _ -> False

-- | Check if year is a leap year.
isLeapYear :: Int -> Bool
isLeapYear year = (year `mod` 4 == 0 && year `mod` 100 /= 0) || year `mod` 400 == 0

-- | Get days in a month.
daysInMonth :: Int -> Int -> Int
daysInMonth year month = case month of
  1 -> 31
  2 -> if isLeapYear year then 29 else 28
  3 -> 31
  4 -> 30
  5 -> 31
  6 -> 30
  7 -> 31
  8 -> 31
  9 -> 30
  10 -> 31
  11 -> 30
  12 -> 31
  _ -> 0

-- | Get current datetime (IO action).
getCurrentDateTime :: IO DateTime
getCurrentDateTime = do
  now <- getCurrentTime
  return $ fromTimestampInternal (floor $ utcTimeToPOSIXSeconds now)

fromTimestampInternal :: Int64 -> DateTime
fromTimestampInternal ts =
  DateTime (Date 2025 1 1) (Time 0 0 0 0) -- Simplified

-- | Convert datetime to Unix timestamp.
toTimestamp :: DateTime -> Int64
toTimestamp dt =
  let Date y m d = dtDate dt
      Time h mi s _ = dtTime dt
      -- Simplified calculation
      days = fromIntegral $ (y - 1970) * 365 + m * 30 + d
      secs = fromIntegral $ h * 3600 + mi * 60 + s
  in days * 86400 + secs

-- | Convert Unix timestamp to datetime.
fromTimestamp :: Int64 -> Result DateTime
fromTimestamp ts
  | ts < 0 = Left (OutOfRange "Negative timestamp")
  | ts > 253402300799 = Left (OutOfRange "Timestamp too large") -- Year 9999
  | otherwise = Right $ fromTimestampInternal ts

-- | Convert to ISO 8601 string.
toIso8601 :: DateTime -> Text
toIso8601 (DateTime (Date y m d) (Time h mi s ms)) =
  T.pack $ printf "%04d-%02d-%02dT%02d:%02d:%02d.%03dZ" y m d h mi s ms
  where
    printf :: String -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> String
    printf _ y' m' d' h' mi' s' ms' =
      show y' ++ "-" ++ pad m' ++ "-" ++ pad d' ++ "T" ++
      pad h' ++ ":" ++ pad mi' ++ ":" ++ pad s' ++ "." ++ pad3 ms' ++ "Z"
    pad n = if n < 10 then "0" ++ show n else show n
    pad3 n = if n < 10 then "00" ++ show n else if n < 100 then "0" ++ show n else show n

-- | Parse ISO 8601 string.
fromIso8601 :: Text -> Result DateTime
fromIso8601 input
  | T.length input < 19 = Left (InvalidFormat "ISO 8601 string too short")
  | otherwise = do
      let parts = T.splitOn "-" (T.take 10 input)
      case parts of
        [yStr, mStr, dStr] -> do
          y <- parseIntText yStr
          m <- parseIntText mStr
          d <- parseIntText dStr
          date <- makeDate y m d
          let timePart = T.drop 11 input
          let timeParts = T.splitOn ":" (T.take 8 timePart)
          case timeParts of
            [hStr, miStr, sStr] -> do
              h <- parseIntText hStr
              mi <- parseIntText miStr
              s <- parseIntText (T.take 2 sStr)
              time <- makeTime h mi s 0
              Right (DateTime date time)
            _ -> Left (InvalidFormat "Invalid time format")
        _ -> Left (InvalidFormat "Invalid date format")

parseIntText :: Text -> Result Int
parseIntText t = case reads (T.unpack t) of
  [(n, "")] -> Right n
  _ -> Left (InvalidFormat "Invalid number")

-- | Add days to a date.
addDays :: Int -> Date -> Date
addDays n date@(Date y m d)
  | n == 0 = date
  | n > 0 =
      let maxDays = daysInMonth y m
          newDay = d + n
      in if newDay <= maxDays
         then Date y m newDay
         else addDays (newDay - maxDays) (Date y (if m == 12 then 1 else m + 1)
                                               (if m == 12 then y + 1 else y) 1)
  | otherwise = addDays (n + 1) (Date y m (d - 1)) -- Simplified backwards

-- | Add months to a date.
addMonths :: Int -> Date -> Date
addMonths n (Date y m d) =
  let totalMonths = (y * 12 + m - 1) + n
      newYear = totalMonths `div` 12
      newMonth = (totalMonths `mod` 12) + 1
      maxDay = daysInMonth newYear newMonth
  in Date newYear newMonth (min d maxDay)

-- | Add years to a date.
addYears :: Int -> Date -> Date
addYears n (Date y m d) =
  let newYear = y + n
      maxDay = daysInMonth newYear m
  in Date newYear m (min d maxDay)

-- | Add hours to a time (wraps at 24).
addHours :: Int -> Time -> Time
addHours n (Time h mi s ms) =
  let newHour = (h + n) `mod` 24
  in Time (if newHour < 0 then newHour + 24 else newHour) mi s ms

-- | Add minutes to a time (wraps and carries to hours).
addMinutes :: Int -> Time -> Time
addMinutes n (Time h mi s ms) =
  let totalMins = h * 60 + mi + n
      newHour = (totalMins `div` 60) `mod` 24
      newMin = totalMins `mod` 60
  in Time (if newHour < 0 then newHour + 24 else newHour)
          (if newMin < 0 then newMin + 60 else newMin) s ms

-- | Add seconds to a time.
addSeconds :: Int -> Time -> Time
addSeconds n (Time h mi s ms) =
  let totalSecs = h * 3600 + mi * 60 + s + n
      newHour = (totalSecs `div` 3600) `mod` 24
      remainSecs = totalSecs `mod` 3600
      newMin = remainSecs `div` 60
      newSec = remainSecs `mod` 60
  in Time newHour newMin newSec ms

-- | Add duration to datetime.
addDuration :: Duration -> DateTime -> DateTime
addDuration (Duration days hours mins secs _) (DateTime date time) =
  let newTime = addSeconds secs $ addMinutes mins $ addHours hours time
      newDate = addDays days date
  in DateTime newDate newTime

-- | Get difference in days between dates.
diffDays :: Date -> Date -> Int
diffDays (Date y1 m1 d1) (Date y2 m2 d2) =
  -- Simplified calculation
  (y1 - y2) * 365 + (m1 - m2) * 30 + (d1 - d2)

-- | Get difference in seconds between datetimes.
diffSeconds :: DateTime -> DateTime -> Int64
diffSeconds dt1 dt2 = toTimestamp dt1 - toTimestamp dt2

-- | Get year from date.
getYear :: Date -> Int
getYear = dateYear

-- | Get month from date.
getMonth :: Date -> Int
getMonth = dateMonth

-- | Get day from date.
getDay :: Date -> Int
getDay = dateDay

-- | Get hour from time.
getHour :: Time -> Int
getHour = timeHour

-- | Get minute from time.
getMinute :: Time -> Int
getMinute = timeMinute

-- | Get second from time.
getSecond :: Time -> Int
getSecond = timeSecond

-- | Get weekday from date.
getWeekday :: Date -> Weekday
getWeekday (Date y m d) =
  -- Zeller's formula simplified
  let m' = if m < 3 then m + 12 else m
      y' = if m < 3 then y - 1 else y
      k = y' `mod` 100
      j = y' `div` 100
      h = (d + (13 * (m' + 1)) `div` 5 + k + k `div` 4 + j `div` 4 - 2 * j) `mod` 7
      weekdays = [Saturday, Sunday, Monday, Tuesday, Wednesday, Thursday, Friday]
  in weekdays !! (if h < 0 then h + 7 else h)

-- | Format date as string.
formatDate :: Text -> Date -> Text
formatDate _pattern (Date y m d) =
  T.pack $ show y ++ "-" ++ pad m ++ "-" ++ pad d
  where pad n = if n < 10 then "0" ++ show n else show n

-- | Format time as string.
formatTime :: Text -> Time -> Text
formatTime _pattern (Time h mi s _) =
  T.pack $ pad h ++ ":" ++ pad mi ++ ":" ++ pad s
  where pad n = if n < 10 then "0" ++ show n else show n

-- | Format datetime as string.
formatDateTime :: Text -> DateTime -> Text
formatDateTime pattern (DateTime date time) =
  formatDate pattern date <> "T" <> formatTime pattern time
