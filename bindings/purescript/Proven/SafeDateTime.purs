-- SPDX-License-Identifier: PMPL-1.0
-- SPDX-FileCopyrightText: 2025 Hyperpolymath
--
-- | Safe date and time operations.
-- |
-- | Provides validated date/time handling with proper timezone support
-- | and ISO 8601 formatting.

module Proven.SafeDateTime
  ( SafeDateTime
  , DateTime(..)
  , Duration(..)
  , now
  , parse
  , parseIso8601
  , toIso8601
  , toUnixMs
  , fromUnixMs
  , addDuration
  , subtractDuration
  , diffMs
  , isValidDate
  , getYear
  , getMonth
  , getDay
  , getHour
  , getMinute
  , getSecond
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Proven.Result (Result(..), ProvenError(..))

-- | SafeDateTime namespace marker (not instantiated).
data SafeDateTime

-- | DateTime value (stored as milliseconds since Unix epoch).
newtype DateTime = DateTime Number

derive instance eqDateTime :: Eq DateTime
derive instance ordDateTime :: Ord DateTime

instance showDateTime :: Show DateTime where
  show dt = toIso8601 dt

-- | Duration in milliseconds.
newtype Duration = Duration Number

derive instance eqDuration :: Eq Duration
derive instance ordDuration :: Ord Duration

instance showDuration :: Show Duration where
  show (Duration ms) = show ms <> "ms"

instance semigroupDuration :: Semigroup Duration where
  append (Duration a) (Duration b) = Duration (a + b)

instance monoidDuration :: Monoid Duration where
  mempty = Duration 0.0

-- | Get the current date/time.
now :: DateTime
now = DateTime nowImpl

foreign import nowImpl :: Number

-- | Parse a date string (various formats).
parse :: String -> Result DateTime ProvenError
parse s =
  let ms = parseDateImpl s
  in if isNaN ms
     then Err (InvalidDateTime "Failed to parse date")
     else Ok (DateTime ms)

foreign import parseDateImpl :: String -> Number
foreign import isNaN :: Number -> Boolean

-- | Parse an ISO 8601 date/time string.
parseIso8601 :: String -> Result DateTime ProvenError
parseIso8601 s =
  let result = parseIso8601Impl s
  in if result.valid
     then Ok (DateTime result.ms)
     else Err (InvalidDateTime result.error)

foreign import parseIso8601Impl :: String ->
  { valid :: Boolean
  , ms :: Number
  , error :: String
  }

-- | Format as ISO 8601 string.
toIso8601 :: DateTime -> String
toIso8601 (DateTime ms) = toIso8601Impl ms

foreign import toIso8601Impl :: Number -> String

-- | Convert to Unix timestamp (milliseconds).
toUnixMs :: DateTime -> Number
toUnixMs (DateTime ms) = ms

-- | Create from Unix timestamp (milliseconds).
fromUnixMs :: Number -> DateTime
fromUnixMs ms = DateTime ms

-- | Add a duration to a date/time.
addDuration :: DateTime -> Duration -> DateTime
addDuration (DateTime dt) (Duration dur) = DateTime (dt + dur)

-- | Subtract a duration from a date/time.
subtractDuration :: DateTime -> Duration -> DateTime
subtractDuration (DateTime dt) (Duration dur) = DateTime (dt - dur)

-- | Get the difference between two date/times in milliseconds.
diffMs :: DateTime -> DateTime -> Duration
diffMs (DateTime a) (DateTime b) = Duration (a - b)

-- | Check if values represent a valid date.
isValidDate :: Int -> Int -> Int -> Boolean
isValidDate year month day =
  month >= 1 && month <= 12 &&
  day >= 1 && day <= daysInMonth year month

daysInMonth :: Int -> Int -> Int
daysInMonth year month =
  case month of
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

isLeapYear :: Int -> Boolean
isLeapYear year =
  (year `mod` 4 == 0 && year `mod` 100 /= 0) || year `mod` 400 == 0

-- | Get the year component.
getYear :: DateTime -> Int
getYear (DateTime ms) = getYearImpl ms

foreign import getYearImpl :: Number -> Int

-- | Get the month component (1-12).
getMonth :: DateTime -> Int
getMonth (DateTime ms) = getMonthImpl ms

foreign import getMonthImpl :: Number -> Int

-- | Get the day component (1-31).
getDay :: DateTime -> Int
getDay (DateTime ms) = getDayImpl ms

foreign import getDayImpl :: Number -> Int

-- | Get the hour component (0-23).
getHour :: DateTime -> Int
getHour (DateTime ms) = getHourImpl ms

foreign import getHourImpl :: Number -> Int

-- | Get the minute component (0-59).
getMinute :: DateTime -> Int
getMinute (DateTime ms) = getMinuteImpl ms

foreign import getMinuteImpl :: Number -> Int

-- | Get the second component (0-59).
getSecond :: DateTime -> Int
getSecond (DateTime ms) = getSecondImpl ms

foreign import getSecondImpl :: Number -> Int
