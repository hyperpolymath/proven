-- SPDX-License-Identifier: Palimpsest-MPL
||| Timezone Handling
|||
||| Safe timezone operations and common timezone definitions.
module Bulletproof.SafeDateTime.Zones

import Bulletproof.Core
import Data.List

%default total

--------------------------------------------------------------------------------
-- Timezone Types
--------------------------------------------------------------------------------

||| A timezone identifier
public export
data Timezone
  = UTC                    -- Coordinated Universal Time
  | FixedOffset Integer    -- Fixed offset from UTC in seconds
  | Named String           -- IANA timezone name (e.g., "America/New_York")

public export
Show Timezone where
  show UTC = "UTC"
  show (FixedOffset secs) = formatOffset secs
    where
      formatOffset : Integer -> String
      formatOffset 0 = "UTC"
      formatOffset s =
        let absS = abs s
            h = absS `div` 3600
            m = (absS `mod` 3600) `div` 60
            sign = if s >= 0 then "+" else "-"
        in "UTC" ++ sign ++ show h ++ (if m > 0 then ":" ++ show m else "")
  show (Named name) = name

public export
Eq Timezone where
  UTC == UTC = True
  (FixedOffset a) == (FixedOffset b) = a == b
  (Named a) == (Named b) = a == b
  _ == _ = False

--------------------------------------------------------------------------------
-- Common Timezones
--------------------------------------------------------------------------------

||| UTC timezone
public export
utc : Timezone
utc = UTC

||| GMT (same as UTC)
public export
gmt : Timezone
gmt = UTC

||| US Eastern Time
public export
usEastern : Timezone
usEastern = Named "America/New_York"

||| US Central Time
public export
usCentral : Timezone
usCentral = Named "America/Chicago"

||| US Mountain Time
public export
usMountain : Timezone
usMountain = Named "America/Denver"

||| US Pacific Time
public export
usPacific : Timezone
usPacific = Named "America/Los_Angeles"

||| London (UK)
public export
london : Timezone
london = Named "Europe/London"

||| Paris (Central Europe)
public export
paris : Timezone
paris = Named "Europe/Paris"

||| Berlin (Germany)
public export
berlin : Timezone
berlin = Named "Europe/Berlin"

||| Tokyo (Japan)
public export
tokyo : Timezone
tokyo = Named "Asia/Tokyo"

||| Sydney (Australia)
public export
sydney : Timezone
sydney = Named "Australia/Sydney"

||| Hong Kong
public export
hongKong : Timezone
hongKong = Named "Asia/Hong_Kong"

||| Singapore
public export
singapore : Timezone
singapore = Named "Asia/Singapore"

||| India Standard Time
public export
kolkata : Timezone
kolkata = Named "Asia/Kolkata"

||| Dubai (UAE)
public export
dubai : Timezone
dubai = Named "Asia/Dubai"

--------------------------------------------------------------------------------
-- Fixed Offset Timezones
--------------------------------------------------------------------------------

||| Create fixed offset timezone from hours
public export
offsetHours : Integer -> Timezone
offsetHours h = FixedOffset (h * 3600)

||| Create fixed offset from hours and minutes
public export
offsetHoursMinutes : Integer -> Integer -> Timezone
offsetHoursMinutes h m =
  let totalSeconds = h * 3600 + (if h >= 0 then m else negate m) * 60
  in FixedOffset totalSeconds

||| Common fixed offsets
public export
utcPlus1 : Timezone
utcPlus1 = offsetHours 1

public export
utcPlus2 : Timezone
utcPlus2 = offsetHours 2

public export
utcPlus3 : Timezone
utcPlus3 = offsetHours 3

public export
utcPlus530 : Timezone  -- India
utcPlus530 = offsetHoursMinutes 5 30

public export
utcPlus8 : Timezone  -- China, Singapore
utcPlus8 = offsetHours 8

public export
utcPlus9 : Timezone  -- Japan, Korea
utcPlus9 = offsetHours 9

public export
utcMinus5 : Timezone  -- US Eastern (standard)
utcMinus5 = offsetHours (-5)

public export
utcMinus8 : Timezone  -- US Pacific (standard)
utcMinus8 = offsetHours (-8)

--------------------------------------------------------------------------------
-- Timezone Operations
--------------------------------------------------------------------------------

||| Get fixed offset for a timezone at a given instant
||| For named timezones, this would require a timezone database (FFI)
public export
getOffset : Timezone -> Integer -> Integer
getOffset UTC _ = 0
getOffset (FixedOffset secs) _ = secs
getOffset (Named _) _ = 0  -- Stub - requires timezone database

||| Check if timezone observes DST
public export
observesDST : Timezone -> Bool
observesDST UTC = False
observesDST (FixedOffset _) = False
observesDST (Named name) =
  -- Named timezones may observe DST
  name `elem` dstTimezones
  where
    dstTimezones : List String
    dstTimezones = [ "America/New_York", "America/Chicago", "America/Denver"
                   , "America/Los_Angeles", "Europe/London", "Europe/Paris"
                   , "Europe/Berlin", "Australia/Sydney" ]

||| Get DST offset if applicable (typically 1 hour = 3600 seconds)
public export
dstOffset : Timezone -> Integer
dstOffset tz = if observesDST tz then 3600 else 0

--------------------------------------------------------------------------------
-- Timezone Validation
--------------------------------------------------------------------------------

||| Known IANA timezone names (subset)
public export
knownTimezones : List String
knownTimezones =
  [ "UTC", "GMT"
  -- Americas
  , "America/New_York", "America/Chicago", "America/Denver", "America/Los_Angeles"
  , "America/Toronto", "America/Vancouver", "America/Phoenix", "America/Anchorage"
  , "America/Sao_Paulo", "America/Buenos_Aires", "America/Mexico_City"
  -- Europe
  , "Europe/London", "Europe/Paris", "Europe/Berlin", "Europe/Rome"
  , "Europe/Madrid", "Europe/Amsterdam", "Europe/Brussels", "Europe/Vienna"
  , "Europe/Stockholm", "Europe/Oslo", "Europe/Helsinki", "Europe/Warsaw"
  , "Europe/Prague", "Europe/Zurich", "Europe/Dublin", "Europe/Lisbon"
  , "Europe/Athens", "Europe/Moscow", "Europe/Istanbul"
  -- Asia
  , "Asia/Tokyo", "Asia/Shanghai", "Asia/Hong_Kong", "Asia/Singapore"
  , "Asia/Seoul", "Asia/Kolkata", "Asia/Mumbai", "Asia/Dubai"
  , "Asia/Bangkok", "Asia/Jakarta", "Asia/Manila", "Asia/Taipei"
  , "Asia/Karachi", "Asia/Dhaka", "Asia/Kuala_Lumpur", "Asia/Riyadh"
  , "Asia/Jerusalem", "Asia/Tehran"
  -- Australia/Pacific
  , "Australia/Sydney", "Australia/Melbourne", "Australia/Brisbane"
  , "Australia/Perth", "Australia/Adelaide", "Pacific/Auckland"
  , "Pacific/Honolulu", "Pacific/Fiji"
  -- Africa
  , "Africa/Cairo", "Africa/Johannesburg", "Africa/Lagos", "Africa/Nairobi"
  ]

||| Validate timezone name
public export
isValidTimezone : String -> Bool
isValidTimezone name = name `elem` knownTimezones

||| Parse timezone from string
public export
parseTimezone : String -> Maybe Timezone
parseTimezone "UTC" = Just UTC
parseTimezone "GMT" = Just UTC
parseTimezone "Z" = Just UTC
parseTimezone s =
  if isValidTimezone s
    then Just (Named s)
    else parseOffset s
  where
    parseOffset : String -> Maybe Timezone
    parseOffset str =
      case unpack str of
        ('+' :: rest) => parseOffsetValue 1 (pack rest)
        ('-' :: rest) => parseOffsetValue (-1) (pack rest)
        _ => Nothing

    parseOffsetValue : Integer -> String -> Maybe Timezone
    parseOffsetValue sign str =
      case split (== ':') str of
        [hourStr, minStr] =>
          case (parsePositive hourStr, parsePositive minStr) of
            (Just h, Just m) =>
              if h <= 14 && m < 60
                then Just (FixedOffset (sign * (cast h * 3600 + cast m * 60)))
                else Nothing
            _ => Nothing
        [hourStr] =>
          case parsePositive hourStr of
            Just h => if h <= 14
                        then Just (FixedOffset (sign * cast h * 3600))
                        else Nothing
            Nothing => Nothing
        _ => Nothing

    split : (Char -> Bool) -> String -> List String
    split p s = go (unpack s) [] []
      where
        go : List Char -> List Char -> List String -> List String
        go [] current acc = reverse (pack (reverse current) :: acc)
        go (c :: cs) current acc =
          if p c
            then go cs [] (pack (reverse current) :: acc)
            else go cs (c :: current) acc

--------------------------------------------------------------------------------
-- Timezone Abbreviations
--------------------------------------------------------------------------------

||| Map common abbreviations to timezones (ambiguous - use with caution)
public export
abbreviationToTimezone : String -> Maybe Timezone
abbreviationToTimezone "UTC" = Just UTC
abbreviationToTimezone "GMT" = Just UTC
abbreviationToTimezone "EST" = Just (offsetHours (-5))   -- US Eastern Standard
abbreviationToTimezone "EDT" = Just (offsetHours (-4))   -- US Eastern Daylight
abbreviationToTimezone "CST" = Just (offsetHours (-6))   -- US Central Standard
abbreviationToTimezone "CDT" = Just (offsetHours (-5))   -- US Central Daylight
abbreviationToTimezone "MST" = Just (offsetHours (-7))   -- US Mountain Standard
abbreviationToTimezone "MDT" = Just (offsetHours (-6))   -- US Mountain Daylight
abbreviationToTimezone "PST" = Just (offsetHours (-8))   -- US Pacific Standard
abbreviationToTimezone "PDT" = Just (offsetHours (-7))   -- US Pacific Daylight
abbreviationToTimezone "BST" = Just (offsetHours 1)      -- British Summer Time
abbreviationToTimezone "CET" = Just (offsetHours 1)      -- Central European Time
abbreviationToTimezone "CEST" = Just (offsetHours 2)     -- Central European Summer
abbreviationToTimezone "JST" = Just (offsetHours 9)      -- Japan Standard Time
abbreviationToTimezone "IST" = Just (offsetHoursMinutes 5 30)  -- India Standard Time
abbreviationToTimezone "AEST" = Just (offsetHours 10)    -- Australian Eastern Standard
abbreviationToTimezone "AEDT" = Just (offsetHours 11)    -- Australian Eastern Daylight
abbreviationToTimezone _ = Nothing
