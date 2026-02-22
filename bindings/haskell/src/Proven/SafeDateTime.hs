{- SPDX-License-Identifier: PMPL-1.0-or-later -}
{- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk> -}

-- | Safe date/time operations via libproven FFI.
--
-- ISO 8601 parsing and leap year detection are performed by the
-- Idris 2 verified core.
module Proven.SafeDateTime
  ( isLeapYear
  , daysInMonth
  ) where

import Data.Word (Word8)
import Foreign.C.Types (CInt)
import Proven.FFI (c_proven_datetime_is_leap_year, c_proven_datetime_days_in_month)

-- | Check if a year is a leap year.
-- Delegates to @proven_datetime_is_leap_year@ in libproven.
isLeapYear :: Int -> IO Bool
isLeapYear year = do
  result <- c_proven_datetime_is_leap_year (fromIntegral year)
  return (result /= 0)

-- | Get the number of days in a month for a given year.
-- Delegates to @proven_datetime_days_in_month@ in libproven.
daysInMonth :: Int -> Int -> IO Word8
daysInMonth year month =
  c_proven_datetime_days_in_month (fromIntegral year) (fromIntegral month)
