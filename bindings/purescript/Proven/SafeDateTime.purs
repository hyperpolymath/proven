-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
--
-- | SafeDateTime - FFI bindings to libproven date/time operations
-- |
-- | All computation delegates to Idris 2 via the Zig FFI layer.

module Proven.SafeDateTime
  ( isLeapYear
  , daysInMonth
  ) where

import Prelude

-- | Check if a year is a leap year (delegates to Idris 2).
foreign import isLeapYearImpl :: Int -> Boolean

isLeapYear :: Int -> Boolean
isLeapYear = isLeapYearImpl

-- | Get the number of days in a month for a given year (delegates to Idris 2).
foreign import daysInMonthImpl :: Int -> Int -> Int

daysInMonth :: Int -> Int -> Int
daysInMonth = daysInMonthImpl
