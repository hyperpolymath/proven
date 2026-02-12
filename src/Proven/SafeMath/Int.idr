-- SPDX-License-Identifier: Apache-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
||| Safe operations on signed integers
module Proven.SafeMath.Int

import Proven.Core

%default total

--------------------------------------------------------------------------------
-- Signed Integer Bounds
--------------------------------------------------------------------------------

||| Maximum 8-bit signed integer
public export
maxInt8 : Integer
maxInt8 = 127

||| Minimum 8-bit signed integer
public export
minInt8 : Integer
minInt8 = -128

||| Maximum 16-bit signed integer
public export
maxInt16 : Integer
maxInt16 = 32767

||| Minimum 16-bit signed integer
public export
minInt16 : Integer
minInt16 = -32768

||| Maximum 32-bit signed integer
public export
maxInt32 : Integer
maxInt32 = 2147483647

||| Minimum 32-bit signed integer
public export
minInt32 : Integer
minInt32 = -2147483648

||| Maximum 64-bit signed integer
public export
maxInt64 : Integer
maxInt64 = 9223372036854775807

||| Minimum 64-bit signed integer
public export
minInt64 : Integer
minInt64 = -9223372036854775808

--------------------------------------------------------------------------------
-- Bounds Checking
--------------------------------------------------------------------------------

||| Check if value fits in 8-bit signed integer
public export
fitsInt8 : Integer -> Bool
fitsInt8 n = n >= minInt8 && n <= maxInt8

||| Check if value fits in 16-bit signed integer
public export
fitsInt16 : Integer -> Bool
fitsInt16 n = n >= minInt16 && n <= maxInt16

||| Check if value fits in 32-bit signed integer
public export
fitsInt32 : Integer -> Bool
fitsInt32 n = n >= minInt32 && n <= maxInt32

||| Check if value fits in 64-bit signed integer
public export
fitsInt64 : Integer -> Bool
fitsInt64 n = n >= minInt64 && n <= maxInt64

--------------------------------------------------------------------------------
-- Safe Conversions
--------------------------------------------------------------------------------

||| Convert to 8-bit integer, returning Nothing if out of range
public export
toInt8 : Integer -> Maybe Integer
toInt8 n = if fitsInt8 n then Just n else Nothing

||| Convert to 16-bit integer, returning Nothing if out of range
public export
toInt16 : Integer -> Maybe Integer
toInt16 n = if fitsInt16 n then Just n else Nothing

||| Convert to 32-bit integer, returning Nothing if out of range
public export
toInt32 : Integer -> Maybe Integer
toInt32 n = if fitsInt32 n then Just n else Nothing

||| Convert to 64-bit integer, returning Nothing if out of range
public export
toInt64 : Integer -> Maybe Integer
toInt64 n = if fitsInt64 n then Just n else Nothing

--------------------------------------------------------------------------------
-- Checked Operations for Specific Widths
--------------------------------------------------------------------------------

||| Add two 64-bit integers with overflow checking
public export
addInt64 : Integer -> Integer -> Maybe Integer
addInt64 a b =
  let result = a + b
  in if fitsInt64 result then Just result else Nothing

||| Subtract two 64-bit integers with underflow checking
public export
subInt64 : Integer -> Integer -> Maybe Integer
subInt64 a b =
  let result = a - b
  in if fitsInt64 result then Just result else Nothing

||| Multiply two 64-bit integers with overflow checking
public export
mulInt64 : Integer -> Integer -> Maybe Integer
mulInt64 a b =
  let result = a * b
  in if fitsInt64 result then Just result else Nothing

||| Negate with overflow checking (MIN_INT cannot be negated)
public export
negateInt64 : Integer -> Maybe Integer
negateInt64 n =
  if n == minInt64
    then Nothing
    else Just (negate n)

--------------------------------------------------------------------------------
-- Sign Operations
--------------------------------------------------------------------------------

||| Get the sign of an integer (-1, 0, or 1)
public export
signum : Integer -> Integer
signum n =
  if n < 0 then -1
  else if n > 0 then 1
  else 0

||| Check if negative
public export
isNegative : Integer -> Bool
isNegative n = n < 0

||| Check if positive
public export
isPositive : Integer -> Bool
isPositive n = n > 0

||| Check if zero
public export
isZero : Integer -> Bool
isZero n = n == 0

||| Check if non-negative (>= 0)
public export
isNonNegative : Integer -> Bool
isNonNegative n = n >= 0

||| Check if non-positive (<= 0)
public export
isNonPositive : Integer -> Bool
isNonPositive n = n <= 0

--------------------------------------------------------------------------------
-- Rounding and Truncation
--------------------------------------------------------------------------------

||| Round towards zero
public export
truncateToward0 : Integer -> Integer -> Maybe Integer
truncateToward0 _ 0 = Nothing
truncateToward0 n d = Just (quot n d)
  where
    quot : Integer -> Integer -> Integer
    quot a b = a `div` b  -- Idris div truncates toward negative infinity

||| Round away from zero
public export
roundAwayFrom0 : Integer -> Integer -> Maybe Integer
roundAwayFrom0 _ 0 = Nothing
roundAwayFrom0 n d =
  let q = n `div` d
      r = n `mod` d
  in if r == 0 then Just q
     else if n > 0 then Just (q + 1)
     else Just (q - 1)
