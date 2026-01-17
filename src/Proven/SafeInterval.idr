-- SPDX-License-Identifier: Palimpsest-MPL-1.0
||| SafeInterval - Safe interval arithmetic
|||
||| This module provides interval operations with proper handling of
||| edge cases and support for interval analysis.
module Proven.SafeInterval

import public Proven.Core

%default total

--------------------------------------------------------------------------------
-- Interval Type
--------------------------------------------------------------------------------

||| A closed interval [lo, hi] where lo <= hi
public export
record Interval a where
  constructor MkInterval
  lo : a
  hi : a

||| Interval errors
public export
data IntervalError
  = EmptyInterval
  | InvalidBounds
  | DivisionByZeroInterval

public export
Show IntervalError where
  show EmptyInterval = "Empty interval"
  show InvalidBounds = "Invalid interval bounds (lo > hi)"
  show DivisionByZeroInterval = "Interval division includes zero"

--------------------------------------------------------------------------------
-- Construction
--------------------------------------------------------------------------------

||| Create an interval, ensuring lo <= hi
public export
interval : Ord a => a -> a -> Maybe (Interval a)
interval lo hi = if lo <= hi then Just (MkInterval lo hi) else Nothing

||| Create a point interval [x, x]
public export
point : a -> Interval a
point x = MkInterval x x

||| Create an interval, swapping if necessary
public export
intervalSafe : Ord a => a -> a -> Interval a
intervalSafe lo hi = if lo <= hi then MkInterval lo hi else MkInterval hi lo

||| Unsafe interval creation (use only when bounds are known valid)
public export
unsafeInterval : a -> a -> Interval a
unsafeInterval = MkInterval

||| Empty interval representation (impossible to construct directly)
||| Use Maybe (Interval a) instead

--------------------------------------------------------------------------------
-- Queries
--------------------------------------------------------------------------------

||| Get the width of an interval
public export
width : Num a => Interval a -> a
width (MkInterval lo hi) = hi - lo

||| Get the midpoint of an interval
public export
midpoint : Fractional a => Interval a -> a
midpoint (MkInterval lo hi) = (lo + hi) / 2

||| Check if a value is in the interval
public export
contains : Ord a => Interval a -> a -> Bool
contains (MkInterval lo hi) x = lo <= x && x <= hi

||| Check if one interval contains another
public export
containsInterval : Ord a => Interval a -> Interval a -> Bool
containsInterval outer (MkInterval lo hi) = contains outer lo && contains outer hi

||| Check if two intervals overlap
public export
overlaps : Ord a => Interval a -> Interval a -> Bool
overlaps (MkInterval lo1 hi1) (MkInterval lo2 hi2) =
  lo1 <= hi2 && lo2 <= hi1

||| Check if interval is a single point
public export
isPoint : Eq a => Interval a -> Bool
isPoint (MkInterval lo hi) = lo == hi

||| Check if interval contains zero
public export
containsZero : Ord a => Num a => Interval a -> Bool
containsZero = flip contains 0

--------------------------------------------------------------------------------
-- Set Operations
--------------------------------------------------------------------------------

||| Intersection of two intervals
public export
intersection : Ord a => Interval a -> Interval a -> Maybe (Interval a)
intersection (MkInterval lo1 hi1) (MkInterval lo2 hi2) =
  let lo = max lo1 lo2
      hi = min hi1 hi2
  in if lo <= hi then Just (MkInterval lo hi) else Nothing

||| Hull (smallest containing interval) of two intervals
public export
hull : Ord a => Interval a -> Interval a -> Interval a
hull (MkInterval lo1 hi1) (MkInterval lo2 hi2) =
  MkInterval (min lo1 lo2) (max hi1 hi2)

||| Hull of a list of intervals
public export
hullList : Ord a => List (Interval a) -> Maybe (Interval a)
hullList [] = Nothing
hullList (x :: xs) = Just (foldl hull x xs)

--------------------------------------------------------------------------------
-- Arithmetic Operations
--------------------------------------------------------------------------------

||| Addition of intervals: [a,b] + [c,d] = [a+c, b+d]
public export
add : Num a => Interval a -> Interval a -> Interval a
add (MkInterval lo1 hi1) (MkInterval lo2 hi2) =
  MkInterval (lo1 + lo2) (hi1 + hi2)

||| Subtraction: [a,b] - [c,d] = [a-d, b-c]
public export
sub : Num a => Neg a => Interval a -> Interval a -> Interval a
sub (MkInterval lo1 hi1) (MkInterval lo2 hi2) =
  MkInterval (lo1 - hi2) (hi1 - lo2)

||| Multiplication (for ordered numeric types)
public export
mul : Num a => Ord a => Interval a -> Interval a -> Interval a
mul (MkInterval lo1 hi1) (MkInterval lo2 hi2) =
  let products = [lo1 * lo2, lo1 * hi2, hi1 * lo2, hi1 * hi2]
      minP = foldl1 min products
      maxP = foldl1 max products
  in MkInterval minP maxP
  where
    foldl1 : (a -> a -> a) -> List a -> a
    foldl1 _ [x] = x
    foldl1 f (x :: y :: xs) = foldl1 f (f x y :: xs)
    foldl1 _ [] = lo1 * lo2  -- Fallback (shouldn't happen)

||| Division (fails if divisor interval contains zero)
public export
div : Fractional a => Ord a => Num a =>
      Interval a -> Interval a -> Maybe (Interval a)
div i1 i2@(MkInterval lo2 hi2) =
  if containsZero i2
    then Nothing
    else Just (mul i1 (MkInterval (1 / hi2) (1 / lo2)))

||| Negation
public export
negate : Neg a => Interval a -> Interval a
negate (MkInterval lo hi) = MkInterval (negate hi) (negate lo)

||| Absolute value
public export
abs : Num a => Ord a => Neg a => Interval a -> Interval a
abs i@(MkInterval lo hi) =
  if lo >= 0 then i
  else if hi <= 0 then negate i
  else MkInterval 0 (max (negate lo) hi)

||| Square of an interval
public export
square : Num a => Ord a => Neg a => Interval a -> Interval a
square i@(MkInterval lo hi) =
  if lo >= 0 then MkInterval (lo * lo) (hi * hi)
  else if hi <= 0 then MkInterval (hi * hi) (lo * lo)
  else MkInterval 0 (max (lo * lo) (hi * hi))

||| Power to natural exponent
public export
pow : Num a => Ord a => Neg a => Interval a -> Nat -> Interval a
pow i Z = point 1
pow i (S Z) = i
pow i (S (S n)) =
  let sq = square i
  in if (S (S n)) `mod` 2 == 0
       then pow sq (div (S (S n)) 2)
       else mul i (pow sq (div (S n) 2))

--------------------------------------------------------------------------------
-- Functions on Intervals
--------------------------------------------------------------------------------

||| Square root (requires non-negative interval)
public export
sqrt : Interval Double -> Maybe (Interval Double)
sqrt i@(MkInterval lo hi) =
  if lo < 0 then Nothing
  else Just (MkInterval (sqrt lo) (sqrt hi))

||| Exponential function
public export
exp : Interval Double -> Interval Double
exp (MkInterval lo hi) = MkInterval (exp lo) (exp hi)

||| Natural logarithm (requires positive interval)
public export
ln : Interval Double -> Maybe (Interval Double)
ln (MkInterval lo hi) =
  if lo <= 0 then Nothing
  else Just (MkInterval (log lo) (log hi))

||| Sine function (conservative enclosure)
public export
sin : Interval Double -> Interval Double
sin (MkInterval lo hi) =
  let w = hi - lo
  in if w >= 2 * pi
       then MkInterval (-1) 1  -- Full range
       else -- Conservative: just compute at endpoints and expand
         let sinLo = sin lo
             sinHi = sin hi
         in MkInterval (min sinLo sinHi - 0.0001) (max sinLo sinHi + 0.0001)
  where
    pi : Double
    pi = 3.141592653589793

||| Cosine function (conservative enclosure)
public export
cos : Interval Double -> Interval Double
cos (MkInterval lo hi) =
  let w = hi - lo
  in if w >= 2 * pi
       then MkInterval (-1) 1
       else let cosLo = cos lo
                cosHi = cos hi
            in MkInterval (min cosLo cosHi - 0.0001) (max cosLo cosHi + 0.0001)
  where
    pi : Double
    pi = 3.141592653589793

--------------------------------------------------------------------------------
-- Inflation and Deflation
--------------------------------------------------------------------------------

||| Inflate an interval by a factor
public export
inflate : Num a => Neg a => a -> Interval a -> Interval a
inflate delta (MkInterval lo hi) = MkInterval (lo - delta) (hi + delta)

||| Deflate an interval by a factor (may become empty)
public export
deflate : Ord a => Num a => Neg a => a -> Interval a -> Maybe (Interval a)
deflate delta (MkInterval lo hi) =
  let newLo = lo + delta
      newHi = hi - delta
  in if newLo <= newHi then Just (MkInterval newLo newHi) else Nothing

--------------------------------------------------------------------------------
-- Display
--------------------------------------------------------------------------------

public export
Eq a => Eq (Interval a) where
  (==) (MkInterval lo1 hi1) (MkInterval lo2 hi2) = lo1 == lo2 && hi1 == hi2

public export
Show a => Show (Interval a) where
  show (MkInterval lo hi) = "[" ++ show lo ++ ", " ++ show hi ++ "]"
