-- SPDX-License-Identifier: MPL-2.0
-- SPDX-FileCopyrightText: 2025 Hyperpolymath
module SafeMathProps

import Proven.Core
import Proven.SafeMath
import Data.List
import System

%default total

||| These are universal definitional proofs of the actual Maybe-returning API.
prop_divByZeroFails : (x : Integer) -> safeDiv x 0 = Nothing
prop_divByZeroFails x = Refl

prop_modByZeroFails : (x : Integer) -> safeMod x 0 = Nothing
prop_modByZeroFails x = Refl

-- The former safeAdd/safeSub/safeNegate names and Boolean-as-Type statements
-- did not describe this API. General arithmetic and bounds theorems remain
-- proof obligations; the following finite executions do not discharge them.
samples : List Integer
samples = [minInt64, minInt64 + 1, -1024, -2, -1, 0, 1, 2, 1024,
           maxInt64 - 1, maxInt64]

check : String -> Bool -> IO ()
check label True = putStrLn ("PASS (execution): " ++ label)
check label False = do
  putStrLn ("FAIL: " ++ label)
  exitFailure

export
runMathProperties : IO ()
runMathProperties = do
  check "checked addition commutes on boundary corpus"
    (all (\x => all (\y => addChecked x y == addChecked y x) samples) samples)
  check "checked addition/subtraction zero"
    (all (\x => addChecked x 0 == Just x && subChecked x 0 == Just x) samples)
  check "division by one"
    (all (\x => safeDiv x 1 == Just x) samples)
  check "zero denominators"
    (all (\x => safeDiv x 0 == Nothing && safeMod x 0 == Nothing) samples)
  check "positive modulo denominator: quotient/remainder identity"
    (all (\x => all (\d => case (safeDiv x d, safeMod x d) of
                           (Just q, Just r) => x == q * d + r && r >= 0 && r < d
                           _ => False) [1, 2, 7, 1024]) samples)
  check "overflow and underflow refused"
    (addChecked maxInt64 1 == Nothing && subChecked minInt64 1 == Nothing &&
     mulChecked maxInt64 2 == Nothing && mulChecked minInt64 (-1) == Nothing)
  check "absolute value boundary"
    (absSafe minInt64 == Nothing && absClamped minInt64 == maxInt64 &&
     all (\x => case absSafe x of
                  Nothing => x == minInt64
                  Just n => n >= 0) samples)
  check "checked negate boundary and involution"
    (negateInt64 minInt64 == Nothing &&
     all (\x => case negateInt64 x of
                  Nothing => x == minInt64
                  Just n => negateInt64 n == Just x) samples)
  check "percentage construction rejects out-of-range input"
    (case (toPercentage 0, toPercentage 100, toPercentage 101) of
       (Just lo, Just hi, Nothing) => percentageValue lo == 0 && percentageValue hi == 100
       _ => False)
  check "clamping boundaries"
    (all (\x => let y = clamp (-10) 10 x in y >= -10 && y <= 10) samples)
  putStrLn "Two definitional zero-denominator proofs typechecked; other checks are finite execution."
