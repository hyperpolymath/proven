{- SPDX-License-Identifier: PMPL-1.0-or-later -}
{- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk> -}

-- | Tests for the proven Haskell FFI binding.
--
-- These tests verify that the FFI marshaling correctly communicates
-- with libproven. All actual computation is performed by the
-- Idris 2 verified core; these tests only validate the binding layer.
--
-- Requires libproven to be built and available at link time:
--   cd ../../ffi/zig && zig build
--   cd ../../bindings/haskell && cabal test

import Test.Hspec
import Proven.FFI (c_proven_init, c_proven_version_major, c_proven_version_minor,
                   c_proven_version_patch, c_proven_module_count)
import Proven.SafeMath (safeDiv, safeMod, safeAdd, safeSub, safeMul, safeAbs,
                        safeClamp, safePow)
import Proven.SafeFloat (safeFloatDiv, isFinite, isNaN', safeSqrt, safeLn)
import Proven.SafeAngle (degToRad, radToDeg, normalizeDegrees)
import Proven.SafeProbability (createProbability, probabilityNot)
import Proven.SafeML (sigmoid, relu, leakyRelu, mlClamp)
import Proven.SafeDateTime (isLeapYear, daysInMonth)

main :: IO ()
main = do
  -- Initialize libproven runtime before all tests
  initResult <- c_proven_init
  if initResult /= 0
    then putStrLn "WARNING: proven_init failed; tests requiring libproven will fail"
    else return ()
  hspec $ do
    describe "Lifecycle" $ do
      it "proven_init returns 0 (ok)" $
        initResult `shouldBe` 0

      it "version_major returns a value" $ do
        v <- c_proven_version_major
        v `shouldSatisfy` (>= 0)

      it "module_count returns > 0" $ do
        c <- c_proven_module_count
        c `shouldSatisfy` (> 0)

    describe "SafeMath (FFI -> Idris2)" $ do
      it "safeDiv divides correctly" $ do
        result <- safeDiv 10 2
        result `shouldBe` Just 5

      it "safeDiv returns Nothing on division by zero" $ do
        result <- safeDiv 10 0
        result `shouldBe` Nothing

      it "safeMod computes modulo correctly" $ do
        result <- safeMod 10 3
        result `shouldBe` Just 1

      it "safeMod returns Nothing on mod by zero" $ do
        result <- safeMod 10 0
        result `shouldBe` Nothing

      it "safeAdd adds correctly" $ do
        result <- safeAdd 1 2
        result `shouldBe` Just 3

      it "safeSub subtracts correctly" $ do
        result <- safeSub 5 3
        result `shouldBe` Just 2

      it "safeMul multiplies correctly" $ do
        result <- safeMul 3 4
        result `shouldBe` Just 12

      it "safeClamp clamps to range" $ do
        result <- safeClamp 0 10 15
        result `shouldBe` 10

      it "safeClamp returns value in range" $ do
        result <- safeClamp 0 10 5
        result `shouldBe` 5

    describe "SafeFloat (FFI -> Idris2)" $ do
      it "safeFloatDiv divides correctly" $ do
        result <- safeFloatDiv 10.0 2.0
        case result of
          Just v  -> v `shouldSatisfy` (\x -> abs (x - 5.0) < 0.001)
          Nothing -> expectationFailure "Expected Just value"

      it "safeFloatDiv returns Nothing on division by zero" $ do
        result <- safeFloatDiv 10.0 0.0
        result `shouldBe` Nothing

    describe "SafeDateTime (FFI -> Idris2)" $ do
      it "isLeapYear detects leap years" $ do
        r2000 <- isLeapYear 2000
        r2000 `shouldBe` True
        r1900 <- isLeapYear 1900
        r1900 `shouldBe` False
        r2024 <- isLeapYear 2024
        r2024 `shouldBe` True

      it "daysInMonth returns correct values" $ do
        feb <- daysInMonth 2024 2
        feb `shouldBe` 29
        jan <- daysInMonth 2024 1
        jan `shouldBe` 31

    describe "SafeProbability (FFI -> Idris2)" $ do
      it "createProbability clamps to [0, 1]" $ do
        result <- createProbability 1.5
        result `shouldSatisfy` (<= 1.0)

      it "probabilityNot is complement" $ do
        result <- probabilityNot 0.3
        result `shouldSatisfy` (\x -> abs (x - 0.7) < 0.001)

    describe "SafeML (FFI -> Idris2)" $ do
      it "sigmoid(0) is 0.5" $ do
        result <- sigmoid 0.0
        result `shouldSatisfy` (\x -> abs (x - 0.5) < 0.001)

      it "relu of negative is 0" $ do
        result <- relu (-5.0)
        result `shouldSatisfy` (\x -> abs x < 0.001)

      it "relu of positive is identity" $ do
        result <- relu 3.0
        result `shouldSatisfy` (\x -> abs (x - 3.0) < 0.001)
