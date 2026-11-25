-- SPDX-License-Identifier: MPL-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
-- Common API probe for the baseline and remediated source.
module EntropyFailureProbe
import Data.Nat
import Proven.SafeCrypto.Random
import System
%default total

failed : Either e a -> Bool
failed (Left _) = True
failed (Right _) = False

main : IO ()
main = do
  let checks =
        [ failed (randomBytes 4)
        , failed (randomNat 2)
        , failed (randomInt 9 3)
        , failed Proven.SafeCrypto.Random.randomDouble
        , failed (shuffle [1,2,3])
        ]
  let failures = length (filter not checks)
  if failures == 0
    then putStrLn "PASS: five common entropy failure checks"
    else do
      putStrLn ("FAIL: " ++ show failures ++ " of five entropy failure checks")
      exitFailure
