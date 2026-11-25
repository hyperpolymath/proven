-- SPDX-License-Identifier: MPL-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
module LifecycleMain
import Proven.ABI.Lifecycle
import System
%default total

checks : Nat -> List Bool
checks count =
  [ lifecycleCommand 0 == 0
  , lifecycleCommand 1 == 1
  , lifecycleCommand 2 == 0
  , lifecycleCommand 3 == 1
  , lifecycleCommand 4294967296 == 2
  , lifecycleCommand 18446744073709551615 == 2
  , stateCode (foldl (flip transition) Stopped [Start, Start, Stop, Stop, Start]) == 1
  , stateCode (foldl (flip transition) Started [Stop, Start, Start, Stop]) == 0
  ] ++ map (\n => lifecycleCommand (cast (n + 4)) == 2) [0..count]

main : IO ()
main = do
  args <- getArgs
  let results = checks (1023 + length args)
  if all id results
    then putStrLn ("PASS: " ++ show (length results) ++ " lifecycle assertions")
    else do
      putStrLn "FAIL: lifecycle protocol assertion"
      exitFailure
