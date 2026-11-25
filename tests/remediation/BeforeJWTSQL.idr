-- SPDX-License-Identifier: MPL-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
module BeforeJWTSQL

import JWTSQLCases
import System

%default total

main : IO ()
main = do
  let failures = filter (not . snd) commonCases
  traverse_ (\(name, _) => putStrLn ("FAIL: " ++ name)) failures
  putStrLn (show (length failures) ++ " of " ++ show (length commonCases) ++ " common JWT/SQL assertions failed")
  if null failures then pure () else exitFailure
