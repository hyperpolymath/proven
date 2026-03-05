-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
--
-- proven-pop3: Main entry point.
--
-- Minimal skeleton that prints the server identity, port constants, and
-- enumerates all type constructors to verify the skeleton compiles and
-- all Show instances are functional.
--
-- Usage:
--   idris2 --build proven-pop3.ipkg
--   ./build/exec/proven-pop3

module Main

import POP3
import POP3.Types

%default total

||| Print all constructors of a sum type given a list and a label.
showAll : Show a => String -> List a -> IO ()
showAll label xs = do
  putStrLn $ "  " ++ label ++ ":"
  traverse_ (\x => putStrLn $ "    - " ++ show x) xs

main : IO ()
main = do
  putStrLn "proven-pop3 v0.1.0 -- POP3 server that cannot crash"
  putStrLn ""
  putStrLn $ "POP3 port:   " ++ show (cast {to = Nat} pop3Port)
  putStrLn $ "POP3S port:  " ++ show (cast {to = Nat} pop3sPort)
  putStrLn ""
  showAll "Command" allCommands
  showAll "State" allStates
  showAll "Response" allResponses
