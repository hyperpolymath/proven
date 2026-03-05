-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
--
-- proven-imap: Main entry point.
--
-- Minimal skeleton that prints the server identity, port constants, and
-- enumerates all type constructors to verify the skeleton compiles and
-- all Show instances are functional.
--
-- Usage:
--   idris2 --build proven-imap.ipkg
--   ./build/exec/proven-imap

module Main

import IMAP
import IMAP.Types

%default total

||| Print all constructors of a sum type given a list and a label.
showAll : Show a => String -> List a -> IO ()
showAll label xs = do
  putStrLn $ "  " ++ label ++ ":"
  traverse_ (\x => putStrLn $ "    - " ++ show x) xs

main : IO ()
main = do
  putStrLn "proven-imap v0.1.0 -- IMAP4rev1 server that cannot crash"
  putStrLn ""
  putStrLn $ "IMAP port:   " ++ show (cast {to = Nat} imapPort)
  putStrLn $ "IMAPS port:  " ++ show (cast {to = Nat} imapsPort)
  putStrLn ""
  showAll "Command" allCommands
  showAll "State" allStates
  showAll "Flag" allFlags
