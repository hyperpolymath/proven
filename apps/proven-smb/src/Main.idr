-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
--
-- proven-smb: Main entry point.
--
-- Minimal skeleton that prints the client identity, port constant, and
-- enumerates all type constructors to verify the skeleton compiles and
-- all Show instances are functional.
--
-- Usage:
--   idris2 --build proven-smb.ipkg
--   ./build/exec/proven-smb

module Main

import SMB
import SMB.Types

%default total

||| Print all constructors of a sum type given a list and a label.
showAll : Show a => String -> List a -> IO ()
showAll label xs = do
  putStrLn $ "  " ++ label ++ ":"
  traverse_ (\x => putStrLn $ "    - " ++ show x) xs

main : IO ()
main = do
  putStrLn "proven-smb v0.1.0 -- SMB2/3 client that cannot crash"
  putStrLn ""
  putStrLn $ "SMB port:  " ++ show (cast {to = Nat} smbPort)
  putStrLn ""
  showAll "Command" allCommands
  showAll "Dialect" allDialects
  showAll "ShareType" allShareTypes
