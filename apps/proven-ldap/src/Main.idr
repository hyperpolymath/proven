-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
--
-- proven-ldap: Main entry point.
--
-- Minimal skeleton that prints the server identity, port constants, and
-- enumerates all type constructors to verify the skeleton compiles and
-- all Show instances are functional.
--
-- Usage:
--   idris2 --build proven-ldap.ipkg
--   ./build/exec/proven-ldap

module Main

import LDAP
import LDAP.Types

%default total

||| Print all constructors of a sum type given a list and a label.
showAll : Show a => String -> List a -> IO ()
showAll label xs = do
  putStrLn $ "  " ++ label ++ ":"
  traverse_ (\x => putStrLn $ "    - " ++ show x) xs

main : IO ()
main = do
  putStrLn "proven-ldap v0.1.0 -- LDAP server that cannot crash"
  putStrLn ""
  putStrLn $ "LDAP port:   " ++ show (cast {to = Nat} ldapPort)
  putStrLn $ "LDAPS port:  " ++ show (cast {to = Nat} ldapsPort)
  putStrLn ""
  showAll "Operation" allOperations
  showAll "SearchScope" allSearchScopes
  showAll "ResultCode" allResultCodes
