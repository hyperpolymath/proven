-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
--
-- proven-snmp: Main entry point.
--
-- Minimal skeleton that prints the agent identity, port constants, and
-- enumerates all type constructors to verify the skeleton compiles and
-- all Show instances are functional.
--
-- Usage:
--   idris2 --build proven-snmp.ipkg
--   ./build/exec/proven-snmp

module Main

import SNMP
import SNMP.Types

%default total

||| Print all constructors of a sum type given a list and a label.
showAll : Show a => String -> List a -> IO ()
showAll label xs = do
  putStrLn $ "  " ++ label ++ ":"
  traverse_ (\x => putStrLn $ "    - " ++ show x) xs

main : IO ()
main = do
  putStrLn "proven-snmp v0.1.0 -- SNMP agent that cannot crash"
  putStrLn ""
  putStrLn $ "SNMP port:  " ++ show (cast {to = Nat} snmpPort)
  putStrLn $ "Trap port:  " ++ show (cast {to = Nat} trapPort)
  putStrLn ""
  showAll "Version" allVersions
  showAll "PDUType" allPDUTypes
  showAll "ErrorStatus" allErrorStatuses
