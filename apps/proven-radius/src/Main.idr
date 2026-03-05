-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
--
-- proven-radius: Main entry point.
--
-- Minimal skeleton that prints the server identity, port constants, and
-- enumerates all type constructors to verify the skeleton compiles and
-- all Show instances are functional.
--
-- Usage:
--   idris2 --build proven-radius.ipkg
--   ./build/exec/proven-radius

module Main

import RADIUS
import RADIUS.Types

%default total

||| Print all constructors of a sum type given a list and a label.
showAll : Show a => String -> List a -> IO ()
showAll label xs = do
  putStrLn $ "  " ++ label ++ ":"
  traverse_ (\x => putStrLn $ "    - " ++ show x) xs

main : IO ()
main = do
  putStrLn "proven-radius v0.1.0 -- RADIUS server that cannot crash"
  putStrLn ""
  putStrLn $ "Auth port:        " ++ show (cast {to = Nat} authPort)
  putStrLn $ "Accounting port:  " ++ show (cast {to = Nat} acctPort)
  putStrLn $ "Max packet size:  " ++ show maxPacketSize ++ " bytes"
  putStrLn ""
  showAll "PacketType" allPacketTypes
  showAll "AttributeType" allAttributeTypes
  showAll "ServiceType" allServiceTypes
