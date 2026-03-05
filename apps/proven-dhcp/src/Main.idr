-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
--
-- proven-dhcp: Main entry point.
--
-- Minimal skeleton that prints the server identity, port constants, and
-- enumerates all type constructors to verify the skeleton compiles and
-- all Show instances are functional.
--
-- Usage:
--   idris2 --build proven-dhcp.ipkg
--   ./build/exec/proven-dhcp

module Main

import DHCP
import DHCP.Types

%default total

||| Print all constructors of a sum type given a list and a label.
showAll : Show a => String -> List a -> IO ()
showAll label xs = do
  putStrLn $ "  " ++ label ++ ":"
  traverse_ (\x => putStrLn $ "    - " ++ show x) xs

main : IO ()
main = do
  putStrLn "proven-dhcp v0.1.0 -- DHCP server that cannot crash"
  putStrLn ""
  putStrLn $ "Server port:       " ++ show (cast {to = Nat} serverPort)
  putStrLn $ "Client port:       " ++ show (cast {to = Nat} clientPort)
  putStrLn $ "Max message size:  " ++ show maxMessageSize ++ " bytes"
  putStrLn ""
  showAll "MessageType" allMessageTypes
  showAll "OptionCode" allOptionCodes
  showAll "HardwareType" allHardwareTypes
