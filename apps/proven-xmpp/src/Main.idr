-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
--
-- proven-xmpp: Main entry point.
--
-- Minimal skeleton that prints the server identity, port constants, and
-- enumerates all type constructors to verify the skeleton compiles and
-- all Show instances are functional.
--
-- Usage:
--   idris2 --build proven-xmpp.ipkg
--   ./build/exec/proven-xmpp

module Main

import XMPP
import XMPP.Types

%default total

||| Print all constructors of a sum type given a list and a label.
showAll : Show a => String -> List a -> IO ()
showAll label xs = do
  putStrLn $ "  " ++ label ++ ":"
  traverse_ (\x => putStrLn $ "    - " ++ show x) xs

main : IO ()
main = do
  putStrLn "proven-xmpp v0.1.0 -- XMPP server that cannot crash"
  putStrLn ""
  putStrLn $ "XMPP c2s port:  " ++ show (cast {to = Nat} xmppPort)
  putStrLn $ "XMPPS port:     " ++ show (cast {to = Nat} xmppsPort)
  putStrLn $ "XMPP s2s port:  " ++ show (cast {to = Nat} xmppServerPort)
  putStrLn ""
  showAll "StanzaType" allStanzaTypes
  showAll "MessageType" allMessageTypes
  showAll "PresenceType" allPresenceTypes
  showAll "IQType" allIQTypes
  showAll "StreamError" allStreamErrors
