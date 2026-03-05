-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
--
-- proven-proxy: Main entry point.
--
-- Minimal skeleton that prints the server identity, port constants, and
-- enumerates all type constructors to verify the skeleton compiles and
-- all Show instances are functional.
--
-- Usage:
--   idris2 --build proven-proxy.ipkg
--   ./build/exec/proven-proxy

module Main

import Proxy
import Proxy.Types

%default total

||| Print all constructors of a sum type given a list and a label.
showAll : Show a => String -> List a -> IO ()
showAll label xs = do
  putStrLn $ "  " ++ label ++ ":"
  traverse_ (\x => putStrLn $ "    - " ++ show x) xs

main : IO ()
main = do
  putStrLn "proven-proxy v0.1.0 -- HTTP proxy that cannot crash"
  putStrLn ""
  putStrLn $ "Default port:     " ++ show (cast {to = Nat} defaultPort)
  putStrLn $ "Max header size:  " ++ show maxHeaderSize ++ " bytes"
  putStrLn ""
  showAll "ProxyMode" allProxyModes
  showAll "HopByHopHeader" allHopByHopHeaders
  showAll "CacheDirective" allCacheDirectives
  showAll "ProxyError" allProxyErrors
