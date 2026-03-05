-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
--
-- proven-nfs: Main entry point.
--
-- Minimal skeleton that prints the server identity, port constant, and
-- enumerates all type constructors to verify the skeleton compiles and
-- all Show instances are functional.
--
-- Usage:
--   idris2 --build proven-nfs.ipkg
--   ./build/exec/proven-nfs

module Main

import NFS
import NFS.Types

%default total

||| Print all constructors of a sum type given a list and a label.
showAll : Show a => String -> List a -> IO ()
showAll label xs = do
  putStrLn $ "  " ++ label ++ ":"
  traverse_ (\x => putStrLn $ "    - " ++ show x) xs

main : IO ()
main = do
  putStrLn "proven-nfs v0.1.0 -- NFSv4 server that cannot crash"
  putStrLn ""
  putStrLn $ "NFS port:  " ++ show (cast {to = Nat} nfsPort)
  putStrLn ""
  showAll "Operation" allOperations
  showAll "FileType" allFileTypes
  showAll "Status" allStatuses
