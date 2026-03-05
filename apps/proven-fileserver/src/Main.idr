-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
--
-- proven-fileserver: Main entry point.
--
-- Minimal main that prints the server name and demonstrates the core
-- type constructors defined in Fileserver.Types.
--
-- Usage:
--   idris2 --build proven-fileserver.ipkg
--   ./build/exec/proven-fileserver

module Main

import Fileserver

%default total

||| Print all constructors of each core sum type for verification.
covering
main : IO ()
main = do
  putStrLn "proven-fileserver v0.1.0 -- network file server"
  putStrLn ""
  putStrLn $ "File server port:  " ++ show fileserverPort
  putStrLn $ "Max path length:   " ++ show maxPathLength ++ " bytes"
  putStrLn $ "Max file size:     " ++ show maxFileSize ++ " bytes"

  putStrLn "\n--- Operation ---"
  putStrLn $ "  " ++ show Read
  putStrLn $ "  " ++ show Write
  putStrLn $ "  " ++ show Create
  putStrLn $ "  " ++ show Delete
  putStrLn $ "  " ++ show Rename
  putStrLn $ "  " ++ show List
  putStrLn $ "  " ++ show Stat
  putStrLn $ "  " ++ show Lock
  putStrLn $ "  " ++ show Unlock
  putStrLn $ "  " ++ show Watch

  putStrLn "\n--- FileType ---"
  putStrLn $ "  " ++ show Regular
  putStrLn $ "  " ++ show Directory
  putStrLn $ "  " ++ show Symlink
  putStrLn $ "  " ++ show BlockDevice
  putStrLn $ "  " ++ show CharDevice
  putStrLn $ "  " ++ show FIFO
  putStrLn $ "  " ++ show Socket

  putStrLn "\n--- Permission ---"
  putStrLn $ "  " ++ show OwnerRead
  putStrLn $ "  " ++ show OwnerWrite
  putStrLn $ "  " ++ show OwnerExecute
  putStrLn $ "  " ++ show GroupRead
  putStrLn $ "  " ++ show GroupWrite
  putStrLn $ "  " ++ show GroupExecute
  putStrLn $ "  " ++ show OtherRead
  putStrLn $ "  " ++ show OtherWrite
  putStrLn $ "  " ++ show OtherExecute

  putStrLn "\n--- LockType ---"
  putStrLn $ "  " ++ show Shared
  putStrLn $ "  " ++ show Exclusive
  putStrLn $ "  " ++ show Advisory
  putStrLn $ "  " ++ show Mandatory

  putStrLn "\n--- ErrorCode ---"
  putStrLn $ "  " ++ show NotFound
  putStrLn $ "  " ++ show PermissionDenied
  putStrLn $ "  " ++ show AlreadyExists
  putStrLn $ "  " ++ show NotEmpty
  putStrLn $ "  " ++ show IsDirectory
  putStrLn $ "  " ++ show NotDirectory
  putStrLn $ "  " ++ show NoSpace
  putStrLn $ "  " ++ show ReadOnly
  putStrLn $ "  " ++ show Locked
  putStrLn $ "  " ++ show IOError
