-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
--
-- proven-dbserver: Main entry point.
--
-- Minimal main that prints the server name and demonstrates the core
-- type constructors defined in Dbserver.Types.
--
-- Usage:
--   idris2 --build proven-dbserver.ipkg
--   ./build/exec/proven-dbserver

module Main

import Dbserver

%default total

||| Print all constructors of each core sum type for verification.
covering
main : IO ()
main = do
  putStrLn "proven-dbserver v0.1.0 -- database server"
  putStrLn ""
  putStrLn $ "DB port:            " ++ show dbPort
  putStrLn $ "Max query length:   " ++ show maxQueryLength ++ " bytes"
  putStrLn $ "Max connections:    " ++ show maxConnections

  putStrLn "\n--- QueryType ---"
  putStrLn $ "  " ++ show Select
  putStrLn $ "  " ++ show Insert
  putStrLn $ "  " ++ show Update
  putStrLn $ "  " ++ show Delete
  putStrLn $ "  " ++ show CreateTable
  putStrLn $ "  " ++ show DropTable
  putStrLn $ "  " ++ show AlterTable
  putStrLn $ "  " ++ show CreateIndex
  putStrLn $ "  " ++ show DropIndex
  putStrLn $ "  " ++ show Begin
  putStrLn $ "  " ++ show Commit
  putStrLn $ "  " ++ show Rollback

  putStrLn "\n--- DataType ---"
  putStrLn $ "  " ++ show Integer
  putStrLn $ "  " ++ show Float
  putStrLn $ "  " ++ show Text
  putStrLn $ "  " ++ show Blob
  putStrLn $ "  " ++ show Boolean
  putStrLn $ "  " ++ show Timestamp
  putStrLn $ "  " ++ show UUID
  putStrLn $ "  " ++ show JSON
  putStrLn $ "  " ++ show Null

  putStrLn "\n--- IsolationLevel ---"
  putStrLn $ "  " ++ show ReadUncommitted
  putStrLn $ "  " ++ show ReadCommitted
  putStrLn $ "  " ++ show RepeatableRead
  putStrLn $ "  " ++ show Serializable

  putStrLn "\n--- ErrorCode ---"
  putStrLn $ "  " ++ show SyntaxError
  putStrLn $ "  " ++ show TableNotFound
  putStrLn $ "  " ++ show ColumnNotFound
  putStrLn $ "  " ++ show DuplicateKey
  putStrLn $ "  " ++ show ConstraintViolation
  putStrLn $ "  " ++ show TypeMismatch
  putStrLn $ "  " ++ show DeadlockDetected
  putStrLn $ "  " ++ show TransactionAborted
  putStrLn $ "  " ++ show DiskFull
  putStrLn $ "  " ++ show ConnectionLost

  putStrLn "\n--- JoinType ---"
  putStrLn $ "  " ++ show Inner
  putStrLn $ "  " ++ show LeftOuter
  putStrLn $ "  " ++ show RightOuter
  putStrLn $ "  " ++ show FullOuter
  putStrLn $ "  " ++ show Cross
