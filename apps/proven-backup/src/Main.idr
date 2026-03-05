-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

||| Entry point for the proven-backup server.
||| Prints server identification and enumerates core type constructors.
module Main

import Backup

%default total

||| Print server name, port, and enumerate all type constructors.
partial
main : IO ()
main = do
  putStrLn "=========================================="
  putStrLn $ " " ++ serverName ++ " (port " ++ show backupPort ++ ")"
  putStrLn "=========================================="
  putStrLn ""
  putStrLn $ "Default retention: " ++ show defaultRetentionDays ++ " days"
  putStrLn $ "Max concurrent jobs: " ++ show maxConcurrentJobs
  putStrLn ""
  putStrLn "--- BackupType ---"
  printLn Full
  printLn Incremental
  printLn Differential
  printLn Snapshot
  printLn Mirror
  putStrLn ""
  putStrLn "--- ScheduleFreq ---"
  printLn Hourly
  printLn Daily
  printLn Weekly
  printLn Monthly
  printLn OnDemand
  putStrLn ""
  putStrLn "--- CompressionAlg ---"
  printLn Backup.Types.None
  printLn Gzip
  printLn Zstd
  printLn LZ4
  printLn XZ
  putStrLn ""
  putStrLn "--- EncryptionAlg ---"
  printLn NoEncryption
  printLn AES256GCM
  printLn ChaCha20Poly1305
  putStrLn ""
  putStrLn "--- BackupState ---"
  printLn Idle
  printLn Running
  printLn Verifying
  printLn Complete
  printLn Failed
  printLn Cancelled
  putStrLn ""
  putStrLn "--- RetentionPolicy ---"
  printLn KeepAll
  printLn KeepLast
  printLn KeepDaily
  printLn KeepWeekly
  printLn KeepMonthly
