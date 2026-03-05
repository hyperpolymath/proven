-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
--
-- proven-diode: Main entry point.
--
-- Minimal main that prints the server name and demonstrates the core
-- type constructors defined in Diode.Types.
--
-- Usage:
--   idris2 --build proven-diode.ipkg
--   ./build/exec/proven-diode

module Main

import Diode

%default total

||| Print all constructors of each core sum type for verification.
covering
main : IO ()
main = do
  putStrLn "proven-diode v0.1.0 -- data diode (unidirectional gateway)"
  putStrLn ""
  putStrLn $ "Max segment size: " ++ show maxSegmentSize ++ " bytes"
  putStrLn $ "Max queue depth:  " ++ show maxQueueDepth

  putStrLn "\n--- Direction ---"
  putStrLn $ "  " ++ show HighToLow
  putStrLn $ "  " ++ show LowToHigh

  putStrLn "\n--- Protocol ---"
  putStrLn $ "  " ++ show UDP
  putStrLn $ "  " ++ show TCP
  putStrLn $ "  " ++ show FileTransfer
  putStrLn $ "  " ++ show Syslog
  putStrLn $ "  " ++ show SNMP

  putStrLn "\n--- TransferState ---"
  putStrLn $ "  " ++ show Queued
  putStrLn $ "  " ++ show Sending
  putStrLn $ "  " ++ show Confirming
  putStrLn $ "  " ++ show Complete
  putStrLn $ "  " ++ show Failed

  putStrLn "\n--- ValidationResult ---"
  putStrLn $ "  " ++ show Passed
  putStrLn $ "  " ++ show FormatError
  putStrLn $ "  " ++ show SizeExceeded
  putStrLn $ "  " ++ show PolicyBlocked

  putStrLn "\n--- IntegrityCheck ---"
  putStrLn $ "  " ++ show CRC32
  putStrLn $ "  " ++ show SHA256
  putStrLn $ "  " ++ show HMAC
