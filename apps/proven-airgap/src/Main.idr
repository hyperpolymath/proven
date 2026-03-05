-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
--
-- Main : Entry point for proven-airgap. Prints server identity and
-- enumerates all protocol type constructors.

module Main

import Airgap

---------------------------------------------------------------------------
-- Main
---------------------------------------------------------------------------

allTransferDirections : List TransferDirection
allTransferDirections = [Import, Export]

allMediaTypes : List MediaType
allMediaTypes = [USB, OpticalDisc, TapeCartridge, DiodeLink]

allScanResults : List ScanResult
allScanResults = [Clean, Suspicious, Malicious, Unscannable]

allTransferStates : List TransferState
allTransferStates = [Pending, Scanning, Approved, Rejected, InProgress, Complete, Failed]

allValidationChecks : List ValidationCheck
allValidationChecks = [HashVerify, SignatureVerify, FormatCheck, ContentInspection, MalwareScan]

main : IO ()
main = do
  putStrLn "proven-airgap : Airgapped data transfer gateway"
  putStrLn $ "  Max transfer size: " ++ show maxTransferSize ++ " bytes"
  putStrLn $ "  Scan timeout: " ++ show scanTimeout ++ " seconds"
  putStrLn $ "  TransferDirections:  " ++ show allTransferDirections
  putStrLn $ "  MediaTypes:          " ++ show allMediaTypes
  putStrLn $ "  ScanResults:         " ++ show allScanResults
  putStrLn $ "  TransferStates:      " ++ show allTransferStates
  putStrLn $ "  ValidationChecks:    " ++ show allValidationChecks
