-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
--
-- proven-lpd: Main entry point
--
-- A Line Printer Daemon implementation that cannot crash on malformed
-- print jobs. Uses proven's type-safe approach for job validation,
-- bounded queues, and protocol state machines.
--
-- Usage:
--   idris2 --build proven-lpd.ipkg
--   ./build/exec/proven-lpd

module Main

import LPD
import LPD.Command
import LPD.Job
import LPD.Queue
import LPD.Protocol
import LPD.Spool

%default total

-- ============================================================================
-- Demo: print job lifecycle
-- ============================================================================

||| Demonstrate creating, enqueuing, and processing print jobs.
covering
demoJobLifecycle : IO ()
demoJobLifecycle = do
  putStrLn "\n--- LPD Job Lifecycle Demo ---\n"

  -- Create a queue
  let q0 = defaultQueue "lp0"
  putStrLn $ "Queue created: " ++ show q0

  -- Submit job 1
  case enqueue "alice" "report.pdf" "PDF content here..." q0 of
    Left err => putStrLn $ "  Submit failed: " ++ show err
    Right (q1, jid1) => do
      putStrLn $ "  Submitted: Job " ++ show jid1 ++ " by alice"

      -- Submit job 2
      case enqueue "bob" "invoice.txt" "Invoice #12345\nAmount: 42.00" q1 of
        Left err => putStrLn $ "  Submit failed: " ++ show err
        Right (q2, jid2) => do
          putStrLn $ "  Submitted: Job " ++ show jid2 ++ " by bob"
          putStrLn $ "  Queue depth: " ++ show (currentDepth q2)

          -- Dequeue and print job 1
          case dequeue q2 of
            Nothing => putStrLn $ "  No jobs to print"
            Just (job, q3) => do
              putStrLn $ "\n  Printing: " ++ show job
              let q4 = completeJob job.jobId q3
              putStrLn $ "  Completed: Job " ++ show job.jobId

              -- Show queue state
              putStrLn $ "\n  Queue state: " ++ show q4
              putStrLn $ "  Pending: " ++ show (countByStatus Pending q4)
              putStrLn $ "  Complete: " ++ show (countByStatus Complete q4)

              -- Short listing
              putStrLn "\n  Short listing:"
              traverse_ (\s => putStrLn $ "    " ++ s) (shortListing q4)

-- ============================================================================
-- Demo: job validation
-- ============================================================================

||| Demonstrate job creation with valid and invalid inputs.
covering
demoJobValidation : IO ()
demoJobValidation = do
  putStrLn "\n--- LPD Job Validation Demo ---\n"

  -- Valid job
  case createJob maxJobSize 1 "alice" "document.ps" "PostScript data..." of
    Left err => putStrLn $ "  Unexpected error: " ++ show err
    Right job => putStrLn $ "  Valid job: " ++ show job

  -- Invalid: empty user
  case createJob maxJobSize 2 "" "document.ps" "data" of
    Left err => putStrLn $ "  Rejected (empty user): " ++ show err
    Right _  => putStrLn $ "  Unexpected success"

  -- Invalid: job ID > 999
  case createJob maxJobSize 1000 "alice" "document.ps" "data" of
    Left err => putStrLn $ "  Rejected (bad ID): " ++ show err
    Right _  => putStrLn $ "  Unexpected success"

  -- Invalid: empty filename
  case createJob maxJobSize 3 "alice" "" "data" of
    Left err => putStrLn $ "  Rejected (empty filename): " ++ show err
    Right _  => putStrLn $ "  Unexpected success"

-- ============================================================================
-- Demo: queue management
-- ============================================================================

||| Demonstrate queue capacity enforcement and job removal.
covering
demoQueueManagement : IO ()
demoQueueManagement = do
  putStrLn "\n--- LPD Queue Management Demo ---\n"

  -- Create a tiny queue (max 3 jobs) for demonstration
  let cfg = MkQueueConfig "laser1" 3 1048576 True
  let q0 = newQueue cfg
  putStrLn $ "Queue: " ++ show q0

  -- Fill the queue
  let Right (q1, _) = enqueue "alice" "a.txt" "aaa" q0
    | Left err => putStrLn $ "  Error: " ++ show err
  let Right (q2, _) = enqueue "bob" "b.txt" "bbb" q1
    | Left err => putStrLn $ "  Error: " ++ show err
  let Right (q3, jid3) = enqueue "carol" "c.txt" "ccc" q2
    | Left err => putStrLn $ "  Error: " ++ show err
  putStrLn $ "  Filled queue: " ++ show (currentDepth q3) ++ "/" ++ show cfg.maxDepth

  -- Try to add one more (should fail: queue full)
  case enqueue "dave" "d.txt" "ddd" q3 of
    Left err => putStrLn $ "  Correctly rejected: " ++ show err
    Right _  => putStrLn $ "  Unexpected success (queue should be full)"

  -- Remove bob's job (by alice should fail, by bob should succeed)
  let jid2 = MkJobId 2
  case removeJob jid2 "alice" q3 of
    Left err => putStrLn $ "  Permission check works: " ++ show err
    Right _  => putStrLn $ "  Unexpected success"
  case removeJob jid2 "bob" q3 of
    Left err => putStrLn $ "  Unexpected error: " ++ show err
    Right q4 => do
      putStrLn $ "  Bob removed own job: depth=" ++ show (currentDepth q4)

  -- Pause and resume
  let q5 = pauseQueue q3
  case enqueue "eve" "e.txt" "eee" q5 of
    Left err => putStrLn $ "  Paused queue rejects: " ++ show err
    Right _  => putStrLn $ "  Unexpected success"

-- ============================================================================
-- Demo: protocol state machine
-- ============================================================================

||| Demonstrate the LPD protocol state machine transitions.
covering
demoProtocol : IO ()
demoProtocol = do
  putStrLn "\n--- LPD Protocol State Machine Demo ---\n"

  -- Start in Idle
  let st0 = Idle
  putStrLn $ "Initial state: " ++ show st0

  -- Receive a ReceiveJob command
  let result1 = handleCommand st0 (ReceiveJob "lp0")
  putStrLn $ "ReceiveJob(lp0): " ++ show result1.newState
             ++ " actions=" ++ show (length result1.actions)

  -- Sub-command: receive control file
  let result2 = handleSubCommand result1.newState (ReceiveControlFile 42 "cfA001host")
  putStrLn $ "ReceiveControlFile: " ++ show result2.newState

  -- Complete control file reception
  let controlContent = "Halice\nJreport.pdf\nldfA001host\nUdfA001host"
  case completeControlFile result2.newState controlContent of
    Left err => putStrLn $ "  Control file error: " ++ show err
    Right result3 => do
      putStrLn $ "Control file complete: " ++ show result3.newState
      putStrLn $ "  Actions: " ++ show (length result3.actions)

  -- Sub-command: receive data file
  let result4 = handleSubCommand Idle (ReceiveDataFile 100 "dfA001host")
  putStrLn $ "ReceiveDataFile: " ++ show result4.newState

  -- Queue state query
  let result5 = handleCommand Idle (ShortQueueState "lp0" [])
  putStrLn $ "ShortQueueState: " ++ show result5.newState
             ++ " actions=" ++ show (length result5.actions)

-- ============================================================================
-- Demo: spool directory management
-- ============================================================================

||| Demonstrate spool file naming and management.
covering
demoSpool : IO ()
demoSpool = do
  putStrLn "\n--- LPD Spool Directory Demo ---\n"

  let sd0 = newSpoolDir "/var/spool/lpd/lp0" "lp0"
  putStrLn $ "Spool: " ++ show sd0

  -- Add files for two jobs
  let jid1 = MkJobId 1
  let jid2 = MkJobId 42
  let sd1 = addJobFiles jid1 "workstation" 128 4096 sd0
  let sd2 = addJobFiles jid2 "laptop" 96 8192 sd1

  putStrLn $ "After adding jobs: " ++ show sd2
  putStrLn $ "  Total spool size: " ++ show (totalSpoolSize sd2) ++ " bytes"
  putStrLn $ "  Job count: " ++ show (jobCount sd2)

  -- Show filenames
  putStrLn "\n  Control file for job 001: "
    ++ controlFilename jid1 "workstation"
  putStrLn $ "  Data file for job 001: "
    ++ dataFilename jid1 "workstation"
  putStrLn $ "  Control file for job 042: "
    ++ controlFilename jid2 "laptop"

  -- List all paths
  putStrLn "\n  All spool files:"
  traverse_ (\p => putStrLn $ "    " ++ p) (allPaths sd2)

  -- Cleanup job 1
  let sd3 = removeJobFiles jid1 sd2
  putStrLn $ "\n  After removing job 001: " ++ show sd3
  putStrLn $ "  Remaining files: " ++ show (length sd3.files)

  -- Check for orphans
  putStrLn $ "  Orphaned data files: " ++ show (length (orphanedDataFiles sd3))

-- ============================================================================
-- Main
-- ============================================================================

covering
main : IO ()
main = do
  putStrLn "proven-lpd v0.1.0 -- LPD that cannot crash"
  putStrLn "Powered by proven (Idris 2 formal verification)"
  putStrLn ""
  putStrLn $ "LPD port: " ++ show (cast {to=Nat} lpdPort)
  putStrLn $ "Max job size: " ++ show maxJobSize ++ " bytes"
  putStrLn $ "Max queue depth: " ++ show maxQueueDepth
  putStrLn $ "Default spool: " ++ defaultSpoolPath

  demoJobLifecycle
  demoJobValidation
  demoQueueManagement
  demoProtocol
  demoSpool

  putStrLn "\n--- All operations proven safe at compile time ---"
  putStrLn "Build with: idris2 --build proven-lpd.ipkg"
  putStrLn "Run with:   ./build/exec/proven-lpd"
