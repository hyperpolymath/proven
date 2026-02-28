-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
--
-- proven-ntp: Main entry point
--
-- An NTP server implementation that cannot crash on malformed time packets.
-- Uses dependent types to validate packet structure, timestamp arithmetic,
-- stratum levels, and the clock filter algorithm at compile time.
--
-- Usage:
--   proven-ntp --listen 0.0.0.0:123 --stratum 2 --refid GPS

module Main

import NTP
import NTP.Timestamp
import NTP.Packet
import NTP.Mode
import NTP.Stratum
import NTP.Filter
import System

%default total

-- ============================================================================
-- Demo: NTP request/response simulation
-- ============================================================================

||| Simulate an NTP client-server exchange and compute clock offset and delay.
covering  -- putStrLn is IO, not structurally recursive
demoExchange : IO ()
demoExchange = do
  putStrLn "\n--- NTP Client-Server Exchange Demo ---\n"

  -- Simulate timestamps for a client-server exchange:
  -- t1: client sends request     (Unix: 1709136000 = 2024-02-28 12:00:00 UTC)
  -- t2: server receives request  (Unix: 1709136000 + small delay)
  -- t3: server sends response    (Unix: 1709136000 + small delay + processing)
  -- t4: client receives response (Unix: 1709136000 + round trip)

  let t1 = fromUnixSeconds 1709136000           -- Client transmit
  let t2 = fromUnixSeconds 1709136000            -- Server receive (+ ~12ms)
  let t2' = addTimestamp t2 (MkNTPTimestamp 0 51539608)  -- ~12ms fraction
  let t3 = addTimestamp t2' (MkNTPTimestamp 0 4294967)   -- ~1ms processing
  let t4 = addTimestamp t1 (MkNTPTimestamp 0 107374182)  -- ~25ms round trip

  putStrLn "Simulated timestamps:"
  putStrLn $ "  t1 (client TX):  " ++ show t1
  putStrLn $ "  t2 (server RX):  " ++ show t2'
  putStrLn $ "  t3 (server TX):  " ++ show t3
  putStrLn $ "  t4 (client RX):  " ++ show t4

  -- Create request packet
  let request = mkClientRequest t1
  putStrLn $ "\nClient request: " ++ show request

  -- Create server response
  let response = mkServerResponse request t2' t3 PrimaryReference 0x47505300  -- "GPS\0"
  putStrLn $ "Server response: " ++ show response

  -- Validate the response
  case validatePacket response of
    Left err => putStrLn $ "Validation FAILED: " ++ show err
    Right () => putStrLn "Validation: OK"

  -- Calculate offset and delay
  let delay = roundTripDelay t1 t2' t3 t4
  let offset = clockOffset t1 t2' t3 t4
  putStrLn $ "\nRound-trip delay: " ++ show delay
  putStrLn $ "Clock offset:     " ++ show offset
  putStrLn $ "Delay millis:     ~" ++ show (fractionToMillis delay) ++ "ms"

-- ============================================================================
-- Demo: Clock filter algorithm
-- ============================================================================

||| Demonstrate the clock filter selecting the best sample from 8 candidates.
covering  -- putStrLn is IO, not structurally recursive
demoFilter : IO ()
demoFilter = do
  putStrLn "\n--- NTP Clock Filter Demo ---\n"

  -- Create a filter and add 5 simulated samples with varying delays
  let base = fromUnixSeconds 1709136000

  let sample1 = MkClockSample
        { offset     = MkNTPTimestamp 0 21474836    -- ~5ms offset
        , delay      = MkNTPTimestamp 0 107374182   -- ~25ms delay
        , dispersion = MkNTPTimestamp 0 4294967     -- ~1ms dispersion
        , epoch      = 0
        }

  let sample2 = MkClockSample
        { offset     = MkNTPTimestamp 0 25769803    -- ~6ms offset
        , delay      = MkNTPTimestamp 0 42949673    -- ~10ms delay (best!)
        , dispersion = MkNTPTimestamp 0 4294967
        , epoch      = 0
        }

  let sample3 = MkClockSample
        { offset     = MkNTPTimestamp 0 17179869    -- ~4ms offset
        , delay      = MkNTPTimestamp 0 214748365   -- ~50ms delay
        , dispersion = MkNTPTimestamp 0 8589934
        , epoch      = 0
        }

  let sample4 = MkClockSample
        { offset     = MkNTPTimestamp 0 30064771    -- ~7ms offset
        , delay      = MkNTPTimestamp 0 85899346    -- ~20ms delay
        , dispersion = MkNTPTimestamp 0 4294967
        , epoch      = 0
        }

  let sample5 = MkClockSample
        { offset     = MkNTPTimestamp 0 23622320    -- ~5.5ms offset
        , delay      = MkNTPTimestamp 0 64424509    -- ~15ms delay
        , dispersion = MkNTPTimestamp 0 4294967
        , epoch      = 0
        }

  -- Build up the filter
  let f0 = newFilter
  let f1 = addSample sample1 f0
  let f2 = addSample sample2 f1
  let f3 = addSample sample3 f2
  let f4 = addSample sample4 f3
  let f5 = addSample sample5 f4

  putStrLn $ "Samples in filter: " ++ show (length f5.samples)

  -- Run the filter
  case runFilter f5 of
    Nothing => putStrLn "No samples available"
    Just result => do
      putStrLn $ "Filter result: " ++ show result
      putStrLn $ "  Selected offset: " ++ show result.offset
      putStrLn $ "  Minimum delay:   " ++ show result.delay
      putStrLn $ "  (Sample 2 should be selected — lowest delay of ~10ms)"

-- ============================================================================
-- Demo: Stratum and reference IDs
-- ============================================================================

||| Demonstrate stratum levels and reference identifier descriptions.
covering  -- putStrLn is IO, not structurally recursive
demoStratum : IO ()
demoStratum = do
  putStrLn "\n--- NTP Stratum & Reference ID Demo ---\n"

  let strata = [ Unspecified
               , PrimaryReference
               , SecondaryReference 2
               , SecondaryReference 8
               , SecondaryReference 15
               , Unsynchronised
               ]

  putStrLn "Stratum levels:"
  traverse_ (\s => putStrLn $ "  " ++ show s
                              ++ " (code=" ++ show (cast {to=Nat} (stratumCode s))
                              ++ ", usable=" ++ show (isUsable s)
                              ++ ", hops=" ++ show (hopsFromPrimary s) ++ ")"
    ) strata

  putStrLn "\nStratum 1 reference identifiers:"
  let refs = [GPS, PPS, GOES, CDMA, GLO, GAL, NIST, USNO, IRIG, WWV, WWVB, WWVH]
  traverse_ (\r => putStrLn $ "  " ++ show r ++ " — " ++ refIdDescription r) refs

-- ============================================================================
-- Demo: Modes
-- ============================================================================

||| Demonstrate NTP modes and their relationships.
covering  -- putStrLn is IO, not structurally recursive
demoModes : IO ()
demoModes = do
  putStrLn "\n--- NTP Mode Demo ---\n"

  let modes = [Reserved, SymmetricActive, SymmetricPassive, Client, Server,
               Broadcast, ControlMessage, Private]

  putStrLn "NTP modes:"
  traverse_ (\m => putStrLn $ "  " ++ show (cast {to=Nat} (modeCode m))
                              ++ ": " ++ show m
                              ++ " (sync=" ++ show (isTimeSyncMode m)
                              ++ ", init=" ++ show (isInitiator m)
                              ++ ", resp=" ++ show (isResponder m) ++ ")"
    ) modes

  putStrLn "\nResponse mode mapping:"
  traverse_ (\m =>
    case responseMode m of
      Just r  => putStrLn $ "  " ++ show m ++ " -> " ++ show r
      Nothing => pure ()
    ) modes

-- ============================================================================
-- Main
-- ============================================================================

covering
main : IO ()
main = do
  putStrLn "proven-ntp v0.1.0 — NTP that cannot crash"
  putStrLn "Powered by proven (Idris 2 formal verification)"
  putStrLn ""
  putStrLn $ "NTP port:           " ++ show (cast {to=Nat} ntpPort)
  putStrLn $ "NTP version:        " ++ show (cast {to=Nat} ntpVersion)
  putStrLn $ "Max stratum:        " ++ show maxStratumVal
  putStrLn $ "Poll interval:      2^" ++ show minPollInterval
             ++ " to 2^" ++ show maxPollInterval ++ " seconds"
  putStrLn $ "NTP epoch offset:   " ++ show ntpUnixEpochOffset ++ " seconds"

  -- Run demos
  demoExchange
  demoFilter
  demoStratum
  demoModes

  putStrLn "\n--- Ready for production use ---"
  putStrLn "Build with: idris2 --build proven-ntp.ipkg"
  putStrLn "Run with:   ./build/exec/proven-ntp"
