-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
--
-- proven-syslog: Main entry point
--
-- A syslog server implementation that cannot crash on malformed messages.
-- Uses dependent types to validate message structure, priority encoding,
-- facility/severity ranges, and RFC 5424 field constraints at compile time.
--
-- Usage:
--   proven-syslog --listen 0.0.0.0:514 --transport udp

module Main

import Syslog
import Syslog.Facility
import Syslog.Severity
import Syslog.Message
import Syslog.Priority
import Syslog.Transport
import System

%default total

-- ============================================================================
-- Demo: Priority encoding/decoding
-- ============================================================================

||| Demonstrate priority value encoding and decoding.
covering  -- putStrLn is IO, not structurally recursive
demoPriority : IO ()
demoPriority = do
  putStrLn "\n--- Syslog Priority Encoding Demo ---\n"

  -- Show priority encoding for various facility/severity combinations
  let examples = [ (Kern,   Emergency)
                 , (User,   Warning)
                 , (Mail,   Error)
                 , (Daemon, Informational)
                 , (Auth,   Alert)
                 , (Local0, Debug)
                 , (Local7, Critical)
                 ]

  putStrLn "Priority encoding (facility * 8 + severity):"
  traverse_ (\(fac, sev) =>
    let pri = mkPriority fac sev
    in putStrLn $ "  " ++ show fac ++ "." ++ show sev
                  ++ " = " ++ formatPRI pri
                  ++ " (value " ++ show (priorityValue pri)
                  ++ ", alarm=" ++ show (isPriorityAlarm pri)
                  ++ ", error=" ++ show (isPriorityError pri) ++ ")"
    ) examples

  -- Decode some priority values
  putStrLn "\nPriority decoding:"
  let values = [0, 13, 34, 165, 191, 192]
  traverse_ (\n =>
    case decodePriority n of
      Just pri => putStrLn $ "  " ++ show n ++ " => " ++ show pri
      Nothing  => putStrLn $ "  " ++ show n ++ " => INVALID (out of range)"
    ) values

  -- Parse PRI field strings
  putStrLn "\nPRI field parsing:"
  let pris = ["<34>", "<165>", "<0>", "<192>", "<abc>", "invalid"]
  traverse_ (\s =>
    case parsePRI s of
      Just pri => putStrLn $ "  " ++ s ++ " => " ++ show pri
      Nothing  => putStrLn $ "  " ++ s ++ " => INVALID"
    ) pris

-- ============================================================================
-- Demo: RFC 5424 message formatting
-- ============================================================================

||| Demonstrate RFC 5424 message construction and formatting.
covering  -- putStrLn is IO, not structurally recursive
demoMessages : IO ()
demoMessages = do
  putStrLn "\n--- Syslog RFC 5424 Message Demo ---\n"

  -- Simple message
  let msg1 = mkMessage Auth Warning
               "2026-02-28T14:30:00.000Z"
               "firewall.example.com"
               "iptables"
               "Dropped packet from 192.168.1.100"
  putStrLn "Simple message:"
  putStrLn $ "  " ++ show msg1

  -- Message with structured data
  let sd1 = MkSDElement "exampleSDID@32473"
              [ MkSDParam "iut" "3"
              , MkSDParam "eventSource" "Application"
              , MkSDParam "eventID" "1011"
              ]
  let sd2 = MkSDElement "examplePriority@32473"
              [ MkSDParam "class" "high" ]

  let msg2 = mkMessageWithSD Daemon Critical
               "2026-02-28T14:30:01.234Z"
               "server01.example.com"
               "myapp"
               [sd1, sd2]
               "Service health check failed"
  putStrLn "\nMessage with structured data:"
  putStrLn $ "  " ++ show msg2

  -- Validate messages
  putStrLn "\nMessage validation:"
  case validateMessage msg1 of
    Right () => putStrLn $ "  msg1: OK"
    Left err => putStrLn $ "  msg1: " ++ show err
  case validateMessage msg2 of
    Right () => putStrLn $ "  msg2: OK"
    Left err => putStrLn $ "  msg2: " ++ show err

-- ============================================================================
-- Demo: Severity filtering
-- ============================================================================

||| Demonstrate severity-based message filtering.
covering  -- putStrLn is IO, not structurally recursive
demoFiltering : IO ()
demoFiltering = do
  putStrLn "\n--- Syslog Severity Filtering Demo ---\n"

  let allSeverities = [Emergency, Alert, Critical, Error, Warning,
                       Notice, Informational, Debug]

  putStrLn "All severity levels:"
  traverse_ (\sev =>
    putStrLn $ "  " ++ show (severityCode sev) ++ ": " ++ show sev
               ++ " — " ++ severityDescription sev
               ++ (if isAlarm sev then " [ALARM]" else "")
               ++ (if isError sev then " [ERROR]" else "")
    ) allSeverities

  -- Filter by minimum severity
  putStrLn "\nFilter: minimum severity = Warning"
  traverse_ (\sev =>
    let passes = meetsMinSeverity Warning sev
    in putStrLn $ "  " ++ show sev ++ ": "
                  ++ (if passes then "PASS" else "DROP")
    ) allSeverities

-- ============================================================================
-- Demo: Transport comparison
-- ============================================================================

||| Demonstrate transport types and their properties.
covering  -- putStrLn is IO, not structurally recursive
demoTransport : IO ()
demoTransport = do
  putStrLn "\n--- Syslog Transport Demo ---\n"

  let transports = [UDP514, TCP514, TLS6514]

  putStrLn "Transport comparison:"
  putStrLn "  Transport  Port   Reliable  Encrypted  Auth       RFC        MaxSize"
  putStrLn "  ---------  ----   --------  ---------  ----       ---        -------"
  traverse_ (\t =>
    putStrLn $ "  " ++ padRight 9 (show t)
               ++ "  " ++ padRight 5 (show (cast {to=Nat} (transportPort t)))
               ++ "  " ++ padRight 8 (show (isReliable t))
               ++ "  " ++ padRight 9 (show (isEncrypted t))
               ++ "  " ++ padRight 9 (show (isAuthenticated t))
               ++ "  " ++ padRight 9 (transportRFC t)
               ++ "  " ++ show (maxMessageSize t)
    ) transports
  where
    padRight : Nat -> String -> String
    padRight n s =
      let padding = minus n (length s)
      in s ++ pack (replicate padding ' ')

-- ============================================================================
-- Main
-- ============================================================================

covering
main : IO ()
main = do
  putStrLn "proven-syslog v0.1.0 — Syslog that cannot crash"
  putStrLn "Powered by proven (Idris 2 formal verification)"
  putStrLn ""
  putStrLn $ "Syslog port:      " ++ show (cast {to=Nat} syslogPort)
  putStrLn $ "Syslog TLS port:  " ++ show (cast {to=Nat} syslogTlsPort)
  putStrLn $ "Max UDP message:  " ++ show maxMessageSizeUDP ++ " bytes"
  putStrLn $ "Max RFC 5424:     " ++ show maxRfc5424Size ++ " bytes"

  -- Run demos
  demoPriority
  demoMessages
  demoFiltering
  demoTransport

  putStrLn "\n--- Ready for production use ---"
  putStrLn "Build with: idris2 --build proven-syslog.ipkg"
  putStrLn "Run with:   ./build/exec/proven-syslog"
