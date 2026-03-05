-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
--
-- proven-ftp: Main entry point
--
-- An FTP server implementation that cannot crash on malformed commands.
-- Uses proven's type-safe approach to ensure all command parsing, path
-- validation, and session state transitions are total.
--
-- Usage:
--   idris2 --build proven-ftp.ipkg
--   ./build/exec/proven-ftp

module Main

import FTP
import FTP.Command
import FTP.Reply
import FTP.Session
import FTP.Transfer
import FTP.Path

%default total

-- ============================================================================
-- Demo: command parsing
-- ============================================================================

||| Demonstrate FTP command parsing with valid and invalid inputs.
covering
demoParsing : IO ()
demoParsing = do
  putStrLn "\n--- FTP Command Parsing Demo ---\n"

  let inputs = [ "USER alice"
               , "PASS secret123"
               , "LIST"
               , "LIST /pub"
               , "RETR /pub/readme.txt"
               , "STOR /upload/data.bin"
               , "CWD /pub"
               , "PWD"
               , "TYPE I"
               , "QUIT"
               , ""
               , "XYZZY"
               , "RETR"
               ]
  traverse_ (\s => case parseCommand s of
    Right cmd => putStrLn $ "  OK:   \"" ++ s ++ "\" -> " ++ show cmd
                            ++ " (auth required: " ++ show (requiresAuth cmd)
                            ++ ", uses data: " ++ show (usesDataConnection cmd) ++ ")"
    Left err  => putStrLn $ "  FAIL: \"" ++ s ++ "\" -> " ++ show err
  ) inputs

-- ============================================================================
-- Demo: path validation
-- ============================================================================

||| Demonstrate path validation and traversal prevention.
covering
demoPathValidation : IO ()
demoPathValidation = do
  putStrLn "\n--- FTP Path Validation Demo ---\n"

  let paths = [ "/pub/files/readme.txt"
              , "/home/alice/../bob/secrets"
              , "../../etc/passwd"
              , "/normal/path"
              , "relative/path"
              , ""
              ]
  traverse_ (\s => case validatePath s of
    Right sp => putStrLn $ "  SAFE:   \"" ++ s ++ "\" -> " ++ show sp
                           ++ " (absolute: " ++ show sp.isAbsolute ++ ")"
    Left err => putStrLn $ "  REJECT: \"" ++ s ++ "\" -> " ++ show err
  ) paths

-- ============================================================================
-- Demo: session state machine
-- ============================================================================

||| Demonstrate a full FTP session lifecycle.
covering
demoSession : IO ()
demoSession = do
  putStrLn "\n--- FTP Session State Machine Demo ---\n"

  let s0 = newSession
  putStrLn $ "Initial state: " ++ show s0.state

  -- Attempt LIST before login (should fail)
  let (s0b, r0b) = processCommand s0 (LIST Nothing)
  putStrLn $ "\nLIST (before login): " ++ show r0b
             ++ " [state: " ++ show s0b.state ++ "]"

  -- Login sequence
  let (s1, r1) = processCommand s0 (USER "alice")
  putStrLn $ "\nUSER alice:  " ++ show r1
             ++ " [state: " ++ show s1.state ++ "]"

  let (s2, r2) = processCommand s1 (PASS "secret")
  putStrLn $ "PASS ****:   " ++ show r2
             ++ " [state: " ++ show s2.state ++ "]"

  -- File operations
  let (s3, r3) = processCommand s2 SYST
  putStrLn $ "\nSYST:        " ++ show r3

  let (s4, r4) = processCommand s3 PWD
  putStrLn $ "PWD:         " ++ show r4

  let (s5, r5) = processCommand s4 (CWD "/pub")
  putStrLn $ "CWD /pub:    " ++ show r5

  let (s6, r6) = processCommand s5 (TYPE "I")
  putStrLn $ "TYPE I:      " ++ show r6
             ++ " [type: " ++ show s6.transferType ++ "]"

  let (s7, r7) = processCommand s6 PASV
  putStrLn $ "PASV:        " ++ show r7

  let (s8, r8) = processCommand s7 (LIST Nothing)
  putStrLn $ "LIST:        " ++ show r8

  let (s9, r9) = processCommand s8 (RETR "readme.txt")
  putStrLn $ "RETR:        " ++ show r9

  -- Rename sequence
  let (s10, r10) = processCommand s9 (RNFR "old.txt")
  putStrLn $ "\nRNFR old.txt: " ++ show r10
             ++ " [state: " ++ show s10.state ++ "]"

  let (s11, r11) = processCommand s10 (RNTO "new.txt")
  putStrLn $ "RNTO new.txt: " ++ show r11
             ++ " [state: " ++ show s11.state ++ "]"

  -- Logout
  let (s12, r12) = processCommand s11 QUIT
  putStrLn $ "\nQUIT:        " ++ show r12
             ++ " [state: " ++ show s12.state ++ "]"

-- ============================================================================
-- Demo: transfer types
-- ============================================================================

||| Demonstrate transfer type parsing and properties.
covering
demoTransfer : IO ()
demoTransfer = do
  putStrLn "\n--- FTP Transfer Types Demo ---\n"

  putStrLn "Type codes:"
  putStrLn $ "  ASCII  -> TYPE " ++ typeCode ASCII
  putStrLn $ "  Binary -> TYPE " ++ typeCode Binary

  putStrLn "\nParse results:"
  let codes = ["A", "I", "a", "i", "X", ""]
  traverse_ (\c =>
    putStrLn $ "  \"" ++ c ++ "\" -> " ++ show (parseType c)
  ) codes

  putStrLn "\nTransfer states:"
  let t0 = Idle
  putStrLn $ "  " ++ show t0 ++ " (can start: " ++ show (canStartTransfer t0) ++ ")"
  let t1 = InProgress 0
  let t2 = addBytes t1 1024
  let t3 = addBytes t2 2048
  putStrLn $ "  " ++ show t3
  let t4 = completeTransfer t3
  putStrLn $ "  " ++ show t4 ++ " (can start: " ++ show (canStartTransfer t4) ++ ")"

-- ============================================================================
-- Demo: reply codes
-- ============================================================================

||| Demonstrate reply code classification.
covering
demoReplyCodes : IO ()
demoReplyCodes = do
  putStrLn "\n--- FTP Reply Code Classification Demo ---\n"

  let codes = [ ServiceReady, NeedPassword, UserLoggedIn, FileActionOk
              , TransferComplete, EnteringPassive, BadSequence, NotLoggedIn
              , SyntaxError, ServiceUnavailable, FileUnavailable
              ]
  putStrLn "Code  Category          Positive"
  putStrLn "----  --------          --------"
  traverse_ (\c =>
    putStrLn $ "  " ++ padRight 6 ' ' (show (replyNumber c))
               ++ padRight 18 ' ' (show (replyCategory c))
               ++ show (isPositive c)
  ) codes
  where
    padRight : Nat -> Char -> String -> String
    padRight n c s = s ++ pack (replicate (minus n (length s)) c)

-- ============================================================================
-- Main
-- ============================================================================

covering
main : IO ()
main = do
  putStrLn "proven-ftp v0.1.0 -- FTP that cannot crash"
  putStrLn "Powered by proven (Idris 2 formal verification)"
  putStrLn ""
  putStrLn $ "FTP port: " ++ show (cast {to=Nat} ftpPort)
  putStrLn $ "FTP data port: " ++ show (cast {to=Nat} ftpDataPort)
  putStrLn $ "FTPS port: " ++ show (cast {to=Nat} ftpsPort)
  putStrLn $ "Max command length: " ++ show maxCommandLength ++ " chars"
  putStrLn $ "Max path length: " ++ show maxPathLength ++ " chars"

  demoParsing
  demoPathValidation
  demoSession
  demoTransfer
  demoReplyCodes

  putStrLn "\n--- All parsing proven safe at compile time ---"
  putStrLn "Build with: idris2 --build proven-ftp.ipkg"
  putStrLn "Run with:   ./build/exec/proven-ftp"
