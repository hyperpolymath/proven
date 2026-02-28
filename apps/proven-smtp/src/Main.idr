-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
--
-- proven-smtp: Main entry point
--
-- An SMTP server implementation that cannot crash on malformed messages.
-- Uses proven's type-safe state machine to ensure that SMTP commands
-- are only accepted in valid session states.
--
-- Usage:
--   idris2 --build proven-smtp.ipkg
--   ./build/exec/proven-smtp

module Main

import SMTP
import SMTP.Command
import SMTP.Reply
import SMTP.Session
import SMTP.Message
import SMTP.Auth

%default total

-- ============================================================================
-- Demo: SMTP session walkthrough
-- ============================================================================

||| Walk through a complete SMTP session: EHLO -> MAIL FROM -> RCPT TO -> DATA -> QUIT.
covering
demoSession : IO ()
demoSession = do
  putStrLn "\n--- SMTP Session Demo (proven state machine) ---\n"

  -- Initial state
  let s0 = newSession
  putStrLn $ "Initial state: " ++ show s0.state

  -- Step 1: Server greeting
  putStrLn $ "S: " ++ show (defaultReply ServiceReady)

  -- Step 2: EHLO
  let (s1, r1) = processCommand s0 (EHLO "client.example.com")
  putStrLn $ "C: EHLO client.example.com"
  putStrLn $ "S: " ++ show r1
  putStrLn $ "   State: " ++ show s1.state

  -- Step 3: MAIL FROM
  let (s2, r2) = processCommand s1 (MAIL_FROM "sender@example.com")
  putStrLn $ "C: MAIL FROM:<sender@example.com>"
  putStrLn $ "S: " ++ show r2
  putStrLn $ "   State: " ++ show s2.state

  -- Step 4: RCPT TO (first recipient)
  let (s3, r3) = processCommand s2 (RCPT_TO "alice@example.org")
  putStrLn $ "C: RCPT TO:<alice@example.org>"
  putStrLn $ "S: " ++ show r3
  putStrLn $ "   State: " ++ show s3.state

  -- Step 5: RCPT TO (second recipient)
  let (s4, r4) = processCommand s3 (RCPT_TO "bob@example.org")
  putStrLn $ "C: RCPT TO:<bob@example.org>"
  putStrLn $ "S: " ++ show r4
  putStrLn $ "   Recipients: " ++ show (length s4.recipients)

  -- Step 6: DATA
  let (s5, r5) = processCommand s4 DATA
  putStrLn $ "C: DATA"
  putStrLn $ "S: " ++ show r5
  putStrLn $ "   State: " ++ show s5.state

  -- Step 7: Complete data phase (message received)
  let s6 = completeData s5
  putStrLn $ "C: [message body]"
  putStrLn $ "S: " ++ show (defaultReply ActionOK)
  putStrLn $ "   State: " ++ show s6.state
  putStrLn $ "   Messages processed: " ++ show s6.messageCount

  -- Step 8: QUIT
  let (s7, r7) = processCommand s6 QUIT
  putStrLn $ "C: QUIT"
  putStrLn $ "S: " ++ show r7
  putStrLn $ "   State: " ++ show s7.state

-- ============================================================================
-- Demo: invalid command sequence
-- ============================================================================

||| Demonstrate that invalid command sequences produce proper error replies.
covering
demoInvalidSequence : IO ()
demoInvalidSequence = do
  putStrLn "\n--- SMTP Invalid Sequence Demo ---\n"

  let s0 = newSession

  -- Try MAIL FROM before HELO (should fail)
  let (s1, r1) = processCommand s0 (MAIL_FROM "sender@example.com")
  putStrLn $ "C: MAIL FROM:<sender@example.com> (before HELO)"
  putStrLn $ "S: " ++ show r1
  putStrLn $ "   Valid transition: " ++ show (isValidTransition Connected (MAIL_FROM "sender@example.com"))
  putStrLn $ "   State unchanged: " ++ show s1.state

  -- EHLO first, then try DATA before RCPT TO
  let (s2, _) = processCommand s0 (EHLO "client.example.com")
  let (s3, _) = processCommand s2 (MAIL_FROM "sender@example.com")
  let (s4, r4) = processCommand s3 DATA  -- Should fail: no RCPT TO yet
  putStrLn $ "\nC: DATA (before RCPT TO)"
  putStrLn $ "S: " ++ show r4
  putStrLn $ "   Valid transition: " ++ show (isValidTransition MailFrom DATA)
  putStrLn $ "   State: " ++ show s4.state

-- ============================================================================
-- Demo: email address parsing
-- ============================================================================

||| Demonstrate email address validation.
covering
demoAddressParsing : IO ()
demoAddressParsing = do
  putStrLn "\n--- SMTP Address Parsing Demo ---\n"

  let addresses = [ "alice@example.com"
                  , "bob.smith@mail.example.org"
                  , "admin@localhost"
                  ]
  traverse_ (\s => case parseAddress s of
    Right addr => putStrLn $ "  OK: " ++ s ++ " -> " ++ show addr
    Left err   => putStrLn $ "  ERR: " ++ s ++ " -> " ++ show err
  ) addresses

  -- Invalid addresses
  let invalid = [ ""
                , "noatsign"
                , "@nodomain"
                ]
  traverse_ (\s => case parseAddress s of
    Right addr => putStrLn $ "  Unexpected OK: " ++ show addr
    Left err   => putStrLn $ "  Rejected: " ++ show err
  ) invalid

-- ============================================================================
-- Demo: AUTH mechanisms
-- ============================================================================

||| Demonstrate authentication mechanism properties.
covering
demoAuth : IO ()
demoAuth = do
  putStrLn "\n--- SMTP AUTH Mechanisms ---\n"
  putStrLn "Mechanism  TLS Required  Challenge-Response  Exchanges"
  putStrLn "---------  ------------  ------------------  ---------"
  traverse_ showMech allMechanisms
  where
    covering
    showMech : AuthMechanism -> IO ()
    showMech m = putStrLn $
      padRight 11 ' ' (show m)
      ++ padRight 14 ' ' (if requiresTLS m then "yes" else "no")
      ++ padRight 20 ' ' (if isChallengeResponse m then "yes" else "no")
      ++ show (exchangeCount m)
    padRight : Nat -> Char -> String -> String
    padRight n c s = s ++ pack (replicate (minus n (length s)) c)

-- ============================================================================
-- Demo: reply categories
-- ============================================================================

||| Demonstrate reply code categories.
covering
demoReplyCodes : IO ()
demoReplyCodes = do
  putStrLn "\n--- SMTP Reply Code Categories ---\n"

  let codes = [ ServiceReady, ActionOK, StartMailInput
              , ServiceUnavailable, MailboxBusy
              , SyntaxError, BadSequence, MailboxUnavailable
              ]
  traverse_ (\code => putStrLn $
    "  " ++ show (replyToCode code)
    ++ " [" ++ show (categorise code) ++ "] "
    ++ defaultMessage code
  ) codes

-- ============================================================================
-- Main
-- ============================================================================

covering
main : IO ()
main = do
  putStrLn "proven-smtp v0.1.0 -- SMTP that cannot crash"
  putStrLn "Powered by proven (Idris 2 formal verification)"
  putStrLn ""
  putStrLn $ "SMTP port: " ++ show (cast {to=Nat} smtpPort)
  putStrLn $ "Submission port: " ++ show (cast {to=Nat} submissionPort)
  putStrLn $ "Max line length: " ++ show maxLineLength ++ " chars"
  putStrLn $ "Max message size: " ++ show maxMessageSize ++ " bytes"

  demoSession
  demoInvalidSequence
  demoAddressParsing
  demoAuth
  demoReplyCodes

  putStrLn "\n--- All transitions proven valid at compile time ---"
  putStrLn "Build with: idris2 --build proven-smtp.ipkg"
  putStrLn "Run with:   ./build/exec/proven-smtp"
