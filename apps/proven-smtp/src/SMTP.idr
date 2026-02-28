-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
--
-- proven-smtp: An SMTP server implementation that cannot crash.
--
-- Architecture:
--   - Command: 9 SMTP commands as a closed sum type (HELO, EHLO, etc.)
--   - Reply: 17 reply codes with 4 categories (Positive, Intermediate, etc.)
--   - Session: State machine (Connected -> Greeted -> MailFrom -> RcptTo -> Data -> Quit)
--   - Message: Email message with validated address parsing
--   - Auth: AUTH mechanisms (PLAIN, LOGIN, CRAM-MD5) with exchange state machine
--
-- This module defines core SMTP constants and re-exports all submodules.

module SMTP

import public SMTP.Command
import public SMTP.Reply
import public SMTP.Session
import public SMTP.Message
import public SMTP.Auth

||| Standard SMTP port (RFC 5321).
public export
smtpPort : Bits16
smtpPort = 25

||| Submission port for authenticated mail (RFC 6409).
public export
submissionPort : Bits16
submissionPort = 587

||| Maximum line length in SMTP (RFC 5321 Section 4.5.3.1.6).
||| 998 characters plus CRLF = 1000 total.
public export
maxLineLength : Nat
maxLineLength = 998

||| Maximum message size in bytes (10 MiB default, per SIZE extension).
public export
maxMessageSize : Nat
maxMessageSize = 10485760

||| SMTPS (implicit TLS) port.
public export
smtpsPort : Bits16
smtpsPort = 465

||| Server identification string for proven-smtp.
public export
serverIdent : String
serverIdent = "proven-smtp/0.1.0"
