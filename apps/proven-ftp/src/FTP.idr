-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
--
-- proven-ftp: An FTP server implementation that cannot crash.
--
-- Architecture:
--   - Command: FTP commands as a closed sum type (USER, PASS, LIST, RETR, etc.)
--   - Reply: 3-digit reply codes with 5 categories (RFC 959 Section 4.2)
--   - Session: State machine (Connected -> UserOk -> Authenticated -> Quit)
--   - Transfer: Transfer modes (ASCII/Binary) and data connection types
--   - Path: Path validation and traversal prevention
--
-- This module defines core FTP constants and re-exports all submodules.

module FTP

import public FTP.Command
import public FTP.Reply
import public FTP.Session
import public FTP.Transfer
import public FTP.Path

%default total

||| Standard FTP control port (RFC 959).
public export
ftpPort : Bits16
ftpPort = 21

||| Standard FTP data port (RFC 959).
public export
ftpDataPort : Bits16
ftpDataPort = 20

||| FTPS (implicit TLS) control port.
public export
ftpsPort : Bits16
ftpsPort = 990

||| Maximum command line length (RFC 959 Section 4.1.3).
||| Commands must be 4 characters, arguments up to 255.
public export
maxCommandLength : Nat
maxCommandLength = 512

||| Maximum path length we accept.
public export
maxPathLength : Nat
maxPathLength = 4096

||| Server identification string.
public export
serverIdent : String
serverIdent = "proven-ftp/0.1.0"
