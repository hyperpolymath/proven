-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

||| Proven.System - Convenience re-export module for system interaction safety
|||
||| Groups all operating system and process interaction Safe* modules into a
||| single import for applications that manage files, paths, environment
||| variables, shell commands, child processes, POSIX signals, terminal I/O,
||| pipe communication, command-line argument parsing, container orchestration,
||| git repository operations, and structured logging.
|||
||| Usage:
|||   import Proven.System
|||
||| This single import provides access to all system interaction safety types,
||| constructors, and validated operations without needing 13 separate imports.
module Proven.System

import public Proven.SafeFile
import public Proven.SafePath
import public Proven.SafeEnv
import public Proven.SafeShell
import public Proven.SafeProcess
import public Proven.SafeSignal
import public Proven.SafeTerminal
import public Proven.SafePipe
import public Proven.SafeArgs
import public Proven.SafeCommand
import public Proven.SafeDocker
import public Proven.SafeGit
import public Proven.SafeLog
