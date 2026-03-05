-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

||| Top-level module for the proven-backup server.
||| Re-exports core types and provides server constants.
module Backup

import public Backup.Types

%default total

---------------------------------------------------------------------------
-- Server constants
---------------------------------------------------------------------------

||| Default TCP port for the backup server.
public export
backupPort : Nat
backupPort = 9876

||| Default number of days to retain backup archives.
public export
defaultRetentionDays : Nat
defaultRetentionDays = 30

||| Maximum number of concurrent backup jobs.
public export
maxConcurrentJobs : Nat
maxConcurrentJobs = 4

||| Human-readable server name for logging and identification.
public export
serverName : String
serverName = "proven-backup"
