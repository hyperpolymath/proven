-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

||| Core protocol types for the proven-backup server.
||| Defines closed sum types for backup operations, scheduling,
||| compression, encryption, state tracking, and retention policies.
module Backup.Types

%default total

---------------------------------------------------------------------------
-- Backup type: the kind of backup operation to perform
---------------------------------------------------------------------------

||| The kind of backup operation to perform.
public export
data BackupType : Type where
  ||| Complete backup of all data.
  Full         : BackupType
  ||| Backup only data changed since last backup of any type.
  Incremental  : BackupType
  ||| Backup only data changed since last full backup.
  Differential : BackupType
  ||| Point-in-time snapshot (copy-on-write).
  Snapshot     : BackupType
  ||| Exact mirror of source to destination.
  Mirror       : BackupType

export
Show BackupType where
  show Full         = "Full"
  show Incremental  = "Incremental"
  show Differential = "Differential"
  show Snapshot     = "Snapshot"
  show Mirror       = "Mirror"

---------------------------------------------------------------------------
-- Schedule frequency: how often a backup job runs
---------------------------------------------------------------------------

||| How often a scheduled backup job executes.
public export
data ScheduleFreq : Type where
  Hourly   : ScheduleFreq
  Daily    : ScheduleFreq
  Weekly   : ScheduleFreq
  Monthly  : ScheduleFreq
  OnDemand : ScheduleFreq

export
Show ScheduleFreq where
  show Hourly   = "Hourly"
  show Daily    = "Daily"
  show Weekly   = "Weekly"
  show Monthly  = "Monthly"
  show OnDemand = "OnDemand"

---------------------------------------------------------------------------
-- Compression algorithm
---------------------------------------------------------------------------

||| Compression algorithm applied to backup data.
public export
data CompressionAlg : Type where
  ||| No compression.
  None : CompressionAlg
  ||| GNU zip (RFC 1952).
  Gzip : CompressionAlg
  ||| Zstandard (RFC 8878).
  Zstd : CompressionAlg
  ||| LZ4 fast compression.
  LZ4  : CompressionAlg
  ||| XZ / LZMA2 high-ratio compression.
  XZ   : CompressionAlg

export
Show CompressionAlg where
  show None = "None"
  show Gzip = "Gzip"
  show Zstd = "Zstd"
  show LZ4  = "LZ4"
  show XZ   = "XZ"

---------------------------------------------------------------------------
-- Encryption algorithm
---------------------------------------------------------------------------

||| Encryption algorithm for backup data at rest.
public export
data EncryptionAlg : Type where
  ||| No encryption.
  NoEncryption     : EncryptionAlg
  ||| AES-256 in GCM mode (NIST SP 800-38D).
  AES256GCM        : EncryptionAlg
  ||| ChaCha20-Poly1305 (RFC 8439).
  ChaCha20Poly1305 : EncryptionAlg

export
Show EncryptionAlg where
  show NoEncryption     = "None"
  show AES256GCM        = "AES-256-GCM"
  show ChaCha20Poly1305 = "ChaCha20-Poly1305"

---------------------------------------------------------------------------
-- Backup state: lifecycle states of a backup job
---------------------------------------------------------------------------

||| Lifecycle state of a backup job.
public export
data BackupState : Type where
  Idle      : BackupState
  Running   : BackupState
  Verifying : BackupState
  Complete  : BackupState
  Failed    : BackupState
  Cancelled : BackupState

export
Show BackupState where
  show Idle      = "Idle"
  show Running   = "Running"
  show Verifying = "Verifying"
  show Complete  = "Complete"
  show Failed    = "Failed"
  show Cancelled = "Cancelled"

---------------------------------------------------------------------------
-- Retention policy: how long to keep backups
---------------------------------------------------------------------------

||| Policy governing how long backup archives are retained.
public export
data RetentionPolicy : Type where
  ||| Keep all backups indefinitely.
  KeepAll     : RetentionPolicy
  ||| Keep only the N most recent backups.
  KeepLast    : RetentionPolicy
  ||| Keep one backup per day for N days.
  KeepDaily   : RetentionPolicy
  ||| Keep one backup per week for N weeks.
  KeepWeekly  : RetentionPolicy
  ||| Keep one backup per month for N months.
  KeepMonthly : RetentionPolicy

export
Show RetentionPolicy where
  show KeepAll     = "KeepAll"
  show KeepLast    = "KeepLast"
  show KeepDaily   = "KeepDaily"
  show KeepWeekly  = "KeepWeekly"
  show KeepMonthly = "KeepMonthly"
