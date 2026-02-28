-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
--
-- LPD Spool Directory Management
--
-- The spool directory holds job control and data files while they are
-- waiting to be printed. Each job has a control file (cfANNN<host>)
-- and a data file (dfANNN<host>) following RFC 1179 naming conventions.
-- This module provides pure functions for name generation, file listing,
-- and cleanup operations.

module LPD.Spool

import LPD.Job

%default total

-- ============================================================================
-- Spool file naming (RFC 1179 Section 6)
-- ============================================================================

||| Types of files in the spool directory.
public export
data SpoolFileType : Type where
  ||| Control file: describes job attributes (cfANNN<host>).
  ControlFile : SpoolFileType
  ||| Data file: contains the actual print data (dfANNN<host>).
  DataFile    : SpoolFileType

public export
Eq SpoolFileType where
  ControlFile == ControlFile = True
  DataFile    == DataFile    = True
  _           == _           = False

public export
Show SpoolFileType where
  show ControlFile = "cf"
  show DataFile    = "df"

||| A spool file entry with metadata.
public export
record SpoolFile where
  constructor MkSpoolFile
  ||| The type of spool file (control or data).
  fileType : SpoolFileType
  ||| The job ID this file belongs to.
  jobId    : JobId
  ||| The originating hostname.
  hostname : String
  ||| The file size in bytes.
  fileSize : Nat

public export
Show SpoolFile where
  show sf = show sf.fileType ++ "A" ++ show sf.jobId ++ sf.hostname
            ++ " (" ++ show sf.fileSize ++ " bytes)"

public export
Eq SpoolFile where
  a == b = a.fileType == b.fileType && a.jobId == b.jobId
           && a.hostname == b.hostname

-- ============================================================================
-- File name generation
-- ============================================================================

||| Generate the spool filename for a job file.
||| Format: {cf|df}A{NNN}{hostname} (RFC 1179 Section 6).
public export
spoolFilename : SpoolFileType -> JobId -> String -> String
spoolFilename ftype jid hostname =
  show ftype ++ "A" ++ show jid ++ hostname

||| Generate the control file name for a job.
public export
controlFilename : JobId -> String -> String
controlFilename = spoolFilename ControlFile

||| Generate the data file name for a job.
public export
dataFilename : JobId -> String -> String
dataFilename = spoolFilename DataFile

-- ============================================================================
-- Spool directory representation
-- ============================================================================

||| A spool directory containing job files.
public export
record SpoolDir where
  constructor MkSpoolDir
  ||| The base path of the spool directory (e.g. "/var/spool/lpd/lp0").
  basePath : String
  ||| The queue name this spool directory serves.
  queueName : String
  ||| List of files currently in the spool directory.
  files    : List SpoolFile

public export
Show SpoolDir where
  show sd = "SpoolDir(" ++ sd.basePath ++ ", " ++ show (length sd.files) ++ " files)"

-- ============================================================================
-- Spool operations (pure)
-- ============================================================================

||| Create an empty spool directory record.
public export
newSpoolDir : String -> String -> SpoolDir
newSpoolDir path qname = MkSpoolDir path qname []

||| Add a spool file entry to the directory.
public export
addFile : SpoolFile -> SpoolDir -> SpoolDir
addFile sf sd = { files $= (sf ::) } sd

||| Add both control and data file entries for a job.
public export
addJobFiles : JobId -> String -> Nat -> Nat -> SpoolDir -> SpoolDir
addJobFiles jid hostname ctrlSize dataSize sd =
  let cf = MkSpoolFile ControlFile jid hostname ctrlSize
      df = MkSpoolFile DataFile jid hostname dataSize
  in addFile df (addFile cf sd)

||| Remove all spool files for a given job ID.
public export
removeJobFiles : JobId -> SpoolDir -> SpoolDir
removeJobFiles jid sd = { files $= filter (\sf => sf.jobId /= jid) } sd

||| Find all spool files for a given job ID.
public export
filesForJob : JobId -> SpoolDir -> List SpoolFile
filesForJob jid sd = filter (\sf => sf.jobId == jid) sd.files

||| Find the control file for a given job ID.
public export
controlFileFor : JobId -> SpoolDir -> Maybe SpoolFile
controlFileFor jid sd =
  find (\sf => sf.jobId == jid && sf.fileType == ControlFile) sd.files

||| Find the data file for a given job ID.
public export
dataFileFor : JobId -> SpoolDir -> Maybe SpoolFile
dataFileFor jid sd =
  find (\sf => sf.jobId == jid && sf.fileType == DataFile) sd.files

||| Get the total size of all files in the spool directory.
public export
totalSpoolSize : SpoolDir -> Nat
totalSpoolSize sd = foldl (\acc, sf => acc + sf.fileSize) 0 sd.files

||| Get the number of unique jobs in the spool directory.
public export
jobCount : SpoolDir -> Nat
jobCount sd = length (nubJobIds sd.files)
  where
    nubJobIds : List SpoolFile -> List JobId
    nubJobIds [] = []
    nubJobIds (sf :: rest) =
      if any (\r => r.jobId == sf.jobId) rest
        then nubJobIds rest
        else sf.jobId :: nubJobIds rest

||| Remove spool files for completed jobs given a list of completed job IDs.
public export
cleanupCompleted : List JobId -> SpoolDir -> SpoolDir
cleanupCompleted completedIds sd =
  { files $= filter (\sf => not (any (== sf.jobId) completedIds)) } sd

||| Build the full file path for a spool file.
public export
fullPath : SpoolDir -> SpoolFile -> String
fullPath sd sf = sd.basePath ++ "/" ++ spoolFilename sf.fileType sf.jobId sf.hostname

||| List all file paths in the spool directory.
public export
allPaths : SpoolDir -> List String
allPaths sd = map (fullPath sd) sd.files

||| Check if a spool directory has orphaned data files (data without control).
public export
orphanedDataFiles : SpoolDir -> List SpoolFile
orphanedDataFiles sd =
  let controlIds = map (\sf => sf.jobId)
                       (filter (\sf => sf.fileType == ControlFile) sd.files)
      dataFiles  = filter (\sf => sf.fileType == DataFile) sd.files
  in filter (\df => not (any (== df.jobId) controlIds)) dataFiles
