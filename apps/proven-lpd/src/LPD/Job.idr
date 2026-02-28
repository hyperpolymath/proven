-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
--
-- LPD Print Job Representation (RFC 1179 Section 7)
--
-- Print jobs carry a unique identifier, the submitting user, the filename,
-- the data size, and the actual print data. All fields are validated at
-- construction time. Job IDs are bounded (000-999 per RFC 1179).

module LPD.Job

%default total

-- ============================================================================
-- Job identification
-- ============================================================================

||| A print job identifier (RFC 1179 Section 7.2).
||| Job numbers are three-digit integers from 000 to 999.
public export
record JobId where
  constructor MkJobId
  ||| The numeric job ID (bounded 0..999).
  jobNumber : Nat

public export
Show JobId where
  show j = let n = j.jobNumber
               s = show n
           in if n < 10 then "00" ++ s
              else if n < 100 then "0" ++ s
              else s

public export
Eq JobId where
  a == b = a.jobNumber == b.jobNumber

public export
Ord JobId where
  compare a b = compare a.jobNumber b.jobNumber

||| Create a valid job ID. Returns Nothing if the number exceeds 999.
public export
mkJobId : Nat -> Maybe JobId
mkJobId n = if n <= 999 then Just (MkJobId n) else Nothing

-- ============================================================================
-- Job status
-- ============================================================================

||| The processing status of a print job.
public export
data JobStatus : Type where
  ||| Job is waiting in the queue to be printed.
  Pending   : JobStatus
  ||| Job is currently being sent to the printer.
  Printing  : JobStatus
  ||| Job has been successfully printed.
  Complete  : JobStatus
  ||| Job failed to print (with a reason).
  Failed    : (reason : String) -> JobStatus

public export
Eq JobStatus where
  Pending    == Pending    = True
  Printing   == Printing   = True
  Complete   == Complete   = True
  (Failed a) == (Failed b) = a == b
  _          == _          = False

public export
Show JobStatus where
  show Pending     = "Pending"
  show Printing    = "Printing"
  show Complete    = "Complete"
  show (Failed r)  = "Failed(" ++ r ++ ")"

||| Whether the job has reached a terminal state (Complete or Failed).
public export
isTerminal : JobStatus -> Bool
isTerminal Complete   = True
isTerminal (Failed _) = True
isTerminal _          = False

||| Whether the job is still active (Pending or Printing).
public export
isActive : JobStatus -> Bool
isActive Pending  = True
isActive Printing = True
isActive _        = False

-- ============================================================================
-- Print job record
-- ============================================================================

||| A complete print job with all metadata and data.
public export
record PrintJob where
  constructor MkPrintJob
  ||| Unique job identifier (000-999).
  jobId    : JobId
  ||| The username who submitted the job.
  user     : String
  ||| The original filename of the document.
  filename : String
  ||| The size of the print data in bytes.
  dataSize : Nat
  ||| The current processing status.
  status   : JobStatus
  ||| The print data content (simplified as String for this model).
  jobData  : String

public export
Show PrintJob where
  show j = "Job " ++ show j.jobId ++ " [" ++ show j.status ++ "] "
           ++ j.filename ++ " (" ++ show j.dataSize ++ " bytes) by " ++ j.user

public export
Eq PrintJob where
  a == b = a.jobId == b.jobId

-- ============================================================================
-- Job validation
-- ============================================================================

||| Errors from job validation.
public export
data JobError : Type where
  ||| The username is empty.
  EmptyUser     : JobError
  ||| The filename is empty.
  EmptyFilename : JobError
  ||| The job data exceeds the maximum size.
  DataTooLarge  : (size : Nat) -> (maxSize : Nat) -> JobError
  ||| The job ID is out of range.
  InvalidJobId  : (number : Nat) -> JobError

public export
Show JobError where
  show EmptyUser          = "Empty username"
  show EmptyFilename      = "Empty filename"
  show (DataTooLarge s m) = "Data too large: " ++ show s
                            ++ " bytes (max " ++ show m ++ ")"
  show (InvalidJobId n)   = "Invalid job ID: " ++ show n ++ " (must be 0-999)"

||| Validate a print job for required fields and size constraints.
public export
validateJob : Nat -> PrintJob -> List JobError
validateJob maxSize job =
  let errors1 = if length job.user == 0 then [EmptyUser] else []
      errors2 = if length job.filename == 0 then [EmptyFilename] else []
      errors3 = if job.dataSize > maxSize
                  then [DataTooLarge job.dataSize maxSize]
                  else []
      errors4 = if job.jobId.jobNumber > 999
                  then [InvalidJobId job.jobId.jobNumber]
                  else []
  in errors1 ++ errors2 ++ errors3 ++ errors4

-- ============================================================================
-- Job construction helpers
-- ============================================================================

||| Create a print job with validation.
||| Returns Left with the first error, or Right with the job.
public export
createJob : Nat -> Nat -> String -> String -> String -> Either JobError PrintJob
createJob maxSize jobNum user filename content =
  case mkJobId jobNum of
    Nothing => Left (InvalidJobId jobNum)
    Just jid =>
      if length user == 0 then Left EmptyUser
      else if length filename == 0 then Left EmptyFilename
      else let size = length content
           in if size > maxSize
                then Left (DataTooLarge size maxSize)
                else Right (MkPrintJob
                  { jobId    = jid
                  , user     = user
                  , filename = filename
                  , dataSize = size
                  , status   = Pending
                  , jobData  = content
                  })

||| Transition a job to Printing status.
public export
startPrinting : PrintJob -> PrintJob
startPrinting = { status := Printing }

||| Mark a job as Complete.
public export
markComplete : PrintJob -> PrintJob
markComplete = { status := Complete }

||| Mark a job as Failed with a reason.
public export
markFailed : String -> PrintJob -> PrintJob
markFailed reason = { status := Failed reason }

||| Format a short status line for a job (one-line summary).
public export
shortStatus : PrintJob -> String
shortStatus j = show j.jobId ++ " " ++ j.user ++ " "
                ++ show j.dataSize ++ " " ++ j.filename

||| Format a long status block for a job (multi-line detail).
public export
longStatus : PrintJob -> String
longStatus j = j.user ++ ": "
               ++ show j.status ++ "\n"
               ++ "  Job ID: " ++ show j.jobId ++ "\n"
               ++ "  File: " ++ j.filename ++ "\n"
               ++ "  Size: " ++ show j.dataSize ++ " bytes"
