-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
--
-- LPD Print Queue Management
--
-- Implements a bounded FIFO print queue with capacity enforcement.
-- Jobs are enqueued at the back and dequeued from the front for
-- processing. The queue tracks job status and supports removal
-- by job ID or user. All operations are pure and total.

module LPD.Queue

import LPD.Job

%default total

-- ============================================================================
-- Queue configuration
-- ============================================================================

||| Configuration for a print queue.
public export
record QueueConfig where
  constructor MkQueueConfig
  ||| The name of this print queue (e.g. "lp0", "laser1").
  queueName   : String
  ||| Maximum number of jobs allowed in the queue.
  maxDepth    : Nat
  ||| Maximum size of a single print job in bytes.
  maxJobSize  : Nat
  ||| Whether the queue is currently accepting jobs.
  accepting   : Bool

public export
Show QueueConfig where
  show cfg = cfg.queueName ++ " (max=" ++ show cfg.maxDepth
             ++ ", accepting=" ++ show cfg.accepting ++ ")"

-- ============================================================================
-- Print Queue
-- ============================================================================

||| A bounded FIFO print queue.
||| Jobs are stored in order of submission (front = oldest, back = newest).
public export
record PrintQueue where
  constructor MkPrintQueue
  ||| Queue configuration.
  config     : QueueConfig
  ||| The ordered list of jobs (FIFO: head is next to print).
  jobs       : List PrintJob
  ||| The next job number to assign (wraps at 999).
  nextJobNum : Nat
  ||| Total jobs submitted since queue creation.
  totalSubmitted : Nat
  ||| Total jobs completed since queue creation.
  totalCompleted : Nat

public export
Show PrintQueue where
  show q = "Queue(" ++ q.config.queueName
           ++ ", jobs=" ++ show (length q.jobs)
           ++ "/" ++ show q.config.maxDepth
           ++ ", submitted=" ++ show q.totalSubmitted
           ++ ", completed=" ++ show q.totalCompleted ++ ")"

-- ============================================================================
-- Queue errors
-- ============================================================================

||| Errors from queue operations.
public export
data QueueError : Type where
  ||| The queue is full (at maximum capacity).
  QueueFull      : (queueName : String) -> (depth : Nat) -> QueueError
  ||| The queue is not accepting jobs.
  NotAccepting   : (queueName : String) -> QueueError
  ||| The specified job was not found in the queue.
  JobNotFound    : (jobId : JobId) -> QueueError
  ||| The job is too large for this queue.
  JobTooLarge    : (size : Nat) -> (maxSize : Nat) -> QueueError
  ||| Permission denied (user cannot remove another user's job).
  PermissionDenied : (user : String) -> (jobOwner : String) -> QueueError

public export
Show QueueError where
  show (QueueFull q d)        = "Queue '" ++ q ++ "' is full (" ++ show d ++ " jobs)"
  show (NotAccepting q)       = "Queue '" ++ q ++ "' is not accepting jobs"
  show (JobNotFound j)        = "Job " ++ show j ++ " not found"
  show (JobTooLarge s m)      = "Job too large: " ++ show s ++ " > " ++ show m
  show (PermissionDenied u o) = "User '" ++ u ++ "' cannot remove job owned by '" ++ o ++ "'"

-- ============================================================================
-- Queue creation
-- ============================================================================

||| Create a new empty print queue with the given configuration.
public export
newQueue : QueueConfig -> PrintQueue
newQueue cfg = MkPrintQueue
  { config         = cfg
  , jobs           = []
  , nextJobNum     = 1
  , totalSubmitted = 0
  , totalCompleted = 0
  }

||| Create a queue with default configuration.
public export
defaultQueue : String -> PrintQueue
defaultQueue name = newQueue (MkQueueConfig name 100 104857600 True)

-- ============================================================================
-- Queue operations
-- ============================================================================

||| Get the current number of jobs in the queue.
public export
currentDepth : PrintQueue -> Nat
currentDepth q = length q.jobs

||| Check if the queue is empty.
public export
isEmpty : PrintQueue -> Bool
isEmpty q = length q.jobs == 0

||| Check if the queue is at maximum capacity.
public export
isFull : PrintQueue -> Bool
isFull q = length q.jobs >= q.config.maxDepth

||| Enqueue a new job. Returns Left if the queue is full or not accepting.
public export
enqueue : String -> String -> String -> PrintQueue -> Either QueueError (PrintQueue, JobId)
enqueue user filename content q =
  if not q.config.accepting
    then Left (NotAccepting q.config.queueName)
  else if isFull q
    then Left (QueueFull q.config.queueName q.config.maxDepth)
  else if length content > q.config.maxJobSize
    then Left (JobTooLarge (length content) q.config.maxJobSize)
  else
    let jid     = MkJobId q.nextJobNum
        job     = MkPrintJob jid user filename (length content) Pending content
        nextNum = if q.nextJobNum >= 999 then 0 else q.nextJobNum + 1
        updated = { jobs           $= (\js => js ++ [job])
                  , nextJobNum     := nextNum
                  , totalSubmitted $= (+ 1)
                  } q
    in Right (updated, jid)

||| Dequeue the next pending job for processing.
||| Returns the job (marked as Printing) and the updated queue.
public export
dequeue : PrintQueue -> Maybe (PrintJob, PrintQueue)
dequeue q = case q.jobs of
  [] => Nothing
  (j :: rest) =>
    if j.status == Pending
      then let updated = { jobs := startPrinting j :: rest } q
           in Just (startPrinting j, updated)
      else Nothing  -- Front job is not pending (already printing/done)

||| Remove a job by ID. Returns the updated queue or an error.
public export
removeJob : JobId -> String -> PrintQueue -> Either QueueError PrintQueue
removeJob jid user q =
  case find (\j => j.jobId == jid) q.jobs of
    Nothing  => Left (JobNotFound jid)
    Just job =>
      if job.user == user || user == "root"
        then Right ({ jobs $= filter (\j => j.jobId /= jid) } q)
        else Left (PermissionDenied user job.user)

||| Mark a job as complete by ID.
public export
completeJob : JobId -> PrintQueue -> PrintQueue
completeJob jid q =
  { jobs $= map (\j => if j.jobId == jid then markComplete j else j)
  , totalCompleted $= (+ 1)
  } q

||| Mark a job as failed by ID.
public export
failJob : JobId -> String -> PrintQueue -> PrintQueue
failJob jid reason q =
  { jobs $= map (\j => if j.jobId == jid then markFailed reason j else j) } q

||| Remove all completed and failed jobs from the queue (cleanup).
public export
purgeCompleted : PrintQueue -> PrintQueue
purgeCompleted q = { jobs $= filter (\j => isActive j.status) } q

||| Get a short queue listing (one line per job).
public export
shortListing : PrintQueue -> List String
shortListing q = map shortStatus q.jobs

||| Get a long queue listing (multi-line per job).
public export
longListing : PrintQueue -> List String
longListing q = map longStatus q.jobs

||| Find a job by ID in the queue.
public export
findJob : JobId -> PrintQueue -> Maybe PrintJob
findJob jid q = find (\j => j.jobId == jid) q.jobs

||| Count jobs by status.
public export
countByStatus : JobStatus -> PrintQueue -> Nat
countByStatus status q = length (filter (\j => j.status == status) q.jobs)

||| Pause the queue (stop accepting new jobs).
public export
pauseQueue : PrintQueue -> PrintQueue
pauseQueue q = { config.accepting := False } q

||| Resume the queue (start accepting new jobs again).
public export
resumeQueue : PrintQueue -> PrintQueue
resumeQueue q = { config.accepting := True } q
