-- SPDX-License-Identifier: PMPL-1.0
module ProvenConcat.SchedulerCore

import Proven.SafeCron
import Proven.SafeRetry
import Proven.SafeCircuitBreaker
import Proven.SafeRateLimiter
import Proven.SafeLog

%default total

public export
record ScheduleRequest where
  constructor MkScheduleRequest
  cron : String
  taskId : String

public export
record SchedulePolicy where
  constructor MkSchedulePolicy
  policyId : String

public export
data ScheduleError
  = InvalidCron String
  | InvalidPolicy String

public export
buildSchedulePolicy : ScheduleRequest -> Either ScheduleError SchedulePolicy
buildSchedulePolicy _ = Left (InvalidCron "unimplemented")
