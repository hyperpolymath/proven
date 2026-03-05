-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

||| Core protocol types for the proven-metrics telemetry server.
||| Defines closed sum types for metric types, scrape results, alert
||| states, aggregation operations, and query errors.
module Metrics.Types

%default total

---------------------------------------------------------------------------
-- MetricType: The kind of metric being collected.
---------------------------------------------------------------------------

||| Classifies the fundamental metric type, determining how values are
||| interpreted, aggregated, and exposed.
public export
data MetricType
  = Counter   -- ^ Monotonically increasing counter (e.g., total requests)
  | Gauge     -- ^ Point-in-time value that can go up or down (e.g., temperature)
  | Histogram -- ^ Distribution of observations bucketed by value range
  | Summary   -- ^ Pre-computed quantile distribution (client-side calculated)
  | Info      -- ^ Informational metric with constant value 1 and identifying labels
  | StateSet  -- ^ Set of boolean states (e.g., feature flags, component status)

||| Display a human-readable label for each metric type.
public export
Show MetricType where
  show Counter   = "Counter"
  show Gauge     = "Gauge"
  show Histogram = "Histogram"
  show Summary   = "Summary"
  show Info      = "Info"
  show StateSet  = "StateSet"

---------------------------------------------------------------------------
-- ScrapeResult: Outcome of a metrics scrape attempt against a target.
---------------------------------------------------------------------------

||| Reports the result of attempting to scrape metrics from a configured
||| target endpoint.
public export
data ScrapeResult
  = Success          -- ^ Scrape completed and metrics were parsed successfully
  | ScrapeTimeout    -- ^ Scrape did not complete within the configured timeout
  | ConnectionRefused -- ^ Target refused the TCP connection
  | InvalidResponse  -- ^ Target responded but the payload could not be parsed

||| Display a human-readable label for each scrape result.
public export
Show ScrapeResult where
  show Success          = "Success"
  show ScrapeTimeout    = "Timeout"
  show ConnectionRefused = "ConnectionRefused"
  show InvalidResponse  = "InvalidResponse"

---------------------------------------------------------------------------
-- AlertState: Lifecycle state of an alerting rule evaluation.
---------------------------------------------------------------------------

||| Tracks the lifecycle of an alert from inactive through pending
||| (for-duration not yet elapsed) to firing and resolved.
public export
data AlertState
  = Inactive -- ^ Alert condition is not met
  | Pending  -- ^ Condition is met but the for-duration has not elapsed
  | Firing   -- ^ Condition met for the required duration; alert is active
  | Resolved -- ^ Alert was firing but the condition has cleared

||| Display a human-readable label for each alert state.
public export
Show AlertState where
  show Inactive = "Inactive"
  show Pending  = "Pending"
  show Firing   = "Firing"
  show Resolved = "Resolved"

---------------------------------------------------------------------------
-- AggregationOp: Operations for aggregating metric time series.
---------------------------------------------------------------------------

||| Query-time aggregation operations that can be applied across label
||| dimensions or over time windows.
public export
data AggregationOp
  = Sum      -- ^ Sum of all values in the group
  | Avg      -- ^ Arithmetic mean of all values in the group
  | Min      -- ^ Minimum value in the group
  | Max      -- ^ Maximum value in the group
  | Count    -- ^ Count of series in the group
  | Rate     -- ^ Per-second rate of increase (for counters)
  | Increase -- ^ Total increase over the time window (for counters)
  | P50      -- ^ 50th percentile (median)
  | P90      -- ^ 90th percentile
  | P95      -- ^ 95th percentile
  | P99      -- ^ 99th percentile

||| Display a human-readable label for each aggregation operation.
public export
Show AggregationOp where
  show Sum      = "Sum"
  show Avg      = "Avg"
  show Min      = "Min"
  show Max      = "Max"
  show Count    = "Count"
  show Rate     = "Rate"
  show Increase = "Increase"
  show P50      = "P50"
  show P90      = "P90"
  show P95      = "P95"
  show P99      = "P99"

---------------------------------------------------------------------------
-- QueryError: Errors that can occur during metric query evaluation.
---------------------------------------------------------------------------

||| Error conditions that may arise when evaluating a metrics query.
public export
data QueryError
  = ParseError     -- ^ The query expression could not be parsed
  | ExecutionError -- ^ An error occurred during query evaluation
  | QueryTimeout   -- ^ Query evaluation exceeded the time limit
  | TooManySeries  -- ^ Query matched more series than the configured limit

||| Display a human-readable label for each query error.
public export
Show QueryError where
  show ParseError     = "ParseError"
  show ExecutionError = "ExecutionError"
  show QueryTimeout   = "Timeout"
  show TooManySeries  = "TooManySeries"
