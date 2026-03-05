-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

||| Core protocol types for the proven-monitor monitoring server.
||| Defines closed sum types for check types, statuses, alert channels,
||| severity levels, and check execution states.
module Monitor.Types

%default total

---------------------------------------------------------------------------
-- CheckType: The kind of health check to perform against a target.
---------------------------------------------------------------------------

||| Enumerates the monitoring check types supported by proven-monitor.
||| Each constructor represents a distinct probe mechanism.
public export
data CheckType
  = HTTP        -- ^ HTTP(S) endpoint check (GET/HEAD with status code validation)
  | TCP         -- ^ Raw TCP connection check (port open/reachable)
  | UDP         -- ^ UDP datagram check (send probe, await response)
  | ICMP        -- ^ ICMP echo request (ping)
  | DNS         -- ^ DNS resolution check (query record, validate answer)
  | Certificate -- ^ TLS certificate validity check (expiry, chain, hostname)
  | Disk        -- ^ Disk usage threshold check
  | CPU         -- ^ CPU utilisation threshold check
  | Memory      -- ^ Memory usage threshold check
  | Process     -- ^ Process existence/state check (by name or PID)
  | Custom      -- ^ User-defined check via external script or plugin

||| Display a human-readable label for each check type.
public export
Show CheckType where
  show HTTP        = "HTTP"
  show TCP         = "TCP"
  show UDP         = "UDP"
  show ICMP        = "ICMP"
  show DNS         = "DNS"
  show Certificate = "Certificate"
  show Disk        = "Disk"
  show CPU         = "CPU"
  show Memory      = "Memory"
  show Process     = "Process"
  show Custom      = "Custom"

---------------------------------------------------------------------------
-- Status: The observed state of a monitored target.
---------------------------------------------------------------------------

||| Represents the overall health status reported after a check completes.
public export
data Status
  = Up          -- ^ Target is reachable and healthy
  | Down        -- ^ Target is unreachable or unhealthy
  | Degraded    -- ^ Target is reachable but performance is below threshold
  | Unknown     -- ^ Check has not yet run or result is indeterminate
  | Maintenance -- ^ Target is under scheduled maintenance (alerts suppressed)

||| Display a human-readable label for each status value.
public export
Show Status where
  show Up          = "Up"
  show Down        = "Down"
  show Degraded    = "Degraded"
  show Unknown     = "Unknown"
  show Maintenance = "Maintenance"

---------------------------------------------------------------------------
-- AlertChannel: Destination for alert notifications.
---------------------------------------------------------------------------

||| The delivery channel through which alert notifications are dispatched.
public export
data AlertChannel
  = Email    -- ^ Email notification (SMTP)
  | SMS      -- ^ SMS text message notification
  | Webhook  -- ^ Generic HTTP webhook callback
  | Slack    -- ^ Slack incoming webhook or API message
  | PagerDuty -- ^ PagerDuty incident creation via Events API

||| Display a human-readable label for each alert channel.
public export
Show AlertChannel where
  show Email     = "Email"
  show SMS       = "SMS"
  show Webhook   = "Webhook"
  show Slack     = "Slack"
  show PagerDuty = "PagerDuty"

---------------------------------------------------------------------------
-- Severity: Alert urgency classification.
---------------------------------------------------------------------------

||| Classifies the urgency of an alert, from informational to critical.
public export
data Severity
  = Info     -- ^ Informational notice, no action required
  | Warning  -- ^ Potential issue, investigation recommended
  | Error    -- ^ Service degradation, action required
  | Critical -- ^ Service outage or data loss risk, immediate action required

||| Display a human-readable label for each severity level.
public export
Show Severity where
  show Info     = "Info"
  show Warning  = "Warning"
  show Error    = "Error"
  show Critical = "Critical"

---------------------------------------------------------------------------
-- CheckState: Execution lifecycle state of a single check invocation.
---------------------------------------------------------------------------

||| Tracks the execution state of an individual check run through its
||| lifecycle from scheduling to completion.
public export
data CheckState
  = Pending -- ^ Check is scheduled but has not started executing
  | Running -- ^ Check is currently in progress
  | Passed  -- ^ Check completed successfully (target is healthy)
  | Failed  -- ^ Check completed and target was found unhealthy
  | Timeout -- ^ Check did not complete within the configured timeout
  | CSError -- ^ Check encountered an internal error during execution

||| Display a human-readable label for each check state.
public export
Show CheckState where
  show Pending = "Pending"
  show Running = "Running"
  show Passed  = "Passed"
  show Failed  = "Failed"
  show Timeout = "Timeout"
  show CSError = "Error"
