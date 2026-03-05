-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
--
-- proven-appserver: Core protocol types for application server.
--
-- All types are closed sum types with total Show instances.
-- No parsers, no full implementations -- skeleton only.

module Appserver.Types

%default total

-- ============================================================================
-- RequestType
-- ============================================================================

||| Supported request protocols handled by the application server.
public export
data RequestType : Type where
  ||| Standard HTTP/HTTPS request.
  HTTP      : RequestType
  ||| WebSocket bidirectional connection.
  WebSocket : RequestType
  ||| gRPC remote procedure call.
  GRPC      : RequestType
  ||| GraphQL query or mutation.
  GraphQL   : RequestType

export
Show RequestType where
  show HTTP      = "HTTP"
  show WebSocket = "WebSocket"
  show GRPC      = "gRPC"
  show GraphQL   = "GraphQL"

-- ============================================================================
-- LifecycleState
-- ============================================================================

||| Lifecycle state of the application server process.
public export
data LifecycleState : Type where
  ||| Server is loading configuration and setting up resources.
  Initializing : LifecycleState
  ||| Server is binding to ports and preparing to accept connections.
  Starting     : LifecycleState
  ||| Server is fully operational and accepting requests.
  Running      : LifecycleState
  ||| Server is finishing in-flight requests but rejecting new ones.
  Draining     : LifecycleState
  ||| Server is releasing resources and shutting down.
  Stopping     : LifecycleState
  ||| Server has fully stopped.
  Stopped      : LifecycleState

export
Show LifecycleState where
  show Initializing = "Initializing"
  show Starting     = "Starting"
  show Running      = "Running"
  show Draining     = "Draining"
  show Stopping     = "Stopping"
  show Stopped      = "Stopped"

-- ============================================================================
-- HealthCheck
-- ============================================================================

||| Kubernetes-style health check probes.
public export
data HealthCheck : Type where
  ||| Is the process alive? (restart if not)
  Liveness  : HealthCheck
  ||| Is the process ready to accept traffic? (remove from LB if not)
  Readiness : HealthCheck
  ||| Has the process finished its startup sequence?
  Startup   : HealthCheck

export
Show HealthCheck where
  show Liveness  = "Liveness"
  show Readiness = "Readiness"
  show Startup   = "Startup"

-- ============================================================================
-- DeployStrategy
-- ============================================================================

||| Deployment strategy for rolling out new server versions.
public export
data DeployStrategy : Type where
  ||| Incrementally replace instances one at a time.
  RollingUpdate : DeployStrategy
  ||| Run two full environments; switch traffic atomically.
  BlueGreen     : DeployStrategy
  ||| Route a fraction of traffic to the new version for validation.
  Canary        : DeployStrategy
  ||| Tear down all old instances, then bring up new ones.
  Recreate      : DeployStrategy

export
Show DeployStrategy where
  show RollingUpdate = "RollingUpdate"
  show BlueGreen     = "BlueGreen"
  show Canary        = "Canary"
  show Recreate      = "Recreate"

-- ============================================================================
-- ErrorCategory
-- ============================================================================

||| Categories of errors the application server can encounter.
public export
data ErrorCategory : Type where
  ||| Error caused by invalid client input (4xx).
  ClientError  : ErrorCategory
  ||| Internal server error (5xx).
  ServerError  : ErrorCategory
  ||| Upstream dependency did not respond in time.
  Timeout      : ErrorCategory
  ||| Circuit breaker is open; upstream is considered unhealthy.
  CircuitOpen  : ErrorCategory
  ||| Client has exceeded their rate limit.
  RateLimited  : ErrorCategory

export
Show ErrorCategory where
  show ClientError = "ClientError"
  show ServerError = "ServerError"
  show Timeout     = "Timeout"
  show CircuitOpen = "CircuitOpen"
  show RateLimited = "RateLimited"
