-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

||| Core protocol types for the proven-container management server.
||| Defines closed sum types for container states, operations, networking,
||| volumes, restart policies, and health checks.
module Container.Types

%default total

---------------------------------------------------------------------------
-- Container state: lifecycle states of a container
---------------------------------------------------------------------------

||| Lifecycle state of a managed container.
public export
data ContainerState : Type where
  Creating   : ContainerState
  Running    : ContainerState
  Paused     : ContainerState
  Restarting : ContainerState
  Stopped    : ContainerState
  Removing   : ContainerState
  Dead       : ContainerState

export
Show ContainerState where
  show Creating   = "Creating"
  show Running    = "Running"
  show Paused     = "Paused"
  show Restarting = "Restarting"
  show Stopped    = "Stopped"
  show Removing   = "Removing"
  show Dead       = "Dead"

---------------------------------------------------------------------------
-- Operation: container management operations
---------------------------------------------------------------------------

||| Operations that can be performed on a container.
public export
data Operation : Type where
  Create  : Operation
  Start   : Operation
  Stop    : Operation
  Restart : Operation
  Pause   : Operation
  Unpause : Operation
  Kill    : Operation
  Remove  : Operation
  Exec    : Operation
  Logs    : Operation
  Inspect : Operation

export
Show Operation where
  show Create  = "Create"
  show Start   = "Start"
  show Stop    = "Stop"
  show Restart = "Restart"
  show Pause   = "Pause"
  show Unpause = "Unpause"
  show Kill    = "Kill"
  show Remove  = "Remove"
  show Exec    = "Exec"
  show Logs    = "Logs"
  show Inspect = "Inspect"

---------------------------------------------------------------------------
-- Network mode: container networking modes
---------------------------------------------------------------------------

||| Networking mode for a container.
public export
data NetworkMode : Type where
  Bridge  : NetworkMode
  Host    : NetworkMode
  None    : NetworkMode
  Overlay : NetworkMode
  Macvlan : NetworkMode

export
Show NetworkMode where
  show Bridge  = "Bridge"
  show Host    = "Host"
  show None    = "None"
  show Overlay = "Overlay"
  show Macvlan = "Macvlan"

---------------------------------------------------------------------------
-- Volume type: container storage volume types
---------------------------------------------------------------------------

||| Type of storage volume attached to a container.
public export
data VolumeType : Type where
  Bind  : VolumeType
  Named : VolumeType
  Tmpfs : VolumeType

export
Show VolumeType where
  show Bind  = "Bind"
  show Named = "Named"
  show Tmpfs = "Tmpfs"

---------------------------------------------------------------------------
-- Restart policy: container restart behaviour
---------------------------------------------------------------------------

||| Policy governing automatic container restarts.
public export
data RestartPolicy : Type where
  No            : RestartPolicy
  Always        : RestartPolicy
  OnFailure     : RestartPolicy
  UnlessStopped : RestartPolicy

export
Show RestartPolicy where
  show No            = "No"
  show Always        = "Always"
  show OnFailure     = "OnFailure"
  show UnlessStopped = "UnlessStopped"

---------------------------------------------------------------------------
-- Health status: container health check results
---------------------------------------------------------------------------

||| Result of a container health check.
public export
data HealthStatus : Type where
  Starting  : HealthStatus
  Healthy   : HealthStatus
  Unhealthy : HealthStatus
  NoCheck   : HealthStatus

export
Show HealthStatus where
  show Starting  = "Starting"
  show Healthy   = "Healthy"
  show Unhealthy = "Unhealthy"
  show NoCheck   = "None"
