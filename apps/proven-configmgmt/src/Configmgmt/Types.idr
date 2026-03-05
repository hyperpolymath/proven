-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

||| Core protocol types for the proven-configmgmt configuration management server.
||| Defines closed sum types for resource types, resource states, change actions,
||| drift detection statuses, and enforcement modes.
module Configmgmt.Types

%default total

---------------------------------------------------------------------------
-- ResourceType: The kind of infrastructure resource being managed.
---------------------------------------------------------------------------

||| Enumerates the categories of infrastructure resources that
||| proven-configmgmt can declare, converge, and audit.
public export
data ResourceType
  = File     -- ^ File or directory on the filesystem (content, permissions, ownership)
  | Package  -- ^ Operating system or language package (install, version pin)
  | Service  -- ^ System service or daemon (systemd, init, launchd)
  | User     -- ^ User account (create, modify, delete, group membership)
  | Group    -- ^ User group (create, modify, delete)
  | Cron     -- ^ Scheduled job entry (crontab, systemd timer)
  | Mount    -- ^ Filesystem mount point (fstab, autofs)
  | Firewall -- ^ Firewall rule (iptables, nftables, firewalld)
  | Registry -- ^ Configuration registry entry (etcd, consul, Windows registry)

||| Display a human-readable label for each resource type.
public export
Show ResourceType where
  show File     = "File"
  show Package  = "Package"
  show Service  = "Service"
  show User     = "User"
  show Group    = "Group"
  show Cron     = "Cron"
  show Mount    = "Mount"
  show Firewall = "Firewall"
  show Registry = "Registry"

---------------------------------------------------------------------------
-- ResourceState: The desired or observed state of a resource.
---------------------------------------------------------------------------

||| Represents the lifecycle state of a managed resource, used both for
||| declaring desired state and reporting observed state.
public export
data ResourceState
  = Present  -- ^ Resource exists (or should exist)
  | Absent   -- ^ Resource does not exist (or should be removed)
  | Running  -- ^ Service/process is actively running
  | Stopped  -- ^ Service/process is stopped
  | Enabled  -- ^ Service is enabled to start on boot
  | Disabled -- ^ Service is disabled from starting on boot

||| Display a human-readable label for each resource state.
public export
Show ResourceState where
  show Present  = "Present"
  show Absent   = "Absent"
  show Running  = "Running"
  show Stopped  = "Stopped"
  show Enabled  = "Enabled"
  show Disabled = "Disabled"

---------------------------------------------------------------------------
-- ChangeAction: The remediation action to apply to a drifted resource.
---------------------------------------------------------------------------

||| Describes the specific action taken (or to be taken) to converge a
||| resource from its current state to its desired state.
public export
data ChangeAction
  = Create  -- ^ Create a new resource that does not yet exist
  | Modify  -- ^ Modify an existing resource to match desired state
  | Delete  -- ^ Delete a resource that should be absent
  | Restart -- ^ Restart a running service to apply configuration changes
  | Reload  -- ^ Reload a service's configuration without full restart
  | Skip    -- ^ Skip this resource (e.g., dry-run or dependency not met)

||| Display a human-readable label for each change action.
public export
Show ChangeAction where
  show Create  = "Create"
  show Modify  = "Modify"
  show Delete  = "Delete"
  show Restart = "Restart"
  show Reload  = "Reload"
  show Skip    = "Skip"

---------------------------------------------------------------------------
-- DriftStatus: Whether a resource matches its declared desired state.
---------------------------------------------------------------------------

||| Reports the convergence status of a managed resource after comparison
||| of observed state against the desired state declaration.
public export
data DriftStatus
  = InSync    -- ^ Observed state matches desired state exactly
  | Drifted   -- ^ Observed state differs from desired state
  | DUnknown  -- ^ State comparison could not be performed
  | Unmanaged -- ^ Resource exists but is not declared in any manifest

||| Display a human-readable label for each drift status.
public export
Show DriftStatus where
  show InSync    = "InSync"
  show Drifted   = "Drifted"
  show DUnknown  = "Unknown"
  show Unmanaged = "Unmanaged"

---------------------------------------------------------------------------
-- ApplyMode: How the configuration management run should behave.
---------------------------------------------------------------------------

||| Controls the enforcement behaviour of a configuration management run.
public export
data ApplyMode
  = Enforce -- ^ Apply changes to converge resources to desired state
  | DryRun  -- ^ Report what would change without modifying anything
  | Audit   -- ^ Report drift status only, no convergence attempted

||| Display a human-readable label for each apply mode.
public export
Show ApplyMode where
  show Enforce = "Enforce"
  show DryRun  = "DryRun"
  show Audit   = "Audit"
