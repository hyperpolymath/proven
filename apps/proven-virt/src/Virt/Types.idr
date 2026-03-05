-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

||| Core protocol types for the proven-virt virtualization server.
||| Defines closed sum types for VM states, operations, disk formats,
||| network types, and boot devices.
module Virt.Types

%default total

---------------------------------------------------------------------------
-- VM state: lifecycle states of a virtual machine
---------------------------------------------------------------------------

||| Lifecycle state of a virtual machine.
public export
data VMState : Type where
  Creating     : VMState
  Running      : VMState
  Paused       : VMState
  Suspended    : VMState
  ShuttingDown : VMState
  Stopped      : VMState
  Crashed      : VMState
  Migrating    : VMState

export
Show VMState where
  show Creating     = "Creating"
  show Running      = "Running"
  show Paused       = "Paused"
  show Suspended    = "Suspended"
  show ShuttingDown = "ShuttingDown"
  show Stopped      = "Stopped"
  show Crashed      = "Crashed"
  show Migrating    = "Migrating"

---------------------------------------------------------------------------
-- Operation: VM management operations
---------------------------------------------------------------------------

||| Operations that can be performed on a virtual machine.
public export
data Operation : Type where
  Create   : Operation
  Start    : Operation
  Stop     : Operation
  Restart  : Operation
  Pause    : Operation
  Resume   : Operation
  Suspend  : Operation
  Migrate  : Operation
  Snapshot : Operation
  Clone    : Operation
  Delete   : Operation

export
Show Operation where
  show Create   = "Create"
  show Start    = "Start"
  show Stop     = "Stop"
  show Restart  = "Restart"
  show Pause    = "Pause"
  show Resume   = "Resume"
  show Suspend  = "Suspend"
  show Migrate  = "Migrate"
  show Snapshot = "Snapshot"
  show Clone    = "Clone"
  show Delete   = "Delete"

---------------------------------------------------------------------------
-- Disk format: virtual disk image formats
---------------------------------------------------------------------------

||| Format of a virtual disk image.
public export
data DiskFormat : Type where
  Raw   : DiskFormat
  QCOW2 : DiskFormat
  VDI   : DiskFormat
  VMDK  : DiskFormat
  VHD   : DiskFormat

export
Show DiskFormat where
  show Raw   = "Raw"
  show QCOW2 = "QCOW2"
  show VDI   = "VDI"
  show VMDK  = "VMDK"
  show VHD   = "VHD"

---------------------------------------------------------------------------
-- Network type: virtual network topologies
---------------------------------------------------------------------------

||| Type of virtual network.
public export
data NetworkType : Type where
  NAT      : NetworkType
  Bridged  : NetworkType
  Internal : NetworkType
  HostOnly : NetworkType

export
Show NetworkType where
  show NAT      = "NAT"
  show Bridged  = "Bridged"
  show Internal = "Internal"
  show HostOnly = "HostOnly"

---------------------------------------------------------------------------
-- Boot device: devices a VM can boot from
---------------------------------------------------------------------------

||| Device from which a virtual machine can boot.
public export
data BootDevice : Type where
  HardDisk : BootDevice
  CDROM    : BootDevice
  Network  : BootDevice
  USB      : BootDevice

export
Show BootDevice where
  show HardDisk = "HardDisk"
  show CDROM    = "CDROM"
  show Network  = "Network"
  show USB      = "USB"
