-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

||| Entry point for the proven-virt virtualization server.
||| Prints server identification and enumerates core type constructors.
module Main

import Virt

%default total

||| Print server name, ports, and enumerate all type constructors.
partial
main : IO ()
main = do
  putStrLn "=========================================="
  putStrLn $ " " ++ serverName ++ " (port " ++ show virtPort ++ ")"
  putStrLn "=========================================="
  putStrLn ""
  putStrLn $ "TLS port: " ++ show virtTLSPort
  putStrLn $ "Max VMs: " ++ show maxVMs
  putStrLn ""
  putStrLn "--- VMState ---"
  printLn Creating
  printLn Running
  printLn Paused
  printLn Suspended
  printLn ShuttingDown
  printLn Stopped
  printLn Crashed
  printLn Migrating
  putStrLn ""
  putStrLn "--- Operation ---"
  printLn Create
  printLn Start
  printLn Stop
  printLn Restart
  printLn Pause
  printLn Resume
  printLn Suspend
  printLn Migrate
  printLn Snapshot
  printLn Clone
  printLn Delete
  putStrLn ""
  putStrLn "--- DiskFormat ---"
  printLn Raw
  printLn QCOW2
  printLn VDI
  printLn VMDK
  printLn VHD
  putStrLn ""
  putStrLn "--- NetworkType ---"
  printLn NAT
  printLn Bridged
  printLn Internal
  printLn HostOnly
  putStrLn ""
  putStrLn "--- BootDevice ---"
  printLn HardDisk
  printLn CDROM
  printLn Network
  printLn USB
