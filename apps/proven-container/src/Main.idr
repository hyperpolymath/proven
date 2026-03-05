-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

||| Entry point for the proven-container management server.
||| Prints server identification and enumerates core type constructors.
module Main

import Container

%default total

||| Print server name, ports, and enumerate all type constructors.
partial
main : IO ()
main = do
  putStrLn "=========================================="
  putStrLn $ " " ++ serverName ++ " (port " ++ show containerPort ++ ")"
  putStrLn "=========================================="
  putStrLn ""
  putStrLn $ "TLS port: " ++ show containerTLSPort
  putStrLn $ "Max containers: " ++ show maxContainers
  putStrLn ""
  putStrLn "--- ContainerState ---"
  printLn Creating
  printLn Running
  printLn Paused
  printLn Restarting
  printLn Stopped
  printLn Removing
  printLn Dead
  putStrLn ""
  putStrLn "--- Operation ---"
  printLn Create
  printLn Start
  printLn Stop
  printLn Restart
  printLn Pause
  printLn Unpause
  printLn Kill
  printLn Remove
  printLn Exec
  printLn Logs
  printLn Inspect
  putStrLn ""
  putStrLn "--- NetworkMode ---"
  printLn Bridge
  printLn Host
  printLn Container.Types.None
  printLn Overlay
  printLn Macvlan
  putStrLn ""
  putStrLn "--- VolumeType ---"
  printLn Bind
  printLn Named
  printLn Tmpfs
  putStrLn ""
  putStrLn "--- RestartPolicy ---"
  printLn No
  printLn Always
  printLn OnFailure
  printLn UnlessStopped
  putStrLn ""
  putStrLn "--- HealthStatus ---"
  printLn Starting
  printLn Healthy
  printLn Unhealthy
  printLn NoCheck
