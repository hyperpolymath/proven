-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

-- | Entry point for the proven-webdav skeleton.
-- | Prints the server name and demonstrates type constructors.

module Main

import WebDAV

%default total

||| All WebDAV method constructors for demonstration.
allMethods : List Method
allMethods = [Propfind, Proppatch, Mkcol, Copy, Move, Lock, Unlock]

||| All WebDAV status code constructors for demonstration.
allStatusCodes : List StatusCode
allStatusCodes =
  [MultiStatus, UnprocessableEntity, Locked, FailedDependency, InsufficientStorage]

||| All WebDAV lock scope constructors for demonstration.
allLockScopes : List LockScope
allLockScopes = [Exclusive, Shared]

||| All WebDAV lock type constructors for demonstration.
allLockTypes : List LockType
allLockTypes = [Write]

||| All WebDAV depth constructors for demonstration.
allDepths : List Depth
allDepths = [Zero, One, Infinity]

||| All WebDAV property operation constructors for demonstration.
allPropertyOps : List PropertyOp
allPropertyOps = [Set, Remove]

main : IO ()
main = do
  putStrLn "proven-webdav: RFC 4918 WebDAV"
  putStrLn $ "  Port:          " ++ show webdavPort
  putStrLn $ "  Lock timeout:  " ++ show defaultLockTimeout ++ "s"
  putStrLn $ "  Max depth:     " ++ show maxDepth
  putStrLn $ "  Methods:       " ++ show allMethods
  putStrLn $ "  Status codes:  " ++ show allStatusCodes
  putStrLn $ "  Lock scopes:   " ++ show allLockScopes
  putStrLn $ "  Lock types:    " ++ show allLockTypes
  putStrLn $ "  Depths:        " ++ show allDepths
  putStrLn $ "  Property ops:  " ++ show allPropertyOps
