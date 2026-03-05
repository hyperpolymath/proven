-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

-- | Top-level module for the proven-webdav skeleton.
-- | Re-exports WebDAV.Types and defines protocol constants for
-- | RFC 4918 WebDAV.

module WebDAV

import public WebDAV.Types

%default total

||| Default WebDAV port (HTTPS).
public export
webdavPort : Nat
webdavPort = 443

||| Default lock timeout in seconds (1 hour).
public export
defaultLockTimeout : Nat
defaultLockTimeout = 3600

||| Maximum recursion depth for operations.
public export
maxDepth : Nat
maxDepth = 20
