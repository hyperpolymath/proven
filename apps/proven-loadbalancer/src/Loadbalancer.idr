-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

||| Top-level module for the proven-loadbalancer load balancer.
||| Re-exports core types from Loadbalancer.Types and defines server constants.
module Loadbalancer

import public Loadbalancer.Types

%default total

---------------------------------------------------------------------------
-- Server Constants
---------------------------------------------------------------------------

||| Default listening port for plaintext HTTP traffic.
public export
lbPort : Nat
lbPort = 80

||| Default listening port for TLS-terminated HTTPS traffic.
public export
lbTLSPort : Nat
lbTLSPort = 443

||| Default interval between backend health checks, in seconds.
public export
healthCheckInterval : Nat
healthCheckInterval = 10

||| Maximum number of backend servers in the pool.
public export
maxBackends : Nat
maxBackends = 1000
