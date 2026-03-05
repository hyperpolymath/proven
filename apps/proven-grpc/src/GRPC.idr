-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

-- | Top-level module for the proven-grpc skeleton.
-- | Re-exports GRPC.Types and defines protocol constants for
-- | gRPC over HTTP/2.

module GRPC

import public GRPC.Types

%default total

||| Default gRPC port (HTTPS).
public export
grpcPort : Nat
grpcPort = 443

||| Default maximum message size in bytes (4 MiB).
public export
maxMessageSize : Nat
maxMessageSize = 4194304

||| Default maximum metadata size in bytes (8 KiB).
public export
maxMetadataSize : Nat
maxMetadataSize = 8192
