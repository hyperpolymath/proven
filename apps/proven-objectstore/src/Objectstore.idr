-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

||| Top-level module for the proven-objectstore S3-compatible object storage server.
||| Re-exports core types from Objectstore.Types and defines server constants.
module Objectstore

import public Objectstore.Types

%default total

---------------------------------------------------------------------------
-- Server Constants
---------------------------------------------------------------------------

||| Default listening port for the S3-compatible API.
public export
objectstorePort : Nat
objectstorePort = 9000

||| Maximum object size in bytes (5 GiB).
public export
maxObjectSize : Nat
maxObjectSize = 5368709120

||| Maximum part size for multipart uploads in bytes (5 GiB).
public export
maxPartSize : Nat
maxPartSize = 5368709120

||| Maximum number of parts allowed in a single multipart upload.
public export
maxPartsPerUpload : Nat
maxPartsPerUpload = 10000
