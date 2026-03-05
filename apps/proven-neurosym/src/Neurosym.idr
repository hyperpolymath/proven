-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
--
-- Neurosym: Top-level module for the neurosymbolic inference server.
-- Re-exports all protocol types from Neurosym.Types and defines
-- server configuration constants.

module Neurosym

import public Neurosym.Types

%default total

------------------------------------------------------------------------
-- Server configuration constants
------------------------------------------------------------------------

||| The TCP port the neurosymbolic inference server listens on.
public export
neurosymPort : Nat
neurosymPort = 9500

||| Maximum depth for recursive inference chains before the server
||| halts and returns a partial result.
public export
maxInferenceDepth : Nat
maxInferenceDepth = 100

||| Default timeout in seconds for a single inference request.
public export
defaultTimeout : Nat
defaultTimeout = 30

||| Maximum number of entries in the knowledge base.
public export
maxKnowledgeBase : Nat
maxKnowledgeBase = 10000000
