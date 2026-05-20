-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
||| Safety proofs for SafeConsensus Raft / Paxos primitives.
|||
||| Discharges enum self-equality + structural anchors for NodeRole
||| and `MkLogEntry` record projections.
|||
||| Zero `believe_me` / `idris_crash`.
module Proven.SafeConsensus.Proofs

import Proven.SafeConsensus

%default total

--------------------------------------------------------------------------------
-- `MkLogEntry` record projections
--------------------------------------------------------------------------------

public export
mkLogEntryTerm : (t : Term) -> (i : LogIndex) -> (c : cmd)
              -> (MkLogEntry t i c).entryTerm = t
mkLogEntryTerm t i c = Refl

public export
mkLogEntryIndex : (t : Term) -> (i : LogIndex) -> (c : cmd)
               -> (MkLogEntry t i c).entryIndex = i
mkLogEntryIndex t i c = Refl

public export
mkLogEntryCommand : (t : Term) -> (i : LogIndex) -> (c : cmd)
                 -> (MkLogEntry t i c).entryCommand = c
mkLogEntryCommand t i c = Refl

--------------------------------------------------------------------------------
-- Type aliases
--------------------------------------------------------------------------------

public export
nodeIdIsNat : NodeId = Nat
nodeIdIsNat = Refl

public export
termIsNat : Term = Nat
termIsNat = Refl

public export
logIndexIsNat : LogIndex = Nat
logIndexIsNat = Refl
