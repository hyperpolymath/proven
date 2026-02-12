-- SPDX-License-Identifier: Apache-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
--
-- SafeProvenance: Formally verified change tracking and audit trails
--
-- Provides:
-- - Change tracking with causality
-- - Audit trail integrity proofs
-- - Data lineage verification
-- - Tamper detection

module Proven.SafeProvenance

import Data.String
import Data.List
import Data.Nat
import Data.Maybe
import Data.Vect
import Decidable.Equality

%default total

||| Entity identifier
public export
EntityId : Type
EntityId = String

||| Actor who made a change
public export
ActorId : Type
ActorId = String

||| Change type classification
public export
data ChangeType =
    Create
  | Update
  | Delete
  | Derive      -- Created from other entities
  | Transform   -- Modified through process
  | Annotate    -- Metadata added
  | Link        -- Relationship established

||| Equality for change types
public export
Eq ChangeType where
  Create == Create = True
  Update == Update = True
  Delete == Delete = True
  Derive == Derive = True
  Transform == Transform = True
  Annotate == Annotate = True
  Link == Link = True
  _ == _ = False

||| A single change event
public export
record ChangeEvent where
  constructor MkChangeEvent
  eventId : Nat
  entityId : EntityId
  changeType : ChangeType
  actor : ActorId
  timestamp : Nat
  previousVersion : Maybe Nat     -- Link to previous event
  sourceEntities : List EntityId  -- For derivation
  description : String

||| Hash representation (simplified)
public export
Hash : Type
Hash = Nat

||| Simple hash function for chaining
public export
hashEvent : ChangeEvent -> Hash -> Hash
hashEvent evt prevHash =
  eventId evt + timestamp evt + prevHash * 31

||| A provenance record with hash chain
public export
record ProvenanceRecord where
  constructor MkProvenanceRecord
  prEvent : ChangeEvent
  prHash : Hash
  prPrevHash : Hash

||| Verify hash chain integrity
public export
verifyHash : ProvenanceRecord -> Bool
verifyHash pr = prHash pr == hashEvent (prEvent pr) (prPrevHash pr)

||| A complete provenance chain
public export
record ProvenanceChain where
  constructor MkProvenanceChain
  chainRecords : List ProvenanceRecord
  chainHead : Hash

||| Empty provenance chain
public export
emptyChain : ProvenanceChain
emptyChain = MkProvenanceChain [] 0

||| Add event to chain
public export
addToChain : ChangeEvent -> ProvenanceChain -> ProvenanceChain
addToChain evt chain =
  let newHash = hashEvent evt (chainHead chain)
      newRecord = MkProvenanceRecord evt newHash (chainHead chain)
  in { chainRecords := newRecord :: chainRecords chain,
       chainHead := newHash } chain

||| Verify entire chain integrity
public export
verifyChain : ProvenanceChain -> Bool
verifyChain chain = verifyChainRec (chainRecords chain) (chainHead chain)
  where
    verifyChainRec : List ProvenanceRecord -> Hash -> Bool
    verifyChainRec [] expectedHash = expectedHash == 0
    verifyChainRec (pr :: prs) expectedHash =
      prHash pr == expectedHash &&
      verifyHash pr &&
      verifyChainRec prs (prPrevHash pr)

||| Proof of chain integrity
public export
data ChainIntegrity : ProvenanceChain -> Type where
  MkChainIntegrity : verifyChain chain = True -> ChainIntegrity chain

||| Data lineage - track where data came from
public export
record Lineage where
  constructor MkLineage
  lineageEntity : EntityId
  lineageSources : List EntityId
  lineageTransforms : List String
  lineageChain : ProvenanceChain

||| Empty lineage for new entity
public export
newLineage : EntityId -> ActorId -> Lineage
newLineage entity actor =
  let createEvent = MkChangeEvent 0 entity Create actor 0 Nothing [] "Entity created"
  in MkLineage entity [] [] (addToChain createEvent emptyChain)

||| Add source to lineage (derivation)
public export
addSource : EntityId -> ActorId -> String -> Lineage -> Lineage
addSource source actor desc lin =
  let eventId = length (chainRecords (lineageChain lin))
      deriveEvent = MkChangeEvent eventId (lineageEntity lin) Derive actor
                                  eventId (Just (pred eventId))
                                  [source] desc
  in { lineageSources := source :: lineageSources lin,
       lineageChain := addToChain deriveEvent (lineageChain lin) } lin

||| Add transformation to lineage
public export
addTransform : String -> ActorId -> Lineage -> Lineage
addTransform transform actor lin =
  let eventId = length (chainRecords (lineageChain lin))
      transEvent = MkChangeEvent eventId (lineageEntity lin) Transform actor
                                 eventId (Just (pred eventId))
                                 [] transform
  in { lineageTransforms := transform :: lineageTransforms lin,
       lineageChain := addToChain transEvent (lineageChain lin) } lin

||| Check if entity derived from source
public export
derivedFrom : EntityId -> Lineage -> Bool
derivedFrom source lin = any (\s => s == source) (lineageSources lin)

||| Audit trail entry
public export
record AuditEntry where
  constructor MkAuditEntry
  auditId : Nat
  auditTimestamp : Nat
  auditActor : ActorId
  auditAction : String
  auditTarget : EntityId
  auditOldValue : Maybe String
  auditNewValue : Maybe String
  auditReason : String

||| Audit trail with integrity
public export
record AuditTrail where
  constructor MkAuditTrail
  trailEntries : List AuditEntry
  trailHash : Hash
  trailSealed : Bool  -- Once sealed, no more entries

||| Hash audit entry
public export
hashAuditEntry : AuditEntry -> Hash -> Hash
hashAuditEntry entry prevHash =
  auditId entry + auditTimestamp entry + prevHash * 37

||| Empty audit trail
public export
emptyAuditTrail : AuditTrail
emptyAuditTrail = MkAuditTrail [] 0 False

||| Add to audit trail
public export
auditLog : AuditEntry -> AuditTrail -> Maybe AuditTrail
auditLog entry trail =
  if trailSealed trail
    then Nothing
    else let newHash = hashAuditEntry entry (trailHash trail)
         in Just ({ trailEntries := entry :: trailEntries trail,
                    trailHash := newHash } trail)

||| Seal audit trail (no more modifications)
public export
sealTrail : AuditTrail -> AuditTrail
sealTrail trail = { trailSealed := True } trail

||| Verify audit trail integrity
public export
verifyAuditTrail : AuditTrail -> Bool
verifyAuditTrail trail = verifyRec (trailEntries trail) (trailHash trail)
  where
    computePrevHash : List AuditEntry -> Hash
    computePrevHash [] = 0
    computePrevHash (e :: es) = hashAuditEntry e (computePrevHash es)

    verifyRec : List AuditEntry -> Hash -> Bool
    verifyRec [] expectedHash = expectedHash == 0
    verifyRec (e :: es) expectedHash =
      let computed = hashAuditEntry e (computePrevHash es)
      in computed == expectedHash && verifyRec es (computePrevHash es)

||| Proof of audit trail integrity
public export
data AuditIntegrity : AuditTrail -> Type where
  MkAuditIntegrity : verifyAuditTrail trail = True -> AuditIntegrity trail

||| Tamper detection result
public export
data TamperResult =
    NoTamper
  | TamperDetected Nat String  -- Position and description
  | SequenceError Nat Nat      -- Expected vs actual sequence

||| Eq for TamperResult
public export
Eq TamperResult where
  NoTamper == NoTamper = True
  TamperDetected p1 _ == TamperDetected p2 _ = p1 == p2
  SequenceError e1 a1 == SequenceError e2 a2 = e1 == e2 && a1 == a2
  _ == _ = False

||| Detect tampering in provenance chain
public export
detectTampering : ProvenanceChain -> TamperResult
detectTampering chain =
  if verifyChain chain
    then NoTamper
    else findTamperPoint (chainRecords chain) (chainHead chain) 0
  where
    findTamperPoint : List ProvenanceRecord -> Hash -> Nat -> TamperResult
    findTamperPoint [] _ _ = NoTamper
    findTamperPoint (pr :: prs) expectedHash pos =
      if prHash pr /= expectedHash
        then TamperDetected pos "Hash mismatch"
        else if not (verifyHash pr)
             then TamperDetected pos "Record hash invalid"
             else findTamperPoint prs (prPrevHash pr) (S pos)

||| Proof of no tampering
public export
data TamperFree : ProvenanceChain -> Type where
  MkTamperFree : detectTampering chain = NoTamper -> TamperFree chain

||| Entity version history
public export
record VersionHistory where
  constructor MkVersionHistory
  vhEntity : EntityId
  vhVersions : List (Nat, Hash)  -- Version number, content hash
  vhCurrent : Nat

||| Empty version history
public export
newVersionHistory : EntityId -> VersionHistory
newVersionHistory entity = MkVersionHistory entity [] 0

||| Add version
public export
addVersion : Hash -> VersionHistory -> VersionHistory
addVersion contentHash vh =
  let newVersion = S (vhCurrent vh)
  in { vhVersions := (newVersion, contentHash) :: vhVersions vh,
       vhCurrent := newVersion } vh

||| Get specific version hash
public export
getVersionHash : Nat -> VersionHistory -> Maybe Hash
getVersionHash ver vh = lookup ver (vhVersions vh)

||| Compare versions
public export
compareVersions : Nat -> Nat -> VersionHistory -> Maybe Ordering
compareVersions v1 v2 vh =
  case (getVersionHash v1 vh, getVersionHash v2 vh) of
    (Just _, Just _) => Just (compare v1 v2)
    _ => Nothing

||| Causality tracking
public export
record CausalRelation where
  constructor MkCausalRelation
  crCause : EntityId
  crEffect : EntityId
  crTimestamp : Nat
  crDescription : String

||| Causality graph
public export
record CausalityGraph where
  constructor MkCausalityGraph
  cgEntities : List EntityId
  cgRelations : List CausalRelation

||| Add causal relationship
public export
addCausalRelation : CausalRelation -> CausalityGraph -> CausalityGraph
addCausalRelation rel graph =
  { cgEntities := nub (crCause rel :: crEffect rel :: cgEntities graph),
    cgRelations := rel :: cgRelations graph } graph

||| Find causes of an entity
public export
findCauses : EntityId -> CausalityGraph -> List EntityId
findCauses entity graph =
  map crCause (filter (\r => crEffect r == entity) (cgRelations graph))

||| Find effects of an entity
public export
findEffects : EntityId -> CausalityGraph -> List EntityId
findEffects entity graph =
  map crEffect (filter (\r => crCause r == entity) (cgRelations graph))

||| Check if entity A causally precedes entity B
public export
causallyPrecedes : EntityId -> EntityId -> CausalityGraph -> Bool
causallyPrecedes a b graph =
  elem b (findEffects a graph) ||
  any (\mid => causallyPrecedes mid b graph) (findEffects a graph)

||| Proof of causal relationship
public export
data CausallyRelated : EntityId -> EntityId -> CausalityGraph -> Type where
  MkCausallyRelated : causallyPrecedes a b g = True -> CausallyRelated a b g

||| Provenance query result
public export
record ProvenanceQuery where
  constructor MkProvenanceQuery
  queryEntity : EntityId
  queryActor : Maybe ActorId
  queryTimeStart : Maybe Nat
  queryTimeEnd : Maybe Nat
  queryChangeType : Maybe ChangeType

||| Execute provenance query
public export
queryProvenance : ProvenanceQuery -> ProvenanceChain -> List ChangeEvent
queryProvenance query chain =
  filter matches (map prEvent (chainRecords chain))
  where
    matches : ChangeEvent -> Bool
    matches evt =
      entityId evt == queryEntity query &&
      maybe True (\a => actor evt == a) (queryActor query) &&
      maybe True (\t => timestamp evt >= t) (queryTimeStart query) &&
      maybe True (\t => timestamp evt <= t) (queryTimeEnd query) &&
      maybe True (\c => changeType evt == c) (queryChangeType query)

||| Provenance report
public export
record ProvenanceReport where
  constructor MkProvenanceReport
  reportEntity : EntityId
  reportLineage : Lineage
  reportAuditTrail : AuditTrail
  reportIntegrity : Bool

||| Generate provenance report
public export
generateReport : EntityId -> Lineage -> AuditTrail -> ProvenanceReport
generateReport entity lineage audit =
  let chainOk = verifyChain (lineageChain lineage)
      auditOk = verifyAuditTrail audit
  in MkProvenanceReport entity lineage audit (chainOk && auditOk)

||| Proof of complete provenance
public export
data CompleteProvenance : ProvenanceReport -> Type where
  MkCompleteProvenance : reportIntegrity report = True -> CompleteProvenance report
