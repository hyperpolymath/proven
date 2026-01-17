// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

/**
 * SafeProvenance - Data provenance tracking that cannot crash.
 *
 * Provides immutable audit trails, data lineage tracking, and chain-of-custody
 * verification for data integrity and compliance requirements.
 */

/** Error types for provenance operations */
type provenanceError =
  | InvalidTimestamp
  | ChainBroken
  | MaxEntriesReached
  | DuplicateEntry
  | InvalidHash

/** Type of provenance event */
type eventType =
  | Created
  | Modified
  | Accessed
  | Copied
  | Transformed
  | Validated
  | Exported
  | Imported
  | Deleted
  | Restored
  | Merged
  | Split

/** Convert event type to human-readable string */
let eventTypeToString = (et: eventType): string => {
  switch et {
  | Created => "created"
  | Modified => "modified"
  | Accessed => "accessed"
  | Copied => "copied"
  | Transformed => "transformed"
  | Validated => "validated"
  | Exported => "exported"
  | Imported => "imported"
  | Deleted => "deleted"
  | Restored => "restored"
  | Merged => "merged"
  | Split => "split"
  }
}

/** Data classification levels */
type classification =
  | Public
  | Internal
  | Confidential
  | Restricted
  | TopSecret

/** Get numeric value of classification */
let classificationValue = (c: classification): int => {
  switch c {
  | Public => 0
  | Internal => 1
  | Confidential => 2
  | Restricted => 3
  | TopSecret => 4
  }
}

/** Check if this classification is at least as restrictive as another */
let classificationIsAtLeast = (self: classification, other: classification): bool => {
  classificationValue(self) >= classificationValue(other)
}

/** Get the more restrictive of two classifications */
let classificationMax = (a: classification, b: classification): classification => {
  if classificationValue(a) > classificationValue(b) {
    a
  } else {
    b
  }
}

/** Source type for data lineage tracking */
type sourceType =
  | Database
  | File
  | Api
  | UserInput
  | SystemGenerated
  | ExternalFeed
  | Derived
  | Unknown

/** Source of data (for lineage tracking) */
type dataSource = {
  sourceType: sourceType,
  identifier: string,
  version: option<string>,
  location: option<string>,
}

/** Create a database source */
let databaseSource = (table: string): dataSource => {
  {
    sourceType: Database,
    identifier: table,
    version: None,
    location: None,
  }
}

/** Create a file source */
let fileSource = (path: string): dataSource => {
  {
    sourceType: File,
    identifier: path,
    version: None,
    location: None,
  }
}

/** Create an API source */
let apiSource = (endpoint: string): dataSource => {
  {
    sourceType: Api,
    identifier: endpoint,
    version: None,
    location: None,
  }
}

/** Create a user input source */
let userInputSource = (userId: string): dataSource => {
  {
    sourceType: UserInput,
    identifier: userId,
    version: None,
    location: None,
  }
}

/** Create a derived data source */
let derivedSource = (from: string): dataSource => {
  {
    sourceType: Derived,
    identifier: from,
    version: None,
    location: None,
  }
}

/** Single provenance entry in the audit trail */
type provenanceEntry = {
  timestamp: float,
  eventType: eventType,
  actor: string,
  description: option<string>,
  source: option<dataSource>,
  previousHash: option<string>,
}

/** Compute a simple hash of an entry */
let computeHash = (entry: provenanceEntry): string => {
  // Simple hash for JavaScript - concatenate fields and hash
  let parts = [
    Belt.Float.toString(entry.timestamp),
    eventTypeToString(entry.eventType),
    entry.actor,
    entry.description->Belt.Option.getWithDefault(""),
  ]
  switch entry.previousHash {
  | Some(h) => {
      let _ = Js.Array2.push(parts, h)
    }
  | None => ()
  }

  // Simple string hash (djb2)
  let str = Js.Array2.join(parts, "|")
  let hash = ref(5381)
  for i in 0 to Js.String2.length(str) - 1 {
    let c = Js.String2.charCodeAt(str, i)->Belt.Float.toInt
    hash := lsl(hash.contents, 5) + hash.contents + c
  }
  Belt.Int.toString(land(hash.contents, 0x7fffffff))
}

/** Maximum number of entries in a chain */
let maxEntries = 1000

/** Provenance chain for tracking data history */
type provenanceChain = {
  dataId: string,
  mutable classification: classification,
  mutable entries: array<provenanceEntry>,
  mutable currentHash: option<string>,
}

/** Initialize a new provenance chain */
let createProvenanceChain = (dataId: string, creator: string, timestamp: float): provenanceChain => {
  let entry: provenanceEntry = {
    timestamp: timestamp,
    eventType: Created,
    actor: creator,
    description: Some("Initial creation"),
    source: None,
    previousHash: None,
  }

  {
    dataId: dataId,
    classification: Internal,
    entries: [entry],
    currentHash: Some(computeHash(entry)),
  }
}

/** Get the last timestamp in the chain */
let getLastTimestamp = (chain: provenanceChain): float => {
  if Belt.Array.length(chain.entries) == 0 {
    0.0
  } else {
    let last = Belt.Array.getUnsafe(chain.entries, Belt.Array.length(chain.entries) - 1)
    last.timestamp
  }
}

/** Get the last entry */
let getLastEntry = (chain: provenanceChain): option<provenanceEntry> => {
  if Belt.Array.length(chain.entries) == 0 {
    None
  } else {
    Some(Belt.Array.getUnsafe(chain.entries, Belt.Array.length(chain.entries) - 1))
  }
}

/** Add a new event to the chain */
let addEvent = (
  chain: provenanceChain,
  eventType: eventType,
  actor: string,
  timestamp: float,
  description: option<string>,
): result<unit, provenanceError> => {
  if Belt.Array.length(chain.entries) >= maxEntries {
    Error(MaxEntriesReached)
  } else if timestamp < getLastTimestamp(chain) {
    Error(InvalidTimestamp)
  } else {
    let entry: provenanceEntry = {
      timestamp: timestamp,
      eventType: eventType,
      actor: actor,
      description: description,
      source: None,
      previousHash: chain.currentHash,
    }

    chain.entries = Belt.Array.concat(chain.entries, [entry])
    chain.currentHash = Some(computeHash(entry))
    Ok()
  }
}

/** Add an event with source information */
let addEventWithSource = (
  chain: provenanceChain,
  eventType: eventType,
  actor: string,
  timestamp: float,
  description: option<string>,
  source: dataSource,
): result<unit, provenanceError> => {
  if Belt.Array.length(chain.entries) >= maxEntries {
    Error(MaxEntriesReached)
  } else if timestamp < getLastTimestamp(chain) {
    Error(InvalidTimestamp)
  } else {
    let entry: provenanceEntry = {
      timestamp: timestamp,
      eventType: eventType,
      actor: actor,
      description: description,
      source: Some(source),
      previousHash: chain.currentHash,
    }

    chain.entries = Belt.Array.concat(chain.entries, [entry])
    chain.currentHash = Some(computeHash(entry))
    Ok()
  }
}

/** Verify the integrity of the chain */
let verifyIntegrity = (chain: provenanceChain): bool => {
  if Belt.Array.length(chain.entries) == 0 {
    true
  } else {
    let prevHash = ref(None)
    let valid = ref(true)

    chain.entries->Belt.Array.forEach(entry => {
      if valid.contents {
        // Check that previous hash matches
        switch (prevHash.contents, entry.previousHash) {
        | (None, None) => ()
        | (Some(expected), Some(actual)) =>
          if expected != actual {
            valid := false
          }
        | _ => valid := false
        }

        prevHash := Some(computeHash(entry))
      }
    })

    // Verify final hash matches stored hash
    if valid.contents {
      switch (chain.currentHash, prevHash.contents) {
      | (Some(stored), Some(computed)) => stored == computed
      | _ => true
      }
    } else {
      false
    }
  }
}

/** Get entries filtered by event type */
let getEventsByType = (chain: provenanceChain, eventType: eventType): array<provenanceEntry> => {
  chain.entries->Belt.Array.keep(entry => entry.eventType == eventType)
}

/** Get all unique actors who have touched this data */
let getActors = (chain: provenanceChain): array<string> => {
  let actors = []
  chain.entries->Belt.Array.forEach(entry => {
    let found = actors->Belt.Array.some(a => a == entry.actor)
    if !found {
      let _ = Js.Array2.push(actors, entry.actor)
    }
  })
  actors
}

/** Check if a specific actor has accessed this data */
let hasActorAccessed = (chain: provenanceChain, actor: string): bool => {
  chain.entries->Belt.Array.some(entry => entry.actor == actor)
}

/** Get the entry count */
let getEntryCount = (chain: provenanceChain): int => {
  Belt.Array.length(chain.entries)
}

/** Set the classification level */
let setClassification = (chain: provenanceChain, classification: classification): unit => {
  chain.classification = classification
}

/** Data lineage node for tracking derivations */
type lineageNode = {
  dataId: string,
  transformation: option<string>,
  mutable parentIds: array<string>,
}

/** Create a lineage node */
let createLineageNode = (dataId: string, transformation: option<string>): lineageNode => {
  {
    dataId: dataId,
    transformation: transformation,
    parentIds: [],
  }
}

/** Add a parent data source */
let addParent = (node: lineageNode, parentId: string): bool => {
  if Belt.Array.length(node.parentIds) >= 8 {
    false
  } else {
    node.parentIds = Belt.Array.concat(node.parentIds, [parentId])
    true
  }
}

/** Check if this data is derived from a specific source */
let isDerivedFrom = (node: lineageNode, sourceId: string): bool => {
  node.parentIds->Belt.Array.some(id => id == sourceId)
}

/** Check if this is root data (no parents) */
let isRoot = (node: lineageNode): bool => {
  Belt.Array.length(node.parentIds) == 0
}

/** Compliance status */
type complianceStatus =
  | Compliant
  | NonCompliant
  | PendingReview
  | Exempted
  | NotApplicable

/** Compliance evidence record */
type complianceRecord = {
  requirement: string,
  status: complianceStatus,
  verifiedAt: float,
  verifiedBy: string,
  evidence: option<string>,
  expiry: option<float>,
}

/** Check if this compliance record is still valid */
let isComplianceValid = (record: complianceRecord, currentTime: float): bool => {
  if record.status != Compliant && record.status != Exempted {
    false
  } else {
    switch record.expiry {
    | Some(exp) => currentTime < exp
    | None => true
    }
  }
}

/** Custody transfer record */
type custodyTransfer = {
  fromCustodian: string,
  toCustodian: string,
  timestamp: float,
  reason: option<string>,
  authorizedBy: option<string>,
}

/** Check if transfer was authorized */
let isTransferAuthorized = (transfer: custodyTransfer): bool => {
  Belt.Option.isSome(transfer.authorizedBy)
}

/** Create a custody transfer record */
let createCustodyTransfer = (
  from: string,
  to: string,
  timestamp: float,
  reason: option<string>,
  authorizedBy: option<string>,
): custodyTransfer => {
  {
    fromCustodian: from,
    toCustodian: to,
    timestamp: timestamp,
    reason: reason,
    authorizedBy: authorizedBy,
  }
}
