-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
||| SafeSupplyChain - Software supply chain provenance validation
|||
||| Provides type-safe SLSA provenance, SBOM validation, and
||| attestation chain verification. Complements SafeSRI and SafeDigest.
||| Prevents: tampered builds, unattested artifacts, provenance forgery.
module Proven.SafeSupplyChain

import Data.String
import Data.List
import Data.Nat

%default total

-- ============================================================================
-- SLSA LEVELS
-- ============================================================================

||| SLSA Build Level (v1.0)
public export
data SLSALevel = L0 | L1 | L2 | L3 | L4

public export
Show SLSALevel where
  show L0 = "SLSA L0"; show L1 = "SLSA L1"
  show L2 = "SLSA L2"; show L3 = "SLSA L3"; show L4 = "SLSA L4"

public export
Eq SLSALevel where
  L0 == L0 = True; L1 == L1 = True; L2 == L2 = True
  L3 == L3 = True; L4 == L4 = True; _ == _ = False

public export
Ord SLSALevel where
  compare L0 L0 = EQ; compare L0 _ = LT
  compare L1 L0 = GT; compare L1 L1 = EQ; compare L1 _ = LT
  compare L2 L0 = GT; compare L2 L1 = GT; compare L2 L2 = EQ; compare L2 _ = LT
  compare L3 L4 = LT; compare L3 L3 = EQ; compare L3 _ = GT
  compare L4 L4 = EQ; compare L4 _ = GT

-- ============================================================================
-- PROVENANCE
-- ============================================================================

||| Build provenance attestation
public export
record Provenance where
  constructor MkProvenance
  buildType     : String     -- e.g., "https://slsa.dev/provenance/v1"
  builderID     : String     -- Who built it
  sourceURI     : String     -- Where source came from
  sourceDigest  : String     -- Hash of source
  buildConfig   : String     -- Build parameters
  artifactDigest : String    -- Hash of output artifact
  timestamp     : Integer    -- Build timestamp

||| Provenance validation errors
public export
data ProvenanceError =
    MissingBuilderID
  | MissingSourceDigest
  | MissingArtifactDigest
  | StaleProvenance Integer    -- Age in seconds
  | UntrustedBuilder String
  | DigestMismatch String String

public export
Show ProvenanceError where
  show MissingBuilderID = "Builder ID is empty"
  show MissingSourceDigest = "Source digest is empty"
  show MissingArtifactDigest = "Artifact digest is empty"
  show (StaleProvenance age) = "Provenance is " ++ show age ++ "s old"
  show (UntrustedBuilder b) = "Untrusted builder: " ++ b
  show (DigestMismatch e a) = "Digest mismatch: expected " ++ e ++ ", got " ++ a

||| Validate provenance
public export
validateProvenance : Provenance -> List String -> Integer -> List ProvenanceError
validateProvenance prov trustedBuilders nowEpoch =
  let builderErr = if prov.builderID == "" then [MissingBuilderID] else []
      srcErr     = if prov.sourceDigest == "" then [MissingSourceDigest] else []
      artErr     = if prov.artifactDigest == "" then [MissingArtifactDigest] else []
      trustErr   = if not (any (== prov.builderID) trustedBuilders)
                     then [UntrustedBuilder prov.builderID] else []
      ageErr     = let age = nowEpoch - prov.timestamp
                   in if age > 86400 * 30 then [StaleProvenance age] else []
  in builderErr ++ srcErr ++ artErr ++ trustErr ++ ageErr

-- ============================================================================
-- SBOM (Software Bill of Materials)
-- ============================================================================

||| SBOM format
public export
data SBOMFormat = SPDX | CycloneDX

public export
Show SBOMFormat where
  show SPDX = "SPDX"
  show CycloneDX = "CycloneDX"

||| A dependency in the SBOM
public export
record Dependency where
  constructor MkDependency
  name    : String
  version : String
  license : String
  digest  : String    -- Package hash

||| Dependency risk assessment
public export
data DependencyRisk =
    NoRisk
  | MissingDigest String
  | UnknownLicense String
  | DeprecatedDep String

public export
Show DependencyRisk where
  show NoRisk = "OK"
  show (MissingDigest n) = "No digest for " ++ n
  show (UnknownLicense n) = "Unknown license for " ++ n
  show (DeprecatedDep n) = "Deprecated: " ++ n

||| Assess a single dependency
public export
assessDependency : Dependency -> DependencyRisk
assessDependency dep =
  if dep.digest == "" then MissingDigest dep.name
  else if dep.license == "" then UnknownLicense dep.name
  else NoRisk

||| Assess all dependencies in an SBOM
public export
assessSBOM : List Dependency -> List DependencyRisk
assessSBOM = filter notOk . map assessDependency
  where
    notOk : DependencyRisk -> Bool
    notOk NoRisk = False
    notOk _ = True

-- ============================================================================
-- ATTESTATION CHAIN
-- ============================================================================

||| An attestation in the chain
public export
record Attestation where
  constructor MkAttestation
  attType   : String    -- e.g., "https://in-toto.io/Statement/v1"
  subject   : String    -- What is attested
  predicate : String    -- Attestation content type
  signer    : String    -- Who signed

||| Verify an attestation chain has no gaps
public export
isChainComplete : List Attestation -> Bool
isChainComplete [] = False
isChainComplete [_] = True
isChainComplete (a :: b :: rest) =
  a.subject == b.subject && isChainComplete (b :: rest)

||| Determine SLSA level from provenance properties
public export
determineSLSALevel : Provenance -> SLSALevel
determineSLSALevel prov =
  if prov.builderID == "" then L0
  else if prov.sourceDigest == "" then L1
  else if prov.buildConfig == "" then L2
  else L3  -- L4 requires additional hermetic build verification
