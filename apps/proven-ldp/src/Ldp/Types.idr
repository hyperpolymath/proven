-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

||| Core protocol types for the proven-ldp Linked Data Platform server.
||| Defines closed sum types per the W3C LDP Recommendation for container
||| types, resource types, preferences, interaction models, and constraints.
module Ldp.Types

%default total

---------------------------------------------------------------------------
-- Container type: LDP container classifications
---------------------------------------------------------------------------

||| W3C LDP container type.
public export
data ContainerType : Type where
  ||| Basic Container (ldp:BasicContainer).
  Basic    : ContainerType
  ||| Direct Container (ldp:DirectContainer).
  Direct   : ContainerType
  ||| Indirect Container (ldp:IndirectContainer).
  Indirect : ContainerType

export
Show ContainerType where
  show Basic    = "ldp:BasicContainer"
  show Direct   = "ldp:DirectContainer"
  show Indirect = "ldp:IndirectContainer"

---------------------------------------------------------------------------
-- Resource type: LDP resource classifications
---------------------------------------------------------------------------

||| W3C LDP resource type.
public export
data ResourceType : Type where
  ||| RDF Source (ldp:RDFSource).
  RDFSource    : ResourceType
  ||| Non-RDF Source (ldp:NonRDFSource), e.g. binary content.
  NonRDFSource : ResourceType
  ||| Container resource (ldp:Container).
  Container    : ResourceType

export
Show ResourceType where
  show RDFSource    = "ldp:RDFSource"
  show NonRDFSource = "ldp:NonRDFSource"
  show Container    = "ldp:Container"

---------------------------------------------------------------------------
-- Preference: client preferences for container responses
---------------------------------------------------------------------------

||| Client preference hints for LDP container responses
||| (Prefer header, RFC 7240).
public export
data Preference : Type where
  ||| Return minimal container representation.
  MinimalContainer    : Preference
  ||| Include containment triples.
  IncludeContainment  : Preference
  ||| Include membership triples.
  IncludeMembership   : Preference
  ||| Omit containment triples.
  OmitContainment     : Preference
  ||| Omit membership triples.
  OmitMembership      : Preference

export
Show Preference where
  show MinimalContainer    = "return=minimal"
  show IncludeContainment  = "include=containment"
  show IncludeMembership   = "include=membership"
  show OmitContainment     = "omit=containment"
  show OmitMembership      = "omit=membership"

---------------------------------------------------------------------------
-- Interaction model: LDP interaction models
---------------------------------------------------------------------------

||| LDP interaction model (determines server behaviour).
public export
data InteractionModel : Type where
  ||| Linked Data Platform Resource.
  LDPR              : InteractionModel
  ||| Linked Data Platform Container (abstract).
  LDPC              : InteractionModel
  ||| LDP Basic Container.
  LDPBasicContainer    : InteractionModel
  ||| LDP Direct Container.
  LDPDirectContainer   : InteractionModel
  ||| LDP Indirect Container.
  LDPIndirectContainer : InteractionModel

export
Show InteractionModel where
  show LDPR                 = "ldp:Resource"
  show LDPC                 = "ldp:Container"
  show LDPBasicContainer    = "ldp:BasicContainer"
  show LDPDirectContainer   = "ldp:DirectContainer"
  show LDPIndirectContainer = "ldp:IndirectContainer"

---------------------------------------------------------------------------
-- Constraint violation: LDP server constraint violations
---------------------------------------------------------------------------

||| Constraint violations reported by the LDP server.
public export
data ConstraintViolation : Type where
  ||| Client attempted to modify membership constants.
  MembershipConstant       : ConstraintViolation
  ||| Client attempted to modify containment triples.
  ContainsTriplesModified  : ConstraintViolation
  ||| Client attempted to modify server-managed properties.
  ServerManaged            : ConstraintViolation
  ||| Interaction model type conflict.
  TypeConflict             : ConstraintViolation

export
Show ConstraintViolation where
  show MembershipConstant      = "MembershipConstant"
  show ContainsTriplesModified = "ContainsTriplesModified"
  show ServerManaged           = "ServerManaged"
  show TypeConflict            = "TypeConflict"
