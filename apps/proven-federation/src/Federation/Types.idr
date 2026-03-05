-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
--
-- Federation.Types: Core protocol types for the federation /
-- decentralised identity server. Covers ActivityPub activity types,
-- actor types, delivery, trust, and object classification.
-- All types are closed sum types with total Show instances.

module Federation.Types

%default total

------------------------------------------------------------------------
-- ActivityType
-- The set of ActivityPub activity types the server handles.
------------------------------------------------------------------------

||| ActivityPub activity types. These correspond to the activities
||| defined in the ActivityStreams vocabulary used by federated
||| social protocols.
public export
data ActivityType : Type where
  ||| Create a new object.
  Create   : ActivityType
  ||| Update an existing object.
  Update   : ActivityType
  ||| Delete an existing object.
  Delete   : ActivityType
  ||| Follow an actor.
  Follow   : ActivityType
  ||| Accept a pending request (e.g. follow request).
  Accept   : ActivityType
  ||| Reject a pending request.
  Reject   : ActivityType
  ||| Announce (boost/share) an object to followers.
  Announce : ActivityType
  ||| Like an object.
  Like     : ActivityType
  ||| Undo a previous activity.
  Undo     : ActivityType
  ||| Block an actor.
  Block    : ActivityType
  ||| Flag content for moderation review.
  Flag     : ActivityType

export
Show ActivityType where
  show Create   = "Create"
  show Update   = "Update"
  show Delete   = "Delete"
  show Follow   = "Follow"
  show Accept   = "Accept"
  show Reject   = "Reject"
  show Announce = "Announce"
  show Like     = "Like"
  show Undo     = "Undo"
  show Block    = "Block"
  show Flag     = "Flag"

------------------------------------------------------------------------
-- ActorType
-- The kinds of actors in the federated system.
------------------------------------------------------------------------

||| The type of actor in the federated identity system. Actors are
||| the entities that send and receive activities.
public export
data ActorType : Type where
  ||| A human user.
  Person       : ActorType
  ||| An automated service (bot, relay, etc.).
  Service      : ActorType
  ||| A client application acting on behalf of a user.
  Application  : ActorType
  ||| A group that aggregates activities from its members.
  Group        : ActorType
  ||| An organisation (may contain multiple persons/services).
  Organization : ActorType

export
Show ActorType where
  show Person       = "Person"
  show Service      = "Service"
  show Application  = "Application"
  show Group        = "Group"
  show Organization = "Organization"

------------------------------------------------------------------------
-- DeliveryStatus
-- The status of activity delivery to a remote inbox.
------------------------------------------------------------------------

||| The delivery status of an activity sent to a remote inbox.
public export
data DeliveryStatus : Type where
  ||| Delivery is queued but not yet attempted.
  Pending   : DeliveryStatus
  ||| Successfully delivered to the remote inbox.
  Delivered : DeliveryStatus
  ||| Delivery failed (remote server unreachable or error).
  Failed    : DeliveryStatus
  ||| Remote server rejected the activity (e.g. blocked domain).
  Rejected  : DeliveryStatus
  ||| Delivery deferred for retry at a later time.
  Deferred  : DeliveryStatus

export
Show DeliveryStatus where
  show Pending   = "Pending"
  show Delivered = "Delivered"
  show Failed    = "Failed"
  show Rejected  = "Rejected"
  show Deferred  = "Deferred"

------------------------------------------------------------------------
-- TrustLevel
-- The trust level assigned to a remote actor or domain.
------------------------------------------------------------------------

||| Trust level assigned to a remote actor or federated domain.
public export
data TrustLevel : Type where
  ||| Identity verified only by the actor's own key (self-signed).
  SelfSigned       : TrustLevel
  ||| Identity verified by one or more peers.
  PeerVerified     : TrustLevel
  ||| Identity trusted by the federation's trust framework.
  FederationTrusted : TrustLevel
  ||| Trust has been revoked (key compromise, abuse, etc.).
  Revoked          : TrustLevel
  ||| Trust level is unknown (no prior interaction).
  Unknown          : TrustLevel

export
Show TrustLevel where
  show SelfSigned        = "SelfSigned"
  show PeerVerified      = "PeerVerified"
  show FederationTrusted = "FederationTrusted"
  show Revoked           = "Revoked"
  show Unknown           = "Unknown"

------------------------------------------------------------------------
-- ObjectType
-- The kinds of objects that can be created, shared, and delivered.
------------------------------------------------------------------------

||| The type of object in the ActivityStreams vocabulary. Objects are
||| the things that activities act upon.
public export
data ObjectType : Type where
  ||| A short text post (toot, tweet, etc.).
  Note              : ObjectType
  ||| A long-form written article.
  Article           : ObjectType
  ||| An image attachment.
  Image             : ObjectType
  ||| A video attachment.
  Video             : ObjectType
  ||| An audio attachment.
  Audio             : ObjectType
  ||| A generic document (PDF, file, etc.).
  Document          : ObjectType
  ||| A calendar event.
  Event             : ObjectType
  ||| An unordered collection of objects.
  Collection        : ObjectType
  ||| An ordered collection of objects (e.g. outbox, inbox).
  OrderedCollection : ObjectType

export
Show ObjectType where
  show Note              = "Note"
  show Article           = "Article"
  show Image             = "Image"
  show Video             = "Video"
  show Audio             = "Audio"
  show Document          = "Document"
  show Event             = "Event"
  show Collection        = "Collection"
  show OrderedCollection = "OrderedCollection"
