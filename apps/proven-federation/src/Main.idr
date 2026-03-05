-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
--
-- Main: Entry point for the proven-federation server skeleton.
-- Prints the server identity, port, configuration constants,
-- and enumerates all protocol type constructors.

module Main

import Federation

%default total

------------------------------------------------------------------------
-- All constructors for each protocol type, collected as lists for
-- display purposes.
------------------------------------------------------------------------

||| All ActivityPub activity types.
allActivityTypes : List ActivityType
allActivityTypes = [Create, Update, Delete, Follow, Accept, Reject, Announce, Like, Undo, Block, Flag]

||| All actor types.
allActorTypes : List ActorType
allActorTypes = [Person, Service, Application, Group, Organization]

||| All delivery statuses.
allDeliveryStatuses : List DeliveryStatus
allDeliveryStatuses = [Pending, Delivered, Failed, Rejected, Deferred]

||| All trust levels.
allTrustLevels : List TrustLevel
allTrustLevels = [SelfSigned, PeerVerified, FederationTrusted, Revoked, Unknown]

||| All object types.
allObjectTypes : List ObjectType
allObjectTypes = [Note, Article, Image, Video, Audio, Document, Event, Collection, OrderedCollection]

------------------------------------------------------------------------
-- Main entry point
------------------------------------------------------------------------

main : IO ()
main = do
  putStrLn "proven-federation: Federation / Decentralised Identity Server"
  putStrLn $ "  Port:             " ++ show federationPort
  putStrLn $ "  Max payload size: " ++ show maxPayloadSize ++ " bytes"
  putStrLn $ "  Delivery timeout: " ++ show deliveryTimeout ++ "s"
  putStrLn $ "  Max recursion:    " ++ show maxRecursion
  putStrLn ""
  putStrLn $ "ActivityType:   " ++ show allActivityTypes
  putStrLn $ "ActorType:      " ++ show allActorTypes
  putStrLn $ "DeliveryStatus: " ++ show allDeliveryStatuses
  putStrLn $ "TrustLevel:     " ++ show allTrustLevels
  putStrLn $ "ObjectType:     " ++ show allObjectTypes
