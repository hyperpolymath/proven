-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
--
-- BGP Peer Management
--
-- Manages the state of configured BGP peers, including their FSM sessions,
-- associated routes, and connection parameters.

module BGP.Peer

import BGP.FSM
import BGP.Route
import BGP.Message

%default total

-- ============================================================================
-- Peer configuration
-- ============================================================================

||| Static configuration for a BGP peer.
public export
record PeerConfig where
  constructor MkPeerConfig
  peerAddress   : Bits32    -- IPv4 address of the peer
  peerAS        : Bits32    -- Autonomous system number
  holdTime      : Bits16    -- Proposed hold time (0 = no keepalives)
  connectRetry  : Nat       -- Connect retry interval in seconds
  passive       : Bool      -- Wait for incoming connections only
  description   : String    -- Human-readable peer description

-- ============================================================================
-- Peer runtime state
-- ============================================================================

||| Complete state for a BGP peer at runtime.
public export
record PeerState where
  constructor MkPeerState
  config        : PeerConfig
  session       : BGPSession
  adjRibIn      : AdjRIBIn       -- Routes received from this peer
  adjRibOut     : AdjRIBOut      -- Routes to send to this peer
  routesReceived : Nat           -- Total routes received
  routesSent     : Nat           -- Total routes sent
  lastError      : Maybe String  -- Last error message

||| Create initial peer state from configuration.
public export
newPeerState : (localAS : Bits32) -> PeerConfig -> PeerState
newPeerState localAS cfg = MkPeerState
  { config         = cfg
  , session        = newSession localAS cfg.peerAS
  , adjRibIn       = []
  , adjRibOut      = []
  , routesReceived = 0
  , routesSent     = 0
  , lastError      = Nothing
  }

||| Apply a BGP event to a peer, returning updated state and actions.
public export
peerApplyEvent : PeerState -> BGPEvent -> (PeerState, List BGPAction)
peerApplyEvent peer event =
  let (newSession, actions) = applyEvent peer.session event
      -- If transitioning away from Established, clear routes
      newPeer = if isEstablished peer.session && not (isEstablished newSession)
                then { session     := newSession
                     , adjRibIn    := []
                     , adjRibOut   := []
                     } peer
                else { session := newSession } peer
  in (newPeer, actions)

||| Process an UPDATE message from this peer.
public export
peerProcessUpdate : PeerState -> UpdateMessage -> PeerState
peerProcessUpdate peer update =
  let -- Remove withdrawn routes
      rib1 = foldl (\rib, pfx => withdrawRoute peer.config.peerAddress pfx rib)
                   peer.adjRibIn
                   update.withdrawnRoutes
      -- Add new routes from NLRI
      newEntries = map (\pfx => MkRouteEntry
        { prefix     = pfx
        , attributes = update.pathAttributes
        , peerAddr   = peer.config.peerAddress
        , peerAS     = peer.config.peerAS
        , isValid    = True
        , isBestPath = False
        }) update.nlri
      rib2 = newEntries ++ rib1
  in { adjRibIn       := rib2
     , routesReceived := peer.routesReceived + length update.nlri
     } peer

||| Check if this peer's session is established.
public export
isPeerEstablished : PeerState -> Bool
isPeerEstablished peer = isEstablished peer.session

||| Get the current FSM state name for this peer.
public export
peerStateName : PeerState -> String
peerStateName peer = show peer.session.currentState
