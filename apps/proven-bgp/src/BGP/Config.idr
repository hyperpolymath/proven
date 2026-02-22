-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
--
-- BGP Router Configuration
--
-- Defines the global configuration for a proven-bgp router instance.

module BGP.Config

import BGP.Peer

%default total

-- ============================================================================
-- Router configuration
-- ============================================================================

||| Global BGP router configuration.
public export
record RouterConfig where
  constructor MkRouterConfig
  routerID   : Bits32          -- BGP Identifier (typically an IPv4 address)
  localAS    : Bits32          -- Local Autonomous System number
  listenAddr : Bits32          -- Address to listen on (0 = all interfaces)
  listenPort : Bits16          -- Port to listen on (default 179)
  peers      : List PeerConfig -- Configured BGP peers
  logLevel   : LogLevel        -- Logging verbosity

||| Logging levels.
public export
data LogLevel : Type where
  LogError   : LogLevel
  LogWarning : LogLevel
  LogInfo    : LogLevel
  LogDebug   : LogLevel

||| Default configuration with sensible defaults.
public export
defaultConfig : (routerID : Bits32) -> (localAS : Bits32) -> RouterConfig
defaultConfig rid las = MkRouterConfig
  { routerID   = rid
  , localAS    = las
  , listenAddr = 0          -- All interfaces
  , listenPort = 179        -- Standard BGP port
  , peers      = []
  , logLevel   = LogInfo
  }

||| Add a peer to the router configuration.
public export
addPeer : PeerConfig -> RouterConfig -> RouterConfig
addPeer peer cfg = { peers $= (peer ::) } cfg

||| Create a basic peer configuration.
public export
simplePeer : (address : Bits32) -> (peerAS : Bits32) -> PeerConfig
simplePeer addr as = MkPeerConfig
  { peerAddress  = addr
  , peerAS       = as
  , holdTime     = 90      -- 90 seconds default
  , connectRetry = 30      -- 30 seconds default
  , passive      = False
  , description  = ""
  }
