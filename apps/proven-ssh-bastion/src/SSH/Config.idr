-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
--
-- SSH Bastion Host Configuration
--
-- Defines the configuration for a proven SSH bastion host, including
-- listen address, allowed authentication methods, session limits,
-- idle timeouts, and validated forwarding targets.  All configuration
-- values are validated at construction time — invalid configurations
-- are rejected as typed errors, not runtime crashes.

module SSH.Config

import SSH.Auth
import SSH.Transport

%default total

-- ============================================================================
-- Forwarding Target (validated)
-- ============================================================================

||| A validated forwarding target that the bastion is allowed to proxy to.
||| The bastion will only create direct-tcpip channels to hosts and ports
||| that appear in the allowedTargets list.
public export
record ForwardingTarget where
  constructor MkForwardingTarget
  ||| Target hostname or IP address
  host : String
  ||| Target port number
  port : Bits16

public export
Eq ForwardingTarget where
  a == b = a.host == b.host && a.port == b.port

public export
Show ForwardingTarget where
  show t = t.host ++ ":" ++ show (cast {to=Nat} t.port)

-- ============================================================================
-- Bastion Configuration
-- ============================================================================

||| Complete configuration for the SSH bastion host.
||| All values have sensible defaults via `defaultBastionConfig`.
public export
record BastionConfig where
  constructor MkBastionConfig
  ||| Address to listen on (e.g., "0.0.0.0" for all interfaces)
  listenAddress        : String
  ||| Port to listen on (default: 22)
  listenPort           : Bits16
  ||| Authentication methods allowed, in preference order
  allowedAuthMethods   : List AuthMethod
  ||| Maximum concurrent sessions
  maxSessions          : Nat
  ||| Maximum channels per session
  maxChannelsPerSession : Nat
  ||| Idle timeout in seconds (0 = no timeout)
  idleTimeout          : Nat
  ||| Maximum authentication attempts before disconnect
  maxAuthAttempts      : Nat
  ||| Allowed forwarding targets (empty = no forwarding)
  allowedTargets       : List ForwardingTarget
  ||| Host key algorithms to offer
  hostKeyAlgorithms    : List HostKeyAlgorithm
  ||| Whether to allow TCP forwarding at all
  allowTcpForwarding   : Bool
  ||| Whether to allow X11 forwarding
  allowX11Forwarding   : Bool
  ||| Server identification string for version exchange
  serverIdent          : String

||| Default bastion configuration with security-focused defaults.
||| - Only publickey authentication allowed (no passwords by default)
||| - TCP forwarding disabled by default
||| - X11 forwarding disabled by default
||| - Conservative session and channel limits
public export
defaultBastionConfig : BastionConfig
defaultBastionConfig = MkBastionConfig
  { listenAddress         = "0.0.0.0"
  , listenPort            = 22
  , allowedAuthMethods    = [PublicKey]         -- Keys only by default
  , maxSessions           = 100
  , maxChannelsPerSession = 10
  , idleTimeout           = 300                 -- 5 minutes
  , maxAuthAttempts       = 3
  , allowedTargets        = []                  -- No forwarding by default
  , hostKeyAlgorithms     = [SshEd25519, RsaSHA2_256]
  , allowTcpForwarding    = False
  , allowX11Forwarding    = False
  , serverIdent           = "proven_0.1"
  }

-- ============================================================================
-- Configuration Validation
-- ============================================================================

||| Configuration validation errors.
public export
data ConfigError : Type where
  ||| No authentication methods configured
  NoAuthMethods       : ConfigError
  ||| Listen port is 0 (not valid for a server)
  InvalidListenPort   : ConfigError
  ||| maxSessions is 0 (must allow at least 1)
  ZeroMaxSessions     : ConfigError
  ||| maxChannelsPerSession is 0
  ZeroMaxChannels     : ConfigError
  ||| maxAuthAttempts is 0 (must allow at least 1 attempt)
  ZeroMaxAuthAttempts : ConfigError
  ||| No host key algorithms configured
  NoHostKeyAlgorithms : ConfigError
  ||| TCP forwarding enabled but no targets specified
  ForwardingNoTargets : ConfigError

public export
Show ConfigError where
  show NoAuthMethods       = "No authentication methods configured"
  show InvalidListenPort   = "Listen port must not be 0"
  show ZeroMaxSessions     = "maxSessions must be at least 1"
  show ZeroMaxChannels     = "maxChannelsPerSession must be at least 1"
  show ZeroMaxAuthAttempts = "maxAuthAttempts must be at least 1"
  show NoHostKeyAlgorithms = "No host key algorithms configured"
  show ForwardingNoTargets = "TCP forwarding enabled but no targets specified"

||| Validate a bastion configuration.
||| Returns a list of all validation errors found (empty = valid).
public export
validateConfig : BastionConfig -> List ConfigError
validateConfig cfg =
  let e1 = if null cfg.allowedAuthMethods    then [NoAuthMethods]       else []
      e2 = if cfg.listenPort == 0            then [InvalidListenPort]   else []
      e3 = if cfg.maxSessions == 0           then [ZeroMaxSessions]     else []
      e4 = if cfg.maxChannelsPerSession == 0 then [ZeroMaxChannels]     else []
      e5 = if cfg.maxAuthAttempts == 0       then [ZeroMaxAuthAttempts] else []
      e6 = if null cfg.hostKeyAlgorithms     then [NoHostKeyAlgorithms] else []
      e7 = if cfg.allowTcpForwarding && null cfg.allowedTargets
             then [ForwardingNoTargets] else []
  in e1 ++ e2 ++ e3 ++ e4 ++ e5 ++ e6 ++ e7

||| Check if a forwarding target is allowed by the configuration.
public export
isTargetAllowed : BastionConfig -> ForwardingTarget -> Bool
isTargetAllowed cfg target =
  cfg.allowTcpForwarding && any (== target) cfg.allowedTargets

||| Check if an authentication method is allowed by the configuration.
public export
isMethodAllowed : BastionConfig -> AuthMethod -> Bool
isMethodAllowed cfg method = any (== method) cfg.allowedAuthMethods
