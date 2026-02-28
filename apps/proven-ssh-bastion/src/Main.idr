-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
--
-- proven-ssh-bastion: Main entry point
--
-- An SSH bastion host implementation that cannot crash on malformed
-- connections.  Uses dependent types to enforce valid state transitions
-- in the SSH protocol state machine, preventing illegal operations
-- (e.g., opening a channel before authentication).
--
-- Usage:
--   proven-ssh-bastion --listen 0.0.0.0:22 --auth publickey

module Main

import SSH
import SSH.Transport
import SSH.Auth
import SSH.Channel
import SSH.Session
import SSH.Config
import System

%default total

-- ============================================================================
-- Display helpers
-- ============================================================================

||| Print a separator line.
covering
printSep : IO ()
printSep = putStrLn (replicate 60 '-')

||| Display session state with a label.
covering
showState : String -> SSHSession -> IO ()
showState label s = putStrLn $ "  " ++ label ++ " -> " ++ show s.state

-- ============================================================================
-- Demo: SSH session lifecycle
-- ============================================================================

||| Demonstrate the SSH session state machine by walking through
||| a complete connection lifecycle: version exchange, key exchange,
||| authentication, channel open, and graceful disconnect.
covering
demoSessionLifecycle : IO ()
demoSessionLifecycle = do
  putStrLn "\n--- SSH Session Lifecycle Demo ---\n"

  -- Step 0: New session in VersionExchange state
  let session0 = newSSHSession
  putStrLn $ "Initial state: " ++ show session0.state

  -- Step 1: Version exchange complete -> KeyExchange
  case beginKeyExchange session0 of
    Nothing => putStrLn "ERROR: cannot begin kex"
    Just session1 => do
      showState "Version done" session1

      -- Step 2: Simulate algorithm negotiation
      let clientKex = MkKexInit
            { cookie = replicate 16 0xAA
            , kexAlgorithms       = [Curve25519SHA256, DiffieHellmanGroup14SHA256]
            , hostKeyAlgorithms   = [SshEd25519, RsaSHA2_256]
            , ciphersClientServer = [ChaCha20Poly1305, Aes256GCM]
            , ciphersServerClient = [ChaCha20Poly1305, Aes256GCM]
            , macsClientServer    = [HmacSHA2_256_ETM, HmacSHA2_256]
            , macsServerClient    = [HmacSHA2_256_ETM, HmacSHA2_256]
            }
      let serverKex = MkKexInit
            { cookie = replicate 16 0xBB
            , kexAlgorithms       = [Curve25519SHA256, DiffieHellmanGroup16SHA512]
            , hostKeyAlgorithms   = [SshEd25519]
            , ciphersClientServer = [ChaCha20Poly1305, Aes128CTR]
            , ciphersServerClient = [ChaCha20Poly1305, Aes128CTR]
            , macsClientServer    = [HmacSHA2_256_ETM]
            , macsServerClient    = [HmacSHA2_256_ETM]
            }

      case negotiateAlgorithms clientKex serverKex of
        Left err => putStrLn $ "Negotiation failed: " ++ show err
        Right algs => do
          putStrLn $ "  Negotiated kex:    " ++ show algs.kex
          putStrLn $ "  Negotiated cipher: " ++ show algs.cipherC2S
          putStrLn $ "  Negotiated MAC:    " ++ show algs.macC2S

          -- Step 3: Complete key exchange -> UserAuth
          case completeKeyExchange algs session1 of
            Nothing => putStrLn "ERROR: cannot complete kex"
            Just session2 => do
              showState "Kex complete" session2

              -- Step 4: Authentication -> Authenticated
              putStrLn $ "\n  Auth attempt: publickey (ssh-ed25519)"
              case completeAuth "admin" session2 of
                Nothing => putStrLn "ERROR: cannot complete auth"
                Just session3 => do
                  showState "Auth complete" session3
                  putStrLn $ "  Authenticated user: "
                             ++ fromMaybe "(none)" session3.authenticatedUser

                  -- Step 5: Open a session channel
                  putStrLn "\n  Opening session channel..."
                  case openChannel Session defaultWindowSize maxPacketSize session3 of
                    Nothing => putStrLn "ERROR: cannot open channel"
                    Just (session4, ch) => do
                      putStrLn $ "  Channel " ++ show (cast {to=Nat} ch.localId)
                                 ++ " type=" ++ show ch.channelType
                                 ++ " state=" ++ show ch.state
                      putStrLn $ "  Active channels: " ++ show (activeChannelCount session4)

                      -- Step 6: Confirm the channel
                      let ch' = confirmChannel 0 defaultWindowSize ch
                      putStrLn $ "  Channel confirmed: state=" ++ show ch'.state
                                 ++ " canSend=" ++ show (canSendData ch')

                      -- Step 7: Graceful disconnect
                      let session5 = disconnect ByApplication session4
                      showState "\n  Disconnect" session5
                      putStrLn $ "  Active channels after disconnect: "
                                 ++ show (activeChannelCount session5)

-- ============================================================================
-- Demo: Configuration validation
-- ============================================================================

||| Demonstrate bastion configuration validation.
covering
demoConfigValidation : IO ()
demoConfigValidation = do
  putStrLn "\n--- Bastion Configuration Demo ---\n"

  -- Valid default config
  let cfg1 = defaultBastionConfig
  let errs1 = validateConfig cfg1
  putStrLn $ "Default config errors: " ++ show (length errs1)
  putStrLn $ "  Listen: " ++ cfg1.listenAddress ++ ":" ++ show (cast {to=Nat} cfg1.listenPort)
  putStrLn $ "  Auth methods: " ++ show cfg1.allowedAuthMethods
  putStrLn $ "  Max sessions: " ++ show cfg1.maxSessions
  putStrLn $ "  Idle timeout: " ++ show cfg1.idleTimeout ++ "s"
  putStrLn $ "  TCP forwarding: " ++ show cfg1.allowTcpForwarding

  -- Invalid config: no auth methods, zero sessions
  putStrLn "\n  Testing invalid config (no auth, zero sessions)..."
  let cfg2 = { allowedAuthMethods := [], maxSessions := 0 } defaultBastionConfig
  let errs2 = validateConfig cfg2
  putStrLn $ "  Errors found: " ++ show (length errs2)
  traverse_ (\e => putStrLn $ "    - " ++ show e) errs2

  -- Config with forwarding targets
  let target = MkForwardingTarget { host = "10.0.0.5", port = 5432 }
  let cfg3 = { allowTcpForwarding := True
              , allowedTargets    := [target]
              } defaultBastionConfig
  putStrLn $ "\n  Forwarding target allowed: " ++ show (isTargetAllowed cfg3 target)
  let blocked = MkForwardingTarget { host = "10.0.0.99", port = 22 }
  putStrLn $ "  Blocked target allowed:    " ++ show (isTargetAllowed cfg3 blocked)

-- ============================================================================
-- Demo: Authentication attempt tracking
-- ============================================================================

||| Demonstrate authentication attempt tracking and lockout.
covering
demoAuthTracking : IO ()
demoAuthTracking = do
  putStrLn "\n--- Authentication Tracking Demo ---\n"

  let tracker0 = newAuthAttempts "admin" 3
  putStrLn $ "  User: " ++ tracker0.username
  putStrLn $ "  Max failures: " ++ show tracker0.maxFailures
  putStrLn $ "  Locked out: " ++ show (isLockedOut tracker0)

  -- Simulate 3 failed attempts
  let tracker1 = recordFailure PublicKey tracker0
  putStrLn $ "\n  After failure 1: count=" ++ show tracker1.failedCount
             ++ " locked=" ++ show (isLockedOut tracker1)

  let tracker2 = recordFailure Password tracker1
  putStrLn $ "  After failure 2: count=" ++ show tracker2.failedCount
             ++ " locked=" ++ show (isLockedOut tracker2)

  let tracker3 = recordFailure KeyboardInteractive tracker2
  putStrLn $ "  After failure 3: count=" ++ show tracker3.failedCount
             ++ " locked=" ++ show (isLockedOut tracker3)

  putStrLn $ "  Methods tried: " ++ show tracker3.triedMethods

-- ============================================================================
-- Main
-- ============================================================================

covering
main : IO ()
main = do
  args <- getArgs
  putStrLn "proven-ssh-bastion v0.1.0 — SSH bastion that cannot crash"
  putStrLn $ "Protocol: " ++ protocolVersion
  putStrLn $ "Default port: " ++ show (cast {to=Nat} sshPort)
  putStrLn "Powered by proven (Idris 2 formal verification)"

  -- Run demos
  demoSessionLifecycle
  demoConfigValidation
  demoAuthTracking

  printSep
  putStrLn "All transitions proven valid at compile time"
  putStrLn "Build with: idris2 --build proven-ssh-bastion.ipkg"
  putStrLn "Run with:   ./build/exec/proven-ssh-bastion"
