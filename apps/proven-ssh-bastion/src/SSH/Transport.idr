-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
--
-- SSH Transport Layer (RFC 4253)
--
-- Defines the SSH version exchange, key exchange initialisation, and
-- algorithm negotiation types.  Every value is bounds-checked and
-- well-typed — malformed version strings or unknown algorithms are
-- returned as typed errors, never as crashes.

module SSH.Transport

%default total

-- ============================================================================
-- Key Exchange Methods (RFC 4253 Section 7)
-- ============================================================================

||| Supported key exchange algorithms.
||| Each constructor corresponds to a named algorithm from the SSH
||| algorithm registry (IANA).
public export
data KexMethod : Type where
  ||| diffie-hellman-group14-sha256 (RFC 4253 + RFC 8268)
  DiffieHellmanGroup14SHA256 : KexMethod
  ||| curve25519-sha256 (RFC 8731)
  Curve25519SHA256            : KexMethod
  ||| diffie-hellman-group16-sha512 (RFC 8268)
  DiffieHellmanGroup16SHA512 : KexMethod
  ||| diffie-hellman-group18-sha512 (RFC 8268)
  DiffieHellmanGroup18SHA512 : KexMethod
  ||| ecdh-sha2-nistp256 (RFC 5656)
  EcdhSHA2NistP256           : KexMethod
  ||| ecdh-sha2-nistp384 (RFC 5656)
  EcdhSHA2NistP384           : KexMethod

public export
Eq KexMethod where
  DiffieHellmanGroup14SHA256 == DiffieHellmanGroup14SHA256 = True
  Curve25519SHA256           == Curve25519SHA256           = True
  DiffieHellmanGroup16SHA512 == DiffieHellmanGroup16SHA512 = True
  DiffieHellmanGroup18SHA512 == DiffieHellmanGroup18SHA512 = True
  EcdhSHA2NistP256           == EcdhSHA2NistP256           = True
  EcdhSHA2NistP384           == EcdhSHA2NistP384           = True
  _                          == _                          = False

public export
Show KexMethod where
  show DiffieHellmanGroup14SHA256 = "diffie-hellman-group14-sha256"
  show Curve25519SHA256           = "curve25519-sha256"
  show DiffieHellmanGroup16SHA512 = "diffie-hellman-group16-sha512"
  show DiffieHellmanGroup18SHA512 = "diffie-hellman-group18-sha512"
  show EcdhSHA2NistP256           = "ecdh-sha2-nistp256"
  show EcdhSHA2NistP384           = "ecdh-sha2-nistp384"

-- ============================================================================
-- Host Key Algorithms (RFC 4253 Section 6.6)
-- ============================================================================

||| Supported host key algorithms for server identity verification.
public export
data HostKeyAlgorithm : Type where
  ||| ssh-ed25519 (RFC 8709)
  SshEd25519    : HostKeyAlgorithm
  ||| rsa-sha2-256 (RFC 8332)
  RsaSHA2_256   : HostKeyAlgorithm
  ||| rsa-sha2-512 (RFC 8332)
  RsaSHA2_512   : HostKeyAlgorithm
  ||| ecdsa-sha2-nistp256 (RFC 5656)
  EcdsaNistP256 : HostKeyAlgorithm

public export
Eq HostKeyAlgorithm where
  SshEd25519    == SshEd25519    = True
  RsaSHA2_256   == RsaSHA2_256   = True
  RsaSHA2_512   == RsaSHA2_512   = True
  EcdsaNistP256 == EcdsaNistP256 = True
  _             == _             = False

public export
Show HostKeyAlgorithm where
  show SshEd25519    = "ssh-ed25519"
  show RsaSHA2_256   = "rsa-sha2-256"
  show RsaSHA2_512   = "rsa-sha2-512"
  show EcdsaNistP256 = "ecdsa-sha2-nistp256"

-- ============================================================================
-- Encryption Algorithms (RFC 4253 Section 6.3)
-- ============================================================================

||| Supported encryption (cipher) algorithms.
public export
data CipherAlgorithm : Type where
  ||| chacha20-poly1305@openssh.com
  ChaCha20Poly1305 : CipherAlgorithm
  ||| aes256-gcm@openssh.com
  Aes256GCM        : CipherAlgorithm
  ||| aes128-gcm@openssh.com
  Aes128GCM        : CipherAlgorithm
  ||| aes256-ctr (RFC 4344)
  Aes256CTR        : CipherAlgorithm
  ||| aes192-ctr (RFC 4344)
  Aes192CTR        : CipherAlgorithm
  ||| aes128-ctr (RFC 4344)
  Aes128CTR        : CipherAlgorithm

public export
Eq CipherAlgorithm where
  ChaCha20Poly1305 == ChaCha20Poly1305 = True
  Aes256GCM        == Aes256GCM        = True
  Aes128GCM        == Aes128GCM        = True
  Aes256CTR        == Aes256CTR        = True
  Aes192CTR        == Aes192CTR        = True
  Aes128CTR        == Aes128CTR        = True
  _                == _                = False

public export
Show CipherAlgorithm where
  show ChaCha20Poly1305 = "chacha20-poly1305@openssh.com"
  show Aes256GCM        = "aes256-gcm@openssh.com"
  show Aes128GCM        = "aes128-gcm@openssh.com"
  show Aes256CTR        = "aes256-ctr"
  show Aes192CTR        = "aes192-ctr"
  show Aes128CTR        = "aes128-ctr"

-- ============================================================================
-- MAC Algorithms (RFC 4253 Section 6.4)
-- ============================================================================

||| Supported MAC (message authentication code) algorithms.
public export
data MACAlgorithm : Type where
  ||| hmac-sha2-256-etm@openssh.com
  HmacSHA2_256_ETM : MACAlgorithm
  ||| hmac-sha2-512-etm@openssh.com
  HmacSHA2_512_ETM : MACAlgorithm
  ||| hmac-sha2-256 (RFC 6668)
  HmacSHA2_256     : MACAlgorithm
  ||| hmac-sha2-512 (RFC 6668)
  HmacSHA2_512     : MACAlgorithm

public export
Eq MACAlgorithm where
  HmacSHA2_256_ETM == HmacSHA2_256_ETM = True
  HmacSHA2_512_ETM == HmacSHA2_512_ETM = True
  HmacSHA2_256     == HmacSHA2_256     = True
  HmacSHA2_512     == HmacSHA2_512     = True
  _                == _                = False

public export
Show MACAlgorithm where
  show HmacSHA2_256_ETM = "hmac-sha2-256-etm@openssh.com"
  show HmacSHA2_512_ETM = "hmac-sha2-512-etm@openssh.com"
  show HmacSHA2_256     = "hmac-sha2-256"
  show HmacSHA2_512     = "hmac-sha2-512"

-- ============================================================================
-- Version Exchange (RFC 4253 Section 4.2)
-- ============================================================================

||| Result of parsing a version exchange string.
||| An SSH version string has the form: SSH-protoversion-softwareversion[ SP comment]
public export
record VersionString where
  constructor MkVersionString
  ||| Protocol version — MUST be "2.0" for SSH-2
  protoVersion    : String
  ||| Software identification (our bastion identifies as "proven_0.1")
  softwareVersion : String
  ||| Optional human-readable comment
  comment         : Maybe String

public export
Show VersionString where
  show vs = "SSH-" ++ vs.protoVersion ++ "-" ++ vs.softwareVersion
            ++ maybe "" (\c => " " ++ c) vs.comment

-- ============================================================================
-- Key Exchange Init (SSH_MSG_KEXINIT, RFC 4253 Section 7.1)
-- ============================================================================

||| Key exchange initialisation packet listing supported algorithms.
||| Each field is an ordered preference list — the first algorithm
||| acceptable to BOTH sides is selected.
public export
record KexInit where
  constructor MkKexInit
  ||| 16-byte random cookie for replay protection
  cookie              : Vect 16 Bits8
  ||| Key exchange methods, in preference order
  kexAlgorithms       : List KexMethod
  ||| Host key algorithms, in preference order
  hostKeyAlgorithms   : List HostKeyAlgorithm
  ||| Ciphers for client-to-server, in preference order
  ciphersClientServer : List CipherAlgorithm
  ||| Ciphers for server-to-client, in preference order
  ciphersServerClient : List CipherAlgorithm
  ||| MACs for client-to-server, in preference order
  macsClientServer    : List MACAlgorithm
  ||| MACs for server-to-client, in preference order
  macsServerClient    : List MACAlgorithm

-- ============================================================================
-- Algorithm Negotiation (RFC 4253 Section 7.1)
-- ============================================================================

||| Negotiation errors — returned as values, never as crashes.
public export
data NegotiationError : Type where
  ||| No common key exchange algorithm found
  NoCommonKex     : NegotiationError
  ||| No common host key algorithm found
  NoCommonHostKey : NegotiationError
  ||| No common cipher found
  NoCommonCipher  : NegotiationError
  ||| No common MAC found
  NoCommonMAC     : NegotiationError

public export
Show NegotiationError where
  show NoCommonKex     = "No common key exchange algorithm"
  show NoCommonHostKey = "No common host key algorithm"
  show NoCommonCipher  = "No common cipher algorithm"
  show NoCommonMAC     = "No common MAC algorithm"

||| Pick the first element from `client` that also appears in `server`.
||| This is the standard SSH negotiation rule (RFC 4253 Section 7.1).
public export
negotiate : Eq a => (client : List a) -> (server : List a) -> Maybe a
negotiate []        _      = Nothing
negotiate (c :: cs) server =
  if any (== c) server
    then Just c
    else negotiate cs server

||| Result of a fully negotiated algorithm set.
public export
record NegotiatedAlgorithms where
  constructor MkNegotiated
  kex       : KexMethod
  hostKey   : HostKeyAlgorithm
  cipherC2S : CipherAlgorithm
  cipherS2C : CipherAlgorithm
  macC2S    : MACAlgorithm
  macS2C    : MACAlgorithm

||| Perform full algorithm negotiation between client and server KexInit.
||| Returns either a negotiation error or the agreed algorithm suite.
public export
negotiateAlgorithms : (client : KexInit) -> (server : KexInit)
                    -> Either NegotiationError NegotiatedAlgorithms
negotiateAlgorithms c s =
  case negotiate c.kexAlgorithms s.kexAlgorithms of
    Nothing  => Left NoCommonKex
    Just kex =>
  case negotiate c.hostKeyAlgorithms s.hostKeyAlgorithms of
    Nothing  => Left NoCommonHostKey
    Just hk  =>
  case negotiate c.ciphersClientServer s.ciphersClientServer of
    Nothing  => Left NoCommonCipher
    Just cc  =>
  case negotiate c.ciphersServerClient s.ciphersServerClient of
    Nothing  => Left NoCommonCipher
    Just cs  =>
  case negotiate c.macsClientServer s.macsClientServer of
    Nothing  => Left NoCommonMAC
    Just mc  =>
  case negotiate c.macsServerClient s.macsServerClient of
    Nothing  => Left NoCommonMAC
    Just ms  =>
  Right (MkNegotiated kex hk cc cs mc ms)

-- ============================================================================
-- Transport errors
-- ============================================================================

||| Transport-layer errors that can occur during connection setup.
public export
data TransportError : Type where
  ||| Peer sent an invalid version string
  InvalidVersionString : (raw : String) -> TransportError
  ||| Peer's protocol version is not 2.0
  UnsupportedVersion   : (version : String) -> TransportError
  ||| Negotiation failed
  NegotiationFailed    : NegotiationError -> TransportError
  ||| Packet too large (exceeds maxPacketSize)
  PacketTooLarge       : (size : Nat) -> TransportError
  ||| MAC verification failed
  MACVerifyFailed      : TransportError

public export
Show TransportError where
  show (InvalidVersionString r) = "Invalid SSH version string: " ++ r
  show (UnsupportedVersion v)   = "Unsupported protocol version: " ++ v
  show (NegotiationFailed e)    = "Negotiation failed: " ++ show e
  show (PacketTooLarge s)       = "Packet too large: " ++ show s ++ " bytes"
  show MACVerifyFailed          = "MAC verification failed"
