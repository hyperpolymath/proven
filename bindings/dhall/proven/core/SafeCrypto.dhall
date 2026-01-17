-- SPDX-License-Identifier: PMPL-1.0
-- SPDX-FileCopyrightText: 2025 Hyperpolymath

{-
SafeCrypto - Safe cryptographic primitives

Provides type-safe cryptographic operations markers and configuration.
Actual cryptographic operations must be performed at runtime.

Part of the Proven safety primitives library.
-}

let Prelude = https://prelude.dhall-lang.org/v23.0.0/package.dhall
    sha256:eb693342eb769f782174157eba9b5924cf8ac6793897fc36a31ccbd6f56dafe2

let Natural/lessThan = Prelude.Natural.lessThan
let Natural/greaterThan = Prelude.Natural.greaterThan

-- Hash algorithm types
let HashAlgorithm = < MD5 | SHA1 | SHA224 | SHA256 | SHA384 | SHA512 | SHA3_256 | SHA3_512 | BLAKE2b | BLAKE2s | BLAKE3 >

-- Hash digest (hex-encoded)
let HashDigest = {
    algorithm : HashAlgorithm,
    digest : Text,
    length : Natural
}

-- Expected hash lengths (in hex characters)
let hashLength
    : HashAlgorithm -> Natural
    = \(alg : HashAlgorithm) ->
        merge {
            MD5 = 32,
            SHA1 = 40,
            SHA224 = 56,
            SHA256 = 64,
            SHA384 = 96,
            SHA512 = 128,
            SHA3_256 = 64,
            SHA3_512 = 128,
            BLAKE2b = 128,
            BLAKE2s = 64,
            BLAKE3 = 64
        } alg

-- Create hash digest marker
let mkHashDigest
    : HashAlgorithm -> Text -> HashDigest
    = \(alg : HashAlgorithm) -> \(digest : Text) ->
        { algorithm = alg, digest = digest, length = hashLength alg }

-- Symmetric encryption algorithms
let SymmetricAlgorithm = < AES128_GCM | AES256_GCM | ChaCha20_Poly1305 | AES128_CBC | AES256_CBC >

-- Key sizes in bits
let keySize
    : SymmetricAlgorithm -> Natural
    = \(alg : SymmetricAlgorithm) ->
        merge {
            AES128_GCM = 128,
            AES256_GCM = 256,
            ChaCha20_Poly1305 = 256,
            AES128_CBC = 128,
            AES256_CBC = 256
        } alg

-- Symmetric key (opaque - actual key is runtime)
let SymmetricKey = {
    algorithm : SymmetricAlgorithm,
    keyId : Optional Text
}

-- Create symmetric key marker
let mkSymmetricKey
    : SymmetricAlgorithm -> Optional Text -> SymmetricKey
    = \(alg : SymmetricAlgorithm) -> \(keyId : Optional Text) ->
        { algorithm = alg, keyId = keyId }

-- Asymmetric algorithm types
let AsymmetricAlgorithm = < RSA2048 | RSA3072 | RSA4096 | Ed25519 | Ed448 | P256 | P384 | P521 | X25519 >

-- Key pair marker
let KeyPair = {
    algorithm : AsymmetricAlgorithm,
    keyId : Optional Text,
    hasPrivate : Bool
}

-- Create key pair marker
let mkKeyPair
    : AsymmetricAlgorithm -> Optional Text -> KeyPair
    = \(alg : AsymmetricAlgorithm) -> \(keyId : Optional Text) ->
        { algorithm = alg, keyId = keyId, hasPrivate = True }

-- Public key only
let mkPublicKey
    : AsymmetricAlgorithm -> Optional Text -> KeyPair
    = \(alg : AsymmetricAlgorithm) -> \(keyId : Optional Text) ->
        { algorithm = alg, keyId = keyId, hasPrivate = False }

-- Signature type
let Signature = {
    algorithm : AsymmetricAlgorithm,
    signature : Text,
    keyId : Optional Text
}

-- KDF (Key Derivation Function) types
let KDF = < PBKDF2 | Argon2id | Argon2i | Argon2d | Scrypt | HKDF | BCrypt >

-- KDF parameters
let KDFParams = {
    algorithm : KDF,
    iterations : Optional Natural,
    memoryKB : Optional Natural,
    parallelism : Optional Natural,
    saltLength : Natural,
    keyLength : Natural
}

-- Recommended KDF parameters
let recommendedKDFParams
    : KDF -> KDFParams
    = \(kdf : KDF) ->
        merge {
            PBKDF2 = {
                algorithm = KDF.PBKDF2,
                iterations = Some 600000,
                memoryKB = None Natural,
                parallelism = None Natural,
                saltLength = 16,
                keyLength = 32
            },
            Argon2id = {
                algorithm = KDF.Argon2id,
                iterations = Some 3,
                memoryKB = Some 65536,
                parallelism = Some 4,
                saltLength = 16,
                keyLength = 32
            },
            Argon2i = {
                algorithm = KDF.Argon2i,
                iterations = Some 3,
                memoryKB = Some 65536,
                parallelism = Some 4,
                saltLength = 16,
                keyLength = 32
            },
            Argon2d = {
                algorithm = KDF.Argon2d,
                iterations = Some 3,
                memoryKB = Some 65536,
                parallelism = Some 4,
                saltLength = 16,
                keyLength = 32
            },
            Scrypt = {
                algorithm = KDF.Scrypt,
                iterations = Some 16384,
                memoryKB = Some 8192,
                parallelism = Some 1,
                saltLength = 16,
                keyLength = 32
            },
            HKDF = {
                algorithm = KDF.HKDF,
                iterations = None Natural,
                memoryKB = None Natural,
                parallelism = None Natural,
                saltLength = 32,
                keyLength = 32
            },
            BCrypt = {
                algorithm = KDF.BCrypt,
                iterations = Some 12,
                memoryKB = None Natural,
                parallelism = None Natural,
                saltLength = 16,
                keyLength = 24
            }
        } kdf

-- Encrypted data envelope
let EncryptedEnvelope = {
    algorithm : SymmetricAlgorithm,
    ciphertext : Text,
    nonce : Text,
    tag : Optional Text,
    keyId : Optional Text
}

-- MAC (Message Authentication Code) types
let MACAlgorithm = < HMAC_SHA256 | HMAC_SHA384 | HMAC_SHA512 | Poly1305 | CMAC_AES >

-- MAC tag
let MACTag = {
    algorithm : MACAlgorithm,
    tag : Text
}

-- Random number configuration
let RandomConfig = {
    source : < CSPRNG | Hardware | Combined >,
    entropyBits : Natural
}

-- Default CSPRNG configuration
let defaultRandomConfig
    : RandomConfig
    = {
        source = < CSPRNG | Hardware | Combined >.CSPRNG,
        entropyBits = 256
    }

-- Certificate type
let CertificateType = < X509 | OpenPGP | SSH >

-- Certificate metadata
let Certificate = {
    certType : CertificateType,
    subject : Text,
    issuer : Text,
    serialNumber : Text,
    notBefore : Text,
    notAfter : Text,
    fingerprint : HashDigest
}

-- TLS version
let TLSVersion = < TLS1_2 | TLS1_3 >

-- TLS configuration
let TLSConfig = {
    minVersion : TLSVersion,
    maxVersion : TLSVersion,
    cipherSuites : List Text,
    verifyPeer : Bool,
    verifyHostname : Bool
}

-- Recommended TLS configuration
let recommendedTLSConfig
    : TLSConfig
    = {
        minVersion = TLSVersion.TLS1_2,
        maxVersion = TLSVersion.TLS1_3,
        cipherSuites = [
            "TLS_AES_256_GCM_SHA384",
            "TLS_AES_128_GCM_SHA256",
            "TLS_CHACHA20_POLY1305_SHA256"
        ],
        verifyPeer = True,
        verifyHostname = True
    }

in {
    -- Types
    HashAlgorithm,
    HashDigest,
    SymmetricAlgorithm,
    SymmetricKey,
    AsymmetricAlgorithm,
    KeyPair,
    Signature,
    KDF,
    KDFParams,
    EncryptedEnvelope,
    MACAlgorithm,
    MACTag,
    RandomConfig,
    CertificateType,
    Certificate,
    TLSVersion,
    TLSConfig,

    -- Constructors
    mkHashDigest,
    mkSymmetricKey,
    mkKeyPair,
    mkPublicKey,

    -- Utilities
    hashLength,
    keySize,
    recommendedKDFParams,
    recommendedTLSConfig,
    defaultRandomConfig
}
