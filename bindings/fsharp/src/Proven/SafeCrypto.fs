// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

namespace Proven

/// Safe cryptographic operations.
module SafeCrypto =
    open System
    open System.Security.Cryptography
    open System.Text

    /// Constant-time byte array comparison to prevent timing attacks.
    let constantTimeEqualsBytes (a: byte[]) (b: byte[]) : bool =
        if a.Length <> b.Length then false
        else
            let mutable result = 0
            for i in 0 .. a.Length - 1 do
                result <- result ||| (int a.[i] ^^^ int b.[i])
            result = 0

    /// Constant-time string comparison to prevent timing attacks.
    let constantTimeEquals (a: string) (b: string) : bool =
        constantTimeEqualsBytes (Encoding.UTF8.GetBytes(a)) (Encoding.UTF8.GetBytes(b))

    /// Generate cryptographically secure random bytes.
    let randomBytes (count: int) : byte[] =
        let bytes = Array.zeroCreate<byte> count
        use rng = RandomNumberGenerator.Create()
        rng.GetBytes(bytes)
        bytes

    /// Convert bytes to hex string.
    let bytesToHex (bytes: byte[]) : string =
        bytes |> Array.map (fun b -> sprintf "%02x" b) |> String.concat ""

    /// Generate random bytes as hex string.
    let randomHex (byteCount: int) : string =
        randomBytes byteCount |> bytesToHex

    /// Generate random bytes as base64 string.
    let randomBase64 (byteCount: int) : string =
        Convert.ToBase64String(randomBytes byteCount)

    /// Generate URL-safe random string.
    let randomUrlSafe (byteCount: int) : string =
        Convert.ToBase64String(randomBytes byteCount)
            .Replace("+", "-")
            .Replace("/", "_")
            .TrimEnd('=')

    /// Generate random integer in range [min, max].
    let randomInt (minVal: int) (maxVal: int) : int =
        let lo, hi = if minVal <= maxVal then minVal, maxVal else maxVal, minVal
        use rng = RandomNumberGenerator.Create()
        let bytes = Array.zeroCreate<byte> 4
        rng.GetBytes(bytes)
        let value = BitConverter.ToUInt32(bytes, 0)
        lo + int (uint64 value % uint64 (hi - lo + 1))

    /// Generate a secure token (for sessions, CSRF, etc).
    let generateToken (length: int) : string =
        randomUrlSafe length

    /// Generate token with default length.
    let generateTokenDefault () : string = generateToken 32

    /// Hash a string with SHA-256.
    let sha256 (input: string) : string =
        use hasher = SHA256.Create()
        let bytes = hasher.ComputeHash(Encoding.UTF8.GetBytes(input))
        bytesToHex bytes

    /// Hash bytes with SHA-256.
    let sha256Bytes (input: byte[]) : string =
        use hasher = SHA256.Create()
        bytesToHex (hasher.ComputeHash(input))

    /// Hash a string with SHA-512.
    let sha512 (input: string) : string =
        use hasher = SHA512.Create()
        let bytes = hasher.ComputeHash(Encoding.UTF8.GetBytes(input))
        bytesToHex bytes

    /// Hash bytes with SHA-512.
    let sha512Bytes (input: byte[]) : string =
        use hasher = SHA512.Create()
        bytesToHex (hasher.ComputeHash(input))

    /// Compute HMAC-SHA256.
    let hmacSha256 (key: string) (message: string) : string =
        use hmac = new HMACSHA256(Encoding.UTF8.GetBytes(key))
        bytesToHex (hmac.ComputeHash(Encoding.UTF8.GetBytes(message)))

    /// Compute HMAC-SHA256 with bytes.
    let hmacSha256Bytes (key: byte[]) (message: byte[]) : string =
        use hmac = new HMACSHA256(key)
        bytesToHex (hmac.ComputeHash(message))

    /// Compute HMAC-SHA512.
    let hmacSha512 (key: string) (message: string) : string =
        use hmac = new HMACSHA512(Encoding.UTF8.GetBytes(key))
        bytesToHex (hmac.ComputeHash(Encoding.UTF8.GetBytes(message)))

    /// Verify HMAC using constant-time comparison.
    let verifyHmacSha256 (key: string) (message: string) (expectedMac: string) : bool =
        constantTimeEquals (hmacSha256 key message) expectedMac

    /// Verify HMAC-SHA512 using constant-time comparison.
    let verifyHmacSha512 (key: string) (message: string) (expectedMac: string) : bool =
        constantTimeEquals (hmacSha512 key message) expectedMac

    /// Hash a string with MD5 (NOT for security, only for checksums).
    let md5 (input: string) : string =
        use hasher = MD5.Create()
        bytesToHex (hasher.ComputeHash(Encoding.UTF8.GetBytes(input)))

    /// Derive a key using PBKDF2-SHA256.
    let pbkdf2 (password: string) (salt: string) (iterations: int) (keyLength: int) : string =
        use deriveBytes = new Rfc2898DeriveBytes(
            password,
            Encoding.UTF8.GetBytes(salt),
            iterations,
            HashAlgorithmName.SHA256)
        bytesToHex (deriveBytes.GetBytes(keyLength))

    /// Derive key with default parameters.
    let pbkdf2Default (password: string) (salt: string) : string =
        pbkdf2 password salt 100000 32

    /// Generate a random password.
    let generatePassword (length: int) (includeUppercase: bool) (includeLowercase: bool) (includeNumbers: bool) (includeSymbols: bool) : string =
        let mutable chars = ""
        if includeLowercase then chars <- chars + "abcdefghijklmnopqrstuvwxyz"
        if includeUppercase then chars <- chars + "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
        if includeNumbers then chars <- chars + "0123456789"
        if includeSymbols then chars <- chars + "!@#$%^&*()_+-=[]{}|;:,.<>?"

        if chars.Length = 0 then ""
        else
            use rng = RandomNumberGenerator.Create()
            let bytes = Array.zeroCreate<byte> length
            rng.GetBytes(bytes)
            bytes |> Array.map (fun b -> chars.[int b % chars.Length]) |> String

    /// Generate password with defaults.
    let generatePasswordDefault () : string =
        generatePassword 16 true true true true

    /// Securely wipe a byte array (best effort).
    let secureWipe (data: byte[]) : unit =
        Array.Clear(data, 0, data.Length)

    /// Check if secure random is available.
    let isSecureRandomAvailable () : bool = true
