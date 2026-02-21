// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath

/// Safe cryptographic operations for Dart.
library;

import 'dart:convert';
import 'dart:math';
import 'dart:typed_data';

import 'package:crypto/crypto.dart';

/// Safe cryptographic operations.
class SafeCrypto {
  static final Random _secureRandom = Random.secure();

  /// Constant-time string comparison to prevent timing attacks.
  static bool constantTimeEquals(String a, String b) {
    final bytesA = utf8.encode(a);
    final bytesB = utf8.encode(b);
    return constantTimeEqualsBytes(bytesA, bytesB);
  }

  /// Constant-time byte comparison.
  static bool constantTimeEqualsBytes(List<int> a, List<int> b) {
    if (a.length != b.length) return false;

    var result = 0;
    for (var i = 0; i < a.length; i++) {
      result |= a[i] ^ b[i];
    }
    return result == 0;
  }

  /// Generate cryptographically secure random bytes.
  static Uint8List randomBytes(int count) {
    final bytes = Uint8List(count);
    for (var i = 0; i < count; i++) {
      bytes[i] = _secureRandom.nextInt(256);
    }
    return bytes;
  }

  /// Generate random bytes as hex string.
  static String randomHex(int byteCount) {
    final bytes = randomBytes(byteCount);
    return bytes.map((b) => b.toRadixString(16).padLeft(2, '0')).join();
  }

  /// Generate random bytes as base64 string.
  static String randomBase64(int byteCount) {
    return base64.encode(randomBytes(byteCount));
  }

  /// Generate URL-safe random string.
  static String randomUrlSafe(int byteCount) {
    return base64Url.encode(randomBytes(byteCount)).replaceAll('=', '');
  }

  /// Generate random integer in range [min, max].
  static int randomInt(int min, int max) {
    if (min > max) {
      final temp = min;
      min = max;
      max = temp;
    }

    final range = max - min + 1;
    return min + _secureRandom.nextInt(range);
  }

  /// Generate a secure token (for sessions, CSRF, etc).
  static String generateToken({int length = 32}) {
    return randomUrlSafe(length);
  }

  /// Hash a string with SHA-256.
  static String sha256Hash(String input) {
    return sha256.convert(utf8.encode(input)).toString();
  }

  /// Hash bytes with SHA-256.
  static String sha256HashBytes(List<int> input) {
    return sha256.convert(input).toString();
  }

  /// Hash a string with SHA-512.
  static String sha512Hash(String input) {
    return sha512.convert(utf8.encode(input)).toString();
  }

  /// Hash bytes with SHA-512.
  static String sha512HashBytes(List<int> input) {
    return sha512.convert(input).toString();
  }

  /// Compute HMAC-SHA256.
  static String hmacSha256(String key, String message) {
    final hmacInstance = Hmac(sha256, utf8.encode(key));
    return hmacInstance.convert(utf8.encode(message)).toString();
  }

  /// Compute HMAC-SHA256 with bytes.
  static String hmacSha256Bytes(List<int> key, List<int> message) {
    final hmacInstance = Hmac(sha256, key);
    return hmacInstance.convert(message).toString();
  }

  /// Compute HMAC-SHA512.
  static String hmacSha512(String key, String message) {
    final hmacInstance = Hmac(sha512, utf8.encode(key));
    return hmacInstance.convert(utf8.encode(message)).toString();
  }

  /// Verify HMAC using constant-time comparison.
  static bool verifyHmacSha256(String key, String message, String expectedMac) {
    final actualMac = hmacSha256(key, message);
    return constantTimeEquals(actualMac, expectedMac);
  }

  /// Verify HMAC-SHA512 using constant-time comparison.
  static bool verifyHmacSha512(String key, String message, String expectedMac) {
    final actualMac = hmacSha512(key, message);
    return constantTimeEquals(actualMac, expectedMac);
  }

  /// Hash a string with MD5 (NOT for security, only for checksums).
  static String md5Hash(String input) {
    return md5.convert(utf8.encode(input)).toString();
  }

  /// Derive a key using PBKDF2-SHA256.
  /// Returns hex-encoded derived key.
  static String pbkdf2(
    String password,
    String salt, {
    int iterations = 100000,
    int keyLength = 32,
  }) {
    // PBKDF2 implementation
    final hmacInstance = Hmac(sha256, utf8.encode(password));
    final saltBytes = utf8.encode(salt);

    final derivedKey = Uint8List(keyLength);
    var offset = 0;
    var blockNum = 1;

    while (offset < keyLength) {
      // U1 = PRF(Password, Salt || INT_32_BE(i))
      final blockData = Uint8List(saltBytes.length + 4);
      blockData.setRange(0, saltBytes.length, saltBytes);
      blockData[saltBytes.length] = (blockNum >> 24) & 0xff;
      blockData[saltBytes.length + 1] = (blockNum >> 16) & 0xff;
      blockData[saltBytes.length + 2] = (blockNum >> 8) & 0xff;
      blockData[saltBytes.length + 3] = blockNum & 0xff;

      var u = hmacInstance.convert(blockData).bytes;
      final block = Uint8List.fromList(u);

      // U2 through Uc
      for (var j = 1; j < iterations; j++) {
        u = hmacInstance.convert(u).bytes;
        for (var k = 0; k < block.length; k++) {
          block[k] ^= u[k];
        }
      }

      // Copy to derived key
      final remaining = keyLength - offset;
      final toCopy = remaining < block.length ? remaining : block.length;
      derivedKey.setRange(offset, offset + toCopy, block);

      offset += toCopy;
      blockNum++;
    }

    return derivedKey.map((b) => b.toRadixString(16).padLeft(2, '0')).join();
  }

  /// Generate a random password.
  static String generatePassword({
    int length = 16,
    bool includeUppercase = true,
    bool includeLowercase = true,
    bool includeNumbers = true,
    bool includeSymbols = true,
  }) {
    final chars = StringBuffer();

    if (includeLowercase) chars.write('abcdefghijklmnopqrstuvwxyz');
    if (includeUppercase) chars.write('ABCDEFGHIJKLMNOPQRSTUVWXYZ');
    if (includeNumbers) chars.write('0123456789');
    if (includeSymbols) chars.write('!@#\$%^&*()_+-=[]{}|;:,.<>?');

    final charset = chars.toString();
    if (charset.isEmpty) return '';

    final password = StringBuffer();
    for (var i = 0; i < length; i++) {
      password.write(charset[_secureRandom.nextInt(charset.length)]);
    }

    return password.toString();
  }

  /// Securely wipe a byte array (best effort).
  static void secureWipe(List<int> data) {
    for (var i = 0; i < data.length; i++) {
      data[i] = 0;
    }
  }

  /// Check if a source of randomness is cryptographically secure.
  /// In Dart, Random.secure() is always available.
  static bool isSecureRandomAvailable() => true;
}
