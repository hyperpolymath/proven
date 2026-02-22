// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/**
 * Benchmarks for Proven JavaScript bindings.
 * Run with: node --experimental-modules benchmarks/benchmark.mjs
 */

import {
  safeAdd, safeMul, safeDiv,
  truncate, sanitize, validateLength, validateUtf8,
  sha256, sha512, hmacSha256, randomBytes, constantTimeEq,
  parseJson, stringify,
  parseIpv4, parseIpv6, parseCidr, validatePort, isPrivateIp, Ipv4,
  parseUrl, encodeQueryValue,
  validateEmail,
  parseUuid, generateUuidV4,
} from '../src/index.mjs';

const ITERATIONS = 100000;

/**
 * Benchmark runner helper
 * @param {string} name - Benchmark name
 * @param {Function} fn - Function to benchmark
 * @param {number} iterations - Number of iterations
 */
function benchmark(name, fn, iterations = ITERATIONS) {
  // Warmup
  for (let i = 0; i < 1000; i++) fn();

  const start = performance.now();
  for (let i = 0; i < iterations; i++) fn();
  const end = performance.now();

  const totalMs = end - start;
  const opsPerSec = (iterations / totalMs) * 1000;
  const nsPerOp = (totalMs * 1e6) / iterations;

  console.log(`  ${name.padEnd(25)} ${totalMs.toFixed(2).padStart(10)} ms  ${opsPerSec.toFixed(0).padStart(12)} ops/sec  ${nsPerOp.toFixed(0).padStart(8)} ns/op`);
}

console.log('╔════════════════════════════════════════════════════════════════════╗');
console.log('║            Proven JavaScript Benchmark Suite                       ║');
console.log('╚════════════════════════════════════════════════════════════════════╝');
console.log(`Iterations: ${ITERATIONS.toLocaleString()}\n`);

// SafeMath benchmarks
console.log('SafeMath');
console.log('--------');
benchmark('safeAdd', () => safeAdd(1000, 2000));
benchmark('safeMul', () => safeMul(100, 200));
benchmark('safeDiv', () => safeDiv(1000, 7));
benchmark('overflow_check', () => safeAdd(Number.MAX_SAFE_INTEGER, 1));
console.log();

// SafeString benchmarks
console.log('SafeString');
console.log('----------');
const testString = "The quick brown fox jumps over the lazy dog";
benchmark('truncate', () => truncate(testString, 20));
benchmark('sanitize', () => sanitize("<script>alert('xss')</script>"));
benchmark('validateLength', () => validateLength(testString, 10, 100));
benchmark('validateUtf8', () => validateUtf8(testString));
console.log();

// SafeCrypto benchmarks
console.log('SafeCrypto');
console.log('----------');
const testData = "benchmark test data for cryptographic operations";
benchmark('sha256', () => sha256(testData), 10000);
benchmark('sha512', () => sha512(testData), 10000);
benchmark('hmacSha256', () => hmacSha256("secret-key", testData), 10000);
benchmark('randomBytes(32)', () => randomBytes(32), 10000);
const hashValue = sha256(testData);
benchmark('constantTimeEq', () => constantTimeEq(hashValue, hashValue));
console.log();

// SafeJson benchmarks
console.log('SafeJson');
console.log('--------');
const simpleJson = '{"name":"test","value":42}';
const nestedJson = '{"user":{"name":"John","age":30},"items":[1,2,3]}';
benchmark('parse_simple', () => parseJson(simpleJson), 50000);
benchmark('parse_nested', () => parseJson(nestedJson), 50000);
benchmark('stringify', () => stringify({ name: "test", value: 42 }), 50000);
console.log();

// SafeNetwork benchmarks
console.log('SafeNetwork');
console.log('-----------');
benchmark('parseIpv4', () => parseIpv4("192.168.1.1"));
benchmark('parseIpv6', () => parseIpv6("2001:0db8:85a3:0000:0000:8a2e:0370:7334"));
benchmark('parseCidr', () => parseCidr("10.0.0.0/8"));
benchmark('validatePort', () => validatePort(8080));
const ip = new Ipv4(192, 168, 1, 1);
benchmark('isPrivateIp', () => isPrivateIp(ip));
console.log();

// SafeUrl benchmarks
console.log('SafeUrl');
console.log('-------');
benchmark('parseUrl', () => parseUrl("https://example.com/path?query=value"));
benchmark('encodeQueryValue', () => encodeQueryValue("hello world & more"));
console.log();

// SafeEmail benchmarks
console.log('SafeEmail');
console.log('---------');
benchmark('validateEmail', () => validateEmail("user+tag@mail.example.com"));
console.log();

// SafeUUID benchmarks
console.log('SafeUUID');
console.log('--------');
benchmark('parseUuid', () => parseUuid("550e8400-e29b-41d4-a716-446655440000"));
benchmark('generateUuidV4', () => generateUuidV4(), 10000);
console.log();

console.log('════════════════════════════════════════════════════════════════════');
console.log('Benchmark complete.');
