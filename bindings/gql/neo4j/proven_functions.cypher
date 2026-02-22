// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

// proven_functions.cypher - Neo4j Cypher examples using proven stored procedures
//
// Prerequisite: Install proven-neo4j-0.5.0.jar in $NEO4J_HOME/plugins/
//               and ensure libproven.so is on the library path.
//
// All functions call libproven (Idris 2 verified core via Zig FFI bridge)
// through JNA. Functions return null on error.

// ============================================================================
// SafeMath: Checked arithmetic on graph properties
// ============================================================================

// Safe addition - returns null on overflow instead of corrupting data
MATCH (a:Account)
RETURN a.name, proven.safeAdd(a.balance, a.pendingDeposit) AS totalBalance;

// Safe subtraction - returns null on underflow
MATCH (a:Account)
WHERE proven.safeSub(a.balance, a.withdrawal) IS NOT NULL
SET a.balance = proven.safeSub(a.balance, a.withdrawal)
RETURN a.name, a.balance;

// Safe multiplication - prevents overflow in quantity calculations
MATCH (o:Order)-[:CONTAINS]->(p:Product)
RETURN o.id, proven.safeMul(o.quantity, p.unitPrice) AS lineTotal;

// Safe division - returns null on division by zero
MATCH (m:Metric)
RETURN m.name, proven.safeDiv(m.totalValue, m.count) AS average;

// ============================================================================
// Validation: Filter graph data by proven validators
// ============================================================================

// Find users with valid email addresses
MATCH (u:User)
WHERE proven.validateEmail(u.email)
RETURN u.name, u.email;

// Flag users with invalid emails
MATCH (u:User)
WHERE NOT proven.validateEmail(u.email)
RETURN u.name, u.email AS invalidEmail;

// Validate URLs stored in graph properties
MATCH (p:Page)
WHERE proven.validateUrl(p.url)
RETURN p.title, p.url;

// Validate IPv4 addresses in network topology graph
MATCH (h:Host)
WHERE proven.validateIpv4(h.ipAddress)
RETURN h.hostname, h.ipAddress;

// Validate JSON configuration stored as node property
MATCH (c:Config)
WHERE proven.validateJson(c.settings)
RETURN c.name, c.settings;

// ============================================================================
// String operations on graph properties
// ============================================================================

// Sanitize user-generated content for safe HTML display
MATCH (p:Post)
RETURN p.id,
       proven.sanitizeString(p.title) AS safeTitle,
       proven.sanitizeString(p.body) AS safeBody;

// Hash node property values
MATCH (u:User)
RETURN u.name, proven.hashSha256(u.email) AS emailHash;

// Hex encode/decode
MATCH (d:Document)
RETURN d.id, proven.hexEncode(d.fingerprint) AS fingerprintHex;

// ============================================================================
// Complex graph queries combining proven functions
// ============================================================================

// Financial audit: safe calculations on transaction graph
MATCH (from:Account)-[t:TRANSFER]->(to:Account)
WITH from, to, t,
     proven.safeMul(t.amount, t.exchangeRate) AS convertedAmount
WHERE convertedAmount IS NOT NULL
RETURN from.name AS sender,
       to.name AS receiver,
       t.amount AS originalAmount,
       convertedAmount;

// Data quality report: validate all contact information
MATCH (c:Contact)
RETURN c.name,
       proven.validateEmail(c.email) AS emailValid,
       proven.validateUrl(c.website) AS websiteValid,
       proven.validateIpv4(c.lastIp) AS ipValid,
       proven.validateJson(c.metadata) AS metadataValid;

// Sanitize an entire subgraph for export
MATCH (u:User)-[:AUTHORED]->(p:Post)-[:TAGGED]->(t:Tag)
RETURN proven.sanitizeString(u.displayName) AS author,
       proven.sanitizeString(p.title) AS title,
       proven.sanitizeString(p.content) AS content,
       t.name AS tag;
