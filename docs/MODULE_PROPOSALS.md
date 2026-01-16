# Module Proposals

This document tracks proposed new modules for the proven library, primarily arising from integration analysis with downstream projects.

## From academic-workflow-suite Integration Analysis (2026-01-16)

### SafeJson

**Status**: Proposed
**Priority**: HIGH
**Use Case**: Crashproof JSON parsing and serialization

**Proposed Operations**:
- `SafeJson.parse(string) -> Result<JsonValue, Error>` - Guaranteed no crashes on malformed input
- `SafeJson.stringify(value) -> Result<string, Error>` - Safe serialization
- `SafeJson.Schema.validate(schema, value) -> Result<(), ValidationError>` - Schema compliance

**Proof Properties**:
- Parse never panics/crashes on any input
- Memory bounded by input size × constant factor
- UTF-8 encoding always valid in output
- No stack overflow on deeply nested structures

**Motivation**: Academic-workflow-suite uses JSON for:
- AI Jail IPC communication (untrusted input)
- Office Add-in API responses
- Configuration file parsing

### SafeRegex

**Status**: Proposed
**Priority**: HIGH
**Use Case**: Regular expression matching without ReDoS vulnerability

**Proposed Operations**:
- `SafeRegex.compile(pattern) -> Result<Regex, PatternError>` - Validated pattern compilation
- `SafeRegex.match(regex, input) -> Result<Match, Error>` - Guaranteed polynomial-time matching
- `SafeRegex.replace(regex, input, replacement) -> string` - Safe substitution

**Proof Properties**:
- Matching time bounded by O(n × m) where n = input length, m = pattern length
- No backtracking explosion (ReDoS-resistant)
- Invalid patterns rejected at compile time

**Motivation**: Academic-workflow-suite uses regex for:
- PII detection patterns (email, student ID)
- Input validation
- Module/assignment code parsing

### SafeStateMachine

**Status**: Proposed
**Priority**: MEDIUM
**Use Case**: Type-safe state machine with verified transitions

**Proposed Operations**:
- `SafeStateMachine.define(states, transitions) -> Machine` - DSL for state machine definition
- `SafeStateMachine.transition(machine, event) -> Result<State, InvalidTransition>` - Safe transitions
- `SafeStateMachine.canTransition(machine, event) -> Bool` - Query valid transitions

**Proof Properties**:
- Only defined transitions are possible
- No invalid states reachable
- Deterministic behavior (same input → same output)
- All events handled (no undefined behavior)

**Motivation**: Academic-workflow-suite uses state machines for:
- Event sourcing (document lifecycle)
- TMA marking workflow states
- Audit trail state transitions

### SafeAudit

**Status**: Proposed
**Priority**: MEDIUM
**Use Case**: Append-only audit log with integrity verification

**Proposed Operations**:
- `SafeAudit.append(log, entry) -> Result<LogId, Error>` - Append with integrity
- `SafeAudit.verify(log) -> Result<(), IntegrityError>` - Chain verification
- `SafeAudit.query(log, timeRange) -> List<Entry>` - Time-bounded queries

**Proof Properties**:
- Append-only (no modification of existing entries)
- Chained integrity (tamper detection)
- Query consistency (reproducible results)

**Motivation**: Academic-workflow-suite requires GDPR-compliant audit trails for:
- Student ID anonymization events
- AI analysis requests/responses
- Tutor feedback editing actions

### SafeOffice (NEW - Domain-Specific)

**Status**: Proposed
**Priority**: LOW
**Use Case**: Safe Office.js operations for add-ins

**Proposed Operations**:
- `SafeOffice.Document.read() -> Result<Content, AccessError>` - Safe document access
- `SafeOffice.Range.insert(position, text) -> Result<(), BoundsError>` - Verified text insertion
- `SafeOffice.Comment.create(range, text) -> Result<Comment, Error>` - Safe comment creation
- `SafeOffice.Property.get(name) -> Result<Value, NotFound>` - Safe property access

**Proof Properties**:
- Operations bounded within document structure
- No invalid range access
- Encoding safety for all text operations

**Motivation**: Office Add-in needs guaranteed safe interaction with Word documents.

### SafePII (NEW - Domain-Specific)

**Status**: Proposed
**Priority**: MEDIUM
**Use Case**: PII detection and anonymization

**Proposed Operations**:
- `SafePII.detect(text, patterns) -> List<PiiMatch>` - Pattern-based detection
- `SafePII.anonymize(text, method) -> AnonymizedText` - Verified anonymization
- `SafePII.hash(identifier, salt) -> Hash` - One-way hashing with timing resistance

**Proof Properties**:
- Detection completeness (no missed patterns)
- Anonymization irreversibility (for hash methods)
- Timing-attack resistance for hashing
- Deterministic (same input → same output)

**Motivation**: Academic-workflow-suite must guarantee student PII never reaches AI systems in identifiable form.

## Implementation Notes

### FFI Requirements

New modules would need bindings for at least:
- **Rust** (core engine) - proven-rust crate
- **ReScript/JavaScript** (Office Add-in) - proven-js package

### Proof Strategy

For each module:
1. Define specification in Idris2 (types capture invariants)
2. Implement with totality checking enabled
3. Export proofs as documentation
4. Generate FFI bindings via codegen

### Related Work

- `SafeString` already handles HTML/SQL/JS escaping
- `SafePath` handles path traversal
- `SafeCrypto` handles constant-time comparison
- New modules should integrate with existing ones where applicable

## Tracking

- [ ] SafeJson - RFC drafted
- [ ] SafeRegex - RFC drafted
- [ ] SafeStateMachine - RFC drafted
- [ ] SafeAudit - RFC drafted
- [ ] SafeOffice - Scoping needed
- [ ] SafePII - Depends on SafeRegex, SafeCrypto
