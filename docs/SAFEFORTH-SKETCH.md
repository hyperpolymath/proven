# SafeForth - Byte-Level Safety Library Sketch

## Concept

Same pattern as proven but for **byte-level safety** using Forth's strengths:
- Stack discipline with depth proofs
- Direct memory/disk with bounds checking
- Minimal runtime (bootloader-viable)

## Core Modules

| Module | What It Does | Key Safety |
|--------|--------------|------------|
| SafeStack | Stack ops (push/pop/dup/swap) | Depth proofs - can't underflow |
| SafeMemory | @/!/c@/c! operations | Bounds checking on regions |
| SafeBlock | Sector read/write | Valid sector proofs |
| SafeWord | Word definitions | Stack effect verification |
| SafeDMA | Direct memory access | Region alignment proofs |
| SafePort | I/O port access | Valid port proofs |

## Stack Effect Types

```
push  : Stack n -> Stack (S n)
pop   : Stack (S n) -> (Cell, Stack n)  -- Requires non-empty!
dup   : Stack (S n) -> Stack (S (S n))  -- Requires at least 1
swap  : Stack (S (S n)) -> Stack (S (S n))  -- Requires at least 2
```

## Memory Safety

```
fetch : (region : Region) -> (offset : Nat) -> {offset + 8 <= region.size} -> Cell
store : (region : Region) -> (offset : Nat) -> Cell -> {offset + 8 <= region.size} -> ()
```

## Block Safety

```
blockRead  : (sector : Nat) -> {sector < diskSectors} -> Vect 512 Byte
blockWrite : (sector : Nat) -> Vect 512 Byte -> {sector < diskSectors} -> ()
```

## Architecture

```
Application (any lang)
        │
   Zig FFI Bridge
        │
   SafeForth Core (Idris 2 types + Forth impl)
        │
   Hardware (disk, memory, ports)
```

## Why Forth?

1. **Minimal** - Fits in boot sector, no runtime bloat
2. **Direct** - Maps 1:1 to hardware operations
3. **Verifiable** - Stack effects can be proven
4. **Proven track record** - OpenFirmware, embedded, spacecraft

## Relation to proven

| proven | SafeForth |
|--------|-----------|
| Type safety | Byte safety |
| JSON/URL/Email parsing | Disk/Memory/Port I/O |
| High-level validation | Low-level hardware |

**Together**: proven parses data safely, SafeForth reads/writes it safely.

## Similar Libraries to Consider

| Language | Strength | Potential Library |
|----------|----------|-------------------|
| **Forth** | Byte-level, stack | SafeForth |
| **SPARK/Ada** | Contracts, real-time | SafeConcurrent |
| **Erlang/OTP** | Fault tolerance | SafeActor |
| **Z3/SMT** | Constraint solving | SafeConstraint |

## Protocol-Squisher: Semantic Preservation Across FFI

**Problem**: Language-specific safety properties don't survive FFI:
- Rust ownership/borrowing → lost at C ABI
- Linear Haskell's linearity → lost at FFI boundary
- Haskell laziness → forced evaluation at FFI

**Solution**: Encode semantic properties in protocol-squisher transport classes:

| Property | Protocol Encoding |
|----------|-------------------|
| Ownership | Capability tokens (use-once, invalidates sender) |
| Borrowing | Lease tokens (time-bounded, auto-return) |
| Linearity | One-shot tickets (consumed on use) |
| Laziness | Thunk references (compute-on-demand RPC) |
| Affine | At-most-once tokens (may drop, can't dup) |

**Extended Transport Classes**:
```
SemanticTransport = DataTransport + {
  ownership_model : (owned | borrowed | shared | affine | linear),
  evaluation      : (strict | lazy | memoized),
  lifetime        : (static | scoped Token | dynamic),
  capability      : CapabilityToken
}
```

**How it works**:
1. Caller encodes semantic intent in protocol message
2. Transport layer validates semantic rules (can't dup linear value)
3. Receiver reconstructs equivalent semantics in its language
4. Protocol enforces what languages can't across FFI

**Example - Linear value across FFI**:
```
send(value, { linear: true, ticket: "abc123" })
# Protocol rejects: send(value, { linear: true, ticket: "abc123" })  -- already used!
```

This means SafeForth (or any safe library) can preserve:
- Memory region ownership (not just bounds)
- Handle linearity (file handles used exactly once)
- Lazy disk reads (only fetch sectors when accessed)

## Next Steps

1. Pick Forth base (gforth or minimal custom)
2. Define stack effect DSL
3. Build Zig FFI (reuse proven's pattern)
4. Create bindings for key languages
5. Integrate protocol-squisher for semantic preservation
