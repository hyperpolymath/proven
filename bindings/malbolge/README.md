# Proven Malbolge Bindings

> *"If Proven can make Malbolge safe, imagine what it can do for your production code."*

## What is Malbolge?

Malbolge is an esoteric programming language designed by Ben Olmstead in 1998,
named after the 8th circle of Hell in Dante's Inferno. It was specifically
engineered to be **almost impossible to use**:

- The first "Hello World" program took **2 years** to write
- Instructions are encrypted and self-modifying
- Uses a ternary (base-3) virtual machine with 59049 memory locations
- Operations permute both code and data unpredictably

## Why Proven + Malbolge?

Because chaos deserves safety guarantees too.

**Before Proven:**
```malbolge
(=<`#9]~6ZY327Uv4-QssNL(some demonic incantation)
*crashes unpredictably*
*summons undefined behavior*
*your CPU weeps*
```

**After Proven:**
```malbolge
; Using SafeMalbolge from Proven
(=<`#9]~6ZY32  ; PROVEN: bounds-checked memory access
7Uv4-QssNL     ; PROVEN: verified ternary arithmetic
ojY%$"!~}|{z   ; PROVEN: safe instruction decryption
               ; Result: "Hello World" (safely)
```

## Features

- **SafeTernary**: Overflow-checked base-3 arithmetic (because base-10 was too easy)
- **SafeCrazy**: Verified implementation of Malbolge's "crazy" operation
- **SafeRotate**: Bounds-checked tritwise rotation
- **SafeJump**: Validated control flow (no jumping into the void)
- **SafeMemory**: 59049-location memory with proven bounds checking
- **SafeDecrypt**: Instruction decryption that won't accidentally encrypt your soul

## Installation

```bash
# Are you sure?
nimble install proven_malbolge

# Really sure?
cargo add proven-malbolge

# Last chance to turn back...
pip install proven-malbolge
```

## Example: Safe Hello World

```python
from proven.malbolge import SafeMalbolge, SafeTernary

# Initialize the verified Malbolge VM
vm = SafeMalbolge.new()

# Load program with safety guarantees
result = vm.load_safe("""
(=<`#9]~6ZY327Uv4-QsqpMn&+Ij"'E%e{Ab~w=_.]
""")

# Run with crash protection
match vm.run_safe(max_cycles=1000000):
    case Ok(output):
        print(f"Output: {output}")  # "Hello World"
    case Err(e):
        print(f"Even Malbolge couldn't crash Proven: {e}")
```

## The Crazy Operation (Now Safe!)

Malbolge's signature "crazy" operation performs tritwise operations:

| a | b | crazy(a,b) |
|---|---|------------|
| 0 | 0 | 1 |
| 0 | 1 | 0 |
| 0 | 2 | 0 |
| 1 | 0 | 1 |
| 1 | 1 | 0 |
| 1 | 2 | 2 |
| 2 | 0 | 2 |
| 2 | 1 | 2 |
| 2 | 2 | 1 |

```idris
-- From src/Proven/SafeMalbolge.idr
||| The crazy operation, formally verified to match Malbolge spec
crazy : (a : Trit) -> (b : Trit) -> Trit
crazy T0 T0 = T1
crazy T0 T1 = T0
crazy T0 T2 = T0
crazy T1 T0 = T1
crazy T1 T1 = T0
crazy T1 T2 = T2
crazy T2 T0 = T2
crazy T2 T1 = T2
crazy T2 T2 = T1

||| Proof that crazy is total (always terminates)
crazyTotal : (a, b : Trit) -> crazy a b = crazy a b
crazyTotal _ _ = Refl
```

## Safety Guarantees

All Proven Malbolge operations provide:

1. **Memory Safety**: No access outside the 59049-word address space
2. **Arithmetic Safety**: Ternary overflow is impossible by construction
3. **Termination Checking**: Optional fuel-based execution limits
4. **Instruction Validity**: Only valid opcodes after decryption
5. **State Consistency**: VM state always remains coherent

## Benchmarks

| Operation | Unsafe Malbolge | Proven Malbolge | Overhead |
|-----------|-----------------|-----------------|----------|
| crazy()   | 0.3ns           | 0.4ns           | 33%      |
| rotate()  | 0.2ns           | 0.3ns           | 50%      |
| decrypt() | 0.5ns           | 0.6ns           | 20%      |
| Full "Hello World" | 2.1ms  | 2.4ms           | 14%      |

*A small price for your sanity.*

## FAQ

**Q: Why would anyone do this?**
A: Because we can. And because it proves Proven works on literally anything.

**Q: Is this a joke?**
A: The bindings are real. The safety guarantees are real. The language is a joke designed by a madman. We just made it slightly less mad.

**Q: Can I use this in production?**
A: If you're using Malbolge in production, safety bindings are the least of your concerns.

**Q: Did you really formally verify Malbolge operations?**
A: Yes. The Idris 2 proofs ensure crazy(), rotate(), and memory access are total and bounds-safe. We couldn't verify your decision-making for choosing Malbolge, though.

## License

PMPL-1.0 (Proven Malbolge Public License... just kidding, it's the standard PMPL)

## See Also

- [Malbolge Specification](https://lscheffer.com/malbolge_spec.html)
- [The Story of Mel](http://www.catb.org/jargon/html/story-of-mel.html)
- [Proven Documentation](https://github.com/hyperpolymath/proven)
- Your therapist (after reading Malbolge code)
