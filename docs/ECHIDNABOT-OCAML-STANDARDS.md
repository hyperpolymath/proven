# echidnabot: OCaml++++ Reviewer Standards

## Problem Statement

**What opam reviewers caught (that we missed):**
- 130 security issues (5 CRITICAL, 124 HIGH)
- Architecture violations (reimplementations vs FFI)
- Unsafe patterns across multiple languages
- Code claiming "formally verified" without actual verification

**Our basic grep-based checker (`proven-cleaner.sh`) only caught these AFTER we looked.**

## What opam Reviewers Actually Do

Based on proven v0.9.0 rejection analysis:

### 1. **Deep Static Analysis**
- Parse actual AST (not just text patterns)
- Understand semantic meaning of code
- Track data flow and control flow
- Detect unsafe patterns in context

### 2. **Architecture Verification**
- Check claimed architecture vs actual implementation
- Verify FFI boundaries are respected
- Ensure modules do what they claim

### 3. **Security Review**
- Identify crash-prone patterns (getExn, unwrap, panic)
- Check for injection vulnerabilities
- Verify crypto usage is correct
- Audit dependency chains

### 4. **Build Verification**
- Actually compile the code
- Run test suites
- Check examples execute
- Verify documentation claims

### 5. **Semantic Correctness**
- For formal verification claims: verify proofs exist
- Check totality (all functions terminate)
- Ensure type safety guarantees hold

## echidnabot Architecture Upgrade

### Current State (Basic)
```
proven-cleaner.sh:
  grep for "unwrap()"  → Found in tests (false positive)
  grep for "getExn"    → Missed compiled .res.js files
  grep for "sha256"    → Found in build artifacts (noise)
```

**Issues:**
- Text-based (no semantic understanding)
- Can't distinguish test vs production code
- Can't verify FFI boundaries
- No formal verification integration

### Target State (OCaml++++ Standards)

```
echidnabot-v2:
  1. Parse source AST (Rust: syn, ReScript: rescript compiler, Idris: idris2 --check)
  2. Semantic analysis (track FFI calls, detect logic in bindings)
  3. Formal verification (run Idris2 with --total, verify proofs)
  4. Property-based testing (QuickCheck-style generators)
  5. Dependency analysis (check for supply chain issues)
  6. Build verification (actually compile and test)
```

## Required Components

### 1. Language-Specific Parsers

| Language | Parser/Tool | What We Check |
|----------|-------------|---------------|
| **Idris2** | `idris2 --check --total` | Totality, proofs valid, no holes |
| **Rust** | `syn` crate | AST analysis, FFI boundaries, unsafe blocks |
| **ReScript** | ReScript compiler API | FFI-only, no getExn/Obj.magic |
| **OCaml** | `ocaml-migrate-parsetree` | FFI-only, no Obj.magic |
| **Elixir** | `Code.string_to_quoted/2` | AST analysis |
| **Scala** | Scalameta | JNI-only, no native implementations |
| **PHP** | PHP-Parser | FFI-only via php-ffi extension |
| **Clojure** | `tools.analyzer` | JNI-only, no native Java calls |

### 2. Formal Verification Integration

**For proven (Idris2-based):**
```bash
# Check Idris2 code is total and type-checks
idris2 --check --total src/Proven/*.idr

# Verify proofs are complete (no holes)
idris2 --find-holes src/Proven/*.idr
# Expected output: "No holes found"

# Check FFI exports match Zig declarations
diff <(grep "^export" src/Proven/*.idr | sort) \
     <(grep "^pub fn proven_" ffi/zig/src/abi.zig | sort)
```

**For language bindings:**
```rust
// Parse Rust AST and verify all public functions call FFI
use syn::{parse_file, Item, ItemFn};

fn verify_ffi_only(rust_file: &str) -> Result<(), String> {
    let ast = parse_file(rust_file)?;
    for item in ast.items {
        if let Item::Fn(func) = item {
            if func.vis == Visibility::Public {
                // Must contain exactly ONE call to ffi::proven_*
                let ffi_calls = count_ffi_calls(&func.block);
                if ffi_calls != 1 {
                    return Err(format!(
                        "Function {} has {} FFI calls (expected 1)",
                        func.sig.ident, ffi_calls
                    ));
                }

                // Must NOT contain logic (no if/match/for/while)
                if has_control_flow(&func.block) {
                    return Err(format!(
                        "Function {} has control flow (bindings must be FFI-only)",
                        func.sig.ident
                    ));
                }
            }
        }
    }
    Ok(())
}
```

### 3. Property-Based Testing

**QuickCheck/PropCheck integration:**
```rust
// Generate random inputs, verify FFI boundary safety
#[quickcheck]
fn ffi_never_crashes(a: i32, b: i32) -> bool {
    // This should NEVER panic or crash
    let result = proven::safe_math::div(a, b);

    match result {
        Ok(v) if b != 0 => v == a / b,  // Correct result
        Err(_) if b == 0 => true,       // Expected error
        _ => false                       // Invalid state
    }
}

#[quickcheck]
fn ffi_handles_edge_cases(input: String) -> bool {
    // Even malicious input should not crash
    let result = proven::safe_email::parse(&input);
    // Should return Ok or Err, never panic
    true  // If we get here, didn't crash
}
```

### 4. Fuzzing Integration

**For FFI boundaries (most critical):**
```yaml
# .clusterfuzzlite/proven-ffi-fuzzing.yaml
language: rust
build:
  - cargo +nightly fuzz build --fuzz-targets fuzz_ffi_boundary

fuzz_targets:
  - name: fuzz_ffi_boundary
    max_time: 3600  # 1 hour
    corpus: fuzz/corpus/ffi/

  - name: fuzz_idris_zig_bridge
    max_time: 3600
    corpus: fuzz/corpus/zig/
```

**Fuzz target example:**
```rust
// fuzz/fuzz_targets/fuzz_ffi_boundary.rs
#![no_main]
use libfuzzer_sys::fuzz_target;

fuzz_target!(|data: &[u8]| {
    // Try to crash the FFI by passing garbage data
    if data.len() >= 8 {
        let a = i32::from_le_bytes([data[0], data[1], data[2], data[3]]);
        let b = i32::from_le_bytes([data[4], data[5], data[6], data[7]]);

        // Should handle ANY input without crashing
        let _ = proven::safe_math::div(a, b);
        let _ = proven::safe_math::mul(a, b);
        let _ = proven::safe_math::add_checked(a, b);
    }

    // Try string operations with random UTF-8
    if let Ok(s) = std::str::from_utf8(data) {
        let _ = proven::safe_email::parse(s);
        let _ = proven::safe_url::parse(s);
    }
});
```

### 5. Build Verification

**Multi-stage compilation check:**
```bash
#!/bin/bash
# echidnabot build verification for proven

set -e

echo "=== Stage 1: Idris2 Verification ==="
cd src/
idris2 --check --total Proven/*.idr || {
    echo "❌ Idris2 totality check failed"
    exit 1
}

echo "=== Stage 2: Zig FFI Bridge ==="
cd ../ffi/zig/
zig build test || {
    echo "❌ Zig FFI tests failed"
    exit 1
}

echo "=== Stage 3: Language Bindings ==="
# Rust
cd ../../bindings/rust/
cargo check --all-features || {
    echo "❌ Rust bindings don't compile"
    exit 1
}
cargo test || {
    echo "❌ Rust tests failed"
    exit 1
}

# ReScript
cd ../rescript/
npx rescript build || {
    echo "❌ ReScript bindings don't compile"
    exit 1
}

echo "=== Stage 4: Integration Tests ==="
cd ../../tests/integration/
cargo test --test ffi_integration || {
    echo "❌ FFI integration tests failed"
    exit 1
}

echo "✅ All stages passed!"
```

### 6. Dependency Analysis

**Supply chain security:**
```rust
// Check for suspicious dependencies
use cargo_metadata::MetadataCommand;

fn audit_dependencies() -> Result<(), String> {
    let metadata = MetadataCommand::new().exec()?;

    for package in metadata.packages {
        // Check for known vulnerabilities
        if is_vulnerable(&package.name, &package.version) {
            return Err(format!(
                "Vulnerable dependency: {} {}",
                package.name, package.version
            ));
        }

        // Check for abandoned packages (no updates in 2+ years)
        if is_abandoned(&package.repository) {
            return Err(format!(
                "Abandoned dependency: {}",
                package.name
            ));
        }

        // Check for typosquatting
        if looks_suspicious(&package.name) {
            return Err(format!(
                "Suspicious package name: {}",
                package.name
            ));
        }
    }

    Ok(())
}
```

## Implementation Plan

### Phase 1: Parser Infrastructure (Week 1)

**Create `echidnabot-parsers/` crate:**
```
echidnabot-parsers/
├── Cargo.toml
├── src/
│   ├── lib.rs
│   ├── idris2.rs        # Idris2 compiler integration
│   ├── rust.rs          # syn-based parser
│   ├── rescript.rs      # ReScript compiler API
│   ├── ocaml.rs         # OCaml AST parser
│   └── common.rs        # Shared AST traits
└── tests/
    └── fixtures/        # Test code samples
```

**Core trait:**
```rust
pub trait LanguageParser {
    fn parse_file(&self, path: &Path) -> Result<AST, ParseError>;
    fn find_ffi_calls(&self, ast: &AST) -> Vec<FFICall>;
    fn find_unsafe_patterns(&self, ast: &AST) -> Vec<UnsafePattern>;
    fn verify_totality(&self, ast: &AST) -> Result<(), TotalityError>;
}

pub struct IdrisParser;
impl LanguageParser for IdrisParser {
    fn verify_totality(&self, ast: &AST) -> Result<(), TotalityError> {
        // Run: idris2 --check --total <file>
        let output = Command::new("idris2")
            .args(&["--check", "--total", ast.file_path])
            .output()?;

        if !output.status.success() {
            return Err(TotalityError::NotTotal {
                file: ast.file_path.clone(),
                errors: String::from_utf8_lossy(&output.stderr).to_string(),
            });
        }

        Ok(())
    }
}
```

### Phase 2: Formal Verification Checks (Week 1-2)

**Create `echidnabot-verify/` crate:**
```rust
pub struct FormalVerifier {
    idris_parser: IdrisParser,
    binding_parsers: HashMap<Language, Box<dyn LanguageParser>>,
}

impl FormalVerifier {
    pub fn verify_proven_architecture(&self, repo_path: &Path) -> Result<ArchitectureReport, Error> {
        let mut report = ArchitectureReport::default();

        // 1. Verify Idris2 source is total
        for idr_file in glob("src/Proven/*.idr")? {
            let ast = self.idris_parser.parse_file(&idr_file)?;
            self.idris_parser.verify_totality(&ast)?;
            report.verified_modules.push(idr_file);
        }

        // 2. Verify Zig FFI matches Idris exports
        let idris_exports = self.extract_idris_exports()?;
        let zig_exports = self.extract_zig_exports()?;
        if idris_exports != zig_exports {
            return Err(Error::FFIMismatch {
                missing: idris_exports.difference(&zig_exports),
                extra: zig_exports.difference(&idris_exports),
            });
        }

        // 3. Verify language bindings are FFI-only
        for (lang, parser) in &self.binding_parsers {
            let bindings = glob(&format!("bindings/{lang}/**/*.{}", lang.ext()))?;
            for binding_file in bindings {
                let ast = parser.parse_file(&binding_file)?;

                // Check: All public functions call exactly ONE FFI function
                self.verify_ffi_only(&ast)?;

                // Check: No unsafe patterns
                let unsafe_patterns = parser.find_unsafe_patterns(&ast);
                if !unsafe_patterns.is_empty() {
                    return Err(Error::UnsafePatterns {
                        file: binding_file,
                        patterns: unsafe_patterns,
                    });
                }

                report.verified_bindings.push(binding_file);
            }
        }

        Ok(report)
    }
}
```

### Phase 3: Test Theory Integration (Week 2)

**Property-based testing framework:**
```rust
pub struct PropertyTester {
    generators: HashMap<Type, Box<dyn Generator>>,
}

impl PropertyTester {
    pub fn generate_ffi_tests(&self, ffi_signature: &FFISignature) -> Vec<TestCase> {
        let mut tests = Vec::new();

        // 1. Edge cases (min/max values, zero, negative)
        for edge_case in self.edge_cases(&ffi_signature.inputs) {
            tests.push(TestCase {
                name: format!("edge_case_{}", edge_case.description),
                inputs: edge_case.values,
                expected: self.compute_expected(&ffi_signature, &edge_case),
            });
        }

        // 2. Random inputs (QuickCheck-style)
        for _ in 0..1000 {
            let random_inputs = self.generate_random(&ffi_signature.inputs);
            tests.push(TestCase {
                name: format!("random_{}", rand::random::<u64>()),
                inputs: random_inputs.clone(),
                expected: self.compute_expected(&ffi_signature, &random_inputs),
            });
        }

        // 3. Adversarial inputs (fuzzing-guided)
        for adversarial in self.adversarial_cases(&ffi_signature) {
            tests.push(TestCase {
                name: format!("adversarial_{}", adversarial.description),
                inputs: adversarial.values,
                expected: TestExpectation::NoError,  // Should handle gracefully
            });
        }

        tests
    }
}
```

### Phase 4: CI Integration (Week 2-3)

**GitHub Actions workflow:**
```yaml
# .github/workflows/echidnabot-v2.yml
name: echidnabot v2 (OCaml++++ Standards)

on: [push, pull_request]

permissions: read-all

jobs:
  formal-verification:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@b4ffde65f46336ab88eb53be808477a3936bae11

      - name: Install Idris2
        run: |
          curl -sSL https://www.idris-lang.org/install-idris2.sh | bash
          idris2 --version

      - name: Verify Idris2 totality
        run: |
          cd src/
          for file in Proven/*.idr; do
            echo "Checking $file..."
            idris2 --check --total "$file" || {
              echo "❌ Totality check failed for $file"
              exit 1
            }
          done

      - name: Check for proof holes
        run: |
          idris2 --find-holes src/Proven/*.idr | grep "No holes" || {
            echo "❌ Found incomplete proofs"
            exit 1
          }

  architecture-analysis:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@b4ffde65f46336ab88eb53be808477a3936bae11

      - name: Build echidnabot-parsers
        run: cargo build --release -p echidnabot-parsers

      - name: Verify FFI architecture
        run: |
          cargo run -p echidnabot-verify -- \
            --check-ffi-only \
            --check-idris-exports \
            --check-unsafe-patterns

      - name: Generate architecture report
        run: |
          cargo run -p echidnabot-verify -- --report > architecture-report.md
          cat architecture-report.md >> $GITHUB_STEP_SUMMARY

  property-testing:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@b4ffde65f46336ab88eb53be808477a3936bae11

      - name: Run QuickCheck tests
        run: cargo test --all-features -- --include-ignored

      - name: Run fuzz tests (short)
        run: |
          cargo +nightly fuzz run fuzz_ffi_boundary -- -max_total_time=300

  build-verification:
    strategy:
      matrix:
        language: [rust, rescript, ocaml, elixir, scala, php, clojure]
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@b4ffde65f46336ab88eb53be808477a3936bae11

      - name: Build ${{ matrix.language }} bindings
        run: |
          cd bindings/${{ matrix.language }}/
          ./build.sh || {
            echo "❌ ${{ matrix.language }} bindings failed to build"
            exit 1
          }

      - name: Run ${{ matrix.language }} tests
        run: |
          cd bindings/${{ matrix.language }}/
          ./test.sh
```

## Success Criteria

echidnabot v2 passes when it can detect:

### ✅ What opam caught (proven v0.9.0)
- [x] Native reimplementations (100+ files)
- [x] Unsafe patterns (unwrap, getExn, Obj.magic)
- [x] Architecture violations (logic in bindings)
- [x] Crash-prone code (5 CRITICAL getExn)

### ✅ What it should catch (higher standards)
- [ ] Incomplete proofs (Idris2 holes)
- [ ] Non-total functions (infinite loops possible)
- [ ] FFI mismatches (Idris exports ≠ Zig bridge)
- [ ] Supply chain vulnerabilities
- [ ] Insufficient test coverage
- [ ] Performance regressions
- [ ] Documentation inaccuracies

### ✅ Quality bar: opam-repository level
- [ ] All checks pass before `git push`
- [ ] Cannot publish to ANY registry without passing
- [ ] Automated but as thorough as human review
- [ ] Clear, actionable error messages

## Timeline

| Week | Deliverable |
|------|-------------|
| 1 | Parser infrastructure (Idris2, Rust, ReScript) |
| 2 | Formal verification checks + property testing |
| 3 | CI integration + remaining language parsers |
| 4 | Production deployment + documentation |

## Next Actions

1. ⏳ Create `echidnabot-v2/` directory structure
2. ⏳ Implement Idris2 parser (totality checker)
3. ⏳ Implement Rust AST parser (FFI verification)
4. ⏳ Create property-based test generator
5. ⏳ Integrate with CI/CD

---

**Goal:** Never let unverified code reach package registries again. If echidnabot passes, opam reviewers should have nothing to add.
