# Security Policy

## Supported Versions

| Version | Supported          |
| ------- | ------------------ |
| 0.x.x   | :white_check_mark: |

## Reporting a Vulnerability

**Do not report security vulnerabilities through public GitHub issues.**

Instead, please report them via:

1. **Email**: security@hyperpolymath.org (preferred)
2. **GitHub Security Advisories**: [Create a private advisory](https://github.com/hyperpolymath/bulletproof-core/security/advisories/new)

### What to include

- Type of vulnerability (buffer overflow, injection, etc.)
- Full path to affected source file(s)
- Step-by-step instructions to reproduce
- Proof-of-concept or exploit code (if available)
- Impact assessment

### Response Timeline

- **Acknowledgment**: Within 48 hours
- **Initial assessment**: Within 7 days
- **Resolution target**: Within 90 days (may vary based on severity)

### Safe Harbor

We consider security research conducted in accordance with this policy to be:
- Authorized
- Lawful
- Helpful

We will not pursue legal action against researchers who follow this policy.

## Security Measures

This project implements:

- [x] Dependabot alerts enabled
- [x] CodeQL static analysis
- [x] OpenSSF Scorecard compliance
- [x] Signed commits required
- [x] Branch protection enabled
- [ ] Formal verification (Idris 2 dependent types)
- [ ] Security audit (planned for 1.0)

## Known Limitations

The core Idris 2 code is formally verified. However:

1. **Language bindings** (Python, Rust, JS) are human-written and may contain bugs
2. **FFI boundaries** are potential attack surfaces
3. **Build toolchain** (Zig, C compiler) is not verified

Report issues in any layer — we take all security concerns seriously.
