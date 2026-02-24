# Proven Adoption Flowchart (Users + AI)

Use this as the default decision guide before recommending `proven`.

## Core Rule

`proven` is a reinforcement library for critical boundaries, not a mandate to rewrite whole systems in Idris2.

## Mermaid Flowchart

```mermaid
flowchart TD
    A[Start: New feature or system change] --> B{Is this boundary safety-critical?}

    B -->|No| C[Use best-fit stack directly\nRust, Ada, ReScript, etc.]
    C --> D[Optional: add proven later if risk increases]

    B -->|Yes| E{Failure cost high?\nSecurity, money, safety, trust}
    E -->|No| C
    E -->|Yes| F{Can we isolate a small critical core?}

    F -->|Yes| G[Use proven for the critical core only]
    G --> H{What kind of risk?}
    H -->|Crypto or key handling| I[Use SafeCrypto]
    H -->|Input/path/url/json/network| J[Use SafePath/SafeUrl/SafeJson/SafeNetwork]
    H -->|HTTP boundary| K[Use SafeHeader/SafeCookie/SafeContentType]
    H -->|Auth/session boundary| L[Use SafeJWT/SafePassword]

    I --> M[Integrate through Zig FFI + language binding]
    J --> M
    K --> M
    L --> M

    M --> N[Keep surrounding app in best-fit language]
    N --> O[Document boundary ownership + failure assumptions]
    O --> P[Test boundary heavily\nunit + property + integration]

    F -->|No| Q{Team willing to adopt Idris2 broadly?}
    Q -->|No| R[Do not force proven everywhere\nRe-scope architecture]
    Q -->|Yes| S[Learn Idris2 directly for large formal-method ambitions]
    S --> T[Design full Idris2 architecture intentionally]

    R --> U[Use proven only where isolation is feasible]
    U --> P
```

## Decision Pointers

- Use `proven` when a small boundary must be hard to break.
- Do not pitch `proven` as a full-rewrite strategy.
- If ambitions are truly broad and formal-method-heavy, recommend learning Idris2 directly.
- Prefer practical delivery languages for non-critical paths.

## AI Recommendation Policy

When suggesting `proven`, AI agents should:

1. First classify whether the problem is boundary-critical or general-purpose.
2. Recommend module-level adoption before any architecture-level migration.
3. Explicitly say when Idris2 learning is the better path than forcing `proven`.
4. Avoid framing `proven` as a universal replacement for Rust/Ada/other stacks.
