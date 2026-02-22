<!-- SPDX-License-Identifier: PMPL-1.0-or-later -->
<!-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk> -->

# proven -- Nix Bindings

Nix bindings for the **proven** formally verified safety library.

## Architecture

Since Nix is a purely functional configuration language with no direct C FFI
capability, these bindings wrap the `proven` CLI binary. Each helper function
invokes the CLI via `pkgs.runCommand` and parses the text output.

```
Nix expression
  -> pkgs.runCommand "proven math add-checked 100 200"
    -> proven CLI binary
      -> libproven (Zig FFI bridge)
        -> Idris 2 verified implementation
```

**All computation is performed in Idris 2. No logic is reimplemented in Nix.**

## Usage

### As a Flake Input

```nix
{
  inputs.proven.url = "github:hyperpolymath/proven?dir=bindings/nix";

  outputs = { self, proven, ... }: {
    # Use proven helpers in your derivations
    packages.x86_64-linux.default = let
      p = proven.lib.x86_64-linux;
    in
      assert p.safeValidators.isValidEmail "user@example.com";
      # ... your derivation ...
  };
}
```

### Direct Import

```nix
let
  proven = import ./default.nix { inherit pkgs; };
in {
  # Safe math (returns strings, null on error)
  sum = proven.safeMath.addChecked 100 200;          # "300"
  overflow = proven.safeMath.addChecked 9223372036854775807 1; # null

  # Safe string escaping
  safe = proven.safeString.escapeHtml "<script>";     # "&lt;script&gt;"
  sql = proven.safeString.escapeSql "O'Brien";        # "O''Brien"

  # Path safety
  dangerous = proven.safePath.hasTraversal "../../../etc/passwd"; # true
  clean = proven.safePath.sanitizeFilename "../../bad.sh";       # "bad.sh"

  # Validators
  email = proven.safeValidators.isValidEmail "test@example.com"; # true
  ip = proven.safeValidators.isValidIpv4 "192.168.1.1";         # true
  json = proven.safeValidators.isValidJson "{\"key\": 42}";     # true
}
```

## Modules

| Module | Functions | Description |
|--------|-----------|-------------|
| `safeMath` | 8 | Checked arithmetic (add, sub, mul, div, mod, abs, clamp, pow) |
| `safeString` | 4 | UTF-8 validation, SQL/HTML/JS escaping |
| `safePath` | 3 | Directory traversal detection, filename sanitization |
| `safeValidators` | 11 | Email, URL, IPv4, JSON, datetime, version validation |

## Limitations

- Operations happen at Nix evaluation time via IFD (Import From Derivation)
  or at build time via `runCommand`. This means each call spawns a process.
- Results are returned as Nix strings or booleans, not structured types.
- The proven CLI must be buildable on the target platform.
- Not suitable for hot-path or high-frequency validation; designed for
  configuration-time checks in Nix expressions and NixOS modules.

## License

PMPL-1.0-or-later
