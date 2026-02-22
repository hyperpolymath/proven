# Proven for Factor

FFI binding to libproven -- a formally verified safety library. All computation
is performed in verified Idris 2 code via the Zig FFI layer. This binding uses
Factor's `alien` vocabulary for C interop; it does NOT reimplement any logic.

## Requirements

- Factor (https://factorcode.org/)
- libproven shared library (`libproven.so`) installed and accessible on the
  library search path

## Installation

Copy the `proven/` directory into your Factor vocabulary path:

```bash
cp -r proven/ ~/.factor/work/proven/
```

## Usage

```factor
USING: proven proven.safe-math proven.safe-string proven.safe-email
       proven.safe-url proven.safe-crypto proven.safe-json ;

! Initialize the runtime
proven-init drop

! Safe math with overflow checking
1000000000 2000000000 safe+ [ . ] [ "overflow!" print ] if

! Division by zero returns f (false)
10 0 safe/ [ . ] [ "division by zero!" print ] if

! XSS prevention
"<script>alert('xss')</script>" escape-html
[ print ] [ "escape failed" print ] if

! Path traversal detection
"../../../etc/passwd" has-traversal?
[ [ "traversal detected!" print ] when ] [ drop ] if

! Email validation
"user@example.com" valid-email?
[ [ "valid email" print ] when ] [ drop ] if

! URL encoding
"hello world&foo=bar" url-encode
[ print ] [ "encode failed" print ] if

! JSON validation
"{\"key\": \"value\"}" json-valid?
[ [ "valid JSON" print ] when ] [ drop ] if

! Cleanup
proven-deinit
```

## Vocabularies

| Vocabulary | Description |
|------------|-------------|
| `proven` | Runtime lifecycle (init, deinit, version) |
| `proven.ffi` | Low-level alien (FFI) declarations |
| `proven.safe-math` | Overflow-checked arithmetic |
| `proven.safe-string` | HTML/SQL/JS escaping, UTF-8 validation, path safety |
| `proven.safe-email` | Email address validation (RFC 5321) |
| `proven.safe-url` | URL percent-encoding and decoding (RFC 3986) |
| `proven.safe-crypto` | Constant-time comparison, hex encoding |
| `proven.safe-json` | JSON validation and root type detection |

## Stack Effects

All fallible operations follow Factor convention, returning a value and a
success flag (`t` for success, `f` for failure):

```factor
! ( inputs -- value t )   on success
! ( inputs -- f f )       on error

10 3 safe/    ! ( -- 3 t )     success: quotient is 3
10 0 safe/    ! ( -- f f )     error: division by zero
```

Use `if` or pattern matching to handle results:

```factor
a b safe+ [
    ! success path: value is on stack
    .
] [
    ! error path
    "overflow" print drop
] if
```

## Architecture

```
Factor (this binding)
  |
  v  alien (FFI)
libproven.so
  |
  v  Zig FFI bridge
Idris 2 (formally verified core)
```

## License

PMPL-1.0-or-later
