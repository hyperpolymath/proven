# Proven for Fortran

Safe, validated operations library for Fortran applications.

All computation is performed in the formally verified Idris 2 core via
the Zig FFI bridge (`libproven`).  The Fortran modules are thin wrappers
that use `iso_c_binding` and `bind(c)` to call the C ABI exported by
`libproven`.  **No safety logic is reimplemented in Fortran.**

## Prerequisites

- `libproven.so` (or `libproven.a`) built from the project root
- A Fortran compiler with Fortran 2003+ support (gfortran, ifort/ifx, etc.)

## Building

```bash
# 1. Build libproven from the project root (Idris 2 + Zig)
cd /path/to/proven
zig build

# 2. Compile the Fortran binding (order matters for module dependencies)
cd bindings/fortran

gfortran -c src/proven_ffi.f90        # C bindings (must be first)
gfortran -c src/safe_math.f90
gfortran -c src/safe_string.f90
gfortran -c src/safe_path.f90
gfortran -c src/safe_email.f90
gfortran -c src/safe_network.f90
gfortran -c src/safe_crypto.f90
gfortran -c src/safe_uuid.f90
gfortran -c src/safe_hex.f90
gfortran -c src/safe_float.f90
gfortran -c src/safe_version.f90
gfortran -c src/safe_angle.f90
gfortran -c src/safe_currency.f90
gfortran -c src/safe_phone.f90
gfortran -c src/proven.f90            # umbrella (must be last)

# 3. Link your program against libproven
gfortran -o myprogram myprogram.f90 *.o -lproven -L/path/to/libproven
```

Or with fpm (Fortran Package Manager):

```toml
# fpm.toml
[dependencies]
proven = { path = "path/to/proven/bindings/fortran" }

[build]
link = ["proven"]
```

## Architecture

```
Fortran application
  |
  | use safe_math / safe_string / ...
  v
proven_ffi.f90          <-- iso_c_binding interface declarations
  |
  | bind(c, name="proven_math_div") etc.
  v
libproven.so / .a       <-- Zig FFI bridge -> compiled Idris 2 RefC
  |
  v
Idris 2 formally verified core (the truth)
```

## Usage

```fortran
program example
    use, intrinsic :: iso_c_binding
    use proven
    implicit none

    type(SafeResult) :: result
    type(IPv4Address) :: ip
    character(len=:), allocatable :: escaped
    integer(c_int32_t) :: init_status

    ! Initialise the Idris 2 runtime
    init_status = proven_init()

    ! Safe math with overflow checking (calls Idris 2 core)
    result = safe_add(1000000000_c_int64_t, 2000000000_c_int64_t)
    if (result%ok) then
        print *, "Sum:", result%value
    else
        print *, "Overflow! status:", result%status
    end if

    ! XSS prevention (calls Idris 2 core)
    escaped = escape_html("<script>alert('xss')</script>")
    print *, "Escaped:", escaped

    ! Path safety (calls Idris 2 core)
    if (has_traversal("../../../etc/passwd")) then
        print *, "Dangerous path!"
    end if

    ! Email validation (calls Idris 2 core)
    if (is_valid_email("user@example.com")) then
        print *, "Valid email"
    end if

    ! IP classification (calls Idris 2 core)
    ip = parse_ipv4("192.168.1.1")
    if (ip%valid) then
        print *, "Private:", is_private_ip(ip)
    end if

    ! Cleanup
    call proven_deinit()

end program example
```

## Modules

| Module | Wraps | Key functions |
|--------|-------|---------------|
| `proven_ffi` | Raw C bindings | `proven_init`, `proven_deinit`, all `proven_*` C functions |
| `safe_math` | SafeMath | `safe_add`, `safe_sub`, `safe_mul`, `safe_div`, `safe_mod`, `safe_abs`, `safe_pow`, `clamp` |
| `safe_string` | SafeString | `escape_html`, `escape_sql`, `escape_js`, `is_valid_utf8` |
| `safe_path` | SafePath | `has_traversal`, `sanitize_filename` |
| `safe_email` | SafeEmail | `is_valid_email` |
| `safe_network` | SafeNetwork | `parse_ipv4`, `is_loopback`, `is_private_ip` |
| `safe_crypto` | SafeCrypto | `constant_time_equals`, `random_bytes` |
| `safe_uuid` | SafeUUID | `generate_uuid_v4`, `parse_uuid`, `format_uuid`, `uuid_is_nil`, `uuid_get_version` |
| `safe_hex` | SafeHex | `hex_encode`, `hex_encode_upper`, `hex_decode` |
| `safe_float` | SafeFloat | `safe_divide`, `safe_sqrt`, `safe_log`, `is_finite`, `is_nan` |
| `safe_version` | SafeVersion | `parse_semver`, `semver_compare` |
| `safe_angle` | SafeAngle | `degrees_to_radians`, `radians_to_degrees`, `normalize_degrees`, `normalize_radians` |
| `safe_currency` | SafeCurrency | `parse_currency`, `format_currency` |
| `safe_phone` | SafePhone | `parse_phone`, `format_phone_e164` |
| `proven` | Umbrella | Re-exports all of the above |

## Result types

Each module exposes a Fortran result type that mirrors the C result
struct.  All result types include:

- `ok` (logical) -- `.true.` if the operation succeeded
- `status` (integer) -- the raw `ProvenStatus` code (0 = OK, negative = error)

## Memory management

String results returned by libproven are copied into Fortran allocatable
strings and immediately freed via `proven_free_string`.  Callers do not
need to manage C memory.

## Compatibility

Requires a Fortran compiler with:
- Fortran 2003 `iso_c_binding` support
- Fortran 2003 allocatable strings and derived types

Compatible compilers:
- gfortran 4.8+
- Intel Fortran (ifort / ifx)
- NAG Fortran
- Cray Fortran
- IBM XL Fortran
- NVIDIA nvfortran

## License

PMPL-1.0-or-later
