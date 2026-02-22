! SPDX-License-Identifier: PMPL-1.0-or-later
! Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
!
! Proven - Safe, validated operations library for Fortran
!
! Umbrella module that re-exports all proven sub-modules for convenience.
! Every sub-module is a thin iso_c_binding wrapper that delegates to
! libproven (Idris 2 core + Zig FFI bridge).
!
! Link with: -lproven

module proven
    use proven_ffi
    use safe_math
    use safe_string
    use safe_path
    use safe_email
    use safe_network
    use safe_crypto
    use safe_uuid
    use safe_hex
    use safe_float
    use safe_version
    use safe_angle
    use safe_currency
    use safe_phone
    implicit none
end module proven
