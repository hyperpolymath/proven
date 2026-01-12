! SPDX-License-Identifier: PMPL-1.0
! SPDX-FileCopyrightText: 2025 Hyperpolymath
!
! Proven - Safe, validated operations library for Fortran
!
! This module re-exports all proven modules for convenience.
!

module proven
    use safe_math
    use safe_string
    use safe_path
    use safe_email
    use safe_network
    use safe_crypto
    use safe_uuid
    use safe_currency
    use safe_phone
    use safe_hex
    implicit none
end module proven
