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
    implicit none
end module proven
