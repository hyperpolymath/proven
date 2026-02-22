! SPDX-License-Identifier: PMPL-1.0-or-later
! Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
!
! Proven SafeCrypto - Cryptographic primitives for Fortran
!
! Thin wrapper around libproven's verified SafeCrypto module.
! All cryptographic logic (constant-time comparison, CSPRNG) is executed
! in the Idris 2 core via the Zig FFI bridge; this module only marshals
! types between Fortran and C.
!
! Link with: -lproven

module safe_crypto
    use, intrinsic :: iso_c_binding
    use proven_ffi
    implicit none
    private

    public :: constant_time_equals, random_bytes

contains

    ! =========================================================================
    ! Constant-time comparison  ->  proven_crypto_constant_time_eq
    ! =========================================================================
    function constant_time_equals(a, b) result(is_equal)
        character(len=*), intent(in) :: a, b
        logical :: is_equal
        character(len=len(a), kind=c_char), target :: c_buf_a
        character(len=len(b), kind=c_char), target :: c_buf_b
        type(c_proven_bool_result) :: c_res

        c_buf_a = a
        c_buf_b = b

        c_res = proven_crypto_constant_time_eq( &
            c_loc(c_buf_a), int(len(a), c_size_t), &
            c_loc(c_buf_b), int(len(b), c_size_t))

        if (c_res%status == PROVEN_OK) then
            is_equal = logical(c_res%value)
        else
            is_equal = .false.
        end if
    end function constant_time_equals

    ! =========================================================================
    ! Fill buffer with cryptographically secure random bytes
    ! ->  proven_crypto_random_bytes
    ! =========================================================================
    function random_bytes(buf, length) result(status)
        integer(c_int8_t), target, intent(inout) :: buf(:)
        integer, intent(in) :: length
        integer(c_int32_t) :: status

        status = proven_crypto_random_bytes( &
            c_loc(buf), int(length, c_size_t))
    end function random_bytes

end module safe_crypto
