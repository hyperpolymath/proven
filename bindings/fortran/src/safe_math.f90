! SPDX-License-Identifier: PMPL-1.0-or-later
! Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
!
! Proven SafeMath - Overflow-checked arithmetic for Fortran
!
! Thin wrapper around libproven's verified SafeMath module.
! All arithmetic logic is executed in the Idris 2 core via the Zig FFI
! bridge; this module only marshals types between Fortran and C.
!
! Link with: -lproven

module safe_math
    use, intrinsic :: iso_c_binding
    use proven_ffi
    implicit none
    private

    !> Result type for safe integer operations.
    !> Maps directly to ProvenIntResult { status, value }.
    type, public :: SafeResult
        integer(c_int64_t) :: value  = 0_c_int64_t
        logical            :: ok     = .false.
        integer(c_int32_t) :: status = PROVEN_ERR_NOT_IMPLEMENTED
    end type SafeResult

    public :: safe_add, safe_sub, safe_mul, safe_div, safe_mod
    public :: safe_abs, safe_pow, clamp

contains

    ! =========================================================================
    ! Checked addition  ->  proven_math_add_checked
    ! =========================================================================
    function safe_add(a, b) result(res)
        integer(c_int64_t), intent(in) :: a, b
        type(SafeResult) :: res
        type(c_proven_int_result) :: c_res

        c_res  = proven_math_add_checked(a, b)
        res%status = c_res%status
        res%value  = c_res%value
        res%ok     = (c_res%status == PROVEN_OK)
    end function safe_add

    ! =========================================================================
    ! Checked subtraction  ->  proven_math_sub_checked
    ! =========================================================================
    function safe_sub(a, b) result(res)
        integer(c_int64_t), intent(in) :: a, b
        type(SafeResult) :: res
        type(c_proven_int_result) :: c_res

        c_res  = proven_math_sub_checked(a, b)
        res%status = c_res%status
        res%value  = c_res%value
        res%ok     = (c_res%status == PROVEN_OK)
    end function safe_sub

    ! =========================================================================
    ! Checked multiplication  ->  proven_math_mul_checked
    ! =========================================================================
    function safe_mul(a, b) result(res)
        integer(c_int64_t), intent(in) :: a, b
        type(SafeResult) :: res
        type(c_proven_int_result) :: c_res

        c_res  = proven_math_mul_checked(a, b)
        res%status = c_res%status
        res%value  = c_res%value
        res%ok     = (c_res%status == PROVEN_OK)
    end function safe_mul

    ! =========================================================================
    ! Safe division  ->  proven_math_div
    ! =========================================================================
    function safe_div(a, b) result(res)
        integer(c_int64_t), intent(in) :: a, b
        type(SafeResult) :: res
        type(c_proven_int_result) :: c_res

        c_res  = proven_math_div(a, b)
        res%status = c_res%status
        res%value  = c_res%value
        res%ok     = (c_res%status == PROVEN_OK)
    end function safe_div

    ! =========================================================================
    ! Safe modulo  ->  proven_math_mod
    ! =========================================================================
    function safe_mod(a, b) result(res)
        integer(c_int64_t), intent(in) :: a, b
        type(SafeResult) :: res
        type(c_proven_int_result) :: c_res

        c_res  = proven_math_mod(a, b)
        res%status = c_res%status
        res%value  = c_res%value
        res%ok     = (c_res%status == PROVEN_OK)
    end function safe_mod

    ! =========================================================================
    ! Safe absolute value  ->  proven_math_abs_safe
    ! =========================================================================
    function safe_abs(a) result(res)
        integer(c_int64_t), intent(in) :: a
        type(SafeResult) :: res
        type(c_proven_int_result) :: c_res

        c_res  = proven_math_abs_safe(a)
        res%status = c_res%status
        res%value  = c_res%value
        res%ok     = (c_res%status == PROVEN_OK)
    end function safe_abs

    ! =========================================================================
    ! Checked exponentiation  ->  proven_math_pow_checked
    ! =========================================================================
    function safe_pow(base, exp) result(res)
        integer(c_int64_t), intent(in) :: base
        integer(c_int32_t), intent(in) :: exp
        type(SafeResult) :: res
        type(c_proven_int_result) :: c_res

        c_res  = proven_math_pow_checked(base, exp)
        res%status = c_res%status
        res%value  = c_res%value
        res%ok     = (c_res%status == PROVEN_OK)
    end function safe_pow

    ! =========================================================================
    ! Clamp value to [lo, hi]  ->  proven_math_clamp
    ! =========================================================================
    function clamp(val, lo, hi) result(clamped)
        integer(c_int64_t), intent(in) :: val, lo, hi
        integer(c_int64_t) :: clamped

        clamped = proven_math_clamp(lo, hi, val)
    end function clamp

end module safe_math
