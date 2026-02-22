! SPDX-License-Identifier: PMPL-1.0-or-later
! Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
!
! Proven SafeFloat - Safe floating-point operations for Fortran
!
! Thin wrapper around libproven's verified SafeFloat module.
! All floating-point safety logic is executed in the Idris 2 core via
! the Zig FFI bridge; this module only marshals types between Fortran
! and C.
!
! Link with: -lproven

module safe_float
    use, intrinsic :: iso_c_binding
    use proven_ffi
    implicit none
    private

    !> Result type for safe float operations
    type, public :: FloatResult
        real(c_double)     :: value  = 0.0_c_double
        logical            :: ok     = .false.
        integer(c_int32_t) :: status = PROVEN_ERR_NOT_IMPLEMENTED
    end type FloatResult

    public :: safe_divide, safe_sqrt, safe_log
    public :: is_finite, is_nan

contains

    ! =========================================================================
    ! Safe floating-point division  ->  proven_float_div
    ! =========================================================================
    function safe_divide(numerator, denominator) result(res)
        real(c_double), intent(in) :: numerator, denominator
        type(FloatResult) :: res
        type(c_proven_float_result) :: c_res

        c_res = proven_float_div(numerator, denominator)

        res%status = c_res%status
        res%value  = c_res%value
        res%ok     = (c_res%status == PROVEN_OK)
    end function safe_divide

    ! =========================================================================
    ! Safe square root  ->  proven_float_sqrt
    ! =========================================================================
    function safe_sqrt(x) result(res)
        real(c_double), intent(in) :: x
        type(FloatResult) :: res
        type(c_proven_float_result) :: c_res

        c_res = proven_float_sqrt(x)

        res%status = c_res%status
        res%value  = c_res%value
        res%ok     = (c_res%status == PROVEN_OK)
    end function safe_sqrt

    ! =========================================================================
    ! Safe natural logarithm  ->  proven_float_ln
    ! =========================================================================
    function safe_log(x) result(res)
        real(c_double), intent(in) :: x
        type(FloatResult) :: res
        type(c_proven_float_result) :: c_res

        c_res = proven_float_ln(x)

        res%status = c_res%status
        res%value  = c_res%value
        res%ok     = (c_res%status == PROVEN_OK)
    end function safe_log

    ! =========================================================================
    ! Check if a double is finite  ->  proven_float_is_finite
    ! =========================================================================
    function is_finite(x) result(finite)
        real(c_double), intent(in) :: x
        logical :: finite

        finite = logical(proven_float_is_finite(x))
    end function is_finite

    ! =========================================================================
    ! Check if a double is NaN  ->  proven_float_is_nan
    ! =========================================================================
    function is_nan(x) result(nan)
        real(c_double), intent(in) :: x
        logical :: nan

        nan = logical(proven_float_is_nan(x))
    end function is_nan

end module safe_float
