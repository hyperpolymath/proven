! SPDX-License-Identifier: PMPL-1.0
! SPDX-FileCopyrightText: 2025 Hyperpolymath
!
! Proven SafeMath - Overflow-checked arithmetic for Fortran
!

module safe_math
    use, intrinsic :: iso_fortran_env, only: int64, int32
    implicit none
    private

    ! Maximum and minimum values for 64-bit integers
    integer(int64), parameter, public :: MAX_INT64 = 9223372036854775807_int64
    integer(int64), parameter, public :: MIN_INT64 = -9223372036854775807_int64 - 1_int64

    ! Result type for safe operations
    type, public :: SafeResult
        integer(int64) :: value = 0
        logical :: ok = .false.
    end type SafeResult

    public :: safe_add, safe_sub, safe_mul, safe_div, safe_mod
    public :: safe_abs, safe_negate, clamp, in_range

contains

    !> Add two integers with overflow checking
    function safe_add(a, b) result(res)
        integer(int64), intent(in) :: a, b
        type(SafeResult) :: res

        res%ok = .true.

        ! Check for positive overflow: b > 0 and a > MAX - b
        if (b > 0) then
            if (a > MAX_INT64 - b) then
                res%ok = .false.
                return
            end if
        end if

        ! Check for negative overflow: b < 0 and a < MIN - b
        if (b < 0) then
            if (a < MIN_INT64 - b) then
                res%ok = .false.
                return
            end if
        end if

        res%value = a + b
    end function safe_add

    !> Subtract two integers with overflow checking
    function safe_sub(a, b) result(res)
        integer(int64), intent(in) :: a, b
        type(SafeResult) :: res

        res%ok = .true.

        ! Check for positive overflow: b < 0 and a > MAX + b
        if (b < 0) then
            if (a > MAX_INT64 + b) then
                res%ok = .false.
                return
            end if
        end if

        ! Check for negative overflow: b > 0 and a < MIN + b
        if (b > 0) then
            if (a < MIN_INT64 + b) then
                res%ok = .false.
                return
            end if
        end if

        res%value = a - b
    end function safe_sub

    !> Multiply two integers with overflow checking
    function safe_mul(a, b) result(res)
        integer(int64), intent(in) :: a, b
        type(SafeResult) :: res
        integer(int64) :: product

        res%ok = .true.

        ! Handle zero cases
        if (a == 0 .or. b == 0) then
            res%value = 0
            return
        end if

        ! Compute product
        product = a * b

        ! Verify by division
        if (a /= 0) then
            if (product / a /= b) then
                res%ok = .false.
                return
            end if
        end if

        res%value = product
    end function safe_mul

    !> Divide two integers with zero check
    function safe_div(a, b) result(res)
        integer(int64), intent(in) :: a, b
        type(SafeResult) :: res

        res%ok = .true.

        ! Check for division by zero
        if (b == 0) then
            res%ok = .false.
            return
        end if

        ! Check for MIN / -1 overflow
        if (a == MIN_INT64 .and. b == -1) then
            res%ok = .false.
            return
        end if

        res%value = a / b
    end function safe_div

    !> Modulo operation with zero check
    function safe_mod(a, b) result(res)
        integer(int64), intent(in) :: a, b
        type(SafeResult) :: res

        res%ok = .true.

        if (b == 0) then
            res%ok = .false.
            return
        end if

        res%value = mod(a, b)
    end function safe_mod

    !> Safe absolute value
    function safe_abs(a) result(res)
        integer(int64), intent(in) :: a
        type(SafeResult) :: res

        res%ok = .true.

        if (a == MIN_INT64) then
            res%ok = .false.
            return
        end if

        res%value = abs(a)
    end function safe_abs

    !> Safe negate
    function safe_negate(a) result(res)
        integer(int64), intent(in) :: a
        type(SafeResult) :: res

        res%ok = .true.

        if (a == MIN_INT64) then
            res%ok = .false.
            return
        end if

        res%value = -a
    end function safe_negate

    !> Clamp value to range
    pure function clamp(val, min_val, max_val) result(clamped)
        integer(int64), intent(in) :: val, min_val, max_val
        integer(int64) :: clamped

        if (val < min_val) then
            clamped = min_val
        else if (val > max_val) then
            clamped = max_val
        else
            clamped = val
        end if
    end function clamp

    !> Check if value is in range
    pure function in_range(val, min_val, max_val) result(is_in)
        integer(int64), intent(in) :: val, min_val, max_val
        logical :: is_in

        is_in = val >= min_val .and. val <= max_val
    end function in_range

end module safe_math
