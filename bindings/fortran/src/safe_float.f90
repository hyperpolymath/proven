! SPDX-License-Identifier: Apache-2.0
! SPDX-FileCopyrightText: 2025 Hyperpolymath
!
! Proven SafeFloat - Safe floating-point operations for Fortran
!

module safe_float
    use, intrinsic :: iso_fortran_env, only: real64, real32, int64
    use, intrinsic :: ieee_arithmetic
    implicit none
    private

    !> Default tolerance for comparisons
    real(real64), parameter :: DEFAULT_TOLERANCE = 1.0e-10_real64
    real(real64), parameter :: DEFAULT_REL_TOLERANCE = 1.0e-9_real64

    !> Special value constants
    real(real64), parameter, public :: POSITIVE_INFINITY = huge(1.0_real64)
    real(real64), parameter, public :: NEGATIVE_INFINITY = -huge(1.0_real64)
    real(real64), parameter, public :: EPSILON_FLOAT64 = epsilon(1.0_real64)

    !> Float result type
    type, public :: FloatResult
        real(real64) :: value = 0.0_real64
        character(len=128) :: error = ''
        logical :: ok = .false.
    end type FloatResult

    public :: is_nan, is_infinite, is_finite, is_normal
    public :: safe_divide, safe_sqrt, safe_log, safe_log10
    public :: safe_exp, safe_pow, safe_asin, safe_acos
    public :: approximately_equal, relative_equal
    public :: clamp_float, lerp, inverse_lerp, remap
    public :: round_to_places, truncate_to_places
    public :: sign_of, copy_sign_float

contains

    !> Check if value is NaN
    pure function is_nan(x) result(is_nan_result)
        real(real64), intent(in) :: x
        logical :: is_nan_result

        is_nan_result = ieee_is_nan(x)
    end function is_nan

    !> Check if value is infinite
    pure function is_infinite(x) result(is_inf)
        real(real64), intent(in) :: x
        logical :: is_inf
        type(ieee_class_type) :: class_type

        class_type = ieee_class(x)
        is_inf = (class_type == ieee_positive_inf .or. class_type == ieee_negative_inf)
    end function is_infinite

    !> Check if value is finite (not NaN and not infinite)
    pure function is_finite(x) result(is_fin)
        real(real64), intent(in) :: x
        logical :: is_fin

        is_fin = ieee_is_finite(x)
    end function is_finite

    !> Check if value is normal (not denormalized, not zero, not NaN, not infinite)
    pure function is_normal(x) result(is_norm)
        real(real64), intent(in) :: x
        logical :: is_norm

        is_norm = ieee_is_normal(x)
    end function is_normal

    !> Safe division with zero check
    function safe_divide(numerator, denominator) result(res)
        real(real64), intent(in) :: numerator, denominator
        type(FloatResult) :: res

        if (abs(denominator) < tiny(1.0_real64)) then
            res%error = 'Division by zero or near-zero'
            return
        end if

        res%value = numerator / denominator

        if (.not. is_finite(res%value)) then
            res%error = 'Division resulted in infinite value'
            return
        end if

        res%ok = .true.
    end function safe_divide

    !> Safe square root with negative check
    function safe_sqrt(x) result(res)
        real(real64), intent(in) :: x
        type(FloatResult) :: res

        if (x < 0.0_real64) then
            res%error = 'Cannot take square root of negative number'
            return
        end if

        res%value = sqrt(x)
        res%ok = .true.
    end function safe_sqrt

    !> Safe natural logarithm with domain check
    function safe_log(x) result(res)
        real(real64), intent(in) :: x
        type(FloatResult) :: res

        if (x <= 0.0_real64) then
            res%error = 'Logarithm requires positive argument'
            return
        end if

        res%value = log(x)
        res%ok = .true.
    end function safe_log

    !> Safe base-10 logarithm with domain check
    function safe_log10(x) result(res)
        real(real64), intent(in) :: x
        type(FloatResult) :: res

        if (x <= 0.0_real64) then
            res%error = 'Logarithm requires positive argument'
            return
        end if

        res%value = log10(x)
        res%ok = .true.
    end function safe_log10

    !> Safe exponential with overflow check
    function safe_exp(x) result(res)
        real(real64), intent(in) :: x
        type(FloatResult) :: res
        real(real64), parameter :: MAX_EXP_ARG = 709.0_real64  ! ln(huge(1.0d0))

        if (x > MAX_EXP_ARG) then
            res%error = 'Exponential would overflow'
            return
        end if

        res%value = exp(x)
        res%ok = .true.
    end function safe_exp

    !> Safe power with domain checks
    function safe_pow(base, exponent) result(res)
        real(real64), intent(in) :: base, exponent
        type(FloatResult) :: res

        ! Handle special cases
        if (base < 0.0_real64 .and. abs(exponent - nint(exponent)) > EPSILON_FLOAT64) then
            res%error = 'Negative base with non-integer exponent'
            return
        end if

        if (base == 0.0_real64 .and. exponent < 0.0_real64) then
            res%error = 'Zero base with negative exponent'
            return
        end if

        res%value = base ** exponent

        if (.not. is_finite(res%value)) then
            res%error = 'Power operation resulted in non-finite value'
            return
        end if

        res%ok = .true.
    end function safe_pow

    !> Safe arcsine with domain check
    function safe_asin(x) result(res)
        real(real64), intent(in) :: x
        type(FloatResult) :: res

        if (x < -1.0_real64 .or. x > 1.0_real64) then
            res%error = 'Arcsine requires argument in [-1, 1]'
            return
        end if

        res%value = asin(x)
        res%ok = .true.
    end function safe_asin

    !> Safe arccosine with domain check
    function safe_acos(x) result(res)
        real(real64), intent(in) :: x
        type(FloatResult) :: res

        if (x < -1.0_real64 .or. x > 1.0_real64) then
            res%error = 'Arccosine requires argument in [-1, 1]'
            return
        end if

        res%value = acos(x)
        res%ok = .true.
    end function safe_acos

    !> Check if two floats are approximately equal (absolute tolerance)
    pure function approximately_equal(a, b, tolerance) result(approx_eq)
        real(real64), intent(in) :: a, b
        real(real64), intent(in), optional :: tolerance
        logical :: approx_eq
        real(real64) :: tol

        if (present(tolerance)) then
            tol = tolerance
        else
            tol = DEFAULT_TOLERANCE
        end if

        approx_eq = abs(a - b) <= tol
    end function approximately_equal

    !> Check if two floats are relatively equal
    pure function relative_equal(a, b, rel_tolerance) result(rel_eq)
        real(real64), intent(in) :: a, b
        real(real64), intent(in), optional :: rel_tolerance
        logical :: rel_eq
        real(real64) :: tol, max_abs

        if (present(rel_tolerance)) then
            tol = rel_tolerance
        else
            tol = DEFAULT_REL_TOLERANCE
        end if

        max_abs = max(abs(a), abs(b))

        if (max_abs < tiny(1.0_real64)) then
            rel_eq = .true.
            return
        end if

        rel_eq = abs(a - b) <= tol * max_abs
    end function relative_equal

    !> Clamp float to range
    pure function clamp_float(x, min_val, max_val) result(clamped)
        real(real64), intent(in) :: x, min_val, max_val
        real(real64) :: clamped

        if (x < min_val) then
            clamped = min_val
        else if (x > max_val) then
            clamped = max_val
        else
            clamped = x
        end if
    end function clamp_float

    !> Linear interpolation
    pure function lerp(a, b, t) result(interpolated)
        real(real64), intent(in) :: a, b, t
        real(real64) :: interpolated

        interpolated = a + (b - a) * t
    end function lerp

    !> Inverse linear interpolation
    function inverse_lerp(a, b, value) result(res)
        real(real64), intent(in) :: a, b, value
        type(FloatResult) :: res
        real(real64) :: range_val

        range_val = b - a

        if (abs(range_val) < tiny(1.0_real64)) then
            res%error = 'Cannot inverse lerp with zero range'
            return
        end if

        res%value = (value - a) / range_val
        res%ok = .true.
    end function inverse_lerp

    !> Remap value from one range to another
    function remap(value, in_min, in_max, out_min, out_max) result(res)
        real(real64), intent(in) :: value, in_min, in_max, out_min, out_max
        type(FloatResult) :: res
        type(FloatResult) :: t_result

        t_result = inverse_lerp(in_min, in_max, value)
        if (.not. t_result%ok) then
            res%error = t_result%error
            return
        end if

        res%value = lerp(out_min, out_max, t_result%value)
        res%ok = .true.
    end function remap

    !> Round to specified decimal places
    pure function round_to_places(x, places) result(rounded)
        real(real64), intent(in) :: x
        integer, intent(in) :: places
        real(real64) :: rounded
        real(real64) :: factor

        factor = 10.0_real64 ** places
        rounded = nint(x * factor) / factor
    end function round_to_places

    !> Truncate to specified decimal places
    pure function truncate_to_places(x, places) result(truncated)
        real(real64), intent(in) :: x
        integer, intent(in) :: places
        real(real64) :: truncated
        real(real64) :: factor

        factor = 10.0_real64 ** places
        truncated = aint(x * factor) / factor
    end function truncate_to_places

    !> Get sign of value (-1, 0, or 1)
    pure function sign_of(x) result(s)
        real(real64), intent(in) :: x
        integer :: s

        if (x > 0.0_real64) then
            s = 1
        else if (x < 0.0_real64) then
            s = -1
        else
            s = 0
        end if
    end function sign_of

    !> Copy sign from one value to another
    pure function copy_sign_float(magnitude, sign_source) result(result_val)
        real(real64), intent(in) :: magnitude, sign_source
        real(real64) :: result_val

        result_val = sign(magnitude, sign_source)
    end function copy_sign_float

end module safe_float
