! SPDX-License-Identifier: PMPL-1.0
! SPDX-FileCopyrightText: 2025 Hyperpolymath
!
! Proven SafeAngle - Angle operations and conversions for Fortran
!

module safe_angle
    use, intrinsic :: iso_fortran_env, only: real64, int64
    implicit none
    private

    !> Mathematical constants
    real(real64), parameter, public :: PI = 3.14159265358979323846_real64
    real(real64), parameter, public :: TWO_PI = 2.0_real64 * PI
    real(real64), parameter, public :: HALF_PI = PI / 2.0_real64
    real(real64), parameter, public :: DEG_TO_RAD = PI / 180.0_real64
    real(real64), parameter, public :: RAD_TO_DEG = 180.0_real64 / PI

    !> Angle unit enumeration
    integer, parameter, public :: ANGLE_UNIT_RADIANS = 1
    integer, parameter, public :: ANGLE_UNIT_DEGREES = 2
    integer, parameter, public :: ANGLE_UNIT_GRADIANS = 3
    integer, parameter, public :: ANGLE_UNIT_TURNS = 4

    !> Angle type
    type, public :: angle_type
        real(real64) :: radians = 0.0_real64
        logical :: valid = .false.
    contains
        procedure :: to_degrees => angle_to_degrees
        procedure :: to_radians => angle_to_radians
        procedure :: to_gradians => angle_to_gradians
        procedure :: to_turns => angle_to_turns
        procedure :: normalize => angle_normalize
        procedure :: sin => angle_sin
        procedure :: cos => angle_cos
        procedure :: tan => angle_tan
    end type angle_type

    !> Angle result type
    type, public :: AngleResult
        type(angle_type) :: angle
        character(len=128) :: error = ''
        logical :: ok = .false.
    end type AngleResult

    public :: degrees_to_radians, radians_to_degrees
    public :: gradians_to_radians, radians_to_gradians
    public :: turns_to_radians, radians_to_turns
    public :: angle_from_degrees, angle_from_radians
    public :: angle_from_gradians, angle_from_turns
    public :: normalize_degrees, normalize_radians
    public :: angle_add, angle_subtract, angle_between
    public :: safe_atan2, bearing_to_angle, angle_to_bearing
    public :: is_acute, is_right, is_obtuse, is_straight, is_reflex

contains

    !> Convert degrees to radians
    pure function degrees_to_radians(degrees) result(radians)
        real(real64), intent(in) :: degrees
        real(real64) :: radians

        radians = degrees * DEG_TO_RAD
    end function degrees_to_radians

    !> Convert radians to degrees
    pure function radians_to_degrees(radians) result(degrees)
        real(real64), intent(in) :: radians
        real(real64) :: degrees

        degrees = radians * RAD_TO_DEG
    end function radians_to_degrees

    !> Convert gradians to radians
    pure function gradians_to_radians(gradians) result(radians)
        real(real64), intent(in) :: gradians
        real(real64) :: radians

        radians = gradians * PI / 200.0_real64
    end function gradians_to_radians

    !> Convert radians to gradians
    pure function radians_to_gradians(radians) result(gradians)
        real(real64), intent(in) :: radians
        real(real64) :: gradians

        gradians = radians * 200.0_real64 / PI
    end function radians_to_gradians

    !> Convert turns to radians
    pure function turns_to_radians(turns) result(radians)
        real(real64), intent(in) :: turns
        real(real64) :: radians

        radians = turns * TWO_PI
    end function turns_to_radians

    !> Convert radians to turns
    pure function radians_to_turns(radians) result(turns)
        real(real64), intent(in) :: radians
        real(real64) :: turns

        turns = radians / TWO_PI
    end function radians_to_turns

    !> Create angle from degrees
    pure function angle_from_degrees(degrees) result(a)
        real(real64), intent(in) :: degrees
        type(angle_type) :: a

        a%radians = degrees_to_radians(degrees)
        a%valid = .true.
    end function angle_from_degrees

    !> Create angle from radians
    pure function angle_from_radians(radians) result(a)
        real(real64), intent(in) :: radians
        type(angle_type) :: a

        a%radians = radians
        a%valid = .true.
    end function angle_from_radians

    !> Create angle from gradians
    pure function angle_from_gradians(gradians) result(a)
        real(real64), intent(in) :: gradians
        type(angle_type) :: a

        a%radians = gradians_to_radians(gradians)
        a%valid = .true.
    end function angle_from_gradians

    !> Create angle from turns
    pure function angle_from_turns(turns) result(a)
        real(real64), intent(in) :: turns
        type(angle_type) :: a

        a%radians = turns_to_radians(turns)
        a%valid = .true.
    end function angle_from_turns

    !> Get angle in degrees
    pure function angle_to_degrees(self) result(degrees)
        class(angle_type), intent(in) :: self
        real(real64) :: degrees

        degrees = radians_to_degrees(self%radians)
    end function angle_to_degrees

    !> Get angle in radians
    pure function angle_to_radians(self) result(radians)
        class(angle_type), intent(in) :: self
        real(real64) :: radians

        radians = self%radians
    end function angle_to_radians

    !> Get angle in gradians
    pure function angle_to_gradians(self) result(gradians)
        class(angle_type), intent(in) :: self
        real(real64) :: gradians

        gradians = radians_to_gradians(self%radians)
    end function angle_to_gradians

    !> Get angle in turns
    pure function angle_to_turns(self) result(turns)
        class(angle_type), intent(in) :: self
        real(real64) :: turns

        turns = radians_to_turns(self%radians)
    end function angle_to_turns

    !> Normalize angle to [0, 2*PI)
    pure function angle_normalize(self) result(normalized)
        class(angle_type), intent(in) :: self
        type(angle_type) :: normalized

        normalized%radians = normalize_radians(self%radians)
        normalized%valid = .true.
    end function angle_normalize

    !> Normalize degrees to [0, 360)
    pure function normalize_degrees(degrees) result(normalized)
        real(real64), intent(in) :: degrees
        real(real64) :: normalized

        normalized = mod(degrees, 360.0_real64)
        if (normalized < 0.0_real64) then
            normalized = normalized + 360.0_real64
        end if
    end function normalize_degrees

    !> Normalize radians to [0, 2*PI)
    pure function normalize_radians(radians) result(normalized)
        real(real64), intent(in) :: radians
        real(real64) :: normalized

        normalized = mod(radians, TWO_PI)
        if (normalized < 0.0_real64) then
            normalized = normalized + TWO_PI
        end if
    end function normalize_radians

    !> Sine of angle
    pure function angle_sin(self) result(s)
        class(angle_type), intent(in) :: self
        real(real64) :: s

        s = sin(self%radians)
    end function angle_sin

    !> Cosine of angle
    pure function angle_cos(self) result(c)
        class(angle_type), intent(in) :: self
        real(real64) :: c

        c = cos(self%radians)
    end function angle_cos

    !> Tangent of angle
    function angle_tan(self) result(res)
        class(angle_type), intent(in) :: self
        type(AngleResult) :: res
        real(real64) :: normalized

        normalized = normalize_radians(self%radians)

        ! Check for undefined tan (at PI/2 and 3*PI/2)
        if (abs(normalized - HALF_PI) < 1.0e-10_real64 .or. &
            abs(normalized - 3.0_real64 * HALF_PI) < 1.0e-10_real64) then
            res%error = 'Tangent undefined at this angle'
            return
        end if

        res%angle%radians = tan(self%radians)
        res%ok = .true.
    end function angle_tan

    !> Add two angles
    pure function angle_add(a, b) result(c)
        type(angle_type), intent(in) :: a, b
        type(angle_type) :: c

        c%radians = a%radians + b%radians
        c%valid = .true.
    end function angle_add

    !> Subtract two angles
    pure function angle_subtract(a, b) result(c)
        type(angle_type), intent(in) :: a, b
        type(angle_type) :: c

        c%radians = a%radians - b%radians
        c%valid = .true.
    end function angle_subtract

    !> Calculate angle between two angles (smallest)
    pure function angle_between(a, b) result(diff)
        type(angle_type), intent(in) :: a, b
        type(angle_type) :: diff
        real(real64) :: d

        d = abs(normalize_radians(a%radians) - normalize_radians(b%radians))
        if (d > PI) d = TWO_PI - d

        diff%radians = d
        diff%valid = .true.
    end function angle_between

    !> Safe atan2 with proper quadrant handling
    function safe_atan2(y, x) result(res)
        real(real64), intent(in) :: y, x
        type(AngleResult) :: res

        if (abs(x) < 1.0e-15_real64 .and. abs(y) < 1.0e-15_real64) then
            res%error = 'atan2(0, 0) is undefined'
            return
        end if

        res%angle%radians = atan2(y, x)
        res%angle%valid = .true.
        res%ok = .true.
    end function safe_atan2

    !> Convert bearing (0=North, clockwise) to mathematical angle (0=East, CCW)
    pure function bearing_to_angle(bearing) result(a)
        real(real64), intent(in) :: bearing
        type(angle_type) :: a

        ! bearing 0 = North (math angle 90)
        ! bearing 90 = East (math angle 0)
        a%radians = degrees_to_radians(90.0_real64 - bearing)
        a%radians = normalize_radians(a%radians)
        a%valid = .true.
    end function bearing_to_angle

    !> Convert mathematical angle to bearing
    pure function angle_to_bearing(a) result(bearing)
        type(angle_type), intent(in) :: a
        real(real64) :: bearing

        bearing = 90.0_real64 - radians_to_degrees(a%radians)
        bearing = normalize_degrees(bearing)
    end function angle_to_bearing

    !> Check if angle is acute (0 < angle < 90 degrees)
    pure function is_acute(a) result(acute)
        type(angle_type), intent(in) :: a
        logical :: acute
        real(real64) :: deg

        deg = normalize_degrees(radians_to_degrees(abs(a%radians)))
        acute = deg > 0.0_real64 .and. deg < 90.0_real64
    end function is_acute

    !> Check if angle is right (exactly 90 degrees)
    pure function is_right(a) result(right)
        type(angle_type), intent(in) :: a
        logical :: right
        real(real64) :: deg

        deg = normalize_degrees(radians_to_degrees(abs(a%radians)))
        right = abs(deg - 90.0_real64) < 1.0e-10_real64
    end function is_right

    !> Check if angle is obtuse (90 < angle < 180 degrees)
    pure function is_obtuse(a) result(obtuse)
        type(angle_type), intent(in) :: a
        logical :: obtuse
        real(real64) :: deg

        deg = normalize_degrees(radians_to_degrees(abs(a%radians)))
        obtuse = deg > 90.0_real64 .and. deg < 180.0_real64
    end function is_obtuse

    !> Check if angle is straight (exactly 180 degrees)
    pure function is_straight(a) result(straight)
        type(angle_type), intent(in) :: a
        logical :: straight
        real(real64) :: deg

        deg = normalize_degrees(radians_to_degrees(abs(a%radians)))
        straight = abs(deg - 180.0_real64) < 1.0e-10_real64
    end function is_straight

    !> Check if angle is reflex (180 < angle < 360 degrees)
    pure function is_reflex(a) result(reflex)
        type(angle_type), intent(in) :: a
        logical :: reflex
        real(real64) :: deg

        deg = normalize_degrees(radians_to_degrees(abs(a%radians)))
        reflex = deg > 180.0_real64 .and. deg < 360.0_real64
    end function is_reflex

end module safe_angle
