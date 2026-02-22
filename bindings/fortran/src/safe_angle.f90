! SPDX-License-Identifier: PMPL-1.0-or-later
! Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
!
! Proven SafeAngle - Angle conversions and normalization for Fortran
!
! Thin wrapper around libproven's verified SafeAngle module.
! All angle conversion and normalization logic is executed in the Idris 2
! core via the Zig FFI bridge; this module only marshals types between
! Fortran and C.
!
! Link with: -lproven

module safe_angle
    use, intrinsic :: iso_c_binding
    use proven_ffi
    implicit none
    private

    public :: degrees_to_radians, radians_to_degrees
    public :: normalize_degrees, normalize_radians

contains

    ! =========================================================================
    ! Convert degrees to radians  ->  proven_angle_deg_to_rad
    ! =========================================================================
    function degrees_to_radians(degrees) result(radians)
        real(c_double), intent(in) :: degrees
        real(c_double) :: radians

        radians = proven_angle_deg_to_rad(degrees)
    end function degrees_to_radians

    ! =========================================================================
    ! Convert radians to degrees  ->  proven_angle_rad_to_deg
    ! =========================================================================
    function radians_to_degrees(radians) result(degrees)
        real(c_double), intent(in) :: radians
        real(c_double) :: degrees

        degrees = proven_angle_rad_to_deg(radians)
    end function radians_to_degrees

    ! =========================================================================
    ! Normalize degrees to [0, 360)  ->  proven_angle_normalize_degrees
    ! =========================================================================
    function normalize_degrees(degrees) result(normalized)
        real(c_double), intent(in) :: degrees
        real(c_double) :: normalized

        normalized = proven_angle_normalize_degrees(degrees)
    end function normalize_degrees

    ! =========================================================================
    ! Normalize radians to [0, 2*pi)  ->  proven_angle_normalize_radians
    ! =========================================================================
    function normalize_radians(radians) result(normalized)
        real(c_double), intent(in) :: radians
        real(c_double) :: normalized

        normalized = proven_angle_normalize_radians(radians)
    end function normalize_radians

end module safe_angle
