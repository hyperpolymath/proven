! SPDX-License-Identifier: PMPL-1.0-or-later
! Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
!
! Proven SafeVersion - Semantic versioning for Fortran
!
! Thin wrapper around libproven's verified SafeVersion module.
! All version parsing and comparison logic is executed in the Idris 2
! core via the Zig FFI bridge; this module only marshals types between
! Fortran and C.
!
! Link with: -lproven

module safe_version
    use, intrinsic :: iso_c_binding
    use proven_ffi
    implicit none
    private

    !> Maximum prerelease string length (Fortran-side storage)
    integer, parameter :: MAX_PRERELEASE_LEN = 64

    !> Semantic version type (Fortran-facing)
    type, public :: semver_type
        integer(c_int32_t) :: major = 0
        integer(c_int32_t) :: minor = 0
        integer(c_int32_t) :: patch = 0
        character(len=MAX_PRERELEASE_LEN) :: prerelease = ''
        logical :: valid = .false.
    end type semver_type

    !> Version result type
    type, public :: VersionResult
        type(semver_type)  :: version
        logical            :: ok     = .false.
        integer(c_int32_t) :: status = PROVEN_ERR_NOT_IMPLEMENTED
    end type VersionResult

    public :: parse_semver, semver_compare

contains

    ! =========================================================================
    ! Parse semantic version string  ->  proven_version_parse
    ! =========================================================================
    function parse_semver(version_string) result(res)
        character(len=*), intent(in) :: version_string
        type(VersionResult) :: res
        character(len=len_trim(version_string), kind=c_char), target :: c_buf
        type(c_proven_version_result) :: c_res
        integer :: trimmed_len

        trimmed_len = len_trim(version_string)
        c_buf = version_string(1:trimmed_len)

        c_res = proven_version_parse(c_loc(c_buf), int(trimmed_len, c_size_t))

        res%status = c_res%status
        res%ok     = (c_res%status == PROVEN_OK)

        if (res%ok) then
            res%version%major = c_res%version%major
            res%version%minor = c_res%version%minor
            res%version%patch = c_res%version%patch
            res%version%valid = .true.

            ! Copy prerelease string if present
            if (c_res%version%prerelease_len > 0 .and. &
                c_associated(c_res%version%prerelease)) then
                block
                    character(len=1, kind=c_char), pointer :: f_ptr(:)
                    integer :: i, copy_len

                    call c_f_pointer(c_res%version%prerelease, f_ptr, &
                                     [c_res%version%prerelease_len])
                    copy_len = min(int(c_res%version%prerelease_len), &
                                   MAX_PRERELEASE_LEN)
                    res%version%prerelease = ''
                    do i = 1, copy_len
                        res%version%prerelease(i:i) = f_ptr(i)
                    end do
                end block
            end if

            ! Free C-allocated prerelease string
            if (c_associated(c_res%version%prerelease)) then
                call proven_version_free(c_res%version%prerelease)
            end if
        end if
    end function parse_semver

    ! =========================================================================
    ! Compare two semantic versions  ->  proven_version_compare
    ! =========================================================================
    function semver_compare(a, b) result(cmp)
        type(semver_type), intent(in) :: a, b
        integer(c_int32_t) :: cmp
        type(c_proven_semantic_version) :: c_a, c_b

        ! Marshal Fortran semver to C struct
        c_a%major = a%major
        c_a%minor = a%minor
        c_a%patch = a%patch
        c_a%prerelease_len = 0_c_size_t
        c_a%prerelease = c_null_ptr

        c_b%major = b%major
        c_b%minor = b%minor
        c_b%patch = b%patch
        c_b%prerelease_len = 0_c_size_t
        c_b%prerelease = c_null_ptr

        ! Note: prerelease strings are not passed through for comparison.
        ! The C function compares major.minor.patch; for prerelease
        ! comparison the full parse result should be retained on the C side.

        cmp = proven_version_compare(c_a, c_b)
    end function semver_compare

end module safe_version
