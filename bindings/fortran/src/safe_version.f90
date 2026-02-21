! SPDX-License-Identifier: PMPL-1.0-or-later
! SPDX-FileCopyrightText: 2025 Hyperpolymath
!
! Proven SafeVersion - Semantic versioning for Fortran
!

module safe_version
    use, intrinsic :: iso_fortran_env, only: int64, int32
    implicit none
    private

    !> Maximum prerelease/build metadata length
    integer, parameter :: MAX_PRERELEASE_LEN = 64
    integer, parameter :: MAX_BUILD_LEN = 64

    !> Semantic version type
    type, public :: semver_type
        integer :: major = 0
        integer :: minor = 0
        integer :: patch = 0
        character(len=MAX_PRERELEASE_LEN) :: prerelease = ''
        character(len=MAX_BUILD_LEN) :: build = ''
        logical :: valid = .false.
    contains
        procedure :: to_string => semver_to_string
        procedure :: is_prerelease => semver_is_prerelease
        procedure :: is_stable => semver_is_stable
    end type semver_type

    !> Version result type
    type, public :: VersionResult
        type(semver_type) :: version
        character(len=256) :: error = ''
        logical :: ok = .false.
    end type VersionResult

    !> Version range type
    type, public :: version_range_type
        type(semver_type) :: min_version
        type(semver_type) :: max_version
        logical :: min_inclusive = .true.
        logical :: max_inclusive = .false.
        logical :: valid = .false.
    end type version_range_type

    public :: parse_semver, format_semver, is_valid_semver
    public :: semver_compare, semver_equal, semver_less_than, semver_greater_than
    public :: semver_satisfies_range, increment_major, increment_minor, increment_patch
    public :: parse_version_range, semver_compatible

contains

    !> Parse a semantic version string (e.g., "1.2.3-alpha+build")
    function parse_semver(version_string) result(res)
        character(len=*), intent(in) :: version_string
        type(VersionResult) :: res
        integer :: str_len, pos, dot1, dot2, hyphen, plus
        integer :: io_stat
        character(len=256) :: working_str

        str_len = len_trim(version_string)

        if (str_len == 0) then
            res%error = 'Empty version string'
            return
        end if

        working_str = adjustl(version_string)

        ! Skip optional 'v' prefix
        pos = 1
        if (working_str(1:1) == 'v' .or. working_str(1:1) == 'V') then
            pos = 2
            working_str = working_str(2:)
            str_len = str_len - 1
        end if

        ! Find separators
        dot1 = index(working_str, '.')
        if (dot1 == 0) then
            res%error = 'Missing first dot separator'
            return
        end if

        dot2 = index(working_str(dot1+1:), '.')
        if (dot2 == 0) then
            res%error = 'Missing second dot separator'
            return
        end if
        dot2 = dot1 + dot2

        ! Find prerelease (-) and build (+) separators
        hyphen = index(working_str(dot2+1:), '-')
        if (hyphen > 0) hyphen = dot2 + hyphen

        plus = index(working_str(dot2+1:), '+')
        if (plus > 0) plus = dot2 + plus

        ! Parse major version
        read(working_str(1:dot1-1), *, iostat=io_stat) res%version%major
        if (io_stat /= 0 .or. res%version%major < 0) then
            res%error = 'Invalid major version'
            return
        end if

        ! Parse minor version
        read(working_str(dot1+1:dot2-1), *, iostat=io_stat) res%version%minor
        if (io_stat /= 0 .or. res%version%minor < 0) then
            res%error = 'Invalid minor version'
            return
        end if

        ! Determine end of patch version
        if (hyphen > 0 .and. (plus == 0 .or. hyphen < plus)) then
            ! Prerelease present
            read(working_str(dot2+1:hyphen-1), *, iostat=io_stat) res%version%patch
            if (io_stat /= 0 .or. res%version%patch < 0) then
                res%error = 'Invalid patch version'
                return
            end if

            if (plus > 0) then
                res%version%prerelease = working_str(hyphen+1:plus-1)
                res%version%build = working_str(plus+1:str_len)
            else
                res%version%prerelease = working_str(hyphen+1:str_len)
            end if
        else if (plus > 0) then
            ! Only build metadata
            read(working_str(dot2+1:plus-1), *, iostat=io_stat) res%version%patch
            if (io_stat /= 0 .or. res%version%patch < 0) then
                res%error = 'Invalid patch version'
                return
            end if
            res%version%build = working_str(plus+1:str_len)
        else
            ! Just major.minor.patch
            read(working_str(dot2+1:str_len), *, iostat=io_stat) res%version%patch
            if (io_stat /= 0 .or. res%version%patch < 0) then
                res%error = 'Invalid patch version'
                return
            end if
        end if

        res%version%valid = .true.
        res%ok = .true.
    end function parse_semver

    !> Format version as string
    function semver_to_string(self) result(version_string)
        class(semver_type), intent(in) :: self
        character(len=128) :: version_string
        character(len=16) :: major_str, minor_str, patch_str

        if (.not. self%valid) then
            version_string = ''
            return
        end if

        write(major_str, '(I0)') self%major
        write(minor_str, '(I0)') self%minor
        write(patch_str, '(I0)') self%patch

        version_string = trim(adjustl(major_str)) // '.' // &
                         trim(adjustl(minor_str)) // '.' // &
                         trim(adjustl(patch_str))

        if (len_trim(self%prerelease) > 0) then
            version_string = trim(version_string) // '-' // trim(self%prerelease)
        end if

        if (len_trim(self%build) > 0) then
            version_string = trim(version_string) // '+' // trim(self%build)
        end if
    end function semver_to_string

    !> Standalone format function
    function format_semver(version) result(version_string)
        type(semver_type), intent(in) :: version
        character(len=128) :: version_string

        version_string = version%to_string()
    end function format_semver

    !> Check if version string is valid
    function is_valid_semver(version_string) result(is_valid)
        character(len=*), intent(in) :: version_string
        logical :: is_valid
        type(VersionResult) :: res

        res = parse_semver(version_string)
        is_valid = res%ok
    end function is_valid_semver

    !> Check if version has prerelease tag
    pure function semver_is_prerelease(self) result(is_pre)
        class(semver_type), intent(in) :: self
        logical :: is_pre

        is_pre = len_trim(self%prerelease) > 0
    end function semver_is_prerelease

    !> Check if version is stable (major > 0 and no prerelease)
    pure function semver_is_stable(self) result(is_stable)
        class(semver_type), intent(in) :: self
        logical :: is_stable

        is_stable = self%major > 0 .and. len_trim(self%prerelease) == 0
    end function semver_is_stable

    !> Compare two versions
    !> Returns: -1 if a < b, 0 if a == b, 1 if a > b
    pure function semver_compare(a, b) result(cmp)
        type(semver_type), intent(in) :: a, b
        integer :: cmp
        integer :: pre_cmp

        cmp = 0

        ! Compare major
        if (a%major < b%major) then
            cmp = -1
            return
        else if (a%major > b%major) then
            cmp = 1
            return
        end if

        ! Compare minor
        if (a%minor < b%minor) then
            cmp = -1
            return
        else if (a%minor > b%minor) then
            cmp = 1
            return
        end if

        ! Compare patch
        if (a%patch < b%patch) then
            cmp = -1
            return
        else if (a%patch > b%patch) then
            cmp = 1
            return
        end if

        ! Compare prerelease (no prerelease > has prerelease)
        if (len_trim(a%prerelease) == 0 .and. len_trim(b%prerelease) > 0) then
            cmp = 1
            return
        else if (len_trim(a%prerelease) > 0 .and. len_trim(b%prerelease) == 0) then
            cmp = -1
            return
        else if (len_trim(a%prerelease) > 0 .and. len_trim(b%prerelease) > 0) then
            ! Lexicographic comparison of prerelease
            if (llt(trim(a%prerelease), trim(b%prerelease))) then
                cmp = -1
            else if (lgt(trim(a%prerelease), trim(b%prerelease))) then
                cmp = 1
            end if
        end if

        ! Build metadata does not affect precedence
    end function semver_compare

    !> Check if two versions are equal
    pure function semver_equal(a, b) result(is_equal)
        type(semver_type), intent(in) :: a, b
        logical :: is_equal

        is_equal = semver_compare(a, b) == 0
    end function semver_equal

    !> Check if a < b
    pure function semver_less_than(a, b) result(is_less)
        type(semver_type), intent(in) :: a, b
        logical :: is_less

        is_less = semver_compare(a, b) < 0
    end function semver_less_than

    !> Check if a > b
    pure function semver_greater_than(a, b) result(is_greater)
        type(semver_type), intent(in) :: a, b
        logical :: is_greater

        is_greater = semver_compare(a, b) > 0
    end function semver_greater_than

    !> Check if version satisfies a range
    function semver_satisfies_range(version, range) result(satisfies)
        type(semver_type), intent(in) :: version
        type(version_range_type), intent(in) :: range
        logical :: satisfies
        integer :: min_cmp, max_cmp

        satisfies = .false.

        if (.not. version%valid .or. .not. range%valid) return

        min_cmp = semver_compare(version, range%min_version)
        max_cmp = semver_compare(version, range%max_version)

        ! Check minimum
        if (range%min_inclusive) then
            if (min_cmp < 0) return
        else
            if (min_cmp <= 0) return
        end if

        ! Check maximum
        if (range%max_inclusive) then
            if (max_cmp > 0) return
        else
            if (max_cmp >= 0) return
        end if

        satisfies = .true.
    end function semver_satisfies_range

    !> Parse a version range string (e.g., ">=1.0.0 <2.0.0")
    function parse_version_range(range_string) result(range)
        character(len=*), intent(in) :: range_string
        type(version_range_type) :: range
        integer :: space_pos
        character(len=128) :: min_str, max_str
        type(VersionResult) :: min_res, max_res

        space_pos = index(range_string, ' ')

        if (space_pos == 0) then
            ! Single version constraint
            min_str = adjustl(range_string)
            max_str = ''
        else
            min_str = adjustl(range_string(1:space_pos-1))
            max_str = adjustl(range_string(space_pos+1:))
        end if

        ! Parse minimum constraint
        if (min_str(1:2) == '>=') then
            range%min_inclusive = .true.
            min_res = parse_semver(min_str(3:))
        else if (min_str(1:1) == '>') then
            range%min_inclusive = .false.
            min_res = parse_semver(min_str(2:))
        else
            range%min_inclusive = .true.
            min_res = parse_semver(min_str)
        end if

        if (.not. min_res%ok) return
        range%min_version = min_res%version

        ! Parse maximum constraint if present
        if (len_trim(max_str) > 0) then
            if (max_str(1:2) == '<=') then
                range%max_inclusive = .true.
                max_res = parse_semver(max_str(3:))
            else if (max_str(1:1) == '<') then
                range%max_inclusive = .false.
                max_res = parse_semver(max_str(2:))
            else
                range%max_inclusive = .true.
                max_res = parse_semver(max_str)
            end if

            if (.not. max_res%ok) return
            range%max_version = max_res%version
        else
            ! Default max to very high version
            range%max_version%major = 999999
            range%max_version%minor = 999999
            range%max_version%patch = 999999
            range%max_version%valid = .true.
            range%max_inclusive = .true.
        end if

        range%valid = .true.
    end function parse_version_range

    !> Check if two versions are compatible (same major, minor >= minor)
    pure function semver_compatible(required, available) result(compatible)
        type(semver_type), intent(in) :: required, available
        logical :: compatible

        compatible = .false.

        if (.not. required%valid .or. .not. available%valid) return

        ! Major must match
        if (required%major /= available%major) return

        ! Available minor must be >= required minor
        if (available%minor < required%minor) return

        ! If same minor, available patch must be >= required patch
        if (available%minor == required%minor) then
            if (available%patch < required%patch) return
        end if

        compatible = .true.
    end function semver_compatible

    !> Increment major version
    pure function increment_major(version) result(new_version)
        type(semver_type), intent(in) :: version
        type(semver_type) :: new_version

        new_version%major = version%major + 1
        new_version%minor = 0
        new_version%patch = 0
        new_version%prerelease = ''
        new_version%build = ''
        new_version%valid = .true.
    end function increment_major

    !> Increment minor version
    pure function increment_minor(version) result(new_version)
        type(semver_type), intent(in) :: version
        type(semver_type) :: new_version

        new_version%major = version%major
        new_version%minor = version%minor + 1
        new_version%patch = 0
        new_version%prerelease = ''
        new_version%build = ''
        new_version%valid = .true.
    end function increment_minor

    !> Increment patch version
    pure function increment_patch(version) result(new_version)
        type(semver_type), intent(in) :: version
        type(semver_type) :: new_version

        new_version%major = version%major
        new_version%minor = version%minor
        new_version%patch = version%patch + 1
        new_version%prerelease = ''
        new_version%build = ''
        new_version%valid = .true.
    end function increment_patch

end module safe_version
