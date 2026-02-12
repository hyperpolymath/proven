! SPDX-License-Identifier: Apache-2.0
! SPDX-FileCopyrightText: 2025 Hyperpolymath
!
! Proven SafePath - Directory traversal prevention for Fortran
!

module safe_path
    implicit none
    private

    !> Path result type
    type, public :: PathResult
        character(len=1024) :: path = ''
        character(len=256) :: error = ''
        logical :: ok = .false.
    end type PathResult

    public :: has_traversal, sanitize_filename, safe_path_join, contains_string

contains

    !> Check if path contains traversal patterns
    function has_traversal(path) result(is_dangerous)
        character(len=*), intent(in) :: path
        logical :: is_dangerous
        character(len=:), allocatable :: lower_path

        is_dangerous = .false.
        lower_path = to_lower_string(path)

        ! Check for ".."
        if (contains_string(path, '..')) then
            is_dangerous = .true.
            return
        end if

        ! Check for "./"
        if (contains_string(path, './')) then
            is_dangerous = .true.
            return
        end if

        ! Check for URL-encoded ".." (%2e%2e)
        if (contains_string(lower_path, '%2e%2e')) then
            is_dangerous = .true.
            return
        end if

        ! Check for null byte (%00)
        if (contains_string(lower_path, '%00')) then
            is_dangerous = .true.
            return
        end if
    end function has_traversal

    !> Sanitize filename by removing dangerous characters
    function sanitize_filename(input) result(output)
        character(len=*), intent(in) :: input
        character(len=:), allocatable :: output
        integer :: i, pos
        character(len=1) :: c
        character(len=1024) :: buffer

        buffer = ''
        pos = 1

        do i = 1, len_trim(input)
            c = input(i:i)
            select case (c)
                case ('/', '\', ':', '*', '?', '"', '<', '>', '|')
                    buffer(pos:pos) = '_'
                    pos = pos + 1
                case default
                    buffer(pos:pos) = c
                    pos = pos + 1
            end select
        end do

        output = trim(buffer(1:pos-1))
    end function sanitize_filename

    !> Safely join base path with filename
    function safe_path_join(base, filename) result(res)
        character(len=*), intent(in) :: base, filename
        type(PathResult) :: res
        character(len=:), allocatable :: sanitized

        ! Check for traversal in filename
        if (has_traversal(filename)) then
            res%ok = .false.
            res%error = 'Path traversal detected in filename'
            return
        end if

        ! Sanitize the filename
        sanitized = sanitize_filename(filename)

        ! Join paths
        if (len_trim(base) > 0) then
            if (base(len_trim(base):len_trim(base)) == '/') then
                res%path = trim(base) // trim(sanitized)
            else
                res%path = trim(base) // '/' // trim(sanitized)
            end if
        else
            res%path = trim(sanitized)
        end if

        res%ok = .true.
    end function safe_path_join

    !> Check if string contains substring
    pure function contains_string(str, substr) result(found)
        character(len=*), intent(in) :: str, substr
        logical :: found

        found = index(str, substr) > 0
    end function contains_string

    !> Convert string to lowercase
    function to_lower_string(input) result(output)
        character(len=*), intent(in) :: input
        character(len=:), allocatable :: output
        integer :: i
        character(len=1) :: c

        allocate(character(len=len(input)) :: output)
        output = input

        do i = 1, len(input)
            c = input(i:i)
            if (c >= 'A' .and. c <= 'Z') then
                output(i:i) = achar(iachar(c) + 32)
            end if
        end do
    end function to_lower_string

end module safe_path
