! SPDX-License-Identifier: PMPL-1.0-or-later
! SPDX-FileCopyrightText: 2025 Hyperpolymath
!
! Proven SafeString - XSS prevention for Fortran
!

module safe_string
    implicit none
    private

    public :: escape_html, escape_sql, sanitize_default, slugify
    public :: is_alpha, is_digit, is_alpha_num, to_lower

contains

    !> Escape HTML special characters
    function escape_html(input) result(output)
        character(len=*), intent(in) :: input
        character(len=:), allocatable :: output
        integer :: i, pos
        character(len=1) :: c
        character(len=4096) :: buffer

        buffer = ''
        pos = 1

        do i = 1, len_trim(input)
            c = input(i:i)
            select case (c)
                case ('&')
                    buffer(pos:pos+4) = '&amp;'
                    pos = pos + 5
                case ('<')
                    buffer(pos:pos+3) = '&lt;'
                    pos = pos + 4
                case ('>')
                    buffer(pos:pos+3) = '&gt;'
                    pos = pos + 4
                case ('"')
                    buffer(pos:pos+5) = '&quot;'
                    pos = pos + 6
                case ("'")
                    buffer(pos:pos+5) = '&#x27;'
                    pos = pos + 6
                case default
                    buffer(pos:pos) = c
                    pos = pos + 1
            end select
        end do

        output = trim(buffer(1:pos-1))
    end function escape_html

    !> Escape SQL single quotes
    function escape_sql(input) result(output)
        character(len=*), intent(in) :: input
        character(len=:), allocatable :: output
        integer :: i, pos
        character(len=1) :: c
        character(len=4096) :: buffer

        buffer = ''
        pos = 1

        do i = 1, len_trim(input)
            c = input(i:i)
            if (c == "'") then
                buffer(pos:pos+1) = "''"
                pos = pos + 2
            else
                buffer(pos:pos) = c
                pos = pos + 1
            end if
        end do

        output = trim(buffer(1:pos-1))
    end function escape_sql

    !> Sanitize to alphanumeric + underscore + hyphen
    function sanitize_default(input) result(output)
        character(len=*), intent(in) :: input
        character(len=:), allocatable :: output
        integer :: i, pos
        character(len=1) :: c
        character(len=4096) :: buffer

        buffer = ''
        pos = 1

        do i = 1, len_trim(input)
            c = input(i:i)
            if (is_alpha_num(c) .or. c == '_' .or. c == '-') then
                buffer(pos:pos) = c
                pos = pos + 1
            end if
        end do

        output = trim(buffer(1:pos-1))
    end function sanitize_default

    !> Convert to URL-safe slug
    function slugify(input) result(output)
        character(len=*), intent(in) :: input
        character(len=:), allocatable :: output
        integer :: i, pos
        character(len=1) :: c
        character(len=4096) :: buffer
        logical :: last_was_hyphen

        buffer = ''
        pos = 1
        last_was_hyphen = .false.

        do i = 1, len_trim(input)
            c = input(i:i)

            ! Convert to lowercase
            c = to_lower(c)

            if (is_alpha_num(c)) then
                buffer(pos:pos) = c
                pos = pos + 1
                last_was_hyphen = .false.
            else if (c == ' ' .or. c == '_' .or. c == '-') then
                ! Add hyphen if not consecutive
                if (.not. last_was_hyphen .and. pos > 1) then
                    buffer(pos:pos) = '-'
                    pos = pos + 1
                    last_was_hyphen = .true.
                end if
            end if
        end do

        ! Remove trailing hyphen
        if (pos > 1 .and. buffer(pos-1:pos-1) == '-') then
            pos = pos - 1
        end if

        output = trim(buffer(1:pos-1))
    end function slugify

    !> Check if character is alphabetic
    pure function is_alpha(c) result(res)
        character(len=1), intent(in) :: c
        logical :: res

        res = (c >= 'A' .and. c <= 'Z') .or. (c >= 'a' .and. c <= 'z')
    end function is_alpha

    !> Check if character is digit
    pure function is_digit(c) result(res)
        character(len=1), intent(in) :: c
        logical :: res

        res = c >= '0' .and. c <= '9'
    end function is_digit

    !> Check if character is alphanumeric
    pure function is_alpha_num(c) result(res)
        character(len=1), intent(in) :: c
        logical :: res

        res = is_alpha(c) .or. is_digit(c)
    end function is_alpha_num

    !> Convert character to lowercase
    pure function to_lower(c) result(lower)
        character(len=1), intent(in) :: c
        character(len=1) :: lower

        if (c >= 'A' .and. c <= 'Z') then
            lower = achar(iachar(c) + 32)
        else
            lower = c
        end if
    end function to_lower

end module safe_string
