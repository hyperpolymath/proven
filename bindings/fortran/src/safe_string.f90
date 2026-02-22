! SPDX-License-Identifier: PMPL-1.0-or-later
! Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
!
! Proven SafeString - XSS prevention for Fortran
!
! Thin wrapper around libproven's verified SafeString module.
! All escaping logic is executed in the Idris 2 core via the Zig FFI
! bridge; this module only marshals Fortran strings to/from C pointers.
!
! Link with: -lproven

module safe_string
    use, intrinsic :: iso_c_binding
    use proven_ffi
    implicit none
    private

    public :: escape_html, escape_sql, escape_js, is_valid_utf8

contains

    ! =========================================================================
    ! Helper: convert a Fortran string into a C pointer + length, call an
    !         FFI function that returns c_proven_string_result, and convert
    !         the result back to an allocatable Fortran string.
    ! =========================================================================

    !> Escape HTML special characters via proven_string_escape_html.
    !> Caller receives an allocatable Fortran string; libproven memory is freed
    !> before returning.
    function escape_html(input) result(output)
        character(len=*), intent(in) :: input
        character(len=:), allocatable :: output
        character(len=len_trim(input), kind=c_char), target :: c_buf
        type(c_proven_string_result) :: c_res
        character(len=1, kind=c_char), pointer :: f_ptr(:)
        integer :: trimmed_len

        trimmed_len = len_trim(input)
        c_buf = input(1:trimmed_len)

        c_res = proven_string_escape_html(c_loc(c_buf), int(trimmed_len, c_size_t))

        if (c_res%status == PROVEN_OK .and. c_associated(c_res%value)) then
            call c_f_pointer(c_res%value, f_ptr, [c_res%length])
            allocate(character(len=c_res%length) :: output)
            block
                integer :: i
                do i = 1, int(c_res%length)
                    output(i:i) = f_ptr(i)
                end do
            end block
            call proven_free_string(c_res%value)
        else
            output = input(1:trimmed_len)
        end if
    end function escape_html

    !> Escape SQL single-quote characters via proven_string_escape_sql.
    function escape_sql(input) result(output)
        character(len=*), intent(in) :: input
        character(len=:), allocatable :: output
        character(len=len_trim(input), kind=c_char), target :: c_buf
        type(c_proven_string_result) :: c_res
        character(len=1, kind=c_char), pointer :: f_ptr(:)
        integer :: trimmed_len

        trimmed_len = len_trim(input)
        c_buf = input(1:trimmed_len)

        c_res = proven_string_escape_sql(c_loc(c_buf), int(trimmed_len, c_size_t))

        if (c_res%status == PROVEN_OK .and. c_associated(c_res%value)) then
            call c_f_pointer(c_res%value, f_ptr, [c_res%length])
            allocate(character(len=c_res%length) :: output)
            block
                integer :: i
                do i = 1, int(c_res%length)
                    output(i:i) = f_ptr(i)
                end do
            end block
            call proven_free_string(c_res%value)
        else
            output = input(1:trimmed_len)
        end if
    end function escape_sql

    !> Escape JavaScript string literal characters via proven_string_escape_js.
    function escape_js(input) result(output)
        character(len=*), intent(in) :: input
        character(len=:), allocatable :: output
        character(len=len_trim(input), kind=c_char), target :: c_buf
        type(c_proven_string_result) :: c_res
        character(len=1, kind=c_char), pointer :: f_ptr(:)
        integer :: trimmed_len

        trimmed_len = len_trim(input)
        c_buf = input(1:trimmed_len)

        c_res = proven_string_escape_js(c_loc(c_buf), int(trimmed_len, c_size_t))

        if (c_res%status == PROVEN_OK .and. c_associated(c_res%value)) then
            call c_f_pointer(c_res%value, f_ptr, [c_res%length])
            allocate(character(len=c_res%length) :: output)
            block
                integer :: i
                do i = 1, int(c_res%length)
                    output(i:i) = f_ptr(i)
                end do
            end block
            call proven_free_string(c_res%value)
        else
            output = input(1:trimmed_len)
        end if
    end function escape_js

    !> Check if a byte sequence is valid UTF-8 via proven_string_is_valid_utf8.
    function is_valid_utf8(input) result(valid)
        character(len=*), intent(in) :: input
        logical :: valid
        character(len=len_trim(input), kind=c_char), target :: c_buf
        type(c_proven_bool_result) :: c_res
        integer :: trimmed_len

        trimmed_len = len_trim(input)
        c_buf = input(1:trimmed_len)

        c_res = proven_string_is_valid_utf8(c_loc(c_buf), int(trimmed_len, c_size_t))

        if (c_res%status == PROVEN_OK) then
            valid = logical(c_res%value)
        else
            valid = .false.
        end if
    end function is_valid_utf8

end module safe_string
