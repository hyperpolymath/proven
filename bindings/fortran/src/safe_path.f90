! SPDX-License-Identifier: PMPL-1.0-or-later
! Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
!
! Proven SafePath - Directory traversal prevention for Fortran
!
! Thin wrapper around libproven's verified SafePath module.
! All path analysis is executed in the Idris 2 core via the Zig FFI
! bridge; this module only marshals Fortran strings to/from C pointers.
!
! Link with: -lproven

module safe_path
    use, intrinsic :: iso_c_binding
    use proven_ffi
    implicit none
    private

    !> Result type for path operations
    type, public :: PathResult
        character(len=:), allocatable :: path
        logical                      :: ok     = .false.
        integer(c_int32_t)           :: status = PROVEN_ERR_NOT_IMPLEMENTED
    end type PathResult

    public :: has_traversal, sanitize_filename

contains

    ! =========================================================================
    ! Check for directory traversal sequences  ->  proven_path_has_traversal
    ! =========================================================================
    function has_traversal(path) result(is_dangerous)
        character(len=*), intent(in) :: path
        logical :: is_dangerous
        character(len=len_trim(path), kind=c_char), target :: c_buf
        type(c_proven_bool_result) :: c_res
        integer :: trimmed_len

        trimmed_len = len_trim(path)
        c_buf = path(1:trimmed_len)

        c_res = proven_path_has_traversal(c_loc(c_buf), int(trimmed_len, c_size_t))

        if (c_res%status == PROVEN_OK) then
            is_dangerous = logical(c_res%value)
        else
            ! Conservative default: treat as dangerous on error
            is_dangerous = .true.
        end if
    end function has_traversal

    ! =========================================================================
    ! Sanitize filename  ->  proven_path_sanitize_filename
    ! =========================================================================
    function sanitize_filename(input) result(output)
        character(len=*), intent(in) :: input
        character(len=:), allocatable :: output
        character(len=len_trim(input), kind=c_char), target :: c_buf
        type(c_proven_string_result) :: c_res
        character(len=1, kind=c_char), pointer :: f_ptr(:)
        integer :: trimmed_len

        trimmed_len = len_trim(input)
        c_buf = input(1:trimmed_len)

        c_res = proven_path_sanitize_filename(c_loc(c_buf), int(trimmed_len, c_size_t))

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
    end function sanitize_filename

end module safe_path
