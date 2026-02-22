! SPDX-License-Identifier: PMPL-1.0-or-later
! Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
!
! Proven SafeEmail - Email validation for Fortran
!
! Thin wrapper around libproven's verified SafeEmail module.
! Email validation logic is executed in the Idris 2 core via the Zig FFI
! bridge; this module only marshals Fortran strings to C pointers.
!
! Link with: -lproven

module safe_email
    use, intrinsic :: iso_c_binding
    use proven_ffi
    implicit none
    private

    public :: is_valid_email

contains

    ! =========================================================================
    ! Email validation  ->  proven_email_is_valid
    ! =========================================================================
    function is_valid_email(email) result(is_valid)
        character(len=*), intent(in) :: email
        logical :: is_valid
        character(len=len_trim(email), kind=c_char), target :: c_buf
        type(c_proven_bool_result) :: c_res
        integer :: trimmed_len

        trimmed_len = len_trim(email)
        c_buf = email(1:trimmed_len)

        c_res = proven_email_is_valid(c_loc(c_buf), int(trimmed_len, c_size_t))

        if (c_res%status == PROVEN_OK) then
            is_valid = logical(c_res%value)
        else
            is_valid = .false.
        end if
    end function is_valid_email

end module safe_email
