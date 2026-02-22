! SPDX-License-Identifier: PMPL-1.0-or-later
! Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
!
! Proven SafePhone - E.164 phone number handling for Fortran
!
! Thin wrapper around libproven's verified SafePhone module.
! All phone number parsing and formatting logic is executed in the
! Idris 2 core via the Zig FFI bridge; this module only marshals types
! between Fortran and C.
!
! Link with: -lproven

module safe_phone
    use, intrinsic :: iso_c_binding
    use proven_ffi
    implicit none
    private

    !> Phone parse result (Fortran-facing)
    type, public :: PhoneResult
        integer(c_int16_t) :: country_code    = 0_c_int16_t
        integer(c_int64_t) :: national_number = 0_c_int64_t
        logical            :: is_valid        = .false.
        logical            :: ok              = .false.
        integer(c_int32_t) :: status          = PROVEN_ERR_NOT_IMPLEMENTED
    end type PhoneResult

    public :: parse_phone, format_phone_e164

contains

    ! =========================================================================
    ! Parse phone number  ->  proven_phone_parse
    ! =========================================================================
    function parse_phone(input) result(res)
        character(len=*), intent(in) :: input
        type(PhoneResult) :: res
        character(len=len_trim(input), kind=c_char), target :: c_buf
        type(c_proven_phone_result) :: c_res
        integer :: trimmed_len

        trimmed_len = len_trim(input)
        c_buf = input(1:trimmed_len)

        c_res = proven_phone_parse(c_loc(c_buf), int(trimmed_len, c_size_t))

        res%status          = c_res%status
        res%ok              = (c_res%status == PROVEN_OK)
        res%country_code    = c_res%country_code
        res%national_number = c_res%national_number
        res%is_valid        = logical(c_res%is_valid)
    end function parse_phone

    ! =========================================================================
    ! Format phone number as E.164  ->  proven_phone_format_e164
    ! =========================================================================
    function format_phone_e164(country_code, national_number) result(output)
        integer(c_int16_t), intent(in) :: country_code
        integer(c_int64_t), intent(in) :: national_number
        character(len=:), allocatable :: output
        type(c_proven_string_result) :: c_res
        character(len=1, kind=c_char), pointer :: f_ptr(:)

        c_res = proven_phone_format_e164(country_code, national_number)

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
            output = ''
        end if
    end function format_phone_e164

end module safe_phone
