! SPDX-License-Identifier: PMPL-1.0-or-later
! Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
!
! Proven SafeCurrency - Monetary values with ISO 4217 codes for Fortran
!
! Thin wrapper around libproven's verified SafeCurrency module.
! All currency parsing and formatting logic is executed in the Idris 2
! core via the Zig FFI bridge; this module only marshals types between
! Fortran and C.
!
! Link with: -lproven

module safe_currency
    use, intrinsic :: iso_c_binding
    use proven_ffi
    implicit none
    private

    !> Currency parse result (Fortran-facing)
    type, public :: CurrencyResult
        integer(c_int64_t) :: amount_minor   = 0_c_int64_t
        character(len=3)   :: currency_code  = '   '
        integer            :: decimal_places = 0
        logical            :: ok             = .false.
        integer(c_int32_t) :: status         = PROVEN_ERR_NOT_IMPLEMENTED
    end type CurrencyResult

    public :: parse_currency, format_currency

contains

    ! =========================================================================
    ! Parse currency string  ->  proven_currency_parse
    ! =========================================================================
    function parse_currency(input) result(res)
        character(len=*), intent(in) :: input
        type(CurrencyResult) :: res
        character(len=len_trim(input), kind=c_char), target :: c_buf
        type(c_proven_currency_result) :: c_res
        integer :: trimmed_len, i

        trimmed_len = len_trim(input)
        c_buf = input(1:trimmed_len)

        c_res = proven_currency_parse(c_loc(c_buf), int(trimmed_len, c_size_t))

        res%status = c_res%status
        res%ok     = (c_res%status == PROVEN_OK)

        if (res%ok) then
            res%amount_minor   = c_res%amount_minor
            res%decimal_places = int(c_res%decimal_places)
            ! Convert int8 code bytes to character(3)
            do i = 1, 3
                res%currency_code(i:i) = achar(iand(int(c_res%currency_code(i)), 255))
            end do
        end if
    end function parse_currency

    ! =========================================================================
    ! Format currency amount  ->  proven_currency_format
    ! =========================================================================
    function format_currency(amount_minor, code, decimal_places) result(output)
        integer(c_int64_t), intent(in) :: amount_minor
        character(len=3), intent(in)   :: code
        integer, intent(in)            :: decimal_places
        character(len=:), allocatable  :: output
        integer(c_int8_t) :: c_code(3)
        type(c_proven_string_result) :: c_res
        character(len=1, kind=c_char), pointer :: f_ptr(:)
        integer :: i

        ! Convert character code to int8 array
        do i = 1, 3
            c_code(i) = int(iachar(code(i:i)), c_int8_t)
        end do

        c_res = proven_currency_format( &
            amount_minor, c_code, int(decimal_places, c_int8_t))

        if (c_res%status == PROVEN_OK .and. c_associated(c_res%value)) then
            call c_f_pointer(c_res%value, f_ptr, [c_res%length])
            allocate(character(len=c_res%length) :: output)
            do i = 1, int(c_res%length)
                output(i:i) = f_ptr(i)
            end do
            call proven_free_string(c_res%value)
        else
            output = ''
        end if
    end function format_currency

end module safe_currency
