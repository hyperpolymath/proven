! SPDX-License-Identifier: PMPL-1.0-or-later
! Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
!
! Proven SafeHex - Hexadecimal encoding/decoding for Fortran
!
! Thin wrapper around libproven's verified SafeHex module.
! All hex encoding/decoding logic is executed in the Idris 2 core via
! the Zig FFI bridge; this module only marshals types between Fortran
! and C.
!
! Link with: -lproven

module safe_hex
    use, intrinsic :: iso_c_binding
    use proven_ffi
    implicit none
    private

    !> Result type for hex decode operations
    type, public :: HexDecodeResult
        integer(c_int8_t), dimension(:), allocatable :: bytes
        integer            :: length = 0
        logical            :: ok     = .false.
        integer(c_int32_t) :: status = PROVEN_ERR_NOT_IMPLEMENTED
    end type HexDecodeResult

    public :: hex_encode, hex_encode_upper, hex_decode

contains

    ! =========================================================================
    ! Encode bytes to lowercase hex  ->  proven_hex_encode(... false)
    ! =========================================================================
    function hex_encode(bytes) result(hex_string)
        integer(c_int8_t), target, intent(in) :: bytes(:)
        character(len=:), allocatable :: hex_string
        type(c_proven_string_result) :: c_res
        character(len=1, kind=c_char), pointer :: f_ptr(:)
        integer :: byte_count

        byte_count = size(bytes)

        c_res = proven_hex_encode( &
            c_loc(bytes), int(byte_count, c_size_t), logical(.false., c_bool))

        if (c_res%status == PROVEN_OK .and. c_associated(c_res%value)) then
            call c_f_pointer(c_res%value, f_ptr, [c_res%length])
            allocate(character(len=c_res%length) :: hex_string)
            block
                integer :: i
                do i = 1, int(c_res%length)
                    hex_string(i:i) = f_ptr(i)
                end do
            end block
            call proven_free_string(c_res%value)
        else
            hex_string = ''
        end if
    end function hex_encode

    ! =========================================================================
    ! Encode bytes to uppercase hex  ->  proven_hex_encode(... true)
    ! =========================================================================
    function hex_encode_upper(bytes) result(hex_string)
        integer(c_int8_t), target, intent(in) :: bytes(:)
        character(len=:), allocatable :: hex_string
        type(c_proven_string_result) :: c_res
        character(len=1, kind=c_char), pointer :: f_ptr(:)
        integer :: byte_count

        byte_count = size(bytes)

        c_res = proven_hex_encode( &
            c_loc(bytes), int(byte_count, c_size_t), logical(.true., c_bool))

        if (c_res%status == PROVEN_OK .and. c_associated(c_res%value)) then
            call c_f_pointer(c_res%value, f_ptr, [c_res%length])
            allocate(character(len=c_res%length) :: hex_string)
            block
                integer :: i
                do i = 1, int(c_res%length)
                    hex_string(i:i) = f_ptr(i)
                end do
            end block
            call proven_free_string(c_res%value)
        else
            hex_string = ''
        end if
    end function hex_encode_upper

    ! =========================================================================
    ! Decode hex string to bytes  ->  proven_hex_decode
    ! =========================================================================
    function hex_decode(hex_string) result(res)
        character(len=*), intent(in) :: hex_string
        type(HexDecodeResult) :: res
        character(len=len_trim(hex_string), kind=c_char), target :: c_buf
        type(c_proven_hex_decode_result) :: c_res
        integer(c_int8_t), pointer :: byte_ptr(:)
        integer :: trimmed_len

        trimmed_len = len_trim(hex_string)
        c_buf = hex_string(1:trimmed_len)

        c_res = proven_hex_decode(c_loc(c_buf), int(trimmed_len, c_size_t))

        res%status = c_res%status
        res%ok     = (c_res%status == PROVEN_OK)

        if (res%ok .and. c_associated(c_res%data)) then
            res%length = int(c_res%length)
            allocate(res%bytes(res%length))

            call c_f_pointer(c_res%data, byte_ptr, [c_res%length])
            res%bytes = byte_ptr(1:res%length)

            ! Free C-allocated memory via proven_hex_free.
            ! The API expects a pointer to the result struct, but we pass
            ! the data pointer for freeing via the dedicated free function.
            call proven_hex_free(c_res%data)
        end if
    end function hex_decode

end module safe_hex
