! SPDX-License-Identifier: PMPL-1.0-or-later
! SPDX-FileCopyrightText: 2025 Hyperpolymath
!
! Proven SafeHex - Hexadecimal encoding/decoding for Fortran
! Includes constant-time comparison for security-sensitive operations.
!

module safe_hex
    use, intrinsic :: iso_fortran_env, only: int64, int32, int8
    implicit none
    private

    !> Hex character sets
    character(len=16), parameter :: HEX_LOWER = '0123456789abcdef'
    character(len=16), parameter :: HEX_UPPER = '0123456789ABCDEF'

    !> Hex result type for decode operations
    type, public :: HexDecodeResult
        integer(int8), dimension(:), allocatable :: bytes
        integer :: length = 0
        character(len=128) :: error = ''
        logical :: ok = .false.
    end type HexDecodeResult

    public :: hex_encode, hex_encode_upper, hex_decode
    public :: constant_time_equal, constant_time_compare
    public :: is_valid_hex, hex_to_int, int_to_hex

contains

    !> Encode bytes to lowercase hex string
    function hex_encode(bytes) result(hex_string)
        integer(int8), intent(in) :: bytes(:)
        character(len=:), allocatable :: hex_string
        integer :: i, byte_count, high_nibble, low_nibble

        byte_count = size(bytes)
        allocate(character(len=byte_count*2) :: hex_string)

        do i = 1, byte_count
            ! Extract nibbles (handling signed int8)
            high_nibble = iand(ishft(int(bytes(i)), -4), 15) + 1
            low_nibble = iand(int(bytes(i)), 15) + 1

            hex_string((i-1)*2+1:(i-1)*2+1) = HEX_LOWER(high_nibble:high_nibble)
            hex_string((i-1)*2+2:(i-1)*2+2) = HEX_LOWER(low_nibble:low_nibble)
        end do
    end function hex_encode

    !> Encode bytes to uppercase hex string
    function hex_encode_upper(bytes) result(hex_string)
        integer(int8), intent(in) :: bytes(:)
        character(len=:), allocatable :: hex_string
        integer :: i, byte_count, high_nibble, low_nibble

        byte_count = size(bytes)
        allocate(character(len=byte_count*2) :: hex_string)

        do i = 1, byte_count
            high_nibble = iand(ishft(int(bytes(i)), -4), 15) + 1
            low_nibble = iand(int(bytes(i)), 15) + 1

            hex_string((i-1)*2+1:(i-1)*2+1) = HEX_UPPER(high_nibble:high_nibble)
            hex_string((i-1)*2+2:(i-1)*2+2) = HEX_UPPER(low_nibble:low_nibble)
        end do
    end function hex_encode_upper

    !> Decode hex string to bytes
    function hex_decode(hex_string) result(res)
        character(len=*), intent(in) :: hex_string
        type(HexDecodeResult) :: res
        integer :: i, str_len, byte_count
        integer :: high_value, low_value
        character(len=1) :: char_high, char_low

        str_len = len_trim(hex_string)

        ! Check for even length
        if (mod(str_len, 2) /= 0) then
            res%error = 'Hex string must have even length'
            return
        end if

        byte_count = str_len / 2
        allocate(res%bytes(byte_count))

        do i = 1, byte_count
            char_high = hex_string((i-1)*2+1:(i-1)*2+1)
            char_low = hex_string((i-1)*2+2:(i-1)*2+2)

            high_value = hex_char_value(char_high)
            low_value = hex_char_value(char_low)

            if (high_value < 0 .or. low_value < 0) then
                res%error = 'Invalid hexadecimal character'
                deallocate(res%bytes)
                return
            end if

            res%bytes(i) = int(ior(ishft(high_value, 4), low_value), int8)
        end do

        res%length = byte_count
        res%ok = .true.
    end function hex_decode

    !> Convert single hex character to integer value (0-15)
    !> Returns -1 for invalid characters
    pure function hex_char_value(char_input) result(value)
        character(len=1), intent(in) :: char_input
        integer :: value

        select case (char_input)
            case ('0':'9')
                value = iachar(char_input) - iachar('0')
            case ('a':'f')
                value = iachar(char_input) - iachar('a') + 10
            case ('A':'F')
                value = iachar(char_input) - iachar('A') + 10
            case default
                value = -1
        end select
    end function hex_char_value

    !> Constant-time comparison of two hex strings
    !> Prevents timing attacks by always examining all characters
    function constant_time_equal(hex_a, hex_b) result(is_equal)
        character(len=*), intent(in) :: hex_a, hex_b
        logical :: is_equal
        integer :: i, len_a, len_b, diff
        integer :: char_a_val, char_b_val

        len_a = len(hex_a)
        len_b = len(hex_b)

        ! Length mismatch - still scan to avoid timing leak
        if (len_a /= len_b) then
            is_equal = .false.
            ! Dummy loop to maintain constant time
            diff = 1
            do i = 1, len_a
                diff = ior(diff, iachar(hex_a(i:i)))
            end do
            return
        end if

        ! Compare all characters, accumulating differences
        diff = 0
        do i = 1, len_a
            ! Normalize hex characters for comparison
            char_a_val = normalize_hex_char(hex_a(i:i))
            char_b_val = normalize_hex_char(hex_b(i:i))

            diff = ior(diff, ieor(char_a_val, char_b_val))
        end do

        is_equal = (diff == 0)
    end function constant_time_equal

    !> Normalize hex character to lowercase value for comparison
    pure function normalize_hex_char(char_input) result(value)
        character(len=1), intent(in) :: char_input
        integer :: value

        ! Convert to lowercase equivalent value
        if (char_input >= 'A' .and. char_input <= 'F') then
            value = iachar(char_input) + 32  ! Convert to lowercase
        else
            value = iachar(char_input)
        end if
    end function normalize_hex_char

    !> Constant-time comparison of two byte arrays
    function constant_time_compare(bytes_a, bytes_b) result(is_equal)
        integer(int8), intent(in) :: bytes_a(:), bytes_b(:)
        logical :: is_equal
        integer :: i, len_a, len_b, diff

        len_a = size(bytes_a)
        len_b = size(bytes_b)

        ! Length mismatch
        if (len_a /= len_b) then
            is_equal = .false.
            ! Dummy loop for constant time
            diff = 1
            do i = 1, len_a
                diff = ior(diff, int(bytes_a(i)))
            end do
            return
        end if

        ! Compare all bytes
        diff = 0
        do i = 1, len_a
            diff = ior(diff, ieor(int(bytes_a(i)), int(bytes_b(i))))
        end do

        is_equal = (diff == 0)
    end function constant_time_compare

    !> Check if a string contains only valid hex characters
    pure function is_valid_hex(hex_string) result(is_valid)
        character(len=*), intent(in) :: hex_string
        logical :: is_valid
        integer :: i, str_len
        character(len=1) :: char_current

        is_valid = .true.
        str_len = len_trim(hex_string)

        ! Must have even length
        if (mod(str_len, 2) /= 0) then
            is_valid = .false.
            return
        end if

        do i = 1, str_len
            char_current = hex_string(i:i)
            if (.not. is_hex_char(char_current)) then
                is_valid = .false.
                return
            end if
        end do
    end function is_valid_hex

    !> Check if a single character is a valid hex digit
    pure function is_hex_char(char_input) result(is_valid)
        character(len=1), intent(in) :: char_input
        logical :: is_valid

        is_valid = (char_input >= '0' .and. char_input <= '9') .or. &
                   (char_input >= 'a' .and. char_input <= 'f') .or. &
                   (char_input >= 'A' .and. char_input <= 'F')
    end function is_hex_char

    !> Convert hex string to integer
    function hex_to_int(hex_string) result(value)
        character(len=*), intent(in) :: hex_string
        integer(int64) :: value
        integer :: i, str_len, digit_value
        character(len=1) :: char_current

        value = 0_int64
        str_len = len_trim(hex_string)

        ! Skip optional 0x prefix
        i = 1
        if (str_len >= 2) then
            if (hex_string(1:2) == '0x' .or. hex_string(1:2) == '0X') then
                i = 3
            end if
        end if

        do while (i <= str_len)
            char_current = hex_string(i:i)
            digit_value = hex_char_value(char_current)

            if (digit_value < 0) then
                value = -1_int64  ! Error indicator
                return
            end if

            value = ishft(value, 4) + int(digit_value, int64)
            i = i + 1
        end do
    end function hex_to_int

    !> Convert integer to hex string
    function int_to_hex(value, min_width) result(hex_string)
        integer(int64), intent(in) :: value
        integer, intent(in), optional :: min_width
        character(len=:), allocatable :: hex_string
        integer(int64) :: remaining
        integer :: i, actual_width, required_width
        character(len=16) :: buffer
        character(len=1) :: hex_digit

        if (value == 0_int64) then
            if (present(min_width)) then
                actual_width = max(min_width, 1)
            else
                actual_width = 1
            end if
            allocate(character(len=actual_width) :: hex_string)
            hex_string = repeat('0', actual_width)
            return
        end if

        ! Build hex string from right to left
        buffer = ''
        remaining = value
        i = 16

        do while (remaining > 0_int64 .and. i >= 1)
            hex_digit = HEX_LOWER(iand(int(remaining), 15) + 1 : iand(int(remaining), 15) + 1)
            buffer(i:i) = hex_digit
            remaining = ishft(remaining, -4)
            i = i - 1
        end do

        required_width = 16 - i

        if (present(min_width)) then
            actual_width = max(min_width, required_width)
        else
            actual_width = required_width
        end if

        allocate(character(len=actual_width) :: hex_string)

        ! Pad with zeros if needed
        if (actual_width > required_width) then
            hex_string = repeat('0', actual_width - required_width) // buffer(i+1:16)
        else
            hex_string = buffer(17-actual_width:16)
        end if
    end function int_to_hex

end module safe_hex
