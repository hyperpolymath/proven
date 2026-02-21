! SPDX-License-Identifier: PMPL-1.0-or-later
! SPDX-FileCopyrightText: 2025 Hyperpolymath
!
! Proven SafeUUID - UUID validation and generation for Fortran
!

module safe_uuid
    use, intrinsic :: iso_fortran_env, only: int64, int32, int8
    implicit none
    private

    !> UUID string length (36 characters: 8-4-4-4-12 with hyphens)
    integer, parameter :: UUID_STRING_LEN = 36
    !> UUID binary length (16 bytes)
    integer, parameter :: UUID_BYTES_LEN = 16

    !> Valid hex characters for validation
    character(len=16), parameter :: HEX_CHARS_LOWER = '0123456789abcdef'
    character(len=16), parameter :: HEX_CHARS_UPPER = '0123456789ABCDEF'

    !> UUID version constants
    integer, parameter, public :: UUID_VERSION_NIL     = 0
    integer, parameter, public :: UUID_VERSION_1       = 1
    integer, parameter, public :: UUID_VERSION_2       = 2
    integer, parameter, public :: UUID_VERSION_3       = 3
    integer, parameter, public :: UUID_VERSION_4       = 4
    integer, parameter, public :: UUID_VERSION_5       = 5
    integer, parameter, public :: UUID_VERSION_6       = 6
    integer, parameter, public :: UUID_VERSION_7       = 7
    integer, parameter, public :: UUID_VERSION_8       = 8
    integer, parameter, public :: UUID_VERSION_UNKNOWN = -1

    !> UUID derived type
    type, public :: uuid_type
        integer(int8) :: bytes(UUID_BYTES_LEN) = 0_int8
        logical :: valid = .false.
    contains
        procedure :: version => uuid_get_version
        procedure :: variant => uuid_get_variant
        procedure :: is_nil => uuid_is_nil
    end type uuid_type

    !> UUID parse result type
    type, public :: UuidResult
        type(uuid_type) :: uuid
        character(len=128) :: error = ''
        logical :: ok = .false.
    end type UuidResult

    public :: parse_uuid, format_uuid, generate_uuid_v4, is_valid_uuid
    public :: uuid_equal, uuid_compare, uuid_nil

contains

    !> Parse a UUID string into uuid_type
    function parse_uuid(uuid_string) result(res)
        character(len=*), intent(in) :: uuid_string
        type(UuidResult) :: res
        integer :: i, byte_idx, str_len
        character(len=1) :: char_high, char_low
        integer :: high_nibble, low_nibble
        integer :: expected_positions(32)

        ! Initialize expected hex positions (skip hyphens at 9, 14, 19, 24)
        expected_positions = [ &
            1, 2, 3, 4, 5, 6, 7, 8, &       ! First group (8 chars)
            10, 11, 12, 13, &               ! Second group (4 chars, skip 9)
            15, 16, 17, 18, &               ! Third group (4 chars, skip 14)
            20, 21, 22, 23, &               ! Fourth group (4 chars, skip 19)
            25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36 & ! Fifth group (12 chars, skip 24)
        ]

        str_len = len_trim(uuid_string)

        ! Check length
        if (str_len /= UUID_STRING_LEN) then
            res%error = 'UUID must be exactly 36 characters'
            return
        end if

        ! Check hyphen positions
        if (uuid_string(9:9) /= '-' .or. uuid_string(14:14) /= '-' .or. &
            uuid_string(19:19) /= '-' .or. uuid_string(24:24) /= '-') then
            res%error = 'Invalid hyphen positions in UUID'
            return
        end if

        ! Parse hex bytes
        byte_idx = 1
        do i = 1, 32, 2
            char_high = uuid_string(expected_positions(i):expected_positions(i))
            char_low = uuid_string(expected_positions(i+1):expected_positions(i+1))

            high_nibble = hex_char_to_int(char_high)
            low_nibble = hex_char_to_int(char_low)

            if (high_nibble < 0 .or. low_nibble < 0) then
                res%error = 'Invalid hexadecimal character in UUID'
                return
            end if

            res%uuid%bytes(byte_idx) = int(ior(ishft(high_nibble, 4), low_nibble), int8)
            byte_idx = byte_idx + 1
        end do

        res%uuid%valid = .true.
        res%ok = .true.
    end function parse_uuid

    !> Convert hex character to integer (0-15), returns -1 if invalid
    pure function hex_char_to_int(character_input) result(value)
        character(len=1), intent(in) :: character_input
        integer :: value

        select case (character_input)
            case ('0':'9')
                value = iachar(character_input) - iachar('0')
            case ('a':'f')
                value = iachar(character_input) - iachar('a') + 10
            case ('A':'F')
                value = iachar(character_input) - iachar('A') + 10
            case default
                value = -1
        end select
    end function hex_char_to_int

    !> Format uuid_type as string
    function format_uuid(uuid_instance) result(uuid_string)
        type(uuid_type), intent(in) :: uuid_instance
        character(len=UUID_STRING_LEN) :: uuid_string
        integer :: i, pos

        pos = 1
        do i = 1, UUID_BYTES_LEN
            call byte_to_hex(uuid_instance%bytes(i), uuid_string(pos:pos+1))
            pos = pos + 2

            ! Insert hyphens at positions 8, 12, 16, 20
            if (i == 4 .or. i == 6 .or. i == 8 .or. i == 10) then
                uuid_string(pos:pos) = '-'
                pos = pos + 1
            end if
        end do
    end function format_uuid

    !> Convert byte to two hex characters
    pure subroutine byte_to_hex(byte_value, hex_output)
        integer(int8), intent(in) :: byte_value
        character(len=2), intent(out) :: hex_output
        integer :: high_nibble, low_nibble

        high_nibble = iand(ishft(int(byte_value), -4), 15) + 1
        low_nibble = iand(int(byte_value), 15) + 1

        hex_output(1:1) = HEX_CHARS_LOWER(high_nibble:high_nibble)
        hex_output(2:2) = HEX_CHARS_LOWER(low_nibble:low_nibble)
    end subroutine byte_to_hex

    !> Generate a random UUID v4
    function generate_uuid_v4() result(uuid_instance)
        type(uuid_type) :: uuid_instance
        integer :: i
        real :: random_value

        ! Seed random number generator
        call random_seed()

        ! Generate random bytes
        do i = 1, UUID_BYTES_LEN
            call random_number(random_value)
            uuid_instance%bytes(i) = int(random_value * 256.0, int8)
        end do

        ! Set version to 4 (bits 6-4 of byte 7)
        uuid_instance%bytes(7) = ior(iand(uuid_instance%bytes(7), int(Z'0F', int8)), int(Z'40', int8))

        ! Set variant to RFC 4122 (bits 7-6 of byte 9 = 10)
        uuid_instance%bytes(9) = ior(iand(uuid_instance%bytes(9), int(Z'3F', int8)), int(Z'80', int8))

        uuid_instance%valid = .true.
    end function generate_uuid_v4

    !> Check if a UUID string is valid
    function is_valid_uuid(uuid_string) result(is_valid)
        character(len=*), intent(in) :: uuid_string
        logical :: is_valid
        type(UuidResult) :: res

        res = parse_uuid(uuid_string)
        is_valid = res%ok
    end function is_valid_uuid

    !> Get UUID version
    pure function uuid_get_version(self) result(version)
        class(uuid_type), intent(in) :: self
        integer :: version
        integer :: version_nibble

        if (.not. self%valid) then
            version = UUID_VERSION_UNKNOWN
            return
        end if

        ! Version is in upper nibble of byte 7 (0-indexed: byte 6)
        version_nibble = iand(ishft(int(self%bytes(7)), -4), 15)

        select case (version_nibble)
            case (0)
                ! Check if nil UUID
                if (uuid_is_nil_internal(self)) then
                    version = UUID_VERSION_NIL
                else
                    version = UUID_VERSION_UNKNOWN
                end if
            case (1:8)
                version = version_nibble
            case default
                version = UUID_VERSION_UNKNOWN
        end select
    end function uuid_get_version

    !> Get UUID variant (0=NCS, 1=RFC4122, 2=Microsoft, 3=Future)
    pure function uuid_get_variant(self) result(variant)
        class(uuid_type), intent(in) :: self
        integer :: variant
        integer :: variant_byte

        if (.not. self%valid) then
            variant = -1
            return
        end if

        variant_byte = int(self%bytes(9))

        ! Check variant bits
        if (iand(variant_byte, int(Z'80', kind=kind(variant_byte))) == 0) then
            variant = 0  ! NCS backward compatibility
        else if (iand(variant_byte, int(Z'C0', kind=kind(variant_byte))) == int(Z'80', kind=kind(variant_byte))) then
            variant = 1  ! RFC 4122
        else if (iand(variant_byte, int(Z'E0', kind=kind(variant_byte))) == int(Z'C0', kind=kind(variant_byte))) then
            variant = 2  ! Microsoft
        else
            variant = 3  ! Future reserved
        end if
    end function uuid_get_variant

    !> Check if UUID is nil (all zeros)
    pure function uuid_is_nil(self) result(is_nil)
        class(uuid_type), intent(in) :: self
        logical :: is_nil

        is_nil = uuid_is_nil_internal(self)
    end function uuid_is_nil

    !> Internal nil check
    pure function uuid_is_nil_internal(uuid_instance) result(is_nil)
        type(uuid_type), intent(in) :: uuid_instance
        logical :: is_nil
        integer :: i

        is_nil = .true.
        do i = 1, UUID_BYTES_LEN
            if (uuid_instance%bytes(i) /= 0_int8) then
                is_nil = .false.
                return
            end if
        end do
    end function uuid_is_nil_internal

    !> Compare two UUIDs for equality
    pure function uuid_equal(uuid_a, uuid_b) result(is_equal)
        type(uuid_type), intent(in) :: uuid_a, uuid_b
        logical :: is_equal
        integer :: i

        is_equal = .true.
        do i = 1, UUID_BYTES_LEN
            if (uuid_a%bytes(i) /= uuid_b%bytes(i)) then
                is_equal = .false.
                return
            end if
        end do
    end function uuid_equal

    !> Compare two UUIDs lexicographically
    !> Returns: -1 if a < b, 0 if a == b, 1 if a > b
    pure function uuid_compare(uuid_a, uuid_b) result(comparison_result)
        type(uuid_type), intent(in) :: uuid_a, uuid_b
        integer :: comparison_result
        integer :: i
        integer :: byte_a, byte_b

        comparison_result = 0
        do i = 1, UUID_BYTES_LEN
            ! Convert to unsigned for comparison
            byte_a = iand(int(uuid_a%bytes(i)), 255)
            byte_b = iand(int(uuid_b%bytes(i)), 255)

            if (byte_a < byte_b) then
                comparison_result = -1
                return
            else if (byte_a > byte_b) then
                comparison_result = 1
                return
            end if
        end do
    end function uuid_compare

    !> Return a nil UUID
    pure function uuid_nil() result(nil_uuid)
        type(uuid_type) :: nil_uuid

        nil_uuid%bytes = 0_int8
        nil_uuid%valid = .true.
    end function uuid_nil

end module safe_uuid
