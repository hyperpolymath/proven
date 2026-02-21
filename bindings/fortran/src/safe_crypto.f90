! SPDX-License-Identifier: PMPL-1.0-or-later
! SPDX-FileCopyrightText: 2025 Hyperpolymath
!
! Proven SafeCrypto - Cryptographic operations for Fortran
! Note: For production, use external crypto libraries
!

module safe_crypto
    use, intrinsic :: iso_fortran_env, only: int64, int32, int8
    implicit none
    private

    !> Character set for random tokens
    character(len=62), parameter :: TOKEN_CHARS = &
        'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789'

    !> Hex character set
    character(len=16), parameter :: HEX_CHARS = '0123456789abcdef'

    public :: constant_time_equals, simple_hash, bytes_to_hex
    public :: generate_token, random_int, secure_wipe

contains

    !> Constant-time comparison to prevent timing attacks
    function constant_time_equals(a, b) result(is_equal)
        character(len=*), intent(in) :: a, b
        logical :: is_equal
        integer :: i, diff, len_a, len_b

        len_a = len(a)
        len_b = len(b)

        if (len_a /= len_b) then
            is_equal = .false.
            return
        end if

        diff = 0
        do i = 1, len_a
            ! XOR characters and accumulate differences
            diff = ior(diff, ieor(iachar(a(i:i)), iachar(b(i:i))))
        end do

        is_equal = diff == 0
    end function constant_time_equals

    !> Simple hash function (NOT cryptographically secure - demo only)
    function simple_hash(input) result(hash)
        character(len=*), intent(in) :: input
        character(len=16) :: hash
        integer(int64) :: h
        integer :: i

        h = 0_int64

        ! Simple XOR-rotate hash (demo only)
        do i = 1, len_trim(input)
            h = ieor(h * 33_int64 + int(iachar(input(i:i)), int64), &
                     ishft(h, -27))
        end do

        ! Convert to hex
        hash = int64_to_hex(h)
    end function simple_hash

    !> Convert bytes to hexadecimal string
    function bytes_to_hex(bytes, n) result(hex)
        integer(int8), intent(in) :: bytes(:)
        integer, intent(in) :: n
        character(len=:), allocatable :: hex
        integer :: i, high, low

        allocate(character(len=n*2) :: hex)

        do i = 1, min(n, size(bytes))
            high = iand(ishft(bytes(i), -4), 15) + 1
            low = iand(int(bytes(i)), 15) + 1
            hex((i-1)*2+1:(i-1)*2+1) = HEX_CHARS(high:high)
            hex((i-1)*2+2:(i-1)*2+2) = HEX_CHARS(low:low)
        end do
    end function bytes_to_hex

    !> Convert int64 to hex string
    function int64_to_hex(val) result(hex)
        integer(int64), intent(in) :: val
        character(len=16) :: hex
        integer(int64) :: v
        integer :: i, idx

        v = val
        do i = 16, 1, -1
            idx = iand(int(v), 15) + 1
            hex(i:i) = HEX_CHARS(idx:idx)
            v = ishft(v, -4)
        end do
    end function int64_to_hex

    !> Generate a random token
    function generate_token(length) result(token)
        integer, intent(in) :: length
        character(len=:), allocatable :: token
        integer :: i, idx
        real :: r

        allocate(character(len=length) :: token)

        ! Seed random number generator if needed
        call random_seed()

        do i = 1, length
            call random_number(r)
            idx = int(r * 62.0) + 1
            if (idx > 62) idx = 62
            token(i:i) = TOKEN_CHARS(idx:idx)
        end do
    end function generate_token

    !> Generate random integer in range [min_val, max_val]
    function random_int(min_val, max_val) result(val)
        integer, intent(in) :: min_val, max_val
        integer :: val
        real :: r
        integer :: range_size

        call random_seed()
        call random_number(r)

        range_size = max_val - min_val + 1
        val = min_val + int(r * real(range_size))
        if (val > max_val) val = max_val
    end function random_int

    !> Securely wipe memory (best effort)
    subroutine secure_wipe(data, n)
        character(len=*), intent(inout) :: data
        integer, intent(in) :: n
        integer :: i

        ! Overwrite with zeros
        do i = 1, min(n, len(data))
            data(i:i) = char(0)
        end do

        ! Overwrite with ones
        do i = 1, min(n, len(data))
            data(i:i) = char(255)
        end do

        ! Final pass with zeros
        do i = 1, min(n, len(data))
            data(i:i) = char(0)
        end do
    end subroutine secure_wipe

end module safe_crypto
