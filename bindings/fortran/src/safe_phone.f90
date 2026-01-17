! SPDX-License-Identifier: PMPL-1.0
! SPDX-FileCopyrightText: 2025 Hyperpolymath
!
! Proven SafePhone - Phone number validation and formatting for Fortran
! Based on E.164 standard (max 15 digits including country code)
!

module safe_phone
    use, intrinsic :: iso_fortran_env, only: int64, int32
    implicit none
    private

    !> Maximum phone number length (E.164)
    integer, parameter :: MAX_PHONE_LEN = 15
    !> Maximum formatted phone length (with spaces, dashes, etc.)
    integer, parameter :: MAX_FORMATTED_LEN = 32

    !> Country code enumeration (ITU-T E.164)
    integer, parameter, public :: COUNTRY_UNKNOWN = 0
    integer, parameter, public :: COUNTRY_US      = 1   ! +1
    integer, parameter, public :: COUNTRY_CA      = 2   ! +1 (same as US)
    integer, parameter, public :: COUNTRY_UK      = 44  ! +44
    integer, parameter, public :: COUNTRY_DE      = 49  ! +49
    integer, parameter, public :: COUNTRY_FR      = 33  ! +33
    integer, parameter, public :: COUNTRY_IT      = 39  ! +39
    integer, parameter, public :: COUNTRY_ES      = 34  ! +34
    integer, parameter, public :: COUNTRY_JP      = 81  ! +81
    integer, parameter, public :: COUNTRY_CN      = 86  ! +86
    integer, parameter, public :: COUNTRY_IN      = 91  ! +91
    integer, parameter, public :: COUNTRY_AU      = 61  ! +61
    integer, parameter, public :: COUNTRY_BR      = 55  ! +55
    integer, parameter, public :: COUNTRY_RU      = 7   ! +7
    integer, parameter, public :: COUNTRY_MX      = 52  ! +52
    integer, parameter, public :: COUNTRY_KR      = 82  ! +82
    integer, parameter, public :: COUNTRY_NL      = 31  ! +31
    integer, parameter, public :: COUNTRY_SE      = 46  ! +46
    integer, parameter, public :: COUNTRY_CH      = 41  ! +41
    integer, parameter, public :: COUNTRY_PL      = 48  ! +48
    integer, parameter, public :: COUNTRY_BE      = 32  ! +32

    !> Country info type
    type, public :: country_info_type
        integer :: country_enum = COUNTRY_UNKNOWN
        integer :: calling_code = 0
        character(len=2) :: iso_alpha2 = '  '
        character(len=32) :: name = ''
        integer :: national_number_length = 10  ! Typical length
    end type country_info_type

    !> Phone number type
    type, public :: phone_number_type
        integer :: country_code = 0              ! E.164 country code
        character(len=MAX_PHONE_LEN) :: national_number = ''
        character(len=MAX_PHONE_LEN) :: full_number = ''  ! Without +
        logical :: valid = .false.
    contains
        procedure :: format_e164 => phone_format_e164
        procedure :: format_national => phone_format_national
        procedure :: format_international => phone_format_international
    end type phone_number_type

    !> Phone parse result type
    type, public :: PhoneResult
        type(phone_number_type) :: phone
        character(len=128) :: error = ''
        logical :: ok = .false.
    end type PhoneResult

    !> Country lookup table
    type(country_info_type), dimension(20), parameter :: COUNTRIES = [ &
        country_info_type(COUNTRY_US, 1,  'US', 'United States', 10), &
        country_info_type(COUNTRY_CA, 1,  'CA', 'Canada', 10), &
        country_info_type(COUNTRY_UK, 44, 'GB', 'United Kingdom', 10), &
        country_info_type(COUNTRY_DE, 49, 'DE', 'Germany', 10), &
        country_info_type(COUNTRY_FR, 33, 'FR', 'France', 9), &
        country_info_type(COUNTRY_IT, 39, 'IT', 'Italy', 10), &
        country_info_type(COUNTRY_ES, 34, 'ES', 'Spain', 9), &
        country_info_type(COUNTRY_JP, 81, 'JP', 'Japan', 10), &
        country_info_type(COUNTRY_CN, 86, 'CN', 'China', 11), &
        country_info_type(COUNTRY_IN, 91, 'IN', 'India', 10), &
        country_info_type(COUNTRY_AU, 61, 'AU', 'Australia', 9), &
        country_info_type(COUNTRY_BR, 55, 'BR', 'Brazil', 11), &
        country_info_type(COUNTRY_RU, 7,  'RU', 'Russia', 10), &
        country_info_type(COUNTRY_MX, 52, 'MX', 'Mexico', 10), &
        country_info_type(COUNTRY_KR, 82, 'KR', 'South Korea', 10), &
        country_info_type(COUNTRY_NL, 31, 'NL', 'Netherlands', 9), &
        country_info_type(COUNTRY_SE, 46, 'SE', 'Sweden', 9), &
        country_info_type(COUNTRY_CH, 41, 'CH', 'Switzerland', 9), &
        country_info_type(COUNTRY_PL, 48, 'PL', 'Poland', 9), &
        country_info_type(COUNTRY_BE, 32, 'BE', 'Belgium', 9) &
    ]

    public :: parse_phone, format_phone_e164, format_phone_national
    public :: format_phone_international, is_valid_phone
    public :: get_country_info, country_from_calling_code, country_from_iso

contains

    !> Parse a phone number string
    function parse_phone(phone_string, default_country) result(res)
        character(len=*), intent(in) :: phone_string
        integer, intent(in), optional :: default_country
        type(PhoneResult) :: res
        character(len=MAX_PHONE_LEN) :: digits_only
        integer :: i, digit_count, str_len
        character(len=1) :: char_current
        logical :: has_plus
        integer :: country_code, cc_digits
        type(country_info_type) :: country_info

        str_len = len_trim(phone_string)
        digit_count = 0
        digits_only = ''
        has_plus = .false.

        if (str_len == 0) then
            res%error = 'Empty phone number'
            return
        end if

        ! Check for leading plus
        if (phone_string(1:1) == '+') then
            has_plus = .true.
        end if

        ! Extract digits only
        do i = 1, str_len
            char_current = phone_string(i:i)
            if (char_current >= '0' .and. char_current <= '9') then
                digit_count = digit_count + 1
                if (digit_count > MAX_PHONE_LEN) then
                    res%error = 'Phone number too long'
                    return
                end if
                digits_only(digit_count:digit_count) = char_current
            else if (char_current /= '+' .and. char_current /= ' ' .and. &
                     char_current /= '-' .and. char_current /= '(' .and. &
                     char_current /= ')' .and. char_current /= '.') then
                res%error = 'Invalid character in phone number'
                return
            end if
        end do

        if (digit_count < 7) then
            res%error = 'Phone number too short'
            return
        end if

        ! Parse country code
        if (has_plus) then
            ! Try to extract country code (1-3 digits)
            country_code = extract_country_code(digits_only(1:digit_count), cc_digits)
            if (country_code == 0) then
                res%error = 'Unknown country code'
                return
            end if

            res%phone%country_code = country_code
            res%phone%national_number = digits_only(cc_digits+1:digit_count)
            res%phone%full_number = digits_only(1:digit_count)
        else
            ! Use default country if provided
            if (present(default_country)) then
                country_info = get_country_info(default_country)
                if (country_info%country_enum /= COUNTRY_UNKNOWN) then
                    res%phone%country_code = country_info%calling_code
                    res%phone%national_number = digits_only(1:digit_count)
                    ! Build full number
                    write(res%phone%full_number, '(I0,A)') &
                        country_info%calling_code, trim(digits_only(1:digit_count))
                else
                    res%error = 'Invalid default country'
                    return
                end if
            else
                ! Assume US/CA format if starts with 1 and is 11 digits
                if (digit_count == 11 .and. digits_only(1:1) == '1') then
                    res%phone%country_code = 1
                    res%phone%national_number = digits_only(2:digit_count)
                    res%phone%full_number = digits_only(1:digit_count)
                else if (digit_count == 10) then
                    ! Assume US/CA without country code
                    res%phone%country_code = 1
                    res%phone%national_number = digits_only(1:digit_count)
                    res%phone%full_number = '1' // digits_only(1:digit_count)
                else
                    res%error = 'Cannot determine country code'
                    return
                end if
            end if
        end if

        res%phone%valid = .true.
        res%ok = .true.
    end function parse_phone

    !> Extract country code from digit string
    function extract_country_code(digits, code_length) result(country_code)
        character(len=*), intent(in) :: digits
        integer, intent(out) :: code_length
        integer :: country_code
        integer :: test_code, i
        character(len=3) :: code_str

        country_code = 0
        code_length = 0

        ! Try 1-digit codes first
        if (len_trim(digits) >= 1) then
            read(digits(1:1), '(I1)') test_code
            if (test_code == 1 .or. test_code == 7) then
                country_code = test_code
                code_length = 1
                return
            end if
        end if

        ! Try 2-digit codes
        if (len_trim(digits) >= 2) then
            read(digits(1:2), '(I2)') test_code
            do i = 1, size(COUNTRIES)
                if (COUNTRIES(i)%calling_code == test_code) then
                    country_code = test_code
                    code_length = 2
                    return
                end if
            end do
        end if

        ! Try 3-digit codes
        if (len_trim(digits) >= 3) then
            read(digits(1:3), '(I3)') test_code
            do i = 1, size(COUNTRIES)
                if (COUNTRIES(i)%calling_code == test_code) then
                    country_code = test_code
                    code_length = 3
                    return
                end if
            end do
        end if
    end function extract_country_code

    !> Format as E.164 (+15551234567)
    function phone_format_e164(self) result(formatted_number)
        class(phone_number_type), intent(in) :: self
        character(len=MAX_FORMATTED_LEN) :: formatted_number

        if (.not. self%valid) then
            formatted_number = ''
            return
        end if

        write(formatted_number, '(A,A)') '+', trim(self%full_number)
    end function phone_format_e164

    !> Format as national format (555-123-4567 for US)
    function phone_format_national(self) result(formatted_number)
        class(phone_number_type), intent(in) :: self
        character(len=MAX_FORMATTED_LEN) :: formatted_number
        integer :: national_len

        if (.not. self%valid) then
            formatted_number = ''
            return
        end if

        national_len = len_trim(self%national_number)

        ! US/CA format
        if (self%country_code == 1 .and. national_len == 10) then
            write(formatted_number, '(A,A,A,A,A,A,A)') &
                '(', self%national_number(1:3), ') ', &
                self%national_number(4:6), '-', &
                self%national_number(7:10)
        else
            ! Generic format: groups of 3
            formatted_number = format_generic_national(self%national_number)
        end if
    end function phone_format_national

    !> Format as international format (+1 555 123 4567)
    function phone_format_international(self) result(formatted_number)
        class(phone_number_type), intent(in) :: self
        character(len=MAX_FORMATTED_LEN) :: formatted_number
        integer :: national_len
        character(len=4) :: cc_str

        if (.not. self%valid) then
            formatted_number = ''
            return
        end if

        write(cc_str, '(I0)') self%country_code
        national_len = len_trim(self%national_number)

        ! US/CA format
        if (self%country_code == 1 .and. national_len == 10) then
            write(formatted_number, '(A,A,A,A,A,A,A,A)') &
                '+', trim(cc_str), ' ', &
                self%national_number(1:3), ' ', &
                self%national_number(4:6), ' ', &
                self%national_number(7:10)
        else
            ! Generic format
            formatted_number = '+' // trim(cc_str) // ' ' // &
                              trim(format_generic_national(self%national_number))
        end if
    end function phone_format_international

    !> Generic national formatting (groups of 3)
    function format_generic_national(national) result(formatted_number)
        character(len=*), intent(in) :: national
        character(len=MAX_FORMATTED_LEN) :: formatted_number
        integer :: i, pos, national_len
        character(len=1) :: char_current

        formatted_number = ''
        pos = 1
        national_len = len_trim(national)

        do i = 1, national_len
            char_current = national(i:i)
            formatted_number(pos:pos) = char_current
            pos = pos + 1

            ! Add space every 3 digits (except at end)
            if (mod(i, 3) == 0 .and. i < national_len) then
                formatted_number(pos:pos) = ' '
                pos = pos + 1
            end if
        end do
    end function format_generic_national

    !> Standalone E.164 format function
    function format_phone_e164(phone_string, default_country) result(formatted_number)
        character(len=*), intent(in) :: phone_string
        integer, intent(in), optional :: default_country
        character(len=MAX_FORMATTED_LEN) :: formatted_number
        type(PhoneResult) :: res

        if (present(default_country)) then
            res = parse_phone(phone_string, default_country)
        else
            res = parse_phone(phone_string)
        end if

        if (res%ok) then
            formatted_number = res%phone%format_e164()
        else
            formatted_number = ''
        end if
    end function format_phone_e164

    !> Standalone national format function
    function format_phone_national(phone_string, default_country) result(formatted_number)
        character(len=*), intent(in) :: phone_string
        integer, intent(in), optional :: default_country
        character(len=MAX_FORMATTED_LEN) :: formatted_number
        type(PhoneResult) :: res

        if (present(default_country)) then
            res = parse_phone(phone_string, default_country)
        else
            res = parse_phone(phone_string)
        end if

        if (res%ok) then
            formatted_number = res%phone%format_national()
        else
            formatted_number = ''
        end if
    end function format_phone_national

    !> Standalone international format function
    function format_phone_international(phone_string, default_country) result(formatted_number)
        character(len=*), intent(in) :: phone_string
        integer, intent(in), optional :: default_country
        character(len=MAX_FORMATTED_LEN) :: formatted_number
        type(PhoneResult) :: res

        if (present(default_country)) then
            res = parse_phone(phone_string, default_country)
        else
            res = parse_phone(phone_string)
        end if

        if (res%ok) then
            formatted_number = res%phone%format_international()
        else
            formatted_number = ''
        end if
    end function format_phone_international

    !> Check if a phone number string is valid
    function is_valid_phone(phone_string, default_country) result(is_valid)
        character(len=*), intent(in) :: phone_string
        integer, intent(in), optional :: default_country
        logical :: is_valid
        type(PhoneResult) :: res

        if (present(default_country)) then
            res = parse_phone(phone_string, default_country)
        else
            res = parse_phone(phone_string)
        end if

        is_valid = res%ok
    end function is_valid_phone

    !> Get country information by country enum
    function get_country_info(country_enum) result(country_info)
        integer, intent(in) :: country_enum
        type(country_info_type) :: country_info
        integer :: i

        country_info%country_enum = COUNTRY_UNKNOWN

        do i = 1, size(COUNTRIES)
            if (COUNTRIES(i)%country_enum == country_enum) then
                country_info = COUNTRIES(i)
                return
            end if
        end do
    end function get_country_info

    !> Get country enum from calling code
    function country_from_calling_code(calling_code) result(country_enum)
        integer, intent(in) :: calling_code
        integer :: country_enum
        integer :: i

        country_enum = COUNTRY_UNKNOWN

        do i = 1, size(COUNTRIES)
            if (COUNTRIES(i)%calling_code == calling_code) then
                country_enum = COUNTRIES(i)%country_enum
                return
            end if
        end do
    end function country_from_calling_code

    !> Get country enum from ISO alpha-2 code
    function country_from_iso(iso_code) result(country_enum)
        character(len=*), intent(in) :: iso_code
        integer :: country_enum
        character(len=2) :: upper_code
        integer :: i

        country_enum = COUNTRY_UNKNOWN

        if (len_trim(iso_code) /= 2) return

        ! Convert to uppercase
        upper_code = adjustl(iso_code)
        do i = 1, 2
            if (upper_code(i:i) >= 'a' .and. upper_code(i:i) <= 'z') then
                upper_code(i:i) = achar(iachar(upper_code(i:i)) - 32)
            end if
        end do

        do i = 1, size(COUNTRIES)
            if (COUNTRIES(i)%iso_alpha2 == upper_code) then
                country_enum = COUNTRIES(i)%country_enum
                return
            end if
        end do
    end function country_from_iso

end module safe_phone
