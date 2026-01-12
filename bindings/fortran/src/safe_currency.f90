! SPDX-License-Identifier: AGPL-3.0-or-later
! SPDX-FileCopyrightText: 2025 Hyperpolymath
!
! Proven SafeCurrency - Currency and money operations for Fortran
! Uses integer arithmetic to avoid floating-point precision issues.
!

module safe_currency
    use, intrinsic :: iso_fortran_env, only: int64, int32
    implicit none
    private

    !> Maximum currency code length (ISO 4217 uses 3)
    integer, parameter :: CURRENCY_CODE_LEN = 3
    !> Maximum currency name length
    integer, parameter :: CURRENCY_NAME_LEN = 32

    !> ISO 4217 currency code enumeration
    integer, parameter, public :: CURRENCY_UNKNOWN = 0
    integer, parameter, public :: CURRENCY_USD = 1   ! US Dollar
    integer, parameter, public :: CURRENCY_EUR = 2   ! Euro
    integer, parameter, public :: CURRENCY_GBP = 3   ! British Pound
    integer, parameter, public :: CURRENCY_JPY = 4   ! Japanese Yen
    integer, parameter, public :: CURRENCY_CHF = 5   ! Swiss Franc
    integer, parameter, public :: CURRENCY_CAD = 6   ! Canadian Dollar
    integer, parameter, public :: CURRENCY_AUD = 7   ! Australian Dollar
    integer, parameter, public :: CURRENCY_CNY = 8   ! Chinese Yuan
    integer, parameter, public :: CURRENCY_INR = 9   ! Indian Rupee
    integer, parameter, public :: CURRENCY_KRW = 10  ! South Korean Won
    integer, parameter, public :: CURRENCY_BRL = 11  ! Brazilian Real
    integer, parameter, public :: CURRENCY_RUB = 12  ! Russian Ruble
    integer, parameter, public :: CURRENCY_MXN = 13  ! Mexican Peso
    integer, parameter, public :: CURRENCY_SGD = 14  ! Singapore Dollar
    integer, parameter, public :: CURRENCY_HKD = 15  ! Hong Kong Dollar
    integer, parameter, public :: CURRENCY_NOK = 16  ! Norwegian Krone
    integer, parameter, public :: CURRENCY_SEK = 17  ! Swedish Krona
    integer, parameter, public :: CURRENCY_DKK = 18  ! Danish Krone
    integer, parameter, public :: CURRENCY_NZD = 19  ! New Zealand Dollar
    integer, parameter, public :: CURRENCY_ZAR = 20  ! South African Rand
    integer, parameter, public :: CURRENCY_BTC = 21  ! Bitcoin (unofficial)
    integer, parameter, public :: CURRENCY_ETH = 22  ! Ethereum (unofficial)

    !> Currency information type
    type, public :: currency_info_type
        integer :: code_enum = CURRENCY_UNKNOWN
        character(len=CURRENCY_CODE_LEN) :: iso_code = '   '
        character(len=CURRENCY_NAME_LEN) :: name = ''
        integer :: minor_units = 2  ! Decimal places (cents, pence, etc.)
        character(len=4) :: symbol = '    '
    end type currency_info_type

    !> Money type - stores amount in minor units (cents, pence, etc.)
    type, public :: money_type
        integer(int64) :: amount_minor = 0_int64  ! Amount in smallest units
        integer :: currency = CURRENCY_UNKNOWN
        logical :: valid = .false.
    contains
        procedure :: to_major => money_to_major_units
        procedure :: format => money_format
    end type money_type

    !> Money result type
    type, public :: MoneyResult
        type(money_type) :: money
        character(len=128) :: error = ''
        logical :: ok = .false.
    end type MoneyResult

    !> Currency lookup table
    type(currency_info_type), dimension(22), parameter :: CURRENCIES = [ &
        currency_info_type(CURRENCY_USD, 'USD', 'US Dollar', 2, '$   '), &
        currency_info_type(CURRENCY_EUR, 'EUR', 'Euro', 2, 'EUR '), &
        currency_info_type(CURRENCY_GBP, 'GBP', 'British Pound', 2, 'GBP '), &
        currency_info_type(CURRENCY_JPY, 'JPY', 'Japanese Yen', 0, 'JPY '), &
        currency_info_type(CURRENCY_CHF, 'CHF', 'Swiss Franc', 2, 'CHF '), &
        currency_info_type(CURRENCY_CAD, 'CAD', 'Canadian Dollar', 2, 'C$  '), &
        currency_info_type(CURRENCY_AUD, 'AUD', 'Australian Dollar', 2, 'A$  '), &
        currency_info_type(CURRENCY_CNY, 'CNY', 'Chinese Yuan', 2, 'CNY '), &
        currency_info_type(CURRENCY_INR, 'INR', 'Indian Rupee', 2, 'INR '), &
        currency_info_type(CURRENCY_KRW, 'KRW', 'South Korean Won', 0, 'KRW '), &
        currency_info_type(CURRENCY_BRL, 'BRL', 'Brazilian Real', 2, 'R$  '), &
        currency_info_type(CURRENCY_RUB, 'RUB', 'Russian Ruble', 2, 'RUB '), &
        currency_info_type(CURRENCY_MXN, 'MXN', 'Mexican Peso', 2, 'MX$ '), &
        currency_info_type(CURRENCY_SGD, 'SGD', 'Singapore Dollar', 2, 'S$  '), &
        currency_info_type(CURRENCY_HKD, 'HKD', 'Hong Kong Dollar', 2, 'HK$ '), &
        currency_info_type(CURRENCY_NOK, 'NOK', 'Norwegian Krone', 2, 'kr  '), &
        currency_info_type(CURRENCY_SEK, 'SEK', 'Swedish Krona', 2, 'kr  '), &
        currency_info_type(CURRENCY_DKK, 'DKK', 'Danish Krone', 2, 'kr  '), &
        currency_info_type(CURRENCY_NZD, 'NZD', 'New Zealand Dollar', 2, 'NZ$ '), &
        currency_info_type(CURRENCY_ZAR, 'ZAR', 'South African Rand', 2, 'R   '), &
        currency_info_type(CURRENCY_BTC, 'BTC', 'Bitcoin', 8, 'BTC '), &
        currency_info_type(CURRENCY_ETH, 'ETH', 'Ethereum', 18, 'ETH ') &
    ]

    public :: money_from_major, money_from_minor, money_from_string
    public :: money_add, money_subtract, money_multiply, money_divide
    public :: money_negate, money_abs, money_compare, money_equal
    public :: currency_from_code, get_currency_info, is_valid_currency_code

contains

    !> Create money from major units (e.g., dollars)
    function money_from_major(amount, currency_code) result(money_instance)
        real, intent(in) :: amount
        integer, intent(in) :: currency_code
        type(money_type) :: money_instance
        type(currency_info_type) :: currency_info
        integer :: minor_units
        integer(int64) :: multiplier

        currency_info = get_currency_info(currency_code)
        if (currency_info%code_enum == CURRENCY_UNKNOWN) then
            return
        end if

        minor_units = currency_info%minor_units
        multiplier = 10_int64 ** minor_units

        money_instance%amount_minor = nint(real(amount, kind=8) * real(multiplier, kind=8), int64)
        money_instance%currency = currency_code
        money_instance%valid = .true.
    end function money_from_major

    !> Create money from minor units (e.g., cents)
    pure function money_from_minor(amount_minor, currency_code) result(money_instance)
        integer(int64), intent(in) :: amount_minor
        integer, intent(in) :: currency_code
        type(money_type) :: money_instance

        money_instance%amount_minor = amount_minor
        money_instance%currency = currency_code
        money_instance%valid = .true.
    end function money_from_minor

    !> Parse money from string (e.g., "123.45" or "123,45")
    function money_from_string(amount_string, currency_code) result(res)
        character(len=*), intent(in) :: amount_string
        integer, intent(in) :: currency_code
        type(MoneyResult) :: res
        type(currency_info_type) :: currency_info
        character(len=256) :: normalized_string
        integer :: i, decimal_pos, str_len
        integer(int64) :: major_part, minor_part, multiplier
        integer :: minor_units, minor_digits

        currency_info = get_currency_info(currency_code)
        if (currency_info%code_enum == CURRENCY_UNKNOWN) then
            res%error = 'Unknown currency code'
            return
        end if

        minor_units = currency_info%minor_units
        str_len = len_trim(amount_string)

        if (str_len == 0) then
            res%error = 'Empty amount string'
            return
        end if

        ! Normalize: replace comma with period
        normalized_string = adjustl(amount_string)
        do i = 1, len_trim(normalized_string)
            if (normalized_string(i:i) == ',') then
                normalized_string(i:i) = '.'
            end if
        end do

        ! Find decimal point
        decimal_pos = index(trim(normalized_string), '.')

        if (decimal_pos == 0) then
            ! No decimal point - treat as major units
            read(normalized_string, *, err=999) major_part
            minor_part = 0_int64
        else
            ! Has decimal point
            if (decimal_pos > 1) then
                read(normalized_string(1:decimal_pos-1), *, err=999) major_part
            else
                major_part = 0_int64
            end if

            minor_digits = len_trim(normalized_string) - decimal_pos
            if (minor_digits > 0 .and. minor_digits <= minor_units) then
                read(normalized_string(decimal_pos+1:), *, err=999) minor_part
                ! Scale up if fewer digits than minor_units
                minor_part = minor_part * (10_int64 ** (minor_units - minor_digits))
            else if (minor_digits > minor_units) then
                res%error = 'Too many decimal places for currency'
                return
            else
                minor_part = 0_int64
            end if
        end if

        multiplier = 10_int64 ** minor_units
        res%money%amount_minor = major_part * multiplier + minor_part
        res%money%currency = currency_code
        res%money%valid = .true.
        res%ok = .true.
        return

999     res%error = 'Invalid number format'
        return
    end function money_from_string

    !> Convert money to major units (floating-point representation)
    pure function money_to_major_units(self) result(major)
        class(money_type), intent(in) :: self
        real(kind=8) :: major
        type(currency_info_type) :: currency_info
        integer :: minor_units
        integer(int64) :: divisor

        currency_info = get_currency_info_pure(self%currency)
        minor_units = currency_info%minor_units
        divisor = 10_int64 ** minor_units

        major = real(self%amount_minor, kind=8) / real(divisor, kind=8)
    end function money_to_major_units

    !> Format money as string
    function money_format(self) result(formatted_string)
        class(money_type), intent(in) :: self
        character(len=64) :: formatted_string
        type(currency_info_type) :: currency_info
        integer :: minor_units
        integer(int64) :: divisor, major_part, minor_part
        character(len=32) :: major_str, minor_str
        character(len=8) :: format_str

        if (.not. self%valid) then
            formatted_string = 'INVALID'
            return
        end if

        currency_info = get_currency_info(self%currency)
        minor_units = currency_info%minor_units
        divisor = 10_int64 ** minor_units

        major_part = abs(self%amount_minor) / divisor
        minor_part = mod(abs(self%amount_minor), divisor)

        write(major_str, '(I0)') major_part

        if (minor_units > 0) then
            write(format_str, '(A,I1,A,I1,A)') '(I', minor_units, '.', minor_units, ')'
            write(minor_str, format_str) minor_part

            if (self%amount_minor < 0) then
                formatted_string = '-' // trim(adjustl(currency_info%symbol)) // &
                                   trim(adjustl(major_str)) // '.' // &
                                   trim(adjustl(minor_str))
            else
                formatted_string = trim(adjustl(currency_info%symbol)) // &
                                   trim(adjustl(major_str)) // '.' // &
                                   trim(adjustl(minor_str))
            end if
        else
            if (self%amount_minor < 0) then
                formatted_string = '-' // trim(adjustl(currency_info%symbol)) // &
                                   trim(adjustl(major_str))
            else
                formatted_string = trim(adjustl(currency_info%symbol)) // &
                                   trim(adjustl(major_str))
            end if
        end if
    end function money_format

    !> Add two money values (must be same currency)
    function money_add(money_a, money_b) result(res)
        type(money_type), intent(in) :: money_a, money_b
        type(MoneyResult) :: res

        if (.not. money_a%valid .or. .not. money_b%valid) then
            res%error = 'Invalid money operand'
            return
        end if

        if (money_a%currency /= money_b%currency) then
            res%error = 'Currency mismatch in addition'
            return
        end if

        res%money%amount_minor = money_a%amount_minor + money_b%amount_minor
        res%money%currency = money_a%currency
        res%money%valid = .true.
        res%ok = .true.
    end function money_add

    !> Subtract two money values (must be same currency)
    function money_subtract(money_a, money_b) result(res)
        type(money_type), intent(in) :: money_a, money_b
        type(MoneyResult) :: res

        if (.not. money_a%valid .or. .not. money_b%valid) then
            res%error = 'Invalid money operand'
            return
        end if

        if (money_a%currency /= money_b%currency) then
            res%error = 'Currency mismatch in subtraction'
            return
        end if

        res%money%amount_minor = money_a%amount_minor - money_b%amount_minor
        res%money%currency = money_a%currency
        res%money%valid = .true.
        res%ok = .true.
    end function money_subtract

    !> Multiply money by scalar
    pure function money_multiply(money_instance, multiplier) result(result_money)
        type(money_type), intent(in) :: money_instance
        integer, intent(in) :: multiplier
        type(money_type) :: result_money

        if (.not. money_instance%valid) return

        result_money%amount_minor = money_instance%amount_minor * int(multiplier, int64)
        result_money%currency = money_instance%currency
        result_money%valid = .true.
    end function money_multiply

    !> Divide money by scalar (integer division with rounding)
    function money_divide(money_instance, divisor) result(res)
        type(money_type), intent(in) :: money_instance
        integer, intent(in) :: divisor
        type(MoneyResult) :: res
        integer(int64) :: half_divisor

        if (.not. money_instance%valid) then
            res%error = 'Invalid money operand'
            return
        end if

        if (divisor == 0) then
            res%error = 'Division by zero'
            return
        end if

        ! Round half away from zero
        half_divisor = int(divisor, int64) / 2_int64
        if (money_instance%amount_minor >= 0) then
            res%money%amount_minor = (money_instance%amount_minor + half_divisor) / int(divisor, int64)
        else
            res%money%amount_minor = (money_instance%amount_minor - half_divisor) / int(divisor, int64)
        end if

        res%money%currency = money_instance%currency
        res%money%valid = .true.
        res%ok = .true.
    end function money_divide

    !> Negate money value
    pure function money_negate(money_instance) result(result_money)
        type(money_type), intent(in) :: money_instance
        type(money_type) :: result_money

        if (.not. money_instance%valid) return

        result_money%amount_minor = -money_instance%amount_minor
        result_money%currency = money_instance%currency
        result_money%valid = .true.
    end function money_negate

    !> Absolute value of money
    pure function money_abs(money_instance) result(result_money)
        type(money_type), intent(in) :: money_instance
        type(money_type) :: result_money

        if (.not. money_instance%valid) return

        result_money%amount_minor = abs(money_instance%amount_minor)
        result_money%currency = money_instance%currency
        result_money%valid = .true.
    end function money_abs

    !> Compare two money values
    !> Returns: -1 if a < b, 0 if a == b, 1 if a > b
    function money_compare(money_a, money_b) result(comparison_result)
        type(money_type), intent(in) :: money_a, money_b
        integer :: comparison_result

        comparison_result = 0

        if (.not. money_a%valid .or. .not. money_b%valid) then
            comparison_result = -2  ! Error indicator
            return
        end if

        if (money_a%currency /= money_b%currency) then
            comparison_result = -2  ! Currency mismatch error
            return
        end if

        if (money_a%amount_minor < money_b%amount_minor) then
            comparison_result = -1
        else if (money_a%amount_minor > money_b%amount_minor) then
            comparison_result = 1
        end if
    end function money_compare

    !> Check if two money values are equal
    pure function money_equal(money_a, money_b) result(is_equal)
        type(money_type), intent(in) :: money_a, money_b
        logical :: is_equal

        is_equal = money_a%valid .and. money_b%valid .and. &
                   money_a%currency == money_b%currency .and. &
                   money_a%amount_minor == money_b%amount_minor
    end function money_equal

    !> Get currency enum from ISO 4217 code string
    function currency_from_code(iso_code) result(currency_enum)
        character(len=*), intent(in) :: iso_code
        integer :: currency_enum
        character(len=3) :: upper_code
        integer :: i

        currency_enum = CURRENCY_UNKNOWN

        if (len_trim(iso_code) /= 3) return

        ! Convert to uppercase
        upper_code = adjustl(iso_code)
        do i = 1, 3
            if (upper_code(i:i) >= 'a' .and. upper_code(i:i) <= 'z') then
                upper_code(i:i) = achar(iachar(upper_code(i:i)) - 32)
            end if
        end do

        ! Lookup in table
        do i = 1, size(CURRENCIES)
            if (CURRENCIES(i)%iso_code == upper_code) then
                currency_enum = CURRENCIES(i)%code_enum
                return
            end if
        end do
    end function currency_from_code

    !> Get currency information
    function get_currency_info(currency_code) result(currency_info)
        integer, intent(in) :: currency_code
        type(currency_info_type) :: currency_info
        integer :: i

        currency_info%code_enum = CURRENCY_UNKNOWN

        do i = 1, size(CURRENCIES)
            if (CURRENCIES(i)%code_enum == currency_code) then
                currency_info = CURRENCIES(i)
                return
            end if
        end do
    end function get_currency_info

    !> Pure version of get_currency_info for use in pure functions
    pure function get_currency_info_pure(currency_code) result(currency_info)
        integer, intent(in) :: currency_code
        type(currency_info_type) :: currency_info
        integer :: i

        currency_info%code_enum = CURRENCY_UNKNOWN

        do i = 1, size(CURRENCIES)
            if (CURRENCIES(i)%code_enum == currency_code) then
                currency_info = CURRENCIES(i)
                return
            end if
        end do
    end function get_currency_info_pure

    !> Check if a currency code string is valid
    function is_valid_currency_code(iso_code) result(is_valid)
        character(len=*), intent(in) :: iso_code
        logical :: is_valid

        is_valid = currency_from_code(iso_code) /= CURRENCY_UNKNOWN
    end function is_valid_currency_code

end module safe_currency
