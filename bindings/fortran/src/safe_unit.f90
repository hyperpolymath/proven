! SPDX-License-Identifier: PMPL-1.0
! SPDX-FileCopyrightText: 2025 Hyperpolymath
!
! Proven SafeUnit - Unit conversion and validation for Fortran
!

module safe_unit
    use, intrinsic :: iso_fortran_env, only: real64, int64
    implicit none
    private

    !> Unit category enumeration
    integer, parameter, public :: UNIT_CAT_LENGTH = 1
    integer, parameter, public :: UNIT_CAT_MASS = 2
    integer, parameter, public :: UNIT_CAT_TIME = 3
    integer, parameter, public :: UNIT_CAT_TEMPERATURE = 4
    integer, parameter, public :: UNIT_CAT_AREA = 5
    integer, parameter, public :: UNIT_CAT_VOLUME = 6
    integer, parameter, public :: UNIT_CAT_SPEED = 7
    integer, parameter, public :: UNIT_CAT_PRESSURE = 8
    integer, parameter, public :: UNIT_CAT_ENERGY = 9
    integer, parameter, public :: UNIT_CAT_DATA = 10

    !> Length units
    integer, parameter, public :: UNIT_METER = 101
    integer, parameter, public :: UNIT_KILOMETER = 102
    integer, parameter, public :: UNIT_CENTIMETER = 103
    integer, parameter, public :: UNIT_MILLIMETER = 104
    integer, parameter, public :: UNIT_MICROMETER = 105
    integer, parameter, public :: UNIT_NANOMETER = 106
    integer, parameter, public :: UNIT_MILE = 107
    integer, parameter, public :: UNIT_YARD = 108
    integer, parameter, public :: UNIT_FOOT = 109
    integer, parameter, public :: UNIT_INCH = 110
    integer, parameter, public :: UNIT_NAUTICAL_MILE = 111

    !> Mass units
    integer, parameter, public :: UNIT_KILOGRAM = 201
    integer, parameter, public :: UNIT_GRAM = 202
    integer, parameter, public :: UNIT_MILLIGRAM = 203
    integer, parameter, public :: UNIT_METRIC_TON = 204
    integer, parameter, public :: UNIT_POUND = 205
    integer, parameter, public :: UNIT_OUNCE = 206
    integer, parameter, public :: UNIT_STONE = 207

    !> Time units
    integer, parameter, public :: UNIT_SECOND = 301
    integer, parameter, public :: UNIT_MILLISECOND = 302
    integer, parameter, public :: UNIT_MICROSECOND = 303
    integer, parameter, public :: UNIT_NANOSECOND = 304
    integer, parameter, public :: UNIT_MINUTE = 305
    integer, parameter, public :: UNIT_HOUR = 306
    integer, parameter, public :: UNIT_DAY = 307
    integer, parameter, public :: UNIT_WEEK = 308
    integer, parameter, public :: UNIT_YEAR = 309

    !> Temperature units
    integer, parameter, public :: UNIT_CELSIUS = 401
    integer, parameter, public :: UNIT_FAHRENHEIT = 402
    integer, parameter, public :: UNIT_KELVIN = 403

    !> Data units
    integer, parameter, public :: UNIT_BIT = 1001
    integer, parameter, public :: UNIT_BYTE = 1002
    integer, parameter, public :: UNIT_KILOBYTE = 1003
    integer, parameter, public :: UNIT_MEGABYTE = 1004
    integer, parameter, public :: UNIT_GIGABYTE = 1005
    integer, parameter, public :: UNIT_TERABYTE = 1006
    integer, parameter, public :: UNIT_KIBIBYTE = 1007
    integer, parameter, public :: UNIT_MEBIBYTE = 1008
    integer, parameter, public :: UNIT_GIBIBYTE = 1009

    !> Quantity type with unit
    type, public :: quantity_type
        real(real64) :: value = 0.0_real64
        integer :: unit = 0
        logical :: valid = .false.
    contains
        procedure :: convert_to => quantity_convert_to
        procedure :: to_si => quantity_to_si
    end type quantity_type

    !> Conversion result type
    type, public :: ConversionResult
        real(real64) :: value = 0.0_real64
        character(len=128) :: error = ''
        logical :: ok = .false.
    end type ConversionResult

    public :: convert, convert_temperature
    public :: quantity_from, get_unit_category, units_compatible
    public :: get_si_unit, get_unit_name, get_unit_symbol
    public :: meters_to_feet, feet_to_meters, kg_to_pounds, pounds_to_kg
    public :: celsius_to_fahrenheit, fahrenheit_to_celsius
    public :: celsius_to_kelvin, kelvin_to_celsius
    public :: bytes_to_human_readable

contains

    !> Convert value from one unit to another
    function convert(value, from_unit, to_unit) result(res)
        real(real64), intent(in) :: value
        integer, intent(in) :: from_unit, to_unit
        type(ConversionResult) :: res
        real(real64) :: si_value
        integer :: from_cat, to_cat

        ! Check unit compatibility
        from_cat = get_unit_category(from_unit)
        to_cat = get_unit_category(to_unit)

        if (from_cat /= to_cat) then
            res%error = 'Incompatible unit categories'
            return
        end if

        ! Handle temperature separately (not linear conversion)
        if (from_cat == UNIT_CAT_TEMPERATURE) then
            res = convert_temperature(value, from_unit, to_unit)
            return
        end if

        ! Convert to SI base unit first
        si_value = to_si_value(value, from_unit)

        ! Convert from SI to target unit
        res%value = from_si_value(si_value, to_unit)
        res%ok = .true.
    end function convert

    !> Convert temperature with proper formulas
    function convert_temperature(value, from_unit, to_unit) result(res)
        real(real64), intent(in) :: value
        integer, intent(in) :: from_unit, to_unit
        type(ConversionResult) :: res
        real(real64) :: kelvin

        ! Convert to Kelvin first
        select case (from_unit)
            case (UNIT_CELSIUS)
                kelvin = value + 273.15_real64
            case (UNIT_FAHRENHEIT)
                kelvin = (value - 32.0_real64) * 5.0_real64 / 9.0_real64 + 273.15_real64
            case (UNIT_KELVIN)
                kelvin = value
            case default
                res%error = 'Unknown temperature unit'
                return
        end select

        ! Validate Kelvin (cannot be negative)
        if (kelvin < 0.0_real64) then
            res%error = 'Temperature below absolute zero'
            return
        end if

        ! Convert from Kelvin to target
        select case (to_unit)
            case (UNIT_CELSIUS)
                res%value = kelvin - 273.15_real64
            case (UNIT_FAHRENHEIT)
                res%value = (kelvin - 273.15_real64) * 9.0_real64 / 5.0_real64 + 32.0_real64
            case (UNIT_KELVIN)
                res%value = kelvin
            case default
                res%error = 'Unknown temperature unit'
                return
        end select

        res%ok = .true.
    end function convert_temperature

    !> Convert to SI base unit value
    pure function to_si_value(value, unit) result(si_value)
        real(real64), intent(in) :: value
        integer, intent(in) :: unit
        real(real64) :: si_value

        select case (unit)
            ! Length (base: meter)
            case (UNIT_METER)
                si_value = value
            case (UNIT_KILOMETER)
                si_value = value * 1000.0_real64
            case (UNIT_CENTIMETER)
                si_value = value / 100.0_real64
            case (UNIT_MILLIMETER)
                si_value = value / 1000.0_real64
            case (UNIT_MICROMETER)
                si_value = value / 1.0e6_real64
            case (UNIT_NANOMETER)
                si_value = value / 1.0e9_real64
            case (UNIT_MILE)
                si_value = value * 1609.344_real64
            case (UNIT_YARD)
                si_value = value * 0.9144_real64
            case (UNIT_FOOT)
                si_value = value * 0.3048_real64
            case (UNIT_INCH)
                si_value = value * 0.0254_real64
            case (UNIT_NAUTICAL_MILE)
                si_value = value * 1852.0_real64

            ! Mass (base: kilogram)
            case (UNIT_KILOGRAM)
                si_value = value
            case (UNIT_GRAM)
                si_value = value / 1000.0_real64
            case (UNIT_MILLIGRAM)
                si_value = value / 1.0e6_real64
            case (UNIT_METRIC_TON)
                si_value = value * 1000.0_real64
            case (UNIT_POUND)
                si_value = value * 0.45359237_real64
            case (UNIT_OUNCE)
                si_value = value * 0.028349523125_real64
            case (UNIT_STONE)
                si_value = value * 6.35029318_real64

            ! Time (base: second)
            case (UNIT_SECOND)
                si_value = value
            case (UNIT_MILLISECOND)
                si_value = value / 1000.0_real64
            case (UNIT_MICROSECOND)
                si_value = value / 1.0e6_real64
            case (UNIT_NANOSECOND)
                si_value = value / 1.0e9_real64
            case (UNIT_MINUTE)
                si_value = value * 60.0_real64
            case (UNIT_HOUR)
                si_value = value * 3600.0_real64
            case (UNIT_DAY)
                si_value = value * 86400.0_real64
            case (UNIT_WEEK)
                si_value = value * 604800.0_real64
            case (UNIT_YEAR)
                si_value = value * 31557600.0_real64  ! Julian year

            ! Data (base: byte)
            case (UNIT_BIT)
                si_value = value / 8.0_real64
            case (UNIT_BYTE)
                si_value = value
            case (UNIT_KILOBYTE)
                si_value = value * 1000.0_real64
            case (UNIT_MEGABYTE)
                si_value = value * 1.0e6_real64
            case (UNIT_GIGABYTE)
                si_value = value * 1.0e9_real64
            case (UNIT_TERABYTE)
                si_value = value * 1.0e12_real64
            case (UNIT_KIBIBYTE)
                si_value = value * 1024.0_real64
            case (UNIT_MEBIBYTE)
                si_value = value * 1048576.0_real64
            case (UNIT_GIBIBYTE)
                si_value = value * 1073741824.0_real64

            case default
                si_value = value
        end select
    end function to_si_value

    !> Convert from SI base unit value
    pure function from_si_value(si_value, unit) result(value)
        real(real64), intent(in) :: si_value
        integer, intent(in) :: unit
        real(real64) :: value

        select case (unit)
            ! Length
            case (UNIT_METER)
                value = si_value
            case (UNIT_KILOMETER)
                value = si_value / 1000.0_real64
            case (UNIT_CENTIMETER)
                value = si_value * 100.0_real64
            case (UNIT_MILLIMETER)
                value = si_value * 1000.0_real64
            case (UNIT_MICROMETER)
                value = si_value * 1.0e6_real64
            case (UNIT_NANOMETER)
                value = si_value * 1.0e9_real64
            case (UNIT_MILE)
                value = si_value / 1609.344_real64
            case (UNIT_YARD)
                value = si_value / 0.9144_real64
            case (UNIT_FOOT)
                value = si_value / 0.3048_real64
            case (UNIT_INCH)
                value = si_value / 0.0254_real64
            case (UNIT_NAUTICAL_MILE)
                value = si_value / 1852.0_real64

            ! Mass
            case (UNIT_KILOGRAM)
                value = si_value
            case (UNIT_GRAM)
                value = si_value * 1000.0_real64
            case (UNIT_MILLIGRAM)
                value = si_value * 1.0e6_real64
            case (UNIT_METRIC_TON)
                value = si_value / 1000.0_real64
            case (UNIT_POUND)
                value = si_value / 0.45359237_real64
            case (UNIT_OUNCE)
                value = si_value / 0.028349523125_real64
            case (UNIT_STONE)
                value = si_value / 6.35029318_real64

            ! Time
            case (UNIT_SECOND)
                value = si_value
            case (UNIT_MILLISECOND)
                value = si_value * 1000.0_real64
            case (UNIT_MICROSECOND)
                value = si_value * 1.0e6_real64
            case (UNIT_NANOSECOND)
                value = si_value * 1.0e9_real64
            case (UNIT_MINUTE)
                value = si_value / 60.0_real64
            case (UNIT_HOUR)
                value = si_value / 3600.0_real64
            case (UNIT_DAY)
                value = si_value / 86400.0_real64
            case (UNIT_WEEK)
                value = si_value / 604800.0_real64
            case (UNIT_YEAR)
                value = si_value / 31557600.0_real64

            ! Data
            case (UNIT_BIT)
                value = si_value * 8.0_real64
            case (UNIT_BYTE)
                value = si_value
            case (UNIT_KILOBYTE)
                value = si_value / 1000.0_real64
            case (UNIT_MEGABYTE)
                value = si_value / 1.0e6_real64
            case (UNIT_GIGABYTE)
                value = si_value / 1.0e9_real64
            case (UNIT_TERABYTE)
                value = si_value / 1.0e12_real64
            case (UNIT_KIBIBYTE)
                value = si_value / 1024.0_real64
            case (UNIT_MEBIBYTE)
                value = si_value / 1048576.0_real64
            case (UNIT_GIBIBYTE)
                value = si_value / 1073741824.0_real64

            case default
                value = si_value
        end select
    end function from_si_value

    !> Get unit category
    pure function get_unit_category(unit) result(category)
        integer, intent(in) :: unit
        integer :: category

        select case (unit / 100)
            case (1)
                category = UNIT_CAT_LENGTH
            case (2)
                category = UNIT_CAT_MASS
            case (3)
                category = UNIT_CAT_TIME
            case (4)
                category = UNIT_CAT_TEMPERATURE
            case (10)
                category = UNIT_CAT_DATA
            case default
                category = 0
        end select
    end function get_unit_category

    !> Check if two units are compatible
    pure function units_compatible(unit1, unit2) result(compatible)
        integer, intent(in) :: unit1, unit2
        logical :: compatible

        compatible = get_unit_category(unit1) == get_unit_category(unit2)
    end function units_compatible

    !> Create quantity from value and unit
    pure function quantity_from(value, unit) result(q)
        real(real64), intent(in) :: value
        integer, intent(in) :: unit
        type(quantity_type) :: q

        q%value = value
        q%unit = unit
        q%valid = .true.
    end function quantity_from

    !> Convert quantity to target unit
    function quantity_convert_to(self, to_unit) result(res)
        class(quantity_type), intent(in) :: self
        integer, intent(in) :: to_unit
        type(ConversionResult) :: res

        if (.not. self%valid) then
            res%error = 'Invalid quantity'
            return
        end if

        res = convert(self%value, self%unit, to_unit)
    end function quantity_convert_to

    !> Convert quantity to SI base unit
    function quantity_to_si(self) result(res)
        class(quantity_type), intent(in) :: self
        type(ConversionResult) :: res
        integer :: si_unit

        if (.not. self%valid) then
            res%error = 'Invalid quantity'
            return
        end if

        si_unit = get_si_unit(get_unit_category(self%unit))
        res = convert(self%value, self%unit, si_unit)
    end function quantity_to_si

    !> Get SI base unit for category
    pure function get_si_unit(category) result(unit)
        integer, intent(in) :: category
        integer :: unit

        select case (category)
            case (UNIT_CAT_LENGTH)
                unit = UNIT_METER
            case (UNIT_CAT_MASS)
                unit = UNIT_KILOGRAM
            case (UNIT_CAT_TIME)
                unit = UNIT_SECOND
            case (UNIT_CAT_TEMPERATURE)
                unit = UNIT_KELVIN
            case (UNIT_CAT_DATA)
                unit = UNIT_BYTE
            case default
                unit = 0
        end select
    end function get_si_unit

    !> Get unit name
    pure function get_unit_name(unit) result(name)
        integer, intent(in) :: unit
        character(len=20) :: name

        select case (unit)
            case (UNIT_METER); name = 'meter'
            case (UNIT_KILOMETER); name = 'kilometer'
            case (UNIT_CENTIMETER); name = 'centimeter'
            case (UNIT_MILE); name = 'mile'
            case (UNIT_FOOT); name = 'foot'
            case (UNIT_INCH); name = 'inch'
            case (UNIT_KILOGRAM); name = 'kilogram'
            case (UNIT_GRAM); name = 'gram'
            case (UNIT_POUND); name = 'pound'
            case (UNIT_SECOND); name = 'second'
            case (UNIT_MINUTE); name = 'minute'
            case (UNIT_HOUR); name = 'hour'
            case (UNIT_CELSIUS); name = 'Celsius'
            case (UNIT_FAHRENHEIT); name = 'Fahrenheit'
            case (UNIT_KELVIN); name = 'Kelvin'
            case (UNIT_BYTE); name = 'byte'
            case (UNIT_KILOBYTE); name = 'kilobyte'
            case (UNIT_MEGABYTE); name = 'megabyte'
            case (UNIT_GIGABYTE); name = 'gigabyte'
            case default; name = 'unknown'
        end select
    end function get_unit_name

    !> Get unit symbol
    pure function get_unit_symbol(unit) result(symbol)
        integer, intent(in) :: unit
        character(len=8) :: symbol

        select case (unit)
            case (UNIT_METER); symbol = 'm'
            case (UNIT_KILOMETER); symbol = 'km'
            case (UNIT_CENTIMETER); symbol = 'cm'
            case (UNIT_MILE); symbol = 'mi'
            case (UNIT_FOOT); symbol = 'ft'
            case (UNIT_INCH); symbol = 'in'
            case (UNIT_KILOGRAM); symbol = 'kg'
            case (UNIT_GRAM); symbol = 'g'
            case (UNIT_POUND); symbol = 'lb'
            case (UNIT_SECOND); symbol = 's'
            case (UNIT_MINUTE); symbol = 'min'
            case (UNIT_HOUR); symbol = 'h'
            case (UNIT_CELSIUS); symbol = '°C'
            case (UNIT_FAHRENHEIT); symbol = '°F'
            case (UNIT_KELVIN); symbol = 'K'
            case (UNIT_BYTE); symbol = 'B'
            case (UNIT_KILOBYTE); symbol = 'KB'
            case (UNIT_MEGABYTE); symbol = 'MB'
            case (UNIT_GIGABYTE); symbol = 'GB'
            case default; symbol = '?'
        end select
    end function get_unit_symbol

    ! Convenience functions
    pure function meters_to_feet(meters) result(feet)
        real(real64), intent(in) :: meters
        real(real64) :: feet
        feet = meters / 0.3048_real64
    end function meters_to_feet

    pure function feet_to_meters(feet) result(meters)
        real(real64), intent(in) :: feet
        real(real64) :: meters
        meters = feet * 0.3048_real64
    end function feet_to_meters

    pure function kg_to_pounds(kg) result(pounds)
        real(real64), intent(in) :: kg
        real(real64) :: pounds
        pounds = kg / 0.45359237_real64
    end function kg_to_pounds

    pure function pounds_to_kg(pounds) result(kg)
        real(real64), intent(in) :: pounds
        real(real64) :: kg
        kg = pounds * 0.45359237_real64
    end function pounds_to_kg

    pure function celsius_to_fahrenheit(celsius) result(fahrenheit)
        real(real64), intent(in) :: celsius
        real(real64) :: fahrenheit
        fahrenheit = celsius * 9.0_real64 / 5.0_real64 + 32.0_real64
    end function celsius_to_fahrenheit

    pure function fahrenheit_to_celsius(fahrenheit) result(celsius)
        real(real64), intent(in) :: fahrenheit
        real(real64) :: celsius
        celsius = (fahrenheit - 32.0_real64) * 5.0_real64 / 9.0_real64
    end function fahrenheit_to_celsius

    pure function celsius_to_kelvin(celsius) result(kelvin)
        real(real64), intent(in) :: celsius
        real(real64) :: kelvin
        kelvin = celsius + 273.15_real64
    end function celsius_to_kelvin

    pure function kelvin_to_celsius(kelvin) result(celsius)
        real(real64), intent(in) :: kelvin
        real(real64) :: celsius
        celsius = kelvin - 273.15_real64
    end function kelvin_to_celsius

    !> Convert bytes to human-readable string
    function bytes_to_human_readable(bytes) result(human)
        integer(int64), intent(in) :: bytes
        character(len=32) :: human
        real(real64) :: value
        character(len=4) :: suffix

        if (bytes >= 1099511627776_int64) then
            value = real(bytes, real64) / 1099511627776.0_real64
            suffix = ' TiB'
        else if (bytes >= 1073741824_int64) then
            value = real(bytes, real64) / 1073741824.0_real64
            suffix = ' GiB'
        else if (bytes >= 1048576_int64) then
            value = real(bytes, real64) / 1048576.0_real64
            suffix = ' MiB'
        else if (bytes >= 1024_int64) then
            value = real(bytes, real64) / 1024.0_real64
            suffix = ' KiB'
        else
            value = real(bytes, real64)
            suffix = ' B'
        end if

        write(human, '(F0.2,A)') value, trim(suffix)
    end function bytes_to_human_readable

end module safe_unit
