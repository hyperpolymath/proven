! SPDX-License-Identifier: PMPL-1.0
! SPDX-FileCopyrightText: 2025 Hyperpolymath
!
! Proven SafeJSON - JSON validation and safe parsing for Fortran
!

module safe_json
    use, intrinsic :: iso_fortran_env, only: int64, int32, real64
    implicit none
    private

    !> Maximum nesting depth for JSON
    integer, parameter :: MAX_NESTING_DEPTH = 100
    !> Maximum string length
    integer, parameter :: MAX_STRING_LEN = 65536
    !> Maximum JSON document length
    integer, parameter :: MAX_JSON_LEN = 1048576

    !> JSON value type enumeration
    integer, parameter, public :: JSON_TYPE_NULL = 0
    integer, parameter, public :: JSON_TYPE_BOOLEAN = 1
    integer, parameter, public :: JSON_TYPE_INTEGER = 2
    integer, parameter, public :: JSON_TYPE_REAL = 3
    integer, parameter, public :: JSON_TYPE_STRING = 4
    integer, parameter, public :: JSON_TYPE_ARRAY = 5
    integer, parameter, public :: JSON_TYPE_OBJECT = 6

    !> JSON value type
    type, public :: json_value_type
        integer :: value_type = JSON_TYPE_NULL
        logical :: bool_value = .false.
        integer(int64) :: int_value = 0_int64
        real(real64) :: real_value = 0.0_real64
        character(len=MAX_STRING_LEN) :: string_value = ''
        logical :: valid = .false.
    end type json_value_type

    !> JSON validation result
    type, public :: JsonResult
        logical :: is_valid = .false.
        integer :: error_position = 0
        character(len=256) :: error = ''
        integer :: nesting_depth = 0
        logical :: ok = .false.
    end type JsonResult

    public :: validate_json, is_valid_json, escape_json_string
    public :: parse_json_string, parse_json_number, parse_json_boolean
    public :: json_type_name, unescape_json_string

contains

    !> Validate JSON syntax
    function validate_json(json_string) result(res)
        character(len=*), intent(in) :: json_string
        type(JsonResult) :: res
        integer :: pos, json_len, depth
        character(len=1) :: c

        json_len = len_trim(json_string)
        pos = 1
        depth = 0

        if (json_len == 0) then
            res%error = 'Empty JSON document'
            res%error_position = 0
            return
        end if

        if (json_len > MAX_JSON_LEN) then
            res%error = 'JSON document exceeds maximum length'
            res%error_position = 0
            return
        end if

        ! Skip leading whitespace
        call skip_whitespace(json_string, pos, json_len)

        if (pos > json_len) then
            res%error = 'Empty JSON document after whitespace'
            res%error_position = pos
            return
        end if

        ! Parse root value
        call validate_value(json_string, pos, json_len, depth, res)

        if (.not. res%ok) return

        ! Skip trailing whitespace
        call skip_whitespace(json_string, pos, json_len)

        ! Check for extra content
        if (pos <= json_len) then
            res%ok = .false.
            res%error = 'Unexpected content after JSON value'
            res%error_position = pos
            return
        end if

        res%is_valid = .true.
        res%nesting_depth = depth
        res%ok = .true.
    end function validate_json

    !> Validate a JSON value at current position
    recursive subroutine validate_value(json_string, pos, json_len, depth, res)
        character(len=*), intent(in) :: json_string
        integer, intent(inout) :: pos
        integer, intent(in) :: json_len
        integer, intent(inout) :: depth
        type(JsonResult), intent(inout) :: res
        character(len=1) :: c

        call skip_whitespace(json_string, pos, json_len)

        if (pos > json_len) then
            res%error = 'Unexpected end of JSON'
            res%error_position = pos
            return
        end if

        c = json_string(pos:pos)

        select case (c)
            case ('{')
                call validate_object(json_string, pos, json_len, depth, res)
            case ('[')
                call validate_array(json_string, pos, json_len, depth, res)
            case ('"')
                call validate_string(json_string, pos, json_len, res)
            case ('t', 'f')
                call validate_boolean(json_string, pos, json_len, res)
            case ('n')
                call validate_null(json_string, pos, json_len, res)
            case ('-', '0':'9')
                call validate_number(json_string, pos, json_len, res)
            case default
                res%error = 'Unexpected character: ' // c
                res%error_position = pos
        end select
    end subroutine validate_value

    !> Validate JSON object
    recursive subroutine validate_object(json_string, pos, json_len, depth, res)
        character(len=*), intent(in) :: json_string
        integer, intent(inout) :: pos
        integer, intent(in) :: json_len
        integer, intent(inout) :: depth
        type(JsonResult), intent(inout) :: res
        logical :: first_member

        depth = depth + 1
        if (depth > MAX_NESTING_DEPTH) then
            res%error = 'Maximum nesting depth exceeded'
            res%error_position = pos
            return
        end if

        pos = pos + 1  ! Skip '{'
        call skip_whitespace(json_string, pos, json_len)

        if (pos > json_len) then
            res%error = 'Unclosed object'
            res%error_position = pos
            return
        end if

        if (json_string(pos:pos) == '}') then
            pos = pos + 1
            res%ok = .true.
            return
        end if

        first_member = .true.

        do while (pos <= json_len)
            if (.not. first_member) then
                call skip_whitespace(json_string, pos, json_len)
                if (pos > json_len .or. json_string(pos:pos) /= ',') then
                    if (pos <= json_len .and. json_string(pos:pos) == '}') exit
                    res%error = 'Expected comma or closing brace'
                    res%error_position = pos
                    return
                end if
                pos = pos + 1
                call skip_whitespace(json_string, pos, json_len)
            end if

            ! Parse key (must be string)
            if (pos > json_len .or. json_string(pos:pos) /= '"') then
                res%error = 'Expected string key'
                res%error_position = pos
                return
            end if

            call validate_string(json_string, pos, json_len, res)
            if (.not. res%ok) return

            ! Parse colon
            call skip_whitespace(json_string, pos, json_len)
            if (pos > json_len .or. json_string(pos:pos) /= ':') then
                res%error = 'Expected colon after key'
                res%error_position = pos
                return
            end if
            pos = pos + 1

            ! Parse value
            call validate_value(json_string, pos, json_len, depth, res)
            if (.not. res%ok) return

            call skip_whitespace(json_string, pos, json_len)
            first_member = .false.

            if (pos <= json_len .and. json_string(pos:pos) == '}') exit
        end do

        if (pos > json_len .or. json_string(pos:pos) /= '}') then
            res%error = 'Unclosed object'
            res%error_position = pos
            return
        end if

        pos = pos + 1
        res%ok = .true.
    end subroutine validate_object

    !> Validate JSON array
    recursive subroutine validate_array(json_string, pos, json_len, depth, res)
        character(len=*), intent(in) :: json_string
        integer, intent(inout) :: pos
        integer, intent(in) :: json_len
        integer, intent(inout) :: depth
        type(JsonResult), intent(inout) :: res
        logical :: first_element

        depth = depth + 1
        if (depth > MAX_NESTING_DEPTH) then
            res%error = 'Maximum nesting depth exceeded'
            res%error_position = pos
            return
        end if

        pos = pos + 1  ! Skip '['
        call skip_whitespace(json_string, pos, json_len)

        if (pos > json_len) then
            res%error = 'Unclosed array'
            res%error_position = pos
            return
        end if

        if (json_string(pos:pos) == ']') then
            pos = pos + 1
            res%ok = .true.
            return
        end if

        first_element = .true.

        do while (pos <= json_len)
            if (.not. first_element) then
                call skip_whitespace(json_string, pos, json_len)
                if (pos > json_len .or. json_string(pos:pos) /= ',') then
                    if (pos <= json_len .and. json_string(pos:pos) == ']') exit
                    res%error = 'Expected comma or closing bracket'
                    res%error_position = pos
                    return
                end if
                pos = pos + 1
            end if

            call validate_value(json_string, pos, json_len, depth, res)
            if (.not. res%ok) return

            call skip_whitespace(json_string, pos, json_len)
            first_element = .false.

            if (pos <= json_len .and. json_string(pos:pos) == ']') exit
        end do

        if (pos > json_len .or. json_string(pos:pos) /= ']') then
            res%error = 'Unclosed array'
            res%error_position = pos
            return
        end if

        pos = pos + 1
        res%ok = .true.
    end subroutine validate_array

    !> Validate JSON string
    subroutine validate_string(json_string, pos, json_len, res)
        character(len=*), intent(in) :: json_string
        integer, intent(inout) :: pos
        integer, intent(in) :: json_len
        type(JsonResult), intent(inout) :: res
        character(len=1) :: c
        integer :: start_pos

        start_pos = pos
        pos = pos + 1  ! Skip opening quote

        do while (pos <= json_len)
            c = json_string(pos:pos)

            if (c == '"') then
                pos = pos + 1
                res%ok = .true.
                return
            else if (c == '\') then
                pos = pos + 1
                if (pos > json_len) then
                    res%error = 'Incomplete escape sequence'
                    res%error_position = pos
                    return
                end if
                c = json_string(pos:pos)
                select case (c)
                    case ('"', '\', '/', 'b', 'f', 'n', 'r', 't')
                        pos = pos + 1
                    case ('u')
                        ! Unicode escape - need 4 hex digits
                        if (pos + 4 > json_len) then
                            res%error = 'Incomplete unicode escape'
                            res%error_position = pos
                            return
                        end if
                        pos = pos + 5
                    case default
                        res%error = 'Invalid escape sequence'
                        res%error_position = pos
                        return
                end select
            else if (iachar(c) < 32) then
                res%error = 'Control character in string'
                res%error_position = pos
                return
            else
                pos = pos + 1
            end if
        end do

        res%error = 'Unterminated string'
        res%error_position = start_pos
    end subroutine validate_string

    !> Validate JSON number
    subroutine validate_number(json_string, pos, json_len, res)
        character(len=*), intent(in) :: json_string
        integer, intent(inout) :: pos
        integer, intent(in) :: json_len
        type(JsonResult), intent(inout) :: res
        character(len=1) :: c
        logical :: has_decimal, has_exponent

        has_decimal = .false.
        has_exponent = .false.

        ! Optional minus
        if (pos <= json_len .and. json_string(pos:pos) == '-') then
            pos = pos + 1
        end if

        ! Integer part
        if (pos > json_len) then
            res%error = 'Invalid number'
            res%error_position = pos
            return
        end if

        c = json_string(pos:pos)
        if (c == '0') then
            pos = pos + 1
        else if (c >= '1' .and. c <= '9') then
            do while (pos <= json_len)
                c = json_string(pos:pos)
                if (c < '0' .or. c > '9') exit
                pos = pos + 1
            end do
        else
            res%error = 'Invalid number'
            res%error_position = pos
            return
        end if

        ! Optional decimal part
        if (pos <= json_len .and. json_string(pos:pos) == '.') then
            has_decimal = .true.
            pos = pos + 1
            if (pos > json_len .or. json_string(pos:pos) < '0' .or. json_string(pos:pos) > '9') then
                res%error = 'Invalid decimal number'
                res%error_position = pos
                return
            end if
            do while (pos <= json_len)
                c = json_string(pos:pos)
                if (c < '0' .or. c > '9') exit
                pos = pos + 1
            end do
        end if

        ! Optional exponent
        if (pos <= json_len .and. (json_string(pos:pos) == 'e' .or. json_string(pos:pos) == 'E')) then
            has_exponent = .true.
            pos = pos + 1
            if (pos <= json_len .and. (json_string(pos:pos) == '+' .or. json_string(pos:pos) == '-')) then
                pos = pos + 1
            end if
            if (pos > json_len .or. json_string(pos:pos) < '0' .or. json_string(pos:pos) > '9') then
                res%error = 'Invalid exponent'
                res%error_position = pos
                return
            end if
            do while (pos <= json_len)
                c = json_string(pos:pos)
                if (c < '0' .or. c > '9') exit
                pos = pos + 1
            end do
        end if

        res%ok = .true.
    end subroutine validate_number

    !> Validate JSON boolean
    subroutine validate_boolean(json_string, pos, json_len, res)
        character(len=*), intent(in) :: json_string
        integer, intent(inout) :: pos
        integer, intent(in) :: json_len
        type(JsonResult), intent(inout) :: res

        if (pos + 3 <= json_len .and. json_string(pos:pos+3) == 'true') then
            pos = pos + 4
            res%ok = .true.
        else if (pos + 4 <= json_len .and. json_string(pos:pos+4) == 'false') then
            pos = pos + 5
            res%ok = .true.
        else
            res%error = 'Invalid boolean'
            res%error_position = pos
        end if
    end subroutine validate_boolean

    !> Validate JSON null
    subroutine validate_null(json_string, pos, json_len, res)
        character(len=*), intent(in) :: json_string
        integer, intent(inout) :: pos
        integer, intent(in) :: json_len
        type(JsonResult), intent(inout) :: res

        if (pos + 3 <= json_len .and. json_string(pos:pos+3) == 'null') then
            pos = pos + 4
            res%ok = .true.
        else
            res%error = 'Invalid null'
            res%error_position = pos
        end if
    end subroutine validate_null

    !> Skip whitespace characters
    subroutine skip_whitespace(json_string, pos, json_len)
        character(len=*), intent(in) :: json_string
        integer, intent(inout) :: pos
        integer, intent(in) :: json_len
        character(len=1) :: c

        do while (pos <= json_len)
            c = json_string(pos:pos)
            if (c /= ' ' .and. c /= char(9) .and. c /= char(10) .and. c /= char(13)) exit
            pos = pos + 1
        end do
    end subroutine skip_whitespace

    !> Simple JSON validity check
    function is_valid_json(json_string) result(is_valid)
        character(len=*), intent(in) :: json_string
        logical :: is_valid
        type(JsonResult) :: res

        res = validate_json(json_string)
        is_valid = res%ok
    end function is_valid_json

    !> Escape a string for JSON output
    function escape_json_string(input) result(escaped)
        character(len=*), intent(in) :: input
        character(len=:), allocatable :: escaped
        character(len=65536) :: buffer
        integer :: i, pos, char_code
        character(len=1) :: c
        character(len=6) :: unicode_escape

        buffer = ''
        pos = 1

        do i = 1, len_trim(input)
            c = input(i:i)
            char_code = iachar(c)

            select case (c)
                case ('"')
                    buffer(pos:pos+1) = '\"'
                    pos = pos + 2
                case ('\')
                    buffer(pos:pos+1) = '\\'
                    pos = pos + 2
                case (char(8))  ! backspace
                    buffer(pos:pos+1) = '\b'
                    pos = pos + 2
                case (char(12)) ! form feed
                    buffer(pos:pos+1) = '\f'
                    pos = pos + 2
                case (char(10)) ! newline
                    buffer(pos:pos+1) = '\n'
                    pos = pos + 2
                case (char(13)) ! carriage return
                    buffer(pos:pos+1) = '\r'
                    pos = pos + 2
                case (char(9))  ! tab
                    buffer(pos:pos+1) = '\t'
                    pos = pos + 2
                case default
                    if (char_code < 32) then
                        ! Control character - use unicode escape
                        write(unicode_escape, '(A2,Z4.4)') '\u', char_code
                        buffer(pos:pos+5) = unicode_escape
                        pos = pos + 6
                    else
                        buffer(pos:pos) = c
                        pos = pos + 1
                    end if
            end select
        end do

        escaped = trim(buffer(1:pos-1))
    end function escape_json_string

    !> Unescape a JSON string
    function unescape_json_string(input) result(unescaped)
        character(len=*), intent(in) :: input
        character(len=:), allocatable :: unescaped
        character(len=65536) :: buffer
        integer :: i, pos, input_len, unicode_val
        character(len=1) :: c
        character(len=4) :: hex_str

        buffer = ''
        pos = 1
        i = 1
        input_len = len_trim(input)

        do while (i <= input_len)
            c = input(i:i)

            if (c == '\' .and. i + 1 <= input_len) then
                i = i + 1
                c = input(i:i)
                select case (c)
                    case ('"', '\', '/')
                        buffer(pos:pos) = c
                        pos = pos + 1
                    case ('b')
                        buffer(pos:pos) = char(8)
                        pos = pos + 1
                    case ('f')
                        buffer(pos:pos) = char(12)
                        pos = pos + 1
                    case ('n')
                        buffer(pos:pos) = char(10)
                        pos = pos + 1
                    case ('r')
                        buffer(pos:pos) = char(13)
                        pos = pos + 1
                    case ('t')
                        buffer(pos:pos) = char(9)
                        pos = pos + 1
                    case ('u')
                        if (i + 4 <= input_len) then
                            hex_str = input(i+1:i+4)
                            read(hex_str, '(Z4)', err=100) unicode_val
                            if (unicode_val < 128) then
                                buffer(pos:pos) = achar(unicode_val)
                                pos = pos + 1
                            end if
                            i = i + 4
                        end if
100                     continue
                    case default
                        buffer(pos:pos) = c
                        pos = pos + 1
                end select
            else
                buffer(pos:pos) = c
                pos = pos + 1
            end if
            i = i + 1
        end do

        unescaped = trim(buffer(1:pos-1))
    end function unescape_json_string

    !> Parse a JSON string value
    function parse_json_string(json_string) result(value)
        character(len=*), intent(in) :: json_string
        type(json_value_type) :: value
        integer :: start_pos, end_pos

        value%value_type = JSON_TYPE_STRING

        ! Find string bounds (skip quotes)
        start_pos = 1
        if (json_string(1:1) == '"') start_pos = 2

        end_pos = len_trim(json_string)
        if (end_pos > 0 .and. json_string(end_pos:end_pos) == '"') end_pos = end_pos - 1

        if (start_pos <= end_pos) then
            value%string_value = unescape_json_string(json_string(start_pos:end_pos))
        end if

        value%valid = .true.
    end function parse_json_string

    !> Parse a JSON number value
    function parse_json_number(json_string) result(value)
        character(len=*), intent(in) :: json_string
        type(json_value_type) :: value
        integer :: io_stat
        logical :: is_integer

        ! Check if it's an integer (no decimal or exponent)
        is_integer = (index(json_string, '.') == 0) .and. &
                     (index(json_string, 'e') == 0) .and. &
                     (index(json_string, 'E') == 0)

        if (is_integer) then
            value%value_type = JSON_TYPE_INTEGER
            read(json_string, *, iostat=io_stat) value%int_value
        else
            value%value_type = JSON_TYPE_REAL
            read(json_string, *, iostat=io_stat) value%real_value
        end if

        value%valid = (io_stat == 0)
    end function parse_json_number

    !> Parse a JSON boolean value
    function parse_json_boolean(json_string) result(value)
        character(len=*), intent(in) :: json_string
        type(json_value_type) :: value

        value%value_type = JSON_TYPE_BOOLEAN

        if (trim(adjustl(json_string)) == 'true') then
            value%bool_value = .true.
            value%valid = .true.
        else if (trim(adjustl(json_string)) == 'false') then
            value%bool_value = .false.
            value%valid = .true.
        end if
    end function parse_json_boolean

    !> Get human-readable type name
    pure function json_type_name(type_code) result(name)
        integer, intent(in) :: type_code
        character(len=16) :: name

        select case (type_code)
            case (JSON_TYPE_NULL)
                name = 'null'
            case (JSON_TYPE_BOOLEAN)
                name = 'boolean'
            case (JSON_TYPE_INTEGER)
                name = 'integer'
            case (JSON_TYPE_REAL)
                name = 'real'
            case (JSON_TYPE_STRING)
                name = 'string'
            case (JSON_TYPE_ARRAY)
                name = 'array'
            case (JSON_TYPE_OBJECT)
                name = 'object'
            case default
                name = 'unknown'
        end select
    end function json_type_name

end module safe_json
