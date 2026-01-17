! SPDX-License-Identifier: PMPL-1.0
! SPDX-FileCopyrightText: 2025 Hyperpolymath
!
! Proven SafeURL - URL validation, parsing, and encoding for Fortran
!

module safe_url
    use, intrinsic :: iso_fortran_env, only: int64, int32
    implicit none
    private

    !> Maximum URL component lengths
    integer, parameter :: MAX_SCHEME_LEN = 16
    integer, parameter :: MAX_HOST_LEN = 253
    integer, parameter :: MAX_PATH_LEN = 2048
    integer, parameter :: MAX_QUERY_LEN = 2048
    integer, parameter :: MAX_URL_LEN = 4096

    !> URL scheme enumeration
    integer, parameter, public :: URL_SCHEME_UNKNOWN = 0
    integer, parameter, public :: URL_SCHEME_HTTP = 1
    integer, parameter, public :: URL_SCHEME_HTTPS = 2
    integer, parameter, public :: URL_SCHEME_FTP = 3
    integer, parameter, public :: URL_SCHEME_FTPS = 4
    integer, parameter, public :: URL_SCHEME_FILE = 5
    integer, parameter, public :: URL_SCHEME_MAILTO = 6
    integer, parameter, public :: URL_SCHEME_DATA = 7

    !> URL components type
    type, public :: url_type
        character(len=MAX_SCHEME_LEN) :: scheme = ''
        character(len=MAX_HOST_LEN) :: host = ''
        integer :: port = 0
        character(len=MAX_PATH_LEN) :: path = ''
        character(len=MAX_QUERY_LEN) :: query = ''
        character(len=128) :: fragment = ''
        character(len=64) :: username = ''
        character(len=64) :: password = ''
        logical :: valid = .false.
    contains
        procedure :: to_string => url_to_string
        procedure :: is_secure => url_is_secure
    end type url_type

    !> URL parse result type
    type, public :: UrlResult
        type(url_type) :: url
        character(len=256) :: error = ''
        logical :: ok = .false.
    end type UrlResult

    !> Characters that must be percent-encoded
    character(len=20), parameter :: RESERVED_CHARS = ':/?#[]@!$&''()*+,;='
    character(len=16), parameter :: HEX_CHARS = '0123456789ABCDEF'

    public :: parse_url, format_url, is_valid_url
    public :: url_encode, url_decode, encode_query_param
    public :: get_default_port, normalize_url

contains

    !> Parse a URL string into components
    function parse_url(url_string) result(res)
        character(len=*), intent(in) :: url_string
        type(UrlResult) :: res
        integer :: i, url_len, scheme_end, authority_start, authority_end
        integer :: host_start, host_end, port_start, path_start, query_start, fragment_start
        character(len=MAX_URL_LEN) :: working_url
        character(len=5) :: port_str

        url_len = len_trim(url_string)

        if (url_len == 0) then
            res%error = 'Empty URL'
            return
        end if

        if (url_len > MAX_URL_LEN) then
            res%error = 'URL exceeds maximum length'
            return
        end if

        working_url = url_string

        ! Find scheme (ends with ://)
        scheme_end = index(working_url, '://')
        if (scheme_end == 0) then
            res%error = 'Missing scheme separator (://)'
            return
        end if

        res%url%scheme = working_url(1:scheme_end-1)

        ! Validate scheme
        if (.not. is_valid_scheme(res%url%scheme)) then
            res%error = 'Invalid URL scheme'
            return
        end if

        ! Authority starts after ://
        authority_start = scheme_end + 3

        ! Find end of authority (first / ? or # after authority)
        authority_end = url_len
        path_start = 0
        query_start = 0
        fragment_start = 0

        do i = authority_start, url_len
            if (working_url(i:i) == '/') then
                authority_end = i - 1
                path_start = i
                exit
            else if (working_url(i:i) == '?') then
                authority_end = i - 1
                query_start = i + 1
                exit
            else if (working_url(i:i) == '#') then
                authority_end = i - 1
                fragment_start = i + 1
                exit
            end if
        end do

        ! Parse authority (userinfo@host:port)
        call parse_authority(working_url(authority_start:authority_end), res%url)

        ! Parse path
        if (path_start > 0) then
            ! Find query or fragment start
            do i = path_start, url_len
                if (working_url(i:i) == '?') then
                    res%url%path = working_url(path_start:i-1)
                    query_start = i + 1
                    exit
                else if (working_url(i:i) == '#') then
                    res%url%path = working_url(path_start:i-1)
                    fragment_start = i + 1
                    exit
                end if
            end do
            if (query_start == 0 .and. fragment_start == 0) then
                res%url%path = working_url(path_start:url_len)
            end if
        end if

        ! Parse query
        if (query_start > 0) then
            do i = query_start, url_len
                if (working_url(i:i) == '#') then
                    res%url%query = working_url(query_start:i-1)
                    fragment_start = i + 1
                    exit
                end if
            end do
            if (fragment_start == 0) then
                res%url%query = working_url(query_start:url_len)
            end if
        end if

        ! Parse fragment
        if (fragment_start > 0) then
            res%url%fragment = working_url(fragment_start:url_len)
        end if

        ! Set default port if not specified
        if (res%url%port == 0) then
            res%url%port = get_default_port(res%url%scheme)
        end if

        res%url%valid = .true.
        res%ok = .true.
    end function parse_url

    !> Parse authority component (userinfo@host:port)
    subroutine parse_authority(authority, url_instance)
        character(len=*), intent(in) :: authority
        type(url_type), intent(inout) :: url_instance
        integer :: at_pos, colon_pos, auth_len, port_start
        character(len=5) :: port_str
        integer :: port_value, io_stat

        auth_len = len_trim(authority)

        ! Find userinfo separator @
        at_pos = index(authority, '@')
        if (at_pos > 0) then
            ! Parse userinfo
            colon_pos = index(authority(1:at_pos-1), ':')
            if (colon_pos > 0) then
                url_instance%username = authority(1:colon_pos-1)
                url_instance%password = authority(colon_pos+1:at_pos-1)
            else
                url_instance%username = authority(1:at_pos-1)
            end if
        end if

        ! Parse host:port
        if (at_pos > 0) then
            port_start = at_pos + 1
        else
            port_start = 1
        end if

        ! Check for IPv6 address (enclosed in [])
        if (authority(port_start:port_start) == '[') then
            ! Find closing bracket
            colon_pos = index(authority(port_start:auth_len), ']:')
            if (colon_pos > 0) then
                url_instance%host = authority(port_start:port_start+colon_pos-1)
                port_str = authority(port_start+colon_pos+1:auth_len)
                read(port_str, *, iostat=io_stat) port_value
                if (io_stat == 0) url_instance%port = port_value
            else
                url_instance%host = authority(port_start:auth_len)
            end if
        else
            ! Regular host:port
            colon_pos = index(authority(port_start:auth_len), ':')
            if (colon_pos > 0) then
                url_instance%host = authority(port_start:port_start+colon_pos-2)
                port_str = authority(port_start+colon_pos:auth_len)
                read(port_str, *, iostat=io_stat) port_value
                if (io_stat == 0) url_instance%port = port_value
            else
                url_instance%host = authority(port_start:auth_len)
            end if
        end if
    end subroutine parse_authority

    !> Check if scheme is valid
    pure function is_valid_scheme(scheme) result(is_valid)
        character(len=*), intent(in) :: scheme
        logical :: is_valid
        character(len=16) :: lower_scheme
        integer :: i

        is_valid = .false.

        if (len_trim(scheme) == 0) return

        ! Convert to lowercase
        lower_scheme = ''
        do i = 1, min(len_trim(scheme), 16)
            if (scheme(i:i) >= 'A' .and. scheme(i:i) <= 'Z') then
                lower_scheme(i:i) = achar(iachar(scheme(i:i)) + 32)
            else
                lower_scheme(i:i) = scheme(i:i)
            end if
        end do

        ! Check known schemes
        select case (trim(lower_scheme))
            case ('http', 'https', 'ftp', 'ftps', 'file', 'mailto', 'data', 'ws', 'wss')
                is_valid = .true.
        end select
    end function is_valid_scheme

    !> Format URL as string
    function url_to_string(self) result(url_string)
        class(url_type), intent(in) :: self
        character(len=MAX_URL_LEN) :: url_string
        integer :: pos
        character(len=10) :: port_str

        if (.not. self%valid) then
            url_string = ''
            return
        end if

        url_string = ''
        pos = 1

        ! Scheme
        url_string = trim(self%scheme) // '://'
        pos = len_trim(url_string) + 1

        ! Userinfo
        if (len_trim(self%username) > 0) then
            url_string = trim(url_string) // trim(self%username)
            if (len_trim(self%password) > 0) then
                url_string = trim(url_string) // ':' // trim(self%password)
            end if
            url_string = trim(url_string) // '@'
        end if

        ! Host
        url_string = trim(url_string) // trim(self%host)

        ! Port (if non-default)
        if (self%port > 0 .and. self%port /= get_default_port(self%scheme)) then
            write(port_str, '(I0)') self%port
            url_string = trim(url_string) // ':' // trim(port_str)
        end if

        ! Path
        if (len_trim(self%path) > 0) then
            url_string = trim(url_string) // trim(self%path)
        end if

        ! Query
        if (len_trim(self%query) > 0) then
            url_string = trim(url_string) // '?' // trim(self%query)
        end if

        ! Fragment
        if (len_trim(self%fragment) > 0) then
            url_string = trim(url_string) // '#' // trim(self%fragment)
        end if
    end function url_to_string

    !> Check if URL uses secure scheme
    pure function url_is_secure(self) result(is_secure)
        class(url_type), intent(in) :: self
        logical :: is_secure
        character(len=16) :: lower_scheme
        integer :: i

        is_secure = .false.

        do i = 1, min(len_trim(self%scheme), 16)
            if (self%scheme(i:i) >= 'A' .and. self%scheme(i:i) <= 'Z') then
                lower_scheme(i:i) = achar(iachar(self%scheme(i:i)) + 32)
            else
                lower_scheme(i:i) = self%scheme(i:i)
            end if
        end do

        select case (trim(lower_scheme))
            case ('https', 'ftps', 'wss')
                is_secure = .true.
        end select
    end function url_is_secure

    !> Standalone format function
    function format_url(url_instance) result(url_string)
        type(url_type), intent(in) :: url_instance
        character(len=MAX_URL_LEN) :: url_string

        url_string = url_instance%to_string()
    end function format_url

    !> Check if URL string is valid
    function is_valid_url(url_string) result(is_valid)
        character(len=*), intent(in) :: url_string
        logical :: is_valid
        type(UrlResult) :: res

        res = parse_url(url_string)
        is_valid = res%ok
    end function is_valid_url

    !> URL-encode a string (percent encoding)
    function url_encode(input) result(encoded)
        character(len=*), intent(in) :: input
        character(len=:), allocatable :: encoded
        character(len=4096) :: buffer
        integer :: i, pos, char_code
        character(len=1) :: c

        buffer = ''
        pos = 1

        do i = 1, len_trim(input)
            c = input(i:i)
            char_code = iachar(c)

            ! Check if character needs encoding
            if (needs_encoding(c)) then
                ! Percent-encode the character
                buffer(pos:pos) = '%'
                buffer(pos+1:pos+1) = HEX_CHARS(char_code / 16 + 1:char_code / 16 + 1)
                buffer(pos+2:pos+2) = HEX_CHARS(mod(char_code, 16) + 1:mod(char_code, 16) + 1)
                pos = pos + 3
            else
                buffer(pos:pos) = c
                pos = pos + 1
            end if
        end do

        encoded = trim(buffer(1:pos-1))
    end function url_encode

    !> Check if character needs URL encoding
    pure function needs_encoding(c) result(needs_enc)
        character(len=1), intent(in) :: c
        logical :: needs_enc

        needs_enc = .true.

        ! Unreserved characters (RFC 3986)
        if ((c >= 'A' .and. c <= 'Z') .or. &
            (c >= 'a' .and. c <= 'z') .or. &
            (c >= '0' .and. c <= '9') .or. &
            c == '-' .or. c == '_' .or. c == '.' .or. c == '~') then
            needs_enc = .false.
        end if
    end function needs_encoding

    !> URL-decode a string
    function url_decode(input) result(decoded)
        character(len=*), intent(in) :: input
        character(len=:), allocatable :: decoded
        character(len=4096) :: buffer
        integer :: i, pos, input_len, hex_value
        character(len=2) :: hex_str

        buffer = ''
        pos = 1
        i = 1
        input_len = len_trim(input)

        do while (i <= input_len)
            if (input(i:i) == '%' .and. i + 2 <= input_len) then
                ! Decode percent-encoded character
                hex_str = input(i+1:i+2)
                hex_value = hex_to_int_value(hex_str)
                if (hex_value >= 0) then
                    buffer(pos:pos) = achar(hex_value)
                    pos = pos + 1
                    i = i + 3
                else
                    ! Invalid hex, keep literal
                    buffer(pos:pos) = input(i:i)
                    pos = pos + 1
                    i = i + 1
                end if
            else if (input(i:i) == '+') then
                ! Plus to space
                buffer(pos:pos) = ' '
                pos = pos + 1
                i = i + 1
            else
                buffer(pos:pos) = input(i:i)
                pos = pos + 1
                i = i + 1
            end if
        end do

        decoded = trim(buffer(1:pos-1))
    end function url_decode

    !> Convert 2-character hex string to integer
    pure function hex_to_int_value(hex_str) result(value)
        character(len=2), intent(in) :: hex_str
        integer :: value
        integer :: high, low

        high = hex_char_to_int(hex_str(1:1))
        low = hex_char_to_int(hex_str(2:2))

        if (high < 0 .or. low < 0) then
            value = -1
        else
            value = high * 16 + low
        end if
    end function hex_to_int_value

    !> Convert hex character to integer
    pure function hex_char_to_int(c) result(value)
        character(len=1), intent(in) :: c
        integer :: value

        select case (c)
            case ('0':'9')
                value = iachar(c) - iachar('0')
            case ('a':'f')
                value = iachar(c) - iachar('a') + 10
            case ('A':'F')
                value = iachar(c) - iachar('A') + 10
            case default
                value = -1
        end select
    end function hex_char_to_int

    !> Encode a query parameter value
    function encode_query_param(value) result(encoded)
        character(len=*), intent(in) :: value
        character(len=:), allocatable :: encoded
        character(len=4096) :: buffer
        integer :: i, pos, char_code
        character(len=1) :: c

        buffer = ''
        pos = 1

        do i = 1, len_trim(value)
            c = value(i:i)
            char_code = iachar(c)

            ! More aggressive encoding for query params
            if ((c >= 'A' .and. c <= 'Z') .or. &
                (c >= 'a' .and. c <= 'z') .or. &
                (c >= '0' .and. c <= '9') .or. &
                c == '-' .or. c == '_' .or. c == '.') then
                buffer(pos:pos) = c
                pos = pos + 1
            else if (c == ' ') then
                buffer(pos:pos) = '+'
                pos = pos + 1
            else
                ! Percent-encode
                buffer(pos:pos) = '%'
                buffer(pos+1:pos+1) = HEX_CHARS(char_code / 16 + 1:char_code / 16 + 1)
                buffer(pos+2:pos+2) = HEX_CHARS(mod(char_code, 16) + 1:mod(char_code, 16) + 1)
                pos = pos + 3
            end if
        end do

        encoded = trim(buffer(1:pos-1))
    end function encode_query_param

    !> Get default port for scheme
    pure function get_default_port(scheme) result(port)
        character(len=*), intent(in) :: scheme
        integer :: port
        character(len=16) :: lower_scheme
        integer :: i

        port = 0

        do i = 1, min(len_trim(scheme), 16)
            if (scheme(i:i) >= 'A' .and. scheme(i:i) <= 'Z') then
                lower_scheme(i:i) = achar(iachar(scheme(i:i)) + 32)
            else
                lower_scheme(i:i) = scheme(i:i)
            end if
        end do

        select case (trim(lower_scheme))
            case ('http', 'ws')
                port = 80
            case ('https', 'wss')
                port = 443
            case ('ftp')
                port = 21
            case ('ftps')
                port = 990
        end select
    end function get_default_port

    !> Normalize a URL (lowercase scheme/host, default port removal)
    function normalize_url(url_string) result(normalized)
        character(len=*), intent(in) :: url_string
        character(len=MAX_URL_LEN) :: normalized
        type(UrlResult) :: res
        integer :: i

        res = parse_url(url_string)

        if (.not. res%ok) then
            normalized = url_string
            return
        end if

        ! Lowercase scheme
        do i = 1, len_trim(res%url%scheme)
            if (res%url%scheme(i:i) >= 'A' .and. res%url%scheme(i:i) <= 'Z') then
                res%url%scheme(i:i) = achar(iachar(res%url%scheme(i:i)) + 32)
            end if
        end do

        ! Lowercase host
        do i = 1, len_trim(res%url%host)
            if (res%url%host(i:i) >= 'A' .and. res%url%host(i:i) <= 'Z') then
                res%url%host(i:i) = achar(iachar(res%url%host(i:i)) + 32)
            end if
        end do

        normalized = res%url%to_string()
    end function normalize_url

end module safe_url
