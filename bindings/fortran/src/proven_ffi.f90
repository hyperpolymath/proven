! SPDX-License-Identifier: PMPL-1.0-or-later
! Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
!
! Proven FFI - Fortran iso_c_binding declarations for libproven
!
! This module declares the complete C ABI of libproven so that all
! safe_* wrapper modules can call the verified Idris 2 implementation
! through the Zig FFI bridge.  Link with -lproven at build time.
!
! All computation is performed in the Idris 2 core; these declarations
! are the only contact surface between Fortran and the library.

module proven_ffi
    use, intrinsic :: iso_c_binding
    implicit none
    private

    ! =========================================================================
    ! Status codes (mirrors ProvenStatus enum)
    ! =========================================================================
    integer(c_int32_t), parameter, public :: PROVEN_OK                   =   0
    integer(c_int32_t), parameter, public :: PROVEN_ERR_NULL_POINTER     =  -1
    integer(c_int32_t), parameter, public :: PROVEN_ERR_INVALID_ARGUMENT =  -2
    integer(c_int32_t), parameter, public :: PROVEN_ERR_OVERFLOW         =  -3
    integer(c_int32_t), parameter, public :: PROVEN_ERR_UNDERFLOW        =  -4
    integer(c_int32_t), parameter, public :: PROVEN_ERR_DIVISION_BY_ZERO =  -5
    integer(c_int32_t), parameter, public :: PROVEN_ERR_PARSE_FAILURE    =  -6
    integer(c_int32_t), parameter, public :: PROVEN_ERR_VALIDATION_FAILED = -7
    integer(c_int32_t), parameter, public :: PROVEN_ERR_OUT_OF_BOUNDS    =  -8
    integer(c_int32_t), parameter, public :: PROVEN_ERR_ENCODING_ERROR   =  -9
    integer(c_int32_t), parameter, public :: PROVEN_ERR_ALLOCATION_FAILED = -10
    integer(c_int32_t), parameter, public :: PROVEN_ERR_NOT_IMPLEMENTED  = -99

    ! =========================================================================
    ! Core result types  (bind(c) structs)
    ! =========================================================================

    !> Integer result: { int32_t status; int64_t value; }
    type, bind(c), public :: c_proven_int_result
        integer(c_int32_t) :: status
        integer(c_int64_t) :: value
    end type c_proven_int_result

    !> Boolean result: { int32_t status; bool value; }
    type, bind(c), public :: c_proven_bool_result
        integer(c_int32_t) :: status
        logical(c_bool)    :: value
    end type c_proven_bool_result

    !> String result: { int32_t status; char* value; size_t length; }
    type, bind(c), public :: c_proven_string_result
        integer(c_int32_t) :: status
        type(c_ptr)        :: value
        integer(c_size_t)  :: length
    end type c_proven_string_result

    !> Float result: { int32_t status; double value; }
    type, bind(c), public :: c_proven_float_result
        integer(c_int32_t) :: status
        real(c_double)     :: value
    end type c_proven_float_result

    !> IPv4 address: { uint8_t octets[4]; }
    type, bind(c), public :: c_proven_ipv4_address
        integer(c_int8_t) :: octets(4)
    end type c_proven_ipv4_address

    !> IPv4 result: { int32_t status; ProvenIPv4Address address; }
    type, bind(c), public :: c_proven_ipv4_result
        integer(c_int32_t)       :: status
        type(c_proven_ipv4_address) :: address
    end type c_proven_ipv4_result

    !> UUID: { uint8_t bytes[16]; }
    type, bind(c), public :: c_proven_uuid
        integer(c_int8_t) :: bytes(16)
    end type c_proven_uuid

    !> UUID result: { int32_t status; ProvenUUID uuid; }
    type, bind(c), public :: c_proven_uuid_result
        integer(c_int32_t) :: status
        type(c_proven_uuid) :: uuid
    end type c_proven_uuid_result

    !> Hex decode result: { int32_t status; uint8_t* data; size_t length; }
    type, bind(c), public :: c_proven_hex_decode_result
        integer(c_int32_t) :: status
        type(c_ptr)        :: data
        integer(c_size_t)  :: length
    end type c_proven_hex_decode_result

    !> Semantic version: { uint32_t major,minor,patch; size_t prerelease_len; char* prerelease; }
    type, bind(c), public :: c_proven_semantic_version
        integer(c_int32_t) :: major
        integer(c_int32_t) :: minor
        integer(c_int32_t) :: patch
        integer(c_size_t)  :: prerelease_len
        type(c_ptr)        :: prerelease
    end type c_proven_semantic_version

    !> Version result: { int32_t status; ProvenSemanticVersion version; }
    type, bind(c), public :: c_proven_version_result
        integer(c_int32_t)              :: status
        type(c_proven_semantic_version) :: version
    end type c_proven_version_result

    !> Currency result
    type, bind(c), public :: c_proven_currency_result
        integer(c_int32_t) :: status
        integer(c_int64_t) :: amount_minor
        integer(c_int8_t)  :: currency_code(3)
        integer(c_int8_t)  :: decimal_places
    end type c_proven_currency_result

    !> Phone result
    type, bind(c), public :: c_proven_phone_result
        integer(c_int32_t)  :: status
        integer(c_int16_t)  :: country_code
        integer(c_int64_t)  :: national_number
        logical(c_bool)     :: is_valid
    end type c_proven_phone_result

    ! =========================================================================
    ! C function interfaces
    ! =========================================================================

    interface

        ! ----- Lifecycle -----------------------------------------------------

        function proven_init() bind(c, name="proven_init") result(status)
            import :: c_int32_t
            integer(c_int32_t) :: status
        end function proven_init

        subroutine proven_deinit() bind(c, name="proven_deinit")
        end subroutine proven_deinit

        function proven_is_initialized() bind(c, name="proven_is_initialized") result(init)
            import :: c_bool
            logical(c_bool) :: init
        end function proven_is_initialized

        ! ----- Version info --------------------------------------------------

        function proven_ffi_abi_version() bind(c, name="proven_ffi_abi_version") result(v)
            import :: c_int32_t
            integer(c_int32_t) :: v
        end function proven_ffi_abi_version

        function proven_version_major() bind(c, name="proven_version_major") result(v)
            import :: c_int32_t
            integer(c_int32_t) :: v
        end function proven_version_major

        function proven_version_minor() bind(c, name="proven_version_minor") result(v)
            import :: c_int32_t
            integer(c_int32_t) :: v
        end function proven_version_minor

        function proven_version_patch() bind(c, name="proven_version_patch") result(v)
            import :: c_int32_t
            integer(c_int32_t) :: v
        end function proven_version_patch

        function proven_module_count() bind(c, name="proven_module_count") result(v)
            import :: c_int32_t
            integer(c_int32_t) :: v
        end function proven_module_count

        ! ----- Memory management ---------------------------------------------

        subroutine proven_free_string(ptr) bind(c, name="proven_free_string")
            import :: c_ptr
            type(c_ptr), value :: ptr
        end subroutine proven_free_string

        ! ----- SafeMath ------------------------------------------------------

        function proven_math_div(a, b) bind(c, name="proven_math_div") result(res)
            import :: c_int64_t, c_proven_int_result
            integer(c_int64_t), value, intent(in) :: a, b
            type(c_proven_int_result) :: res
        end function proven_math_div

        function proven_math_mod(a, b) bind(c, name="proven_math_mod") result(res)
            import :: c_int64_t, c_proven_int_result
            integer(c_int64_t), value, intent(in) :: a, b
            type(c_proven_int_result) :: res
        end function proven_math_mod

        function proven_math_add_checked(a, b) &
                bind(c, name="proven_math_add_checked") result(res)
            import :: c_int64_t, c_proven_int_result
            integer(c_int64_t), value, intent(in) :: a, b
            type(c_proven_int_result) :: res
        end function proven_math_add_checked

        function proven_math_sub_checked(a, b) &
                bind(c, name="proven_math_sub_checked") result(res)
            import :: c_int64_t, c_proven_int_result
            integer(c_int64_t), value, intent(in) :: a, b
            type(c_proven_int_result) :: res
        end function proven_math_sub_checked

        function proven_math_mul_checked(a, b) &
                bind(c, name="proven_math_mul_checked") result(res)
            import :: c_int64_t, c_proven_int_result
            integer(c_int64_t), value, intent(in) :: a, b
            type(c_proven_int_result) :: res
        end function proven_math_mul_checked

        function proven_math_abs_safe(n) &
                bind(c, name="proven_math_abs_safe") result(res)
            import :: c_int64_t, c_proven_int_result
            integer(c_int64_t), value, intent(in) :: n
            type(c_proven_int_result) :: res
        end function proven_math_abs_safe

        function proven_math_clamp(lo, hi, val) &
                bind(c, name="proven_math_clamp") result(clamped)
            import :: c_int64_t
            integer(c_int64_t), value, intent(in) :: lo, hi, val
            integer(c_int64_t) :: clamped
        end function proven_math_clamp

        function proven_math_pow_checked(base, exp) &
                bind(c, name="proven_math_pow_checked") result(res)
            import :: c_int64_t, c_int32_t, c_proven_int_result
            integer(c_int64_t), value, intent(in) :: base
            integer(c_int32_t), value, intent(in) :: exp
            type(c_proven_int_result) :: res
        end function proven_math_pow_checked

        ! ----- SafeString ----------------------------------------------------

        function proven_string_is_valid_utf8(ptr, length) &
                bind(c, name="proven_string_is_valid_utf8") result(res)
            import :: c_ptr, c_size_t, c_proven_bool_result
            type(c_ptr), value, intent(in)        :: ptr
            integer(c_size_t), value, intent(in)  :: length
            type(c_proven_bool_result) :: res
        end function proven_string_is_valid_utf8

        function proven_string_escape_sql(ptr, length) &
                bind(c, name="proven_string_escape_sql") result(res)
            import :: c_ptr, c_size_t, c_proven_string_result
            type(c_ptr), value, intent(in)        :: ptr
            integer(c_size_t), value, intent(in)  :: length
            type(c_proven_string_result) :: res
        end function proven_string_escape_sql

        function proven_string_escape_html(ptr, length) &
                bind(c, name="proven_string_escape_html") result(res)
            import :: c_ptr, c_size_t, c_proven_string_result
            type(c_ptr), value, intent(in)        :: ptr
            integer(c_size_t), value, intent(in)  :: length
            type(c_proven_string_result) :: res
        end function proven_string_escape_html

        function proven_string_escape_js(ptr, length) &
                bind(c, name="proven_string_escape_js") result(res)
            import :: c_ptr, c_size_t, c_proven_string_result
            type(c_ptr), value, intent(in)        :: ptr
            integer(c_size_t), value, intent(in)  :: length
            type(c_proven_string_result) :: res
        end function proven_string_escape_js

        ! ----- SafePath ------------------------------------------------------

        function proven_path_has_traversal(ptr, length) &
                bind(c, name="proven_path_has_traversal") result(res)
            import :: c_ptr, c_size_t, c_proven_bool_result
            type(c_ptr), value, intent(in)        :: ptr
            integer(c_size_t), value, intent(in)  :: length
            type(c_proven_bool_result) :: res
        end function proven_path_has_traversal

        function proven_path_sanitize_filename(ptr, length) &
                bind(c, name="proven_path_sanitize_filename") result(res)
            import :: c_ptr, c_size_t, c_proven_string_result
            type(c_ptr), value, intent(in)        :: ptr
            integer(c_size_t), value, intent(in)  :: length
            type(c_proven_string_result) :: res
        end function proven_path_sanitize_filename

        ! ----- SafeEmail -----------------------------------------------------

        function proven_email_is_valid(ptr, length) &
                bind(c, name="proven_email_is_valid") result(res)
            import :: c_ptr, c_size_t, c_proven_bool_result
            type(c_ptr), value, intent(in)        :: ptr
            integer(c_size_t), value, intent(in)  :: length
            type(c_proven_bool_result) :: res
        end function proven_email_is_valid

        ! ----- SafeNetwork ---------------------------------------------------

        function proven_network_parse_ipv4(ptr, length) &
                bind(c, name="proven_network_parse_ipv4") result(res)
            import :: c_ptr, c_size_t, c_proven_ipv4_result
            type(c_ptr), value, intent(in)        :: ptr
            integer(c_size_t), value, intent(in)  :: length
            type(c_proven_ipv4_result) :: res
        end function proven_network_parse_ipv4

        function proven_network_ipv4_is_private(addr) &
                bind(c, name="proven_network_ipv4_is_private") result(res)
            import :: c_bool, c_proven_ipv4_address
            type(c_proven_ipv4_address), value, intent(in) :: addr
            logical(c_bool) :: res
        end function proven_network_ipv4_is_private

        function proven_network_ipv4_is_loopback(addr) &
                bind(c, name="proven_network_ipv4_is_loopback") result(res)
            import :: c_bool, c_proven_ipv4_address
            type(c_proven_ipv4_address), value, intent(in) :: addr
            logical(c_bool) :: res
        end function proven_network_ipv4_is_loopback

        ! ----- SafeCrypto ----------------------------------------------------

        function proven_crypto_constant_time_eq(p1, l1, p2, l2) &
                bind(c, name="proven_crypto_constant_time_eq") result(res)
            import :: c_ptr, c_size_t, c_proven_bool_result
            type(c_ptr), value, intent(in)        :: p1
            integer(c_size_t), value, intent(in)  :: l1
            type(c_ptr), value, intent(in)        :: p2
            integer(c_size_t), value, intent(in)  :: l2
            type(c_proven_bool_result) :: res
        end function proven_crypto_constant_time_eq

        function proven_crypto_random_bytes(ptr, length) &
                bind(c, name="proven_crypto_random_bytes") result(status)
            import :: c_ptr, c_size_t, c_int32_t
            type(c_ptr), value    :: ptr
            integer(c_size_t), value, intent(in) :: length
            integer(c_int32_t) :: status
        end function proven_crypto_random_bytes

        ! ----- SafeUUID -----------------------------------------------------

        function proven_uuid_v4() bind(c, name="proven_uuid_v4") result(res)
            import :: c_proven_uuid_result
            type(c_proven_uuid_result) :: res
        end function proven_uuid_v4

        function proven_uuid_to_string(uuid) &
                bind(c, name="proven_uuid_to_string") result(res)
            import :: c_proven_uuid, c_proven_string_result
            type(c_proven_uuid), value, intent(in) :: uuid
            type(c_proven_string_result) :: res
        end function proven_uuid_to_string

        function proven_uuid_parse(ptr, length) &
                bind(c, name="proven_uuid_parse") result(res)
            import :: c_ptr, c_size_t, c_proven_uuid_result
            type(c_ptr), value, intent(in)        :: ptr
            integer(c_size_t), value, intent(in)  :: length
            type(c_proven_uuid_result) :: res
        end function proven_uuid_parse

        function proven_uuid_is_nil(uuid) &
                bind(c, name="proven_uuid_is_nil") result(res)
            import :: c_bool, c_proven_uuid
            type(c_proven_uuid), value, intent(in) :: uuid
            logical(c_bool) :: res
        end function proven_uuid_is_nil

        function proven_uuid_version(uuid) &
                bind(c, name="proven_uuid_version") result(ver)
            import :: c_int8_t, c_proven_uuid
            type(c_proven_uuid), value, intent(in) :: uuid
            integer(c_int8_t) :: ver
        end function proven_uuid_version

        ! ----- SafeHex ------------------------------------------------------

        function proven_hex_encode(ptr, length, uppercase) &
                bind(c, name="proven_hex_encode") result(res)
            import :: c_ptr, c_size_t, c_bool, c_proven_string_result
            type(c_ptr), value, intent(in)        :: ptr
            integer(c_size_t), value, intent(in)  :: length
            logical(c_bool), value, intent(in)    :: uppercase
            type(c_proven_string_result) :: res
        end function proven_hex_encode

        function proven_hex_decode(ptr, length) &
                bind(c, name="proven_hex_decode") result(res)
            import :: c_ptr, c_size_t, c_proven_hex_decode_result
            type(c_ptr), value, intent(in)        :: ptr
            integer(c_size_t), value, intent(in)  :: length
            type(c_proven_hex_decode_result) :: res
        end function proven_hex_decode

        subroutine proven_hex_free(res) bind(c, name="proven_hex_free")
            import :: c_ptr
            type(c_ptr), value :: res
        end subroutine proven_hex_free

        ! ----- SafeFloat ----------------------------------------------------

        function proven_float_div(a, b) &
                bind(c, name="proven_float_div") result(res)
            import :: c_double, c_proven_float_result
            real(c_double), value, intent(in) :: a, b
            type(c_proven_float_result) :: res
        end function proven_float_div

        function proven_float_is_finite(x) &
                bind(c, name="proven_float_is_finite") result(res)
            import :: c_double, c_bool
            real(c_double), value, intent(in) :: x
            logical(c_bool) :: res
        end function proven_float_is_finite

        function proven_float_is_nan(x) &
                bind(c, name="proven_float_is_nan") result(res)
            import :: c_double, c_bool
            real(c_double), value, intent(in) :: x
            logical(c_bool) :: res
        end function proven_float_is_nan

        function proven_float_sqrt(x) &
                bind(c, name="proven_float_sqrt") result(res)
            import :: c_double, c_proven_float_result
            real(c_double), value, intent(in) :: x
            type(c_proven_float_result) :: res
        end function proven_float_sqrt

        function proven_float_ln(x) &
                bind(c, name="proven_float_ln") result(res)
            import :: c_double, c_proven_float_result
            real(c_double), value, intent(in) :: x
            type(c_proven_float_result) :: res
        end function proven_float_ln

        ! ----- SafeVersion --------------------------------------------------

        function proven_version_parse(ptr, length) &
                bind(c, name="proven_version_parse") result(res)
            import :: c_ptr, c_size_t, c_proven_version_result
            type(c_ptr), value, intent(in)        :: ptr
            integer(c_size_t), value, intent(in)  :: length
            type(c_proven_version_result) :: res
        end function proven_version_parse

        function proven_version_compare(a, b) &
                bind(c, name="proven_version_compare") result(cmp)
            import :: c_int32_t, c_proven_semantic_version
            type(c_proven_semantic_version), value, intent(in) :: a, b
            integer(c_int32_t) :: cmp
        end function proven_version_compare

        subroutine proven_version_free(ver) bind(c, name="proven_version_free")
            import :: c_ptr
            type(c_ptr), value :: ver
        end subroutine proven_version_free

        ! ----- SafeAngle ----------------------------------------------------

        function proven_angle_deg_to_rad(degrees) &
                bind(c, name="proven_angle_deg_to_rad") result(radians)
            import :: c_double
            real(c_double), value, intent(in) :: degrees
            real(c_double) :: radians
        end function proven_angle_deg_to_rad

        function proven_angle_rad_to_deg(radians) &
                bind(c, name="proven_angle_rad_to_deg") result(degrees)
            import :: c_double
            real(c_double), value, intent(in) :: radians
            real(c_double) :: degrees
        end function proven_angle_rad_to_deg

        function proven_angle_normalize_degrees(degrees) &
                bind(c, name="proven_angle_normalize_degrees") result(normalized)
            import :: c_double
            real(c_double), value, intent(in) :: degrees
            real(c_double) :: normalized
        end function proven_angle_normalize_degrees

        function proven_angle_normalize_radians(radians) &
                bind(c, name="proven_angle_normalize_radians") result(normalized)
            import :: c_double
            real(c_double), value, intent(in) :: radians
            real(c_double) :: normalized
        end function proven_angle_normalize_radians

        ! ----- SafeCurrency -------------------------------------------------

        function proven_currency_parse(ptr, length) &
                bind(c, name="proven_currency_parse") result(res)
            import :: c_ptr, c_size_t, c_proven_currency_result
            type(c_ptr), value, intent(in)        :: ptr
            integer(c_size_t), value, intent(in)  :: length
            type(c_proven_currency_result) :: res
        end function proven_currency_parse

        function proven_currency_format(amount_minor, code, decimal_places) &
                bind(c, name="proven_currency_format") result(res)
            import :: c_int64_t, c_int8_t, c_proven_string_result
            integer(c_int64_t), value, intent(in) :: amount_minor
            integer(c_int8_t), intent(in)         :: code(3)
            integer(c_int8_t), value, intent(in)  :: decimal_places
            type(c_proven_string_result) :: res
        end function proven_currency_format

        ! ----- SafePhone ----------------------------------------------------

        function proven_phone_parse(ptr, length) &
                bind(c, name="proven_phone_parse") result(res)
            import :: c_ptr, c_size_t, c_proven_phone_result
            type(c_ptr), value, intent(in)        :: ptr
            integer(c_size_t), value, intent(in)  :: length
            type(c_proven_phone_result) :: res
        end function proven_phone_parse

        function proven_phone_format_e164(country_code, national_number) &
                bind(c, name="proven_phone_format_e164") result(res)
            import :: c_int16_t, c_int64_t, c_proven_string_result
            integer(c_int16_t), value, intent(in) :: country_code
            integer(c_int64_t), value, intent(in) :: national_number
            type(c_proven_string_result) :: res
        end function proven_phone_format_e164

    end interface

    ! Make all interfaces public
    public :: proven_init, proven_deinit, proven_is_initialized
    public :: proven_ffi_abi_version
    public :: proven_version_major, proven_version_minor, proven_version_patch
    public :: proven_module_count
    public :: proven_free_string
    public :: proven_math_div, proven_math_mod
    public :: proven_math_add_checked, proven_math_sub_checked
    public :: proven_math_mul_checked, proven_math_abs_safe
    public :: proven_math_clamp, proven_math_pow_checked
    public :: proven_string_is_valid_utf8
    public :: proven_string_escape_sql, proven_string_escape_html
    public :: proven_string_escape_js
    public :: proven_path_has_traversal, proven_path_sanitize_filename
    public :: proven_email_is_valid
    public :: proven_network_parse_ipv4
    public :: proven_network_ipv4_is_private, proven_network_ipv4_is_loopback
    public :: proven_crypto_constant_time_eq, proven_crypto_random_bytes
    public :: proven_uuid_v4, proven_uuid_to_string, proven_uuid_parse
    public :: proven_uuid_is_nil, proven_uuid_version
    public :: proven_hex_encode, proven_hex_decode, proven_hex_free
    public :: proven_float_div, proven_float_is_finite, proven_float_is_nan
    public :: proven_float_sqrt, proven_float_ln
    public :: proven_version_parse, proven_version_compare, proven_version_free
    public :: proven_angle_deg_to_rad, proven_angle_rad_to_deg
    public :: proven_angle_normalize_degrees, proven_angle_normalize_radians
    public :: proven_currency_parse, proven_currency_format
    public :: proven_phone_parse, proven_phone_format_e164

end module proven_ffi
