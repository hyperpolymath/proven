# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

"""
    Proven

Thin Julia FFI bindings to libproven, a formally verified safety library.

All computation is performed in verified Idris 2 code via the Zig FFI bridge.
These bindings ONLY marshal data to and from the C ABI; they do NOT reimplement
any logic. See `proven.h` for the canonical function documentation.

## Modules (41 total)

Core: SafeMath, SafeString, SafePath, SafeEmail, SafeUrl, SafeNetwork, SafeCrypto,
      SafeUUID, SafeJson, SafeDateTime, SafeFloat, SafeVersion, SafeColor, SafeAngle,
      SafeUnit, SafeHex, SafeCurrency, SafePhone, SafePassword

Data Structures: SafeBuffer, SafeQueue, SafeBloom, SafeLRU, SafeGraph

Resilience: SafeRateLimiter, SafeCircuitBreaker, SafeRetry, SafeMonotonic

State: SafeStateMachine, SafeCalculator

Algorithm: SafeGeo, SafeProbability, SafeChecksum, SafeTensor, SafeMl

HTTP: SafeHeader, SafeCookie, SafeContentType, SafeHttp

Container: SafeRegistry, SafeDigest

Events: SafeCallback
"""
module Proven

# ---------------------------------------------------------------------------
# Library handle
# ---------------------------------------------------------------------------

"""Path or name of the libproven shared library."""
const LIBPROVEN = "libproven"

# ---------------------------------------------------------------------------
# FFI result structs -- must match proven.h layout exactly
# ---------------------------------------------------------------------------

"""Result type for integer operations (mirrors ProvenIntResult)."""
struct IntResult
    status::Int32
    value::Int64
end

"""Result type for boolean operations (mirrors ProvenBoolResult)."""
struct BoolResult
    status::Int32
    value::Bool
end

"""Result type for string operations (mirrors ProvenStringResult).
Caller must free `value` via `proven_free_string`."""
struct StringResult
    status::Int32
    value::Ptr{UInt8}
    length::Csize_t
end

"""Result type for floating-point operations (mirrors ProvenFloatResult)."""
struct FloatResult
    status::Int32
    value::Float64
end

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

const PROVEN_OK = Int32(0)

"""Extract a Julia `String` from a `StringResult`, freeing the C allocation."""
function _extract_string(r::StringResult)::Union{String, Nothing}
    r.status != PROVEN_OK && return nothing
    r.value == C_NULL && return nothing
    s = unsafe_string(r.value, r.length)
    ccall((:proven_free_string, LIBPROVEN), Cvoid, (Ptr{UInt8},), r.value)
    s
end

"""Convert Julia string to (Ptr{UInt8}, Csize_t) for FFI calls."""
function _str_args(s::AbstractString)
    buf = Vector{UInt8}(s)
    (pointer(buf), Csize_t(length(buf))), buf  # return buf to keep it alive
end

# ===================================================================
# Lifecycle (3 functions)
# ===================================================================
module Lifecycle
    using ..Proven: LIBPROVEN, PROVEN_OK

    """Initialize the Proven runtime. Must be called before other functions."""
    function init()::Bool
        r = ccall((:proven_init, LIBPROVEN), Int32, ())
        r == PROVEN_OK
    end

    """Cleanup the Proven runtime."""
    function deinit()::Nothing
        ccall((:proven_deinit, LIBPROVEN), Cvoid, ())
        nothing
    end

    """Check if the runtime is initialized."""
    function is_initialized()::Bool
        ccall((:proven_is_initialized, LIBPROVEN), Bool, ())
    end
end

# ===================================================================
# Version Information (5 functions)
# ===================================================================
module Version
    using ..Proven: LIBPROVEN

    """Get FFI ABI version."""
    abi_version()::UInt32 = ccall((:proven_ffi_abi_version, LIBPROVEN), UInt32, ())

    """Get major version."""
    major()::UInt32 = ccall((:proven_version_major, LIBPROVEN), UInt32, ())

    """Get minor version."""
    minor()::UInt32 = ccall((:proven_version_minor, LIBPROVEN), UInt32, ())

    """Get patch version."""
    patch()::UInt32 = ccall((:proven_version_patch, LIBPROVEN), UInt32, ())

    """Get total module count."""
    module_count()::UInt32 = ccall((:proven_module_count, LIBPROVEN), UInt32, ())
end

# ===================================================================
# SafeMath - Arithmetic that cannot crash (9 functions)
# ===================================================================
module SafeMath
    using ..Proven: LIBPROVEN, PROVEN_OK, IntResult

    """Safe integer division. Returns `nothing` on error."""
    function div(a::Int64, b::Int64)::Union{Int64, Nothing}
        r = ccall((:proven_math_div, LIBPROVEN), IntResult, (Int64, Int64), a, b)
        r.status == PROVEN_OK ? r.value : nothing
    end

    """Safe modulo. Returns `nothing` on division by zero."""
    function mod(a::Int64, b::Int64)::Union{Int64, Nothing}
        r = ccall((:proven_math_mod, LIBPROVEN), IntResult, (Int64, Int64), a, b)
        r.status == PROVEN_OK ? r.value : nothing
    end

    """Checked addition. Returns `nothing` on overflow."""
    function add_checked(a::Int64, b::Int64)::Union{Int64, Nothing}
        r = ccall((:proven_math_add_checked, LIBPROVEN), IntResult, (Int64, Int64), a, b)
        r.status == PROVEN_OK ? r.value : nothing
    end

    """Checked subtraction. Returns `nothing` on underflow."""
    function sub_checked(a::Int64, b::Int64)::Union{Int64, Nothing}
        r = ccall((:proven_math_sub_checked, LIBPROVEN), IntResult, (Int64, Int64), a, b)
        r.status == PROVEN_OK ? r.value : nothing
    end

    """Checked multiplication. Returns `nothing` on overflow."""
    function mul_checked(a::Int64, b::Int64)::Union{Int64, Nothing}
        r = ccall((:proven_math_mul_checked, LIBPROVEN), IntResult, (Int64, Int64), a, b)
        r.status == PROVEN_OK ? r.value : nothing
    end

    """Safe absolute value. Returns `nothing` for INT64_MIN."""
    function abs_safe(n::Int64)::Union{Int64, Nothing}
        r = ccall((:proven_math_abs_safe, LIBPROVEN), IntResult, (Int64,), n)
        r.status == PROVEN_OK ? r.value : nothing
    end

    """Clamp value to [lo, hi]."""
    function clamp(lo::Int64, hi::Int64, value::Int64)::Int64
        ccall((:proven_math_clamp, LIBPROVEN), Int64, (Int64, Int64, Int64), lo, hi, value)
    end

    """Checked exponentiation. Returns `nothing` on overflow."""
    function pow_checked(base::Int64, exp::UInt32)::Union{Int64, Nothing}
        r = ccall((:proven_math_pow_checked, LIBPROVEN), IntResult, (Int64, UInt32), base, exp)
        r.status == PROVEN_OK ? r.value : nothing
    end
end

# ===================================================================
# SafeString - Text operations (4 functions)
# ===================================================================
module SafeString
    using ..Proven: LIBPROVEN, PROVEN_OK, BoolResult, StringResult, _extract_string

    """Check if bytes are valid UTF-8."""
    function is_valid_utf8(data::AbstractVector{UInt8})::Union{Bool, Nothing}
        r = ccall((:proven_string_is_valid_utf8, LIBPROVEN), BoolResult,
                  (Ptr{UInt8}, Csize_t), data, Csize_t(length(data)))
        r.status == PROVEN_OK ? r.value : nothing
    end

    """Escape string for SQL (single quotes). Caller should prefer parameterized queries."""
    function escape_sql(s::AbstractString)::Union{String, Nothing}
        buf = Vector{UInt8}(s)
        r = ccall((:proven_string_escape_sql, LIBPROVEN), StringResult,
                  (Ptr{UInt8}, Csize_t), buf, Csize_t(length(buf)))
        _extract_string(r)
    end

    """Escape string for HTML (prevents XSS)."""
    function escape_html(s::AbstractString)::Union{String, Nothing}
        buf = Vector{UInt8}(s)
        r = ccall((:proven_string_escape_html, LIBPROVEN), StringResult,
                  (Ptr{UInt8}, Csize_t), buf, Csize_t(length(buf)))
        _extract_string(r)
    end

    """Escape string for JavaScript string literals."""
    function escape_js(s::AbstractString)::Union{String, Nothing}
        buf = Vector{UInt8}(s)
        r = ccall((:proven_string_escape_js, LIBPROVEN), StringResult,
                  (Ptr{UInt8}, Csize_t), buf, Csize_t(length(buf)))
        _extract_string(r)
    end
end

# ===================================================================
# SafePath - Filesystem traversal prevention (2 functions)
# ===================================================================
module SafePath
    using ..Proven: LIBPROVEN, PROVEN_OK, BoolResult, StringResult, _extract_string

    """Check if path contains directory traversal (..)."""
    function has_traversal(s::AbstractString)::Union{Bool, Nothing}
        buf = Vector{UInt8}(s)
        r = ccall((:proven_path_has_traversal, LIBPROVEN), BoolResult,
                  (Ptr{UInt8}, Csize_t), buf, Csize_t(length(buf)))
        r.status == PROVEN_OK ? r.value : nothing
    end

    """Sanitize a filename by removing dangerous characters."""
    function sanitize_filename(s::AbstractString)::Union{String, Nothing}
        buf = Vector{UInt8}(s)
        r = ccall((:proven_path_sanitize_filename, LIBPROVEN), StringResult,
                  (Ptr{UInt8}, Csize_t), buf, Csize_t(length(buf)))
        _extract_string(r)
    end
end

# ===================================================================
# SafeCrypto - Cryptographic primitives (2 functions)
# ===================================================================
module SafeCrypto
    using ..Proven: LIBPROVEN, PROVEN_OK, BoolResult

    """Constant-time byte comparison (timing-attack safe)."""
    function constant_time_eq(a::AbstractVector{UInt8}, b::AbstractVector{UInt8})::Union{Bool, Nothing}
        r = ccall((:proven_crypto_constant_time_eq, LIBPROVEN), BoolResult,
                  (Ptr{UInt8}, Csize_t, Ptr{UInt8}, Csize_t),
                  a, Csize_t(length(a)), b, Csize_t(length(b)))
        r.status == PROVEN_OK ? r.value : nothing
    end

    """Fill buffer with cryptographically secure random bytes."""
    function random_bytes!(buf::Vector{UInt8})::Bool
        r = ccall((:proven_crypto_random_bytes, LIBPROVEN), Int32,
                  (Ptr{UInt8}, Csize_t), buf, Csize_t(length(buf)))
        r == PROVEN_OK
    end
end

# ===================================================================
# SafeUrl - URL parsing and validation (2 functions)
# ===================================================================
module SafeUrl
    using ..Proven: LIBPROVEN, PROVEN_OK

    # Mirror ProvenUrlComponents layout
    struct UrlComponents
        scheme::Ptr{UInt8};    scheme_len::Csize_t
        host::Ptr{UInt8};      host_len::Csize_t
        port::UInt16;          has_port::Bool
        path::Ptr{UInt8};      path_len::Csize_t
        query::Ptr{UInt8};     query_len::Csize_t
        fragment::Ptr{UInt8};  fragment_len::Csize_t
    end

    struct UrlResult
        status::Int32
        components::UrlComponents
    end

    """Parsed URL components (Julia-friendly)."""
    struct ParsedUrl
        scheme::String
        host::String
        port::Union{UInt16, Nothing}
        path::String
        query::String
        fragment::String
    end

    """Parse a URL into components. Returns `nothing` on failure."""
    function parse(s::AbstractString)::Union{ParsedUrl, Nothing}
        buf = Vector{UInt8}(s)
        r = ccall((:proven_url_parse, LIBPROVEN), UrlResult,
                  (Ptr{UInt8}, Csize_t), buf, Csize_t(length(buf)))
        r.status != PROVEN_OK && return nothing
        c = r.components
        result = ParsedUrl(
            c.scheme == C_NULL ? "" : unsafe_string(c.scheme, c.scheme_len),
            c.host == C_NULL ? "" : unsafe_string(c.host, c.host_len),
            c.has_port ? c.port : nothing,
            c.path == C_NULL ? "" : unsafe_string(c.path, c.path_len),
            c.query == C_NULL ? "" : unsafe_string(c.query, c.query_len),
            c.fragment == C_NULL ? "" : unsafe_string(c.fragment, c.fragment_len),
        )
        # Free the C-allocated components
        ref = Ref(c)
        ccall((:proven_url_free, LIBPROVEN), Cvoid, (Ptr{UrlComponents},), ref)
        result
    end
end

# ===================================================================
# SafeEmail - Email validation (1 function)
# ===================================================================
module SafeEmail
    using ..Proven: LIBPROVEN, PROVEN_OK, BoolResult

    """Validate email address (RFC 5321 simplified)."""
    function is_valid(s::AbstractString)::Union{Bool, Nothing}
        buf = Vector{UInt8}(s)
        r = ccall((:proven_email_is_valid, LIBPROVEN), BoolResult,
                  (Ptr{UInt8}, Csize_t), buf, Csize_t(length(buf)))
        r.status == PROVEN_OK ? r.value : nothing
    end
end

# ===================================================================
# SafeNetwork - IP address parsing (3 functions)
# ===================================================================
module SafeNetwork
    using ..Proven: LIBPROVEN, PROVEN_OK

    struct IPv4Address
        octets::NTuple{4, UInt8}
    end

    struct IPv4Result
        status::Int32
        address::IPv4Address
    end

    """Parse IPv4 address string. Returns `nothing` on failure."""
    function parse_ipv4(s::AbstractString)::Union{IPv4Address, Nothing}
        buf = Vector{UInt8}(s)
        r = ccall((:proven_network_parse_ipv4, LIBPROVEN), IPv4Result,
                  (Ptr{UInt8}, Csize_t), buf, Csize_t(length(buf)))
        r.status == PROVEN_OK ? r.address : nothing
    end

    """Check if IPv4 address is private (RFC 1918)."""
    function ipv4_is_private(addr::IPv4Address)::Bool
        ccall((:proven_network_ipv4_is_private, LIBPROVEN), Bool, (IPv4Address,), addr)
    end

    """Check if IPv4 address is loopback (127.0.0.0/8)."""
    function ipv4_is_loopback(addr::IPv4Address)::Bool
        ccall((:proven_network_ipv4_is_loopback, LIBPROVEN), Bool, (IPv4Address,), addr)
    end
end

# ===================================================================
# SafeHeader - HTTP header safety (6 functions)
# ===================================================================
module SafeHeader
    using ..Proven: LIBPROVEN, PROVEN_OK, BoolResult, StringResult, _extract_string

    """Check for CRLF injection in header value."""
    function has_crlf(s::AbstractString)::Union{Bool, Nothing}
        buf = Vector{UInt8}(s)
        r = ccall((:proven_header_has_crlf, LIBPROVEN), BoolResult,
                  (Ptr{UInt8}, Csize_t), buf, Csize_t(length(buf)))
        r.status == PROVEN_OK ? r.value : nothing
    end

    """Check if header name is a valid token per RFC 7230."""
    function is_valid_name(s::AbstractString)::Union{Bool, Nothing}
        buf = Vector{UInt8}(s)
        r = ccall((:proven_header_is_valid_name, LIBPROVEN), BoolResult,
                  (Ptr{UInt8}, Csize_t), buf, Csize_t(length(buf)))
        r.status == PROVEN_OK ? r.value : nothing
    end

    """Check if header name is dangerous."""
    function is_dangerous(s::AbstractString)::Union{Bool, Nothing}
        buf = Vector{UInt8}(s)
        r = ccall((:proven_header_is_dangerous, LIBPROVEN), BoolResult,
                  (Ptr{UInt8}, Csize_t), buf, Csize_t(length(buf)))
        r.status == PROVEN_OK ? r.value : nothing
    end

    """Create validated header string 'Name: Value'."""
    function render(name::AbstractString, value::AbstractString)::Union{String, Nothing}
        nbuf = Vector{UInt8}(name)
        vbuf = Vector{UInt8}(value)
        r = ccall((:proven_header_render, LIBPROVEN), StringResult,
                  (Ptr{UInt8}, Csize_t, Ptr{UInt8}, Csize_t),
                  nbuf, Csize_t(length(nbuf)), vbuf, Csize_t(length(vbuf)))
        _extract_string(r)
    end

    """Build Content-Security-Policy header value from JSON directives."""
    function build_csp(json::AbstractString)::Union{String, Nothing}
        buf = Vector{UInt8}(json)
        r = ccall((:proven_header_build_csp, LIBPROVEN), StringResult,
                  (Ptr{UInt8}, Csize_t), buf, Csize_t(length(buf)))
        _extract_string(r)
    end

    """Build HSTS header value."""
    function build_hsts(max_age::Int64, include_subdomains::Bool, preload::Bool)::Union{String, Nothing}
        r = ccall((:proven_header_build_hsts, LIBPROVEN), StringResult,
                  (Int64, Bool, Bool), max_age, include_subdomains, preload)
        _extract_string(r)
    end
end

# ===================================================================
# SafeCookie - HTTP cookie safety (6 functions)
# ===================================================================
module SafeCookie
    using ..Proven: LIBPROVEN, PROVEN_OK, BoolResult, IntResult, StringResult, _extract_string

    const SAMESITE_STRICT = Int32(0)
    const SAMESITE_LAX    = Int32(1)
    const SAMESITE_NONE   = Int32(2)

    struct CookieAttributes
        domain::Ptr{UInt8};     domain_len::Csize_t
        path::Ptr{UInt8};       path_len::Csize_t
        max_age::Int64
        secure::Bool
        http_only::Bool
        same_site::Int32
        partitioned::Bool
    end

    """Check for cookie injection characters."""
    function has_injection(s::AbstractString)::Union{Bool, Nothing}
        buf = Vector{UInt8}(s)
        r = ccall((:proven_cookie_has_injection, LIBPROVEN), BoolResult,
                  (Ptr{UInt8}, Csize_t), buf, Csize_t(length(buf)))
        r.status == PROVEN_OK ? r.value : nothing
    end

    """Validate cookie name."""
    function validate_name(s::AbstractString)::Union{Bool, Nothing}
        buf = Vector{UInt8}(s)
        r = ccall((:proven_cookie_validate_name, LIBPROVEN), BoolResult,
                  (Ptr{UInt8}, Csize_t), buf, Csize_t(length(buf)))
        r.status == PROVEN_OK ? r.value : nothing
    end

    """Validate cookie value."""
    function validate_value(s::AbstractString)::Union{Bool, Nothing}
        buf = Vector{UInt8}(s)
        r = ccall((:proven_cookie_validate_value, LIBPROVEN), BoolResult,
                  (Ptr{UInt8}, Csize_t), buf, Csize_t(length(buf)))
        r.status == PROVEN_OK ? r.value : nothing
    end

    """Get cookie prefix type: 0=none, 1=__Secure-, 2=__Host-."""
    function get_prefix(s::AbstractString)::Union{Int64, Nothing}
        buf = Vector{UInt8}(s)
        r = ccall((:proven_cookie_get_prefix, LIBPROVEN), IntResult,
                  (Ptr{UInt8}, Csize_t), buf, Csize_t(length(buf)))
        r.status == PROVEN_OK ? r.value : nothing
    end

    """Build Set-Cookie header value."""
    function build_set_cookie(name::AbstractString, value::AbstractString, attrs::CookieAttributes)::Union{String, Nothing}
        nbuf = Vector{UInt8}(name)
        vbuf = Vector{UInt8}(value)
        r = ccall((:proven_cookie_build_set_cookie, LIBPROVEN), StringResult,
                  (Ptr{UInt8}, Csize_t, Ptr{UInt8}, Csize_t, CookieAttributes),
                  nbuf, Csize_t(length(nbuf)), vbuf, Csize_t(length(vbuf)), attrs)
        _extract_string(r)
    end

    """Build delete cookie header value."""
    function build_delete(name::AbstractString)::Union{String, Nothing}
        buf = Vector{UInt8}(name)
        r = ccall((:proven_cookie_build_delete, LIBPROVEN), StringResult,
                  (Ptr{UInt8}, Csize_t), buf, Csize_t(length(buf)))
        _extract_string(r)
    end
end

# ===================================================================
# SafeContentType - Content-Type / MIME handling (6 functions)
# ===================================================================
module SafeContentType
    using ..Proven: LIBPROVEN, PROVEN_OK, BoolResult, StringResult, _extract_string

    # Mirrors ProvenContentTypeResult (opaque; we free via FFI)
    struct ContentTypeResult
        status::Int32
        media_type::Ptr{UInt8};   media_type_len::Csize_t
        subtype::Ptr{UInt8};      subtype_len::Csize_t
        suffix::Ptr{UInt8};       suffix_len::Csize_t
        category::Int32
        charset::Int32
        has_charset::Bool
    end

    """Parsed content type (Julia-friendly)."""
    struct ParsedContentType
        media_type::String
        subtype::String
        suffix::String
        category::Int32
        charset::Int32
        has_charset::Bool
    end

    """Parse Content-Type header."""
    function parse(s::AbstractString)::Union{ParsedContentType, Nothing}
        buf = Vector{UInt8}(s)
        r = ccall((:proven_content_type_parse, LIBPROVEN), ContentTypeResult,
                  (Ptr{UInt8}, Csize_t), buf, Csize_t(length(buf)))
        r.status != PROVEN_OK && return nothing
        result = ParsedContentType(
            r.media_type == C_NULL ? "" : unsafe_string(r.media_type, r.media_type_len),
            r.subtype == C_NULL ? "" : unsafe_string(r.subtype, r.subtype_len),
            r.suffix == C_NULL ? "" : unsafe_string(r.suffix, r.suffix_len),
            r.category, r.charset, r.has_charset
        )
        ref = Ref(r)
        ccall((:proven_content_type_free, LIBPROVEN), Cvoid, (Ptr{ContentTypeResult},), ref)
        result
    end

    """Check if content type can be sniffed to something dangerous."""
    function can_sniff_dangerous(s::AbstractString)::Union{Bool, Nothing}
        buf = Vector{UInt8}(s)
        r = ccall((:proven_content_type_can_sniff_dangerous, LIBPROVEN), BoolResult,
                  (Ptr{UInt8}, Csize_t), buf, Csize_t(length(buf)))
        r.status == PROVEN_OK ? r.value : nothing
    end

    """Render content type to string."""
    function render(type_str::AbstractString, subtype::AbstractString,
                    suffix::AbstractString, charset::Int32, has_charset::Bool)::Union{String, Nothing}
        tbuf = Vector{UInt8}(type_str)
        sbuf = Vector{UInt8}(subtype)
        xbuf = Vector{UInt8}(suffix)
        r = ccall((:proven_content_type_render, LIBPROVEN), StringResult,
                  (Ptr{UInt8}, Csize_t, Ptr{UInt8}, Csize_t, Ptr{UInt8}, Csize_t, Int32, Bool),
                  tbuf, Csize_t(length(tbuf)),
                  sbuf, Csize_t(length(sbuf)),
                  xbuf, Csize_t(length(xbuf)),
                  charset, has_charset)
        _extract_string(r)
    end

    """Check if content type is JSON."""
    function is_json(subtype::AbstractString, suffix::AbstractString)::Union{Bool, Nothing}
        sbuf = Vector{UInt8}(subtype)
        xbuf = Vector{UInt8}(suffix)
        r = ccall((:proven_content_type_is_json, LIBPROVEN), BoolResult,
                  (Ptr{UInt8}, Csize_t, Ptr{UInt8}, Csize_t),
                  sbuf, Csize_t(length(sbuf)), xbuf, Csize_t(length(xbuf)))
        r.status == PROVEN_OK ? r.value : nothing
    end

    """Check if content type is XML."""
    function is_xml(subtype::AbstractString, suffix::AbstractString)::Union{Bool, Nothing}
        sbuf = Vector{UInt8}(subtype)
        xbuf = Vector{UInt8}(suffix)
        r = ccall((:proven_content_type_is_xml, LIBPROVEN), BoolResult,
                  (Ptr{UInt8}, Csize_t, Ptr{UInt8}, Csize_t),
                  sbuf, Csize_t(length(sbuf)), xbuf, Csize_t(length(xbuf)))
        r.status == PROVEN_OK ? r.value : nothing
    end
end

# ===================================================================
# SafeUUID - UUID generation and validation (5 functions)
# ===================================================================
module SafeUUID
    using ..Proven: LIBPROVEN, PROVEN_OK, StringResult, _extract_string

    struct UUID
        bytes::NTuple{16, UInt8}
    end

    struct UUIDResult
        status::Int32
        uuid::UUID
    end

    """Generate a UUID v4 (random)."""
    function v4()::Union{UUID, Nothing}
        r = ccall((:proven_uuid_v4, LIBPROVEN), UUIDResult, ())
        r.status == PROVEN_OK ? r.uuid : nothing
    end

    """Format UUID as canonical string."""
    function to_string(uuid::UUID)::Union{String, Nothing}
        r = ccall((:proven_uuid_to_string, LIBPROVEN), StringResult, (UUID,), uuid)
        _extract_string(r)
    end

    """Parse UUID from string."""
    function parse(s::AbstractString)::Union{UUID, Nothing}
        buf = Vector{UInt8}(s)
        r = ccall((:proven_uuid_parse, LIBPROVEN), UUIDResult,
                  (Ptr{UInt8}, Csize_t), buf, Csize_t(length(buf)))
        r.status == PROVEN_OK ? r.uuid : nothing
    end

    """Check if UUID is nil (all zeros)."""
    function is_nil(uuid::UUID)::Bool
        ccall((:proven_uuid_is_nil, LIBPROVEN), Bool, (UUID,), uuid)
    end

    """Get UUID version number."""
    function version(uuid::UUID)::UInt8
        ccall((:proven_uuid_version, LIBPROVEN), UInt8, (UUID,), uuid)
    end
end

# ===================================================================
# SafeJson - JSON validation (2 functions)
# ===================================================================
module SafeJson
    using ..Proven: LIBPROVEN, PROVEN_OK, BoolResult

    """Check if string is valid JSON."""
    function is_valid(s::AbstractString)::Union{Bool, Nothing}
        buf = Vector{UInt8}(s)
        r = ccall((:proven_json_is_valid, LIBPROVEN), BoolResult,
                  (Ptr{UInt8}, Csize_t), buf, Csize_t(length(buf)))
        r.status == PROVEN_OK ? r.value : nothing
    end

    """Get JSON value type at root level. Returns -1 for invalid."""
    function get_type(s::AbstractString)::Int32
        buf = Vector{UInt8}(s)
        ccall((:proven_json_get_type, LIBPROVEN), Int32,
              (Ptr{UInt8}, Csize_t), buf, Csize_t(length(buf)))
    end
end

# ===================================================================
# SafeDateTime - ISO 8601 handling (4 functions)
# ===================================================================
module SafeDateTime
    using ..Proven: LIBPROVEN, PROVEN_OK, StringResult, _extract_string

    struct DateTime
        year::Int32
        month::UInt8
        day::UInt8
        hour::UInt8
        minute::UInt8
        second::UInt8
        nanosecond::UInt32
        tz_offset_minutes::Int16
    end

    struct DateTimeResult
        status::Int32
        datetime::DateTime
    end

    """Parse ISO 8601 date string."""
    function parse(s::AbstractString)::Union{DateTime, Nothing}
        buf = Vector{UInt8}(s)
        r = ccall((:proven_datetime_parse, LIBPROVEN), DateTimeResult,
                  (Ptr{UInt8}, Csize_t), buf, Csize_t(length(buf)))
        r.status == PROVEN_OK ? r.datetime : nothing
    end

    """Format DateTime as ISO 8601 string."""
    function format_iso8601(dt::DateTime)::Union{String, Nothing}
        r = ccall((:proven_datetime_format_iso8601, LIBPROVEN), StringResult, (DateTime,), dt)
        _extract_string(r)
    end

    """Check if year is a leap year."""
    function is_leap_year(year::Int32)::Bool
        ccall((:proven_datetime_is_leap_year, LIBPROVEN), Bool, (Int32,), year)
    end

    """Get number of days in a month. Returns 0 if invalid."""
    function days_in_month(year::Int32, month::UInt8)::UInt8
        ccall((:proven_datetime_days_in_month, LIBPROVEN), UInt8, (Int32, UInt8), year, month)
    end
end

# ===================================================================
# SafeFloat - Safe floating-point operations (5 functions)
# ===================================================================
module SafeFloat
    using ..Proven: LIBPROVEN, PROVEN_OK, FloatResult

    """Safe floating-point division."""
    function div(a::Float64, b::Float64)::Union{Float64, Nothing}
        r = ccall((:proven_float_div, LIBPROVEN), FloatResult, (Float64, Float64), a, b)
        r.status == PROVEN_OK ? r.value : nothing
    end

    """Check if float is finite (not NaN or Inf)."""
    function is_finite(x::Float64)::Bool
        ccall((:proven_float_is_finite, LIBPROVEN), Bool, (Float64,), x)
    end

    """Check if float is NaN."""
    function is_nan(x::Float64)::Bool
        ccall((:proven_float_is_nan, LIBPROVEN), Bool, (Float64,), x)
    end

    """Safe square root."""
    function sqrt(x::Float64)::Union{Float64, Nothing}
        r = ccall((:proven_float_sqrt, LIBPROVEN), FloatResult, (Float64,), x)
        r.status == PROVEN_OK ? r.value : nothing
    end

    """Safe natural logarithm."""
    function ln(x::Float64)::Union{Float64, Nothing}
        r = ccall((:proven_float_ln, LIBPROVEN), FloatResult, (Float64,), x)
        r.status == PROVEN_OK ? r.value : nothing
    end
end

# ===================================================================
# SafePassword - Password validation (2 functions)
# ===================================================================
module SafePassword
    using ..Proven: LIBPROVEN

    struct PasswordResult
        strength::Int32
        has_lowercase::Bool
        has_uppercase::Bool
        has_digit::Bool
        has_special::Bool
        length::Csize_t
    end

    """Validate password strength."""
    function validate(s::AbstractString)::PasswordResult
        buf = Vector{UInt8}(s)
        ccall((:proven_password_validate, LIBPROVEN), PasswordResult,
              (Ptr{UInt8}, Csize_t), buf, Csize_t(length(buf)))
    end

    """Check if password is in common passwords list."""
    function is_common(s::AbstractString)::Bool
        buf = Vector{UInt8}(s)
        ccall((:proven_password_is_common, LIBPROVEN), Bool,
              (Ptr{UInt8}, Csize_t), buf, Csize_t(length(buf)))
    end
end

# ===================================================================
# SafeHex - Hexadecimal encoding/decoding (3 functions)
# ===================================================================
module SafeHex
    using ..Proven: LIBPROVEN, PROVEN_OK, StringResult, _extract_string

    struct HexDecodeResult
        status::Int32
        data::Ptr{UInt8}
        length::Csize_t
    end

    """Encode bytes to hex string."""
    function encode(data::AbstractVector{UInt8}; uppercase::Bool=false)::Union{String, Nothing}
        r = ccall((:proven_hex_encode, LIBPROVEN), StringResult,
                  (Ptr{UInt8}, Csize_t, Bool), data, Csize_t(length(data)), uppercase)
        _extract_string(r)
    end

    """Decode hex string to bytes."""
    function decode(s::AbstractString)::Union{Vector{UInt8}, Nothing}
        buf = Vector{UInt8}(s)
        r = ccall((:proven_hex_decode, LIBPROVEN), HexDecodeResult,
                  (Ptr{UInt8}, Csize_t), buf, Csize_t(length(buf)))
        r.status != PROVEN_OK && return nothing
        r.data == C_NULL && return nothing
        result = copy(unsafe_wrap(Array, r.data, r.length))
        ref = Ref(r)
        ccall((:proven_hex_free, LIBPROVEN), Cvoid, (Ptr{HexDecodeResult},), ref)
        result
    end
end

# ===================================================================
# SafeCurrency - Monetary values (2 functions)
# ===================================================================
module SafeCurrency
    using ..Proven: LIBPROVEN, PROVEN_OK, StringResult, _extract_string

    struct CurrencyResult
        status::Int32
        amount_minor::Int64
        currency_code::NTuple{3, UInt8}
        decimal_places::UInt8
    end

    """Parse currency amount (e.g. 'USD 123.45')."""
    function parse(s::AbstractString)::Union{CurrencyResult, Nothing}
        buf = Vector{UInt8}(s)
        r = ccall((:proven_currency_parse, LIBPROVEN), CurrencyResult,
                  (Ptr{UInt8}, Csize_t), buf, Csize_t(length(buf)))
        r.status == PROVEN_OK ? r : nothing
    end

    """Format currency amount."""
    function format(amount_minor::Int64, code::NTuple{3, UInt8}, decimal_places::UInt8)::Union{String, Nothing}
        r = ccall((:proven_currency_format, LIBPROVEN), StringResult,
                  (Int64, NTuple{3, UInt8}, UInt8), amount_minor, code, decimal_places)
        _extract_string(r)
    end
end

# ===================================================================
# SafePhone - E.164 phone number handling (2 functions)
# ===================================================================
module SafePhone
    using ..Proven: LIBPROVEN, PROVEN_OK, StringResult, _extract_string

    struct PhoneResult
        status::Int32
        country_code::UInt16
        national_number::UInt64
        is_valid::Bool
    end

    """Parse phone number to E.164 format."""
    function parse(s::AbstractString)::Union{PhoneResult, Nothing}
        buf = Vector{UInt8}(s)
        r = ccall((:proven_phone_parse, LIBPROVEN), PhoneResult,
                  (Ptr{UInt8}, Csize_t), buf, Csize_t(length(buf)))
        r.status == PROVEN_OK ? r : nothing
    end

    """Format phone number as E.164 string."""
    function format_e164(country_code::UInt16, national_number::UInt64)::Union{String, Nothing}
        r = ccall((:proven_phone_format_e164, LIBPROVEN), StringResult,
                  (UInt16, UInt64), country_code, national_number)
        _extract_string(r)
    end
end

# ===================================================================
# SafeVersion - Semantic versioning (3 functions)
# ===================================================================
module SafeVersion
    using ..Proven: LIBPROVEN, PROVEN_OK

    struct SemanticVersion
        major::UInt32
        minor::UInt32
        patch::UInt32
        prerelease_len::Csize_t
        prerelease::Ptr{UInt8}
    end

    struct VersionResult
        status::Int32
        version::SemanticVersion
    end

    """Parsed version (Julia-friendly, owns its strings)."""
    struct ParsedVersion
        major::UInt32
        minor::UInt32
        patch::UInt32
        prerelease::Union{String, Nothing}
    end

    """Parse semantic version string."""
    function parse(s::AbstractString)::Union{ParsedVersion, Nothing}
        buf = Vector{UInt8}(s)
        r = ccall((:proven_version_parse, LIBPROVEN), VersionResult,
                  (Ptr{UInt8}, Csize_t), buf, Csize_t(length(buf)))
        r.status != PROVEN_OK && return nothing
        v = r.version
        pre = v.prerelease == C_NULL ? nothing : unsafe_string(v.prerelease, v.prerelease_len)
        result = ParsedVersion(v.major, v.minor, v.patch, pre)
        ref = Ref(v)
        ccall((:proven_version_free, LIBPROVEN), Cvoid, (Ptr{SemanticVersion},), ref)
        result
    end

    """Compare two semantic versions. Returns <0, 0, or >0."""
    function compare(a::SemanticVersion, b::SemanticVersion)::Int32
        ccall((:proven_version_compare, LIBPROVEN), Int32,
              (SemanticVersion, SemanticVersion), a, b)
    end
end

# ===================================================================
# SafeGeo - Geographic coordinates (3 functions)
# ===================================================================
module SafeGeo
    using ..Proven: LIBPROVEN, PROVEN_OK, FloatResult

    struct GeoCoordinate
        latitude::Float64
        longitude::Float64
    end

    struct GeoResult
        status::Int32
        coordinate::GeoCoordinate
    end

    """Validate and normalize geographic coordinate."""
    function validate(lat::Float64, lon::Float64)::Union{GeoCoordinate, Nothing}
        r = ccall((:proven_geo_validate, LIBPROVEN), GeoResult, (Float64, Float64), lat, lon)
        r.status == PROVEN_OK ? r.coordinate : nothing
    end

    """Calculate distance between two points (Haversine formula) in meters."""
    function distance(a::GeoCoordinate, b::GeoCoordinate)::Union{Float64, Nothing}
        r = ccall((:proven_geo_distance, LIBPROVEN), FloatResult,
                  (GeoCoordinate, GeoCoordinate), a, b)
        r.status == PROVEN_OK ? r.value : nothing
    end

    """Check if coordinate is inside a bounding box."""
    function in_bounds(coord::GeoCoordinate, min_lat::Float64, max_lat::Float64,
                       min_lon::Float64, max_lon::Float64)::Bool
        ccall((:proven_geo_in_bounds, LIBPROVEN), Bool,
              (GeoCoordinate, Float64, Float64, Float64, Float64),
              coord, min_lat, max_lat, min_lon, max_lon)
    end
end

# ===================================================================
# SafeChecksum - CRC and hash verification (2 functions)
# ===================================================================
module SafeChecksum
    using ..Proven: LIBPROVEN, PROVEN_OK, IntResult, BoolResult

    """Calculate CRC32 checksum."""
    function crc32(data::AbstractVector{UInt8})::Union{Int64, Nothing}
        r = ccall((:proven_checksum_crc32, LIBPROVEN), IntResult,
                  (Ptr{UInt8}, Csize_t), data, Csize_t(length(data)))
        r.status == PROVEN_OK ? r.value : nothing
    end

    """Verify CRC32 matches expected value."""
    function verify_crc32(data::AbstractVector{UInt8}, expected::UInt32)::Union{Bool, Nothing}
        r = ccall((:proven_checksum_verify_crc32, LIBPROVEN), BoolResult,
                  (Ptr{UInt8}, Csize_t, UInt32), data, Csize_t(length(data)), expected)
        r.status == PROVEN_OK ? r.value : nothing
    end
end

# ===================================================================
# SafeProbability - Probability values (4 functions)
# ===================================================================
module SafeProbability
    using ..Proven: LIBPROVEN

    """Create probability value (clamped to [0, 1])."""
    create(value::Float64)::Float64 =
        ccall((:proven_probability_create, LIBPROVEN), Float64, (Float64,), value)

    """Multiply probabilities (independent events)."""
    and(a::Float64, b::Float64)::Float64 =
        ccall((:proven_probability_and, LIBPROVEN), Float64, (Float64, Float64), a, b)

    """Add probabilities (mutually exclusive events, clamped to 1.0)."""
    or_exclusive(a::Float64, b::Float64)::Float64 =
        ccall((:proven_probability_or_exclusive, LIBPROVEN), Float64, (Float64, Float64), a, b)

    """Complement probability: 1 - P(A)."""
    not(p::Float64)::Float64 =
        ccall((:proven_probability_not, LIBPROVEN), Float64, (Float64,), p)
end

# ===================================================================
# SafeCalculator - Expression evaluation (1 function)
# ===================================================================
module SafeCalculator
    using ..Proven: LIBPROVEN, PROVEN_OK, FloatResult

    """Evaluate an arithmetic expression safely."""
    function eval_expr(s::AbstractString)::Union{Float64, Nothing}
        buf = Vector{UInt8}(s)
        r = ccall((:proven_calculator_eval, LIBPROVEN), FloatResult,
                  (Ptr{UInt8}, Csize_t), buf, Csize_t(length(buf)))
        r.status == PROVEN_OK ? r.value : nothing
    end
end

# ===================================================================
# SafeBuffer - Bounded buffer operations (4 functions)
# ===================================================================
module SafeBuffer
    using ..Proven: LIBPROVEN, PROVEN_OK

    struct BufferResult
        status::Int32
        buffer::Ptr{Cvoid}
    end

    """Create a bounded buffer with given capacity."""
    function create(capacity::Csize_t)::Union{Ptr{Cvoid}, Nothing}
        r = ccall((:proven_buffer_create, LIBPROVEN), BufferResult, (Csize_t,), capacity)
        r.status == PROVEN_OK ? r.buffer : nothing
    end

    """Append data to buffer. Returns true on success."""
    function append!(buffer::Ptr{Cvoid}, data::AbstractVector{UInt8})::Bool
        r = ccall((:proven_buffer_append, LIBPROVEN), Int32,
                  (Ptr{Cvoid}, Ptr{UInt8}, Csize_t), buffer, data, Csize_t(length(data)))
        r == PROVEN_OK
    end

    """Get buffer contents as a Vector{UInt8}."""
    function get(buffer::Ptr{Cvoid})::Union{Vector{UInt8}, Nothing}
        out_ptr = Ref(Ptr{UInt8}(C_NULL))
        out_len = Ref(Csize_t(0))
        r = ccall((:proven_buffer_get, LIBPROVEN), Int32,
                  (Ptr{Cvoid}, Ptr{Ptr{UInt8}}, Ptr{Csize_t}), buffer, out_ptr, out_len)
        r != PROVEN_OK && return nothing
        out_ptr[] == C_NULL && return UInt8[]
        copy(unsafe_wrap(Array, out_ptr[], out_len[]))
    end

    """Free buffer."""
    function free(buffer::Ptr{Cvoid})::Nothing
        ccall((:proven_buffer_free, LIBPROVEN), Cvoid, (Ptr{Cvoid},), buffer)
        nothing
    end
end

# ===================================================================
# SafeRateLimiter - Token bucket (3 functions)
# ===================================================================
module SafeRateLimiter
    using ..Proven: LIBPROVEN

    """Create a rate limiter. Returns opaque handle or nothing."""
    function create(capacity::Float64, refill_rate::Float64)::Union{Ptr{Cvoid}, Nothing}
        p = ccall((:proven_rate_limiter_create, LIBPROVEN), Ptr{Cvoid},
                  (Float64, Float64), capacity, refill_rate)
        p == C_NULL ? nothing : p
    end

    """Try to acquire tokens. Returns true if tokens acquired."""
    function try_acquire(limiter::Ptr{Cvoid}, tokens::Float64)::Bool
        ccall((:proven_rate_limiter_try_acquire, LIBPROVEN), Bool,
              (Ptr{Cvoid}, Float64), limiter, tokens)
    end

    """Free rate limiter."""
    function free(limiter::Ptr{Cvoid})::Nothing
        ccall((:proven_rate_limiter_free, LIBPROVEN), Cvoid, (Ptr{Cvoid},), limiter)
        nothing
    end
end

# ===================================================================
# SafeCircuitBreaker - Fault tolerance (6 functions)
# ===================================================================
module SafeCircuitBreaker
    using ..Proven: LIBPROVEN

    const STATE_CLOSED    = Int32(0)
    const STATE_OPEN      = Int32(1)
    const STATE_HALF_OPEN = Int32(2)

    """Create a circuit breaker."""
    function create(failure_threshold::UInt32, success_threshold::UInt32, timeout_ms::Int64)::Union{Ptr{Cvoid}, Nothing}
        p = ccall((:proven_circuit_breaker_create, LIBPROVEN), Ptr{Cvoid},
                  (UInt32, UInt32, Int64), failure_threshold, success_threshold, timeout_ms)
        p == C_NULL ? nothing : p
    end

    """Check if request should be allowed."""
    function allow(cb::Ptr{Cvoid})::Bool
        ccall((:proven_circuit_breaker_allow, LIBPROVEN), Bool, (Ptr{Cvoid},), cb)
    end

    """Record a successful operation."""
    function success(cb::Ptr{Cvoid})::Nothing
        ccall((:proven_circuit_breaker_success, LIBPROVEN), Cvoid, (Ptr{Cvoid},), cb)
        nothing
    end

    """Record a failed operation."""
    function failure(cb::Ptr{Cvoid})::Nothing
        ccall((:proven_circuit_breaker_failure, LIBPROVEN), Cvoid, (Ptr{Cvoid},), cb)
        nothing
    end

    """Get current circuit state."""
    function state(cb::Ptr{Cvoid})::Int32
        ccall((:proven_circuit_breaker_state, LIBPROVEN), Int32, (Ptr{Cvoid},), cb)
    end

    """Free circuit breaker."""
    function free(cb::Ptr{Cvoid})::Nothing
        ccall((:proven_circuit_breaker_free, LIBPROVEN), Cvoid, (Ptr{Cvoid},), cb)
        nothing
    end
end

# ===================================================================
# SafeRetry - Exponential backoff (2 functions)
# ===================================================================
module SafeRetry
    using ..Proven: LIBPROVEN

    struct RetryConfig
        max_attempts::UInt32
        base_delay_ms::UInt64
        max_delay_ms::UInt64
        multiplier::Float64
    end

    """Calculate delay for a given attempt (with jitter)."""
    function delay(config::RetryConfig, attempt::UInt32)::UInt64
        ccall((:proven_retry_delay, LIBPROVEN), UInt64,
              (RetryConfig, UInt32), config, attempt)
    end

    """Check if retry should be attempted."""
    function should_retry(config::RetryConfig, attempt::UInt32)::Bool
        ccall((:proven_retry_should_retry, LIBPROVEN), Bool,
              (RetryConfig, UInt32), config, attempt)
    end
end

# ===================================================================
# SafeMonotonic - Monotonically increasing sequences (3 functions)
# ===================================================================
module SafeMonotonic
    using ..Proven: LIBPROVEN, PROVEN_OK, IntResult

    """Create a monotonic counter. Returns opaque handle or nothing."""
    function create(initial::UInt64, max_value::UInt64)::Union{Ptr{Cvoid}, Nothing}
        p = ccall((:proven_monotonic_create, LIBPROVEN), Ptr{Cvoid},
                  (UInt64, UInt64), initial, max_value)
        p == C_NULL ? nothing : p
    end

    """Get next value and increment. Returns nothing on overflow."""
    function next(counter::Ptr{Cvoid})::Union{Int64, Nothing}
        r = ccall((:proven_monotonic_next, LIBPROVEN), IntResult, (Ptr{Cvoid},), counter)
        r.status == PROVEN_OK ? r.value : nothing
    end

    """Free monotonic counter."""
    function free(counter::Ptr{Cvoid})::Nothing
        ccall((:proven_monotonic_free, LIBPROVEN), Cvoid, (Ptr{Cvoid},), counter)
        nothing
    end
end

# ===================================================================
# SafeStateMachine - Type-safe state transitions (5 functions)
# ===================================================================
module SafeStateMachine
    using ..Proven: LIBPROVEN

    """Create a state machine. Returns opaque handle or nothing."""
    function create(state_count::UInt32, initial_state::UInt32)::Union{Ptr{Cvoid}, Nothing}
        p = ccall((:proven_state_machine_create, LIBPROVEN), Ptr{Cvoid},
                  (UInt32, UInt32), state_count, initial_state)
        p == C_NULL ? nothing : p
    end

    """Allow a state transition."""
    function allow(sm::Ptr{Cvoid}, from::UInt32, to::UInt32)::Bool
        ccall((:proven_state_machine_allow, LIBPROVEN), Bool,
              (Ptr{Cvoid}, UInt32, UInt32), sm, from, to)
    end

    """Attempt to transition to a new state."""
    function transition(sm::Ptr{Cvoid}, to::UInt32)::Bool
        ccall((:proven_state_machine_transition, LIBPROVEN), Bool,
              (Ptr{Cvoid}, UInt32), sm, to)
    end

    """Get current state index."""
    function state(sm::Ptr{Cvoid})::UInt32
        ccall((:proven_state_machine_state, LIBPROVEN), UInt32, (Ptr{Cvoid},), sm)
    end

    """Free state machine."""
    function free(sm::Ptr{Cvoid})::Nothing
        ccall((:proven_state_machine_free, LIBPROVEN), Cvoid, (Ptr{Cvoid},), sm)
        nothing
    end
end

# ===================================================================
# SafeTensor - Basic 2D tensor operations (5 functions)
# ===================================================================
module SafeTensor
    using ..Proven: LIBPROVEN, PROVEN_OK, FloatResult

    """Create a 2D tensor initialized to zero. Returns opaque handle."""
    function create(rows::Csize_t, cols::Csize_t)::Union{Ptr{Cvoid}, Nothing}
        p = ccall((:proven_tensor_create, LIBPROVEN), Ptr{Cvoid}, (Csize_t, Csize_t), rows, cols)
        p == C_NULL ? nothing : p
    end

    """Set tensor value at (row, col). Returns true on success."""
    function set!(tensor::Ptr{Cvoid}, row::Csize_t, col::Csize_t, value::Float64)::Bool
        r = ccall((:proven_tensor_set, LIBPROVEN), Int32,
                  (Ptr{Cvoid}, Csize_t, Csize_t, Float64), tensor, row, col, value)
        r == PROVEN_OK
    end

    """Get tensor value at (row, col)."""
    function get(tensor::Ptr{Cvoid}, row::Csize_t, col::Csize_t)::Union{Float64, Nothing}
        r = ccall((:proven_tensor_get, LIBPROVEN), FloatResult,
                  (Ptr{Cvoid}, Csize_t, Csize_t), tensor, row, col)
        r.status == PROVEN_OK ? r.value : nothing
    end

    """Matrix multiplication (a * b). Returns new tensor handle or nothing."""
    function matmul(a::Ptr{Cvoid}, b::Ptr{Cvoid})::Union{Ptr{Cvoid}, Nothing}
        p = ccall((:proven_tensor_matmul, LIBPROVEN), Ptr{Cvoid},
                  (Ptr{Cvoid}, Ptr{Cvoid}), a, b)
        p == C_NULL ? nothing : p
    end

    """Free tensor."""
    function free(tensor::Ptr{Cvoid})::Nothing
        ccall((:proven_tensor_free, LIBPROVEN), Cvoid, (Ptr{Cvoid},), tensor)
        nothing
    end
end

# ===================================================================
# SafeMl - Machine learning activation functions (5 functions)
# ===================================================================
module SafeMl
    using ..Proven: LIBPROVEN, PROVEN_OK

    """Softmax normalization over an array. Writes output in-place."""
    function softmax!(input::Vector{Float64}, output::Vector{Float64})::Bool
        @assert length(input) == length(output)
        r = ccall((:proven_ml_softmax, LIBPROVEN), Int32,
                  (Ptr{Float64}, Ptr{Float64}, Csize_t), input, output, Csize_t(length(input)))
        r == PROVEN_OK
    end

    """Sigmoid function: 1 / (1 + exp(-x))."""
    sigmoid(x::Float64)::Float64 =
        ccall((:proven_ml_sigmoid, LIBPROVEN), Float64, (Float64,), x)

    """ReLU function: max(0, x)."""
    relu(x::Float64)::Float64 =
        ccall((:proven_ml_relu, LIBPROVEN), Float64, (Float64,), x)

    """Leaky ReLU: x >= 0 ? x : alpha * x."""
    leaky_relu(x::Float64, alpha::Float64)::Float64 =
        ccall((:proven_ml_leaky_relu, LIBPROVEN), Float64, (Float64, Float64), x, alpha)

    """Clamp value to [min_val, max_val]."""
    clamp(x::Float64, min_val::Float64, max_val::Float64)::Float64 =
        ccall((:proven_ml_clamp, LIBPROVEN), Float64, (Float64, Float64, Float64), x, min_val, max_val)
end

# ===================================================================
# SafeLRU - LRU cache (4 functions)
# ===================================================================
module SafeLRU
    using ..Proven: LIBPROVEN, PROVEN_OK, IntResult

    """Create an LRU cache. Returns opaque handle or nothing."""
    function create(capacity::Csize_t)::Union{Ptr{Cvoid}, Nothing}
        p = ccall((:proven_lru_create, LIBPROVEN), Ptr{Cvoid}, (Csize_t,), capacity)
        p == C_NULL ? nothing : p
    end

    """Get value from cache (promotes to MRU)."""
    function get(cache::Ptr{Cvoid}, key::UInt64)::Union{Int64, Nothing}
        r = ccall((:proven_lru_get, LIBPROVEN), IntResult, (Ptr{Cvoid}, UInt64), cache, key)
        r.status == PROVEN_OK ? r.value : nothing
    end

    """Put value in cache. Evicts LRU entry if full. Returns true on success."""
    function put!(cache::Ptr{Cvoid}, key::UInt64, value::Int64)::Bool
        r = ccall((:proven_lru_put, LIBPROVEN), Int32,
                  (Ptr{Cvoid}, UInt64, Int64), cache, key, value)
        r == PROVEN_OK
    end

    """Free LRU cache."""
    function free(cache::Ptr{Cvoid})::Nothing
        ccall((:proven_lru_free, LIBPROVEN), Cvoid, (Ptr{Cvoid},), cache)
        nothing
    end
end

# ===================================================================
# SafeGraph - Directed graph (4 functions)
# ===================================================================
module SafeGraph
    using ..Proven: LIBPROVEN, PROVEN_OK

    """Create a directed graph. Returns opaque handle or nothing."""
    function create(node_count::Csize_t)::Union{Ptr{Cvoid}, Nothing}
        p = ccall((:proven_graph_create, LIBPROVEN), Ptr{Cvoid}, (Csize_t,), node_count)
        p == C_NULL ? nothing : p
    end

    """Add directed edge from -> to. Returns true on success."""
    function add_edge!(graph::Ptr{Cvoid}, from::Csize_t, to::Csize_t)::Bool
        r = ccall((:proven_graph_add_edge, LIBPROVEN), Int32,
                  (Ptr{Cvoid}, Csize_t, Csize_t), graph, from, to)
        r == PROVEN_OK
    end

    """Check if directed edge exists."""
    function has_edge(graph::Ptr{Cvoid}, from::Csize_t, to::Csize_t)::Bool
        ccall((:proven_graph_has_edge, LIBPROVEN), Bool,
              (Ptr{Cvoid}, Csize_t, Csize_t), graph, from, to)
    end

    """Free graph."""
    function free(graph::Ptr{Cvoid})::Nothing
        ccall((:proven_graph_free, LIBPROVEN), Cvoid, (Ptr{Cvoid},), graph)
        nothing
    end
end

# ===================================================================
# SafeQueue - Bounded FIFO queue (5 functions)
# ===================================================================
module SafeQueue
    using ..Proven: LIBPROVEN, PROVEN_OK, IntResult

    """Create a bounded queue. Returns opaque handle or nothing."""
    function create(capacity::Csize_t)::Union{Ptr{Cvoid}, Nothing}
        p = ccall((:proven_queue_create, LIBPROVEN), Ptr{Cvoid}, (Csize_t,), capacity)
        p == C_NULL ? nothing : p
    end

    """Push value to queue. Returns true on success."""
    function push!(queue::Ptr{Cvoid}, value::Int64)::Bool
        ccall((:proven_queue_push, LIBPROVEN), Bool, (Ptr{Cvoid}, Int64), queue, value)
    end

    """Pop value from queue (FIFO). Returns nothing if empty."""
    function pop!(queue::Ptr{Cvoid})::Union{Int64, Nothing}
        r = ccall((:proven_queue_pop, LIBPROVEN), IntResult, (Ptr{Cvoid},), queue)
        r.status == PROVEN_OK ? r.value : nothing
    end

    """Get queue element count."""
    function size(queue::Ptr{Cvoid})::Csize_t
        ccall((:proven_queue_size, LIBPROVEN), Csize_t, (Ptr{Cvoid},), queue)
    end

    """Free queue."""
    function free(queue::Ptr{Cvoid})::Nothing
        ccall((:proven_queue_free, LIBPROVEN), Cvoid, (Ptr{Cvoid},), queue)
        nothing
    end
end

# ===================================================================
# SafeBloom - Bloom filter (4 functions)
# ===================================================================
module SafeBloom
    using ..Proven: LIBPROVEN

    """Create a Bloom filter. Returns opaque handle or nothing."""
    function create(expected_elements::Csize_t, false_positive_rate::Float64)::Union{Ptr{Cvoid}, Nothing}
        p = ccall((:proven_bloom_create, LIBPROVEN), Ptr{Cvoid},
                  (Csize_t, Float64), expected_elements, false_positive_rate)
        p == C_NULL ? nothing : p
    end

    """Add element to Bloom filter."""
    function add!(filter::Ptr{Cvoid}, data::AbstractVector{UInt8})::Nothing
        ccall((:proven_bloom_add, LIBPROVEN), Cvoid,
              (Ptr{Cvoid}, Ptr{UInt8}, Csize_t), filter, data, Csize_t(length(data)))
        nothing
    end

    """Check if element might be in filter."""
    function contains(filter::Ptr{Cvoid}, data::AbstractVector{UInt8})::Bool
        ccall((:proven_bloom_contains, LIBPROVEN), Bool,
              (Ptr{Cvoid}, Ptr{UInt8}, Csize_t), filter, data, Csize_t(length(data)))
    end

    """Free Bloom filter."""
    function free(filter::Ptr{Cvoid})::Nothing
        ccall((:proven_bloom_free, LIBPROVEN), Cvoid, (Ptr{Cvoid},), filter)
        nothing
    end
end

# ===================================================================
# SafeColor - Color space conversions (3 functions)
# ===================================================================
module SafeColor
    using ..Proven: LIBPROVEN, PROVEN_OK, StringResult, _extract_string

    struct RGBColor
        r::UInt8
        g::UInt8
        b::UInt8
    end

    struct HSLColor
        h::Float64
        s::Float64
        l::Float64
    end

    struct ColorResult
        status::Int32
        color::RGBColor
    end

    """Parse hex color string (#RRGGBB or #RGB)."""
    function parse_hex(s::AbstractString)::Union{RGBColor, Nothing}
        buf = Vector{UInt8}(s)
        r = ccall((:proven_color_parse_hex, LIBPROVEN), ColorResult,
                  (Ptr{UInt8}, Csize_t), buf, Csize_t(length(buf)))
        r.status == PROVEN_OK ? r.color : nothing
    end

    """Convert RGB to HSL."""
    function rgb_to_hsl(rgb::RGBColor)::HSLColor
        ccall((:proven_color_rgb_to_hsl, LIBPROVEN), HSLColor, (RGBColor,), rgb)
    end

    """Format RGB as hex string."""
    function to_hex(rgb::RGBColor)::Union{String, Nothing}
        r = ccall((:proven_color_to_hex, LIBPROVEN), StringResult, (RGBColor,), rgb)
        _extract_string(r)
    end
end

# ===================================================================
# SafeAngle - Angle conversions (4 functions)
# ===================================================================
module SafeAngle
    using ..Proven: LIBPROVEN

    """Convert degrees to radians."""
    deg_to_rad(deg::Float64)::Float64 =
        ccall((:proven_angle_deg_to_rad, LIBPROVEN), Float64, (Float64,), deg)

    """Convert radians to degrees."""
    rad_to_deg(rad::Float64)::Float64 =
        ccall((:proven_angle_rad_to_deg, LIBPROVEN), Float64, (Float64,), rad)

    """Normalize angle to [0, 360) degrees."""
    normalize_degrees(deg::Float64)::Float64 =
        ccall((:proven_angle_normalize_degrees, LIBPROVEN), Float64, (Float64,), deg)

    """Normalize angle to [0, 2*pi) radians."""
    normalize_radians(rad::Float64)::Float64 =
        ccall((:proven_angle_normalize_radians, LIBPROVEN), Float64, (Float64,), rad)
end

# ===================================================================
# SafeUnit - Physical unit conversions (2 functions)
# ===================================================================
module SafeUnit
    using ..Proven: LIBPROVEN, PROVEN_OK, FloatResult

    # Length unit enum values matching ProvenLengthUnit
    const LENGTH_METERS      = Int32(0)
    const LENGTH_KILOMETERS  = Int32(1)
    const LENGTH_CENTIMETERS = Int32(2)
    const LENGTH_MILLIMETERS = Int32(3)
    const LENGTH_FEET        = Int32(4)
    const LENGTH_INCHES      = Int32(5)
    const LENGTH_MILES       = Int32(6)
    const LENGTH_YARDS       = Int32(7)

    # Temperature unit enum values matching ProvenTempUnit
    const TEMP_CELSIUS    = Int32(0)
    const TEMP_FAHRENHEIT = Int32(1)
    const TEMP_KELVIN     = Int32(2)

    """Convert length between units."""
    function convert_length(value::Float64, from::Int32, to::Int32)::Union{Float64, Nothing}
        r = ccall((:proven_unit_convert_length, LIBPROVEN), FloatResult,
                  (Float64, Int32, Int32), value, from, to)
        r.status == PROVEN_OK ? r.value : nothing
    end

    """Convert temperature between units."""
    function convert_temp(value::Float64, from::Int32, to::Int32)::Union{Float64, Nothing}
        r = ccall((:proven_unit_convert_temp, LIBPROVEN), FloatResult,
                  (Float64, Int32, Int32), value, from, to)
        r.status == PROVEN_OK ? r.value : nothing
    end
end

# ===================================================================
# SafeHttp - HTTP URL encoding (3 functions)
# ===================================================================
module SafeHttp
    using ..Proven: LIBPROVEN, PROVEN_OK, StringResult, _extract_string

    """URL-encode a string (RFC 3986 percent encoding)."""
    function url_encode(s::AbstractString)::Union{String, Nothing}
        buf = Vector{UInt8}(s)
        r = ccall((:proven_http_url_encode, LIBPROVEN), StringResult,
                  (Ptr{UInt8}, Csize_t), buf, Csize_t(length(buf)))
        _extract_string(r)
    end

    """URL-decode a percent-encoded string."""
    function url_decode(s::AbstractString)::Union{String, Nothing}
        buf = Vector{UInt8}(s)
        r = ccall((:proven_http_url_decode, LIBPROVEN), StringResult,
                  (Ptr{UInt8}, Csize_t), buf, Csize_t(length(buf)))
        _extract_string(r)
    end

    # parse_www_authenticate omitted for brevity -- complex struct
end

# ===================================================================
# SafeRegistry - OCI image reference parsing (3 functions)
# ===================================================================
module SafeRegistry
    using ..Proven: LIBPROVEN, PROVEN_OK, BoolResult, StringResult, _extract_string

    # parse and to_string involve complex nested structs; forward-declare
    # as thin wrappers when the struct layout is finalized.
end

# ===================================================================
# SafeDigest - Cryptographic digest operations (3 functions)
# ===================================================================
module SafeDigest
    using ..Proven: LIBPROVEN, PROVEN_OK, BoolResult, StringResult, _extract_string

    # Digest operations involve nested pointer structs; forward-declare.
end

# ===================================================================
# SafeCallback - Bidirectional FFI event system (6 functions)
# ===================================================================
module SafeCallback
    using ..Proven: LIBPROVEN, PROVEN_OK

    """Register a callback. Returns non-zero handle on success."""
    function register(event_type::Int32, callback::Ptr{Cvoid}, context::Ptr{Cvoid})::UInt32
        ccall((:proven_callback_register, LIBPROVEN), UInt32,
              (Int32, Ptr{Cvoid}, Ptr{Cvoid}), event_type, callback, context)
    end

    """Unregister a callback by handle. Returns true on success."""
    function unregister(handle::UInt32)::Bool
        r = ccall((:proven_callback_unregister, LIBPROVEN), Int32, (UInt32,), handle)
        r == PROVEN_OK
    end

    """Fire an event, invoking all registered callbacks."""
    function fire(event_type::Int32, data::Union{AbstractString, Nothing}, code::Int32)::Int32
        if data === nothing
            ccall((:proven_callback_fire, LIBPROVEN), Int32,
                  (Int32, Ptr{UInt8}, Csize_t, Int32), event_type, C_NULL, Csize_t(0), code)
        else
            buf = Vector{UInt8}(data)
            ccall((:proven_callback_fire, LIBPROVEN), Int32,
                  (Int32, Ptr{UInt8}, Csize_t, Int32), event_type, buf, Csize_t(length(buf)), code)
        end
    end

    """Query how many callbacks are registered for an event type."""
    function count(event_type::Int32)::UInt32
        ccall((:proven_callback_count, LIBPROVEN), UInt32, (Int32,), event_type)
    end

    """Unregister all callbacks. Returns number removed."""
    function clear_all()::UInt32
        ccall((:proven_callback_clear_all, LIBPROVEN), UInt32, ())
    end
end

# ===================================================================
# Exports
# ===================================================================

# Submodules
export Lifecycle, Version
export SafeMath, SafeString, SafePath, SafeEmail, SafeUrl, SafeNetwork, SafeCrypto
export SafeUUID, SafeJson, SafeDateTime, SafeFloat, SafeVersion, SafeColor
export SafeAngle, SafeUnit, SafeHex, SafeCurrency, SafePhone, SafePassword
export SafeBuffer, SafeQueue, SafeBloom, SafeLRU, SafeGraph
export SafeRateLimiter, SafeCircuitBreaker, SafeRetry, SafeMonotonic
export SafeStateMachine, SafeCalculator
export SafeGeo, SafeProbability, SafeChecksum, SafeTensor, SafeMl
export SafeHeader, SafeCookie, SafeContentType, SafeHttp
export SafeRegistry, SafeDigest, SafeCallback

# Result types (for advanced users)
export IntResult, BoolResult, StringResult, FloatResult

end # module Proven
