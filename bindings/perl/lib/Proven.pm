# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
#
# Proven - FFI binding to libproven (formally verified safety library).
# All computation is performed in Idris 2 via the Zig FFI layer.
# This module is a thin wrapper; it does NOT reimplement any logic.

package Proven;
use strict;
use warnings;
use FFI::Platypus 2.00;
use FFI::Platypus::Buffer qw(scalar_to_buffer buffer_to_scalar);
use Exporter 'import';

our $VERSION = '0.9.0';

# ============================================================================
# FFI setup
# ============================================================================

my $ffi = FFI::Platypus->new(api => 2, lib => [undef, 'libproven.so']);

# Status code: 0 = ok, negative = error
$ffi->type('sint32' => 'ProvenStatus');

# Define record types via custom types for result structs.
# We use closures to unpack the return values.

# IntResult: { status: i32, value: i64 }   = 4 + padding(4) + 8 = 16 bytes
$ffi->custom_type('IntResult' => {
    native_type    => 'opaque',
    native_to_perl => sub {
        my $ptr = shift;
        return undef unless defined $ptr;
        my ($status, $value) = unpack('l< x4 q<', $$ptr) if ref $ptr;
        return { status => $status, value => $value };
    },
});

# ============================================================================
# Runtime lifecycle
# ============================================================================

$ffi->attach(proven_init          => []         => 'sint32');
$ffi->attach(proven_deinit        => []         => 'void');
$ffi->attach(proven_is_initialized => []        => 'bool');
$ffi->attach(proven_ffi_abi_version => []       => 'uint32');
$ffi->attach(proven_version_major => []         => 'uint32');
$ffi->attach(proven_version_minor => []         => 'uint32');
$ffi->attach(proven_version_patch => []         => 'uint32');
$ffi->attach(proven_module_count  => []         => 'uint32');
$ffi->attach(proven_free_string   => ['opaque'] => 'void');

# ============================================================================
# Internal helpers for calling FFI functions that return packed result structs
# ============================================================================

# For functions returning IntResult (16 bytes: i32 status + i64 value)
my $_math_div         = $ffi->function(proven_math_div         => ['sint64','sint64'] => 'record(16)');
my $_math_mod         = $ffi->function(proven_math_mod         => ['sint64','sint64'] => 'record(16)');
my $_math_add_checked = $ffi->function(proven_math_add_checked => ['sint64','sint64'] => 'record(16)');
my $_math_sub_checked = $ffi->function(proven_math_sub_checked => ['sint64','sint64'] => 'record(16)');
my $_math_mul_checked = $ffi->function(proven_math_mul_checked => ['sint64','sint64'] => 'record(16)');
my $_math_abs_safe    = $ffi->function(proven_math_abs_safe    => ['sint64']          => 'record(16)');
my $_math_pow_checked = $ffi->function(proven_math_pow_checked => ['sint64','uint32'] => 'record(16)');

$ffi->attach(proven_math_clamp => ['sint64','sint64','sint64'] => 'sint64');

# BoolResult (8 bytes: i32 status + bool value + padding)
my $_string_is_valid_utf8 = $ffi->function(proven_string_is_valid_utf8 => ['opaque','size_t'] => 'record(8)');
my $_email_is_valid       = $ffi->function(proven_email_is_valid       => ['opaque','size_t'] => 'record(8)');
my $_path_has_traversal   = $ffi->function(proven_path_has_traversal   => ['opaque','size_t'] => 'record(8)');
my $_json_is_valid        = $ffi->function(proven_json_is_valid        => ['opaque','size_t'] => 'record(8)');
my $_header_has_crlf      = $ffi->function(proven_header_has_crlf      => ['opaque','size_t'] => 'record(8)');
my $_header_is_valid_name = $ffi->function(proven_header_is_valid_name => ['opaque','size_t'] => 'record(8)');
my $_header_is_dangerous  = $ffi->function(proven_header_is_dangerous  => ['opaque','size_t'] => 'record(8)');
my $_cookie_has_injection = $ffi->function(proven_cookie_has_injection => ['opaque','size_t'] => 'record(8)');
my $_cookie_validate_name = $ffi->function(proven_cookie_validate_name => ['opaque','size_t'] => 'record(8)');
my $_cookie_validate_value = $ffi->function(proven_cookie_validate_value => ['opaque','size_t'] => 'record(8)');
my $_content_type_can_sniff = $ffi->function(proven_content_type_can_sniff_dangerous => ['opaque','size_t'] => 'record(8)');
my $_crypto_constant_time_eq = $ffi->function(proven_crypto_constant_time_eq => ['opaque','size_t','opaque','size_t'] => 'record(8)');

# StringResult (24 bytes on 64-bit: i32 status + padding(4) + ptr(8) + size_t(8))
my $_string_escape_sql  = $ffi->function(proven_string_escape_sql  => ['opaque','size_t'] => 'record(24)');
my $_string_escape_html = $ffi->function(proven_string_escape_html => ['opaque','size_t'] => 'record(24)');
my $_string_escape_js   = $ffi->function(proven_string_escape_js   => ['opaque','size_t'] => 'record(24)');
my $_path_sanitize      = $ffi->function(proven_path_sanitize_filename => ['opaque','size_t'] => 'record(24)');
my $_hex_encode         = $ffi->function(proven_hex_encode => ['opaque','size_t','bool'] => 'record(24)');
my $_http_url_encode    = $ffi->function(proven_http_url_encode => ['opaque','size_t'] => 'record(24)');
my $_http_url_decode    = $ffi->function(proven_http_url_decode => ['opaque','size_t'] => 'record(24)');
my $_header_render      = $ffi->function(proven_header_render => ['opaque','size_t','opaque','size_t'] => 'record(24)');
my $_header_build_csp   = $ffi->function(proven_header_build_csp => ['opaque','size_t'] => 'record(24)');
my $_header_build_hsts  = $ffi->function(proven_header_build_hsts => ['sint64','bool','bool'] => 'record(24)');
my $_cookie_build_delete = $ffi->function(proven_cookie_build_delete => ['opaque','size_t'] => 'record(24)');
my $_color_to_hex       = $ffi->function(proven_color_to_hex => ['uint8','uint8','uint8'] => 'record(24)');

# FloatResult (16 bytes: i32 status + padding(4) + f64 value)
my $_float_div  = $ffi->function(proven_float_div  => ['double','double'] => 'record(16)');
my $_float_sqrt = $ffi->function(proven_float_sqrt => ['double']          => 'record(16)');
my $_float_ln   = $ffi->function(proven_float_ln   => ['double']          => 'record(16)');
my $_calc_eval  = $ffi->function(proven_calculator_eval => ['opaque','size_t'] => 'record(16)');

# Simple returns
$ffi->attach(proven_float_is_finite => ['double'] => 'bool');
$ffi->attach(proven_float_is_nan    => ['double'] => 'bool');
$ffi->attach(proven_json_get_type   => ['opaque','size_t'] => 'sint32');
$ffi->attach(proven_datetime_is_leap_year  => ['sint32'] => 'bool');
$ffi->attach(proven_datetime_days_in_month => ['sint32','uint8'] => 'uint8');
$ffi->attach(proven_angle_deg_to_rad         => ['double'] => 'double');
$ffi->attach(proven_angle_rad_to_deg         => ['double'] => 'double');
$ffi->attach(proven_angle_normalize_degrees  => ['double'] => 'double');
$ffi->attach(proven_angle_normalize_radians  => ['double'] => 'double');
$ffi->attach(proven_probability_create       => ['double'] => 'double');
$ffi->attach(proven_probability_and          => ['double','double'] => 'double');
$ffi->attach(proven_probability_or_exclusive => ['double','double'] => 'double');
$ffi->attach(proven_probability_not          => ['double'] => 'double');
$ffi->attach(proven_ml_sigmoid    => ['double'] => 'double');
$ffi->attach(proven_ml_relu       => ['double'] => 'double');
$ffi->attach(proven_ml_leaky_relu => ['double','double'] => 'double');
$ffi->attach(proven_ml_clamp      => ['double','double','double'] => 'double');
$ffi->attach(proven_password_is_common => ['opaque','size_t'] => 'bool');
$ffi->attach(proven_crypto_random_bytes => ['opaque','size_t'] => 'sint32');

# ============================================================================
# Unpacking helpers
# ============================================================================

sub _unpack_int_result {
    my $rec = shift;
    my ($status, $value) = unpack('l< x4 q<', $rec);
    return undef if $status != 0;
    return $value;
}

sub _unpack_bool_result {
    my $rec = shift;
    my ($status, $value) = unpack('l< C', $rec);
    return undef if $status != 0;
    return $value ? 1 : 0;
}

sub _unpack_string_result {
    my $rec = shift;
    my ($status, $ptr_val, $length) = unpack('l< x4 Q< Q<', $rec);
    return undef if $status != 0 || $ptr_val == 0;
    my $str = buffer_to_scalar($ptr_val, $length);
    proven_free_string($ptr_val);
    return $str;
}

sub _unpack_float_result {
    my $rec = shift;
    my ($status, $value) = unpack('l< x4 d<', $rec);
    return undef if $status != 0;
    return $value;
}

sub _str_to_buf {
    my $s = shift;
    return scalar_to_buffer($s);
}

# ============================================================================
# Public API: SafeMath
# ============================================================================

sub safe_div {
    my ($a, $b) = @_;
    return _unpack_int_result($_math_div->call($a, $b));
}

sub safe_mod {
    my ($a, $b) = @_;
    return _unpack_int_result($_math_mod->call($a, $b));
}

sub add_checked {
    my ($a, $b) = @_;
    return _unpack_int_result($_math_add_checked->call($a, $b));
}

sub sub_checked {
    my ($a, $b) = @_;
    return _unpack_int_result($_math_sub_checked->call($a, $b));
}

sub mul_checked {
    my ($a, $b) = @_;
    return _unpack_int_result($_math_mul_checked->call($a, $b));
}

sub abs_safe {
    my ($n) = @_;
    return _unpack_int_result($_math_abs_safe->call($n));
}

sub math_clamp {
    my ($lo, $hi, $value) = @_;
    return proven_math_clamp($lo, $hi, $value);
}

sub pow_checked {
    my ($base, $exp) = @_;
    return _unpack_int_result($_math_pow_checked->call($base, $exp));
}

# ============================================================================
# Public API: SafeString
# ============================================================================

sub is_valid_utf8 {
    my ($s) = @_;
    my ($buf, $len) = _str_to_buf($s);
    return _unpack_bool_result($_string_is_valid_utf8->call($buf, $len));
}

sub escape_sql {
    my ($s) = @_;
    my ($buf, $len) = _str_to_buf($s);
    return _unpack_string_result($_string_escape_sql->call($buf, $len));
}

sub escape_html {
    my ($s) = @_;
    my ($buf, $len) = _str_to_buf($s);
    return _unpack_string_result($_string_escape_html->call($buf, $len));
}

sub escape_js {
    my ($s) = @_;
    my ($buf, $len) = _str_to_buf($s);
    return _unpack_string_result($_string_escape_js->call($buf, $len));
}

# ============================================================================
# Public API: SafePath
# ============================================================================

sub has_traversal {
    my ($s) = @_;
    my ($buf, $len) = _str_to_buf($s);
    return _unpack_bool_result($_path_has_traversal->call($buf, $len));
}

sub sanitize_filename {
    my ($s) = @_;
    my ($buf, $len) = _str_to_buf($s);
    return _unpack_string_result($_path_sanitize->call($buf, $len));
}

# ============================================================================
# Public API: SafeEmail
# ============================================================================

sub email_is_valid {
    my ($s) = @_;
    my ($buf, $len) = _str_to_buf($s);
    return _unpack_bool_result($_email_is_valid->call($buf, $len));
}

# ============================================================================
# Public API: SafeCrypto
# ============================================================================

sub constant_time_eq {
    my ($a, $b) = @_;
    my ($buf_a, $len_a) = _str_to_buf($a);
    my ($buf_b, $len_b) = _str_to_buf($b);
    return _unpack_bool_result($_crypto_constant_time_eq->call($buf_a, $len_a, $buf_b, $len_b));
}

sub random_bytes {
    my ($n) = @_;
    my $buf = "\0" x $n;
    my ($ptr, $len) = _str_to_buf($buf);
    my $status = proven_crypto_random_bytes($ptr, $n);
    return undef if $status != 0;
    return buffer_to_scalar($ptr, $n);
}

# ============================================================================
# Public API: SafeJson
# ============================================================================

sub json_is_valid {
    my ($s) = @_;
    my ($buf, $len) = _str_to_buf($s);
    return _unpack_bool_result($_json_is_valid->call($buf, $len));
}

sub json_get_type {
    my ($s) = @_;
    my ($buf, $len) = _str_to_buf($s);
    my $t = proven_json_get_type($buf, $len);
    my %type_names = (
        0  => 'null',    1  => 'boolean', 2  => 'number',
        3  => 'string',  4  => 'array',   5  => 'object',
        -1 => 'invalid',
    );
    return $type_names{$t} // 'invalid';
}

# ============================================================================
# Public API: SafeHeader
# ============================================================================

sub header_has_crlf {
    my ($s) = @_;
    my ($buf, $len) = _str_to_buf($s);
    return _unpack_bool_result($_header_has_crlf->call($buf, $len));
}

sub header_is_valid_name {
    my ($s) = @_;
    my ($buf, $len) = _str_to_buf($s);
    return _unpack_bool_result($_header_is_valid_name->call($buf, $len));
}

sub header_is_dangerous {
    my ($s) = @_;
    my ($buf, $len) = _str_to_buf($s);
    return _unpack_bool_result($_header_is_dangerous->call($buf, $len));
}

sub header_render {
    my ($name, $value) = @_;
    my ($nb, $nl) = _str_to_buf($name);
    my ($vb, $vl) = _str_to_buf($value);
    return _unpack_string_result($_header_render->call($nb, $nl, $vb, $vl));
}

sub header_build_csp {
    my ($json) = @_;
    my ($buf, $len) = _str_to_buf($json);
    return _unpack_string_result($_header_build_csp->call($buf, $len));
}

sub header_build_hsts {
    my ($max_age, $include_subdomains, $preload) = @_;
    return _unpack_string_result($_header_build_hsts->call(
        $max_age, $include_subdomains ? 1 : 0, $preload ? 1 : 0));
}

# ============================================================================
# Public API: SafeCookie
# ============================================================================

sub cookie_has_injection {
    my ($s) = @_;
    my ($buf, $len) = _str_to_buf($s);
    return _unpack_bool_result($_cookie_has_injection->call($buf, $len));
}

sub cookie_validate_name {
    my ($s) = @_;
    my ($buf, $len) = _str_to_buf($s);
    return _unpack_bool_result($_cookie_validate_name->call($buf, $len));
}

sub cookie_validate_value {
    my ($s) = @_;
    my ($buf, $len) = _str_to_buf($s);
    return _unpack_bool_result($_cookie_validate_value->call($buf, $len));
}

# ============================================================================
# Public API: SafeFloat
# ============================================================================

sub float_div {
    my ($a, $b) = @_;
    return _unpack_float_result($_float_div->call($a, $b));
}

sub float_sqrt {
    my ($x) = @_;
    return _unpack_float_result($_float_sqrt->call($x));
}

sub float_ln {
    my ($x) = @_;
    return _unpack_float_result($_float_ln->call($x));
}

# ============================================================================
# Public API: SafeHex
# ============================================================================

sub hex_encode {
    my ($data, $uppercase) = @_;
    my ($buf, $len) = _str_to_buf($data);
    return _unpack_string_result($_hex_encode->call($buf, $len, $uppercase ? 1 : 0));
}

# ============================================================================
# Public API: SafeHTTP
# ============================================================================

sub http_url_encode {
    my ($s) = @_;
    my ($buf, $len) = _str_to_buf($s);
    return _unpack_string_result($_http_url_encode->call($buf, $len));
}

sub http_url_decode {
    my ($s) = @_;
    my ($buf, $len) = _str_to_buf($s);
    return _unpack_string_result($_http_url_decode->call($buf, $len));
}

# ============================================================================
# Public API: SafeContentType
# ============================================================================

sub content_type_can_sniff_dangerous {
    my ($s) = @_;
    my ($buf, $len) = _str_to_buf($s);
    return _unpack_bool_result($_content_type_can_sniff->call($buf, $len));
}

# ============================================================================
# Public API: SafeCalculator
# ============================================================================

sub calculator_eval {
    my ($expr) = @_;
    my ($buf, $len) = _str_to_buf($expr);
    return _unpack_float_result($_calc_eval->call($buf, $len));
}

# ============================================================================
# Public API: SafeAngle
# ============================================================================

sub angle_deg_to_rad         { return proven_angle_deg_to_rad($_[0]); }
sub angle_rad_to_deg         { return proven_angle_rad_to_deg($_[0]); }
sub angle_normalize_degrees  { return proven_angle_normalize_degrees($_[0]); }
sub angle_normalize_radians  { return proven_angle_normalize_radians($_[0]); }

# ============================================================================
# Public API: SafeProbability
# ============================================================================

sub probability_create       { return proven_probability_create($_[0]); }
sub probability_and          { return proven_probability_and($_[0], $_[1]); }
sub probability_or_exclusive { return proven_probability_or_exclusive($_[0], $_[1]); }
sub probability_not          { return proven_probability_not($_[0]); }

# ============================================================================
# Public API: SafeML
# ============================================================================

sub ml_sigmoid    { return proven_ml_sigmoid($_[0]); }
sub ml_relu       { return proven_ml_relu($_[0]); }
sub ml_leaky_relu { return proven_ml_leaky_relu($_[0], $_[1]); }
sub ml_clamp      { return proven_ml_clamp($_[0], $_[1], $_[2]); }

# ============================================================================
# Public API: SafeDateTime
# ============================================================================

sub datetime_is_leap_year  { return proven_datetime_is_leap_year($_[0]); }
sub datetime_days_in_month { return proven_datetime_days_in_month($_[0], $_[1]); }

# ============================================================================
# Public API: SafePassword
# ============================================================================

sub password_is_common {
    my ($s) = @_;
    my ($buf, $len) = _str_to_buf($s);
    return proven_password_is_common($buf, $len);
}

# ============================================================================
# Public API: SafeColor
# ============================================================================

sub color_to_hex {
    my ($r, $g, $b) = @_;
    return _unpack_string_result($_color_to_hex->call($r, $g, $b));
}

# ============================================================================
# Exports
# ============================================================================

our @EXPORT_OK = qw(
    proven_init proven_deinit proven_is_initialized
    proven_ffi_abi_version proven_version_major proven_version_minor
    proven_version_patch proven_module_count

    safe_div safe_mod add_checked sub_checked mul_checked
    abs_safe math_clamp pow_checked

    is_valid_utf8 escape_sql escape_html escape_js

    has_traversal sanitize_filename

    email_is_valid

    constant_time_eq random_bytes

    json_is_valid json_get_type

    header_has_crlf header_is_valid_name header_is_dangerous
    header_render header_build_csp header_build_hsts

    cookie_has_injection cookie_validate_name cookie_validate_value

    float_div float_sqrt float_ln
    proven_float_is_finite proven_float_is_nan

    hex_encode

    http_url_encode http_url_decode

    content_type_can_sniff_dangerous

    calculator_eval

    angle_deg_to_rad angle_rad_to_deg
    angle_normalize_degrees angle_normalize_radians

    probability_create probability_and probability_or_exclusive probability_not

    ml_sigmoid ml_relu ml_leaky_relu ml_clamp

    datetime_is_leap_year datetime_days_in_month

    password_is_common

    color_to_hex
);

our %EXPORT_TAGS = (all => \@EXPORT_OK);

1;

__END__

=head1 NAME

Proven - FFI binding to libproven (formally verified safety library)

=head1 SYNOPSIS

    use Proven qw(safe_div add_checked escape_html email_is_valid);

    my $result = safe_div(10, 0);       # undef (division by zero)
    my $sum    = add_checked($a, $b);   # undef on overflow
    my $safe   = escape_html('<script>alert(1)</script>');
    my $valid  = email_is_valid('user@example.com');

=head1 DESCRIPTION

Proven is a Perl binding to libproven, a formally verified safety library.
All computation is performed in Idris 2 (with dependent types and totality
checking) via a Zig FFI layer. This module is a thin FFI wrapper using
FFI::Platypus; it does NOT reimplement any logic.

All operations return C<undef> on error instead of dying.

=head1 AUTHOR

Jonathan D.A. Jewell <jonathan.jewell@open.ac.uk>

=head1 LICENSE

PMPL-1.0-or-later

=cut
