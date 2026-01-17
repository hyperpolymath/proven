# SPDX-License-Identifier: PMPL-1.0
# SPDX-FileCopyrightText: 2025 Hyperpolymath

package Proven::SafeString;
use strict;
use warnings;
use Encode qw(decode encode is_utf8);
use Exporter 'import';

our $VERSION = '0.4.0';
our @EXPORT_OK = qw(
    is_valid_utf8 escape_html escape_sql escape_js
    escape_url truncate_safe safe_decode strip_html
);

=head1 NAME

Proven::SafeString - Safe string operations for escaping and sanitization

=head1 SYNOPSIS

    use Proven::SafeString qw(escape_html escape_sql);

    my $safe = escape_html('<script>alert(1)</script>');
    my $sql_safe = escape_sql("O'Brien");

=head1 DESCRIPTION

Provides safe UTF-8 validation and escaping for SQL, HTML, and JavaScript
without exceptions or security vulnerabilities.

=cut

=head2 is_valid_utf8($data)

Check if bytes are valid UTF-8.

=cut

sub is_valid_utf8 {
    my ($data) = @_;
    return 0 unless defined $data;

    eval {
        decode('UTF-8', $data, Encode::FB_CROAK);
    };
    return $@ ? 0 : 1;
}

=head2 escape_html($value)

Escape a string for safe HTML insertion.
Prevents XSS by escaping < > & " ' characters.

=cut

sub escape_html {
    my ($value) = @_;
    return '' unless defined $value;

    $value =~ s/&/&amp;/g;
    $value =~ s/</&lt;/g;
    $value =~ s/>/&gt;/g;
    $value =~ s/"/&quot;/g;
    $value =~ s/'/&#x27;/g;

    return $value;
}

=head2 escape_sql($value)

Escape a string for safe SQL interpolation.
Note: Prefer parameterized queries over string interpolation.

=cut

sub escape_sql {
    my ($value) = @_;
    return '' unless defined $value;

    $value =~ s/'/''/g;
    return $value;
}

=head2 escape_js($value)

Escape a string for safe JavaScript string literal insertion.
Prevents XSS in JavaScript contexts.

=cut

sub escape_js {
    my ($value) = @_;
    return '' unless defined $value;

    $value =~ s/\\/\\\\/g;
    $value =~ s/"/\\"/g;
    $value =~ s/'/\\'/g;
    $value =~ s/\n/\\n/g;
    $value =~ s/\r/\\r/g;
    $value =~ s/\t/\\t/g;
    $value =~ s{</}{<\\/}g;  # Prevent </script> injection

    return $value;
}

=head2 escape_url($value)

Percent-encode a string for safe URL inclusion.

=cut

sub escape_url {
    my ($value) = @_;
    return '' unless defined $value;

    # Encode all characters except unreserved ones
    $value =~ s/([^A-Za-z0-9\-_.~])/sprintf("%%%02X", ord($1))/ge;
    return $value;
}

=head2 truncate_safe($value, $max_length, $suffix)

Safely truncate a string to a maximum length.
Default suffix is "...".

=cut

sub truncate_safe {
    my ($value, $max_length, $suffix) = @_;
    $suffix //= '...';

    return '' if $max_length < 0;
    return '' unless defined $value;

    my $length = length($value);
    return $value if $length <= $max_length;

    my $suffix_len = length($suffix);
    if ($max_length <= $suffix_len) {
        return substr($value, 0, $max_length);
    }

    return substr($value, 0, $max_length - $suffix_len) . $suffix;
}

=head2 safe_decode($data, $fallback)

Decode bytes to string, replacing invalid UTF-8 sequences.
Default fallback is the Unicode replacement character.

=cut

sub safe_decode {
    my ($data, $fallback) = @_;
    $fallback //= "\x{FFFD}";

    return '' unless defined $data;

    if (is_valid_utf8($data)) {
        return decode('UTF-8', $data);
    }

    # Replace invalid sequences
    return decode('UTF-8', $data, Encode::FB_DEFAULT);
}

=head2 strip_html($value)

Strip all HTML tags from a string safely.

=cut

sub strip_html {
    my ($value) = @_;
    return '' unless defined $value;

    $value =~ s/<[^>]*>//g;
    return $value;
}

1;

__END__

=head1 AUTHOR

Hyperpolymath

=head1 LICENSE

PMPL-1.0

=cut
