# SPDX-License-Identifier: PMPL-1.0-or-later
# SPDX-FileCopyrightText: 2025 Hyperpolymath

package Proven;
use strict;
use warnings;

our $VERSION = '0.3.0';

=head1 NAME

Proven - Code that cannot crash

=head1 SYNOPSIS

    use Proven;
    use Proven::SafeMath qw(div add_checked);
    use Proven::SafeString qw(escape_html escape_sql);
    use Proven::SafePath qw(has_traversal safe_join);
    use Proven::SafeEmail qw(is_valid normalize);
    use Proven::SafeNetwork qw(is_private is_loopback);
    use Proven::SafeCrypto qw(constant_time_compare random_bytes);
    use Proven::SafeUUID qw(parse format new_uuid);
    use Proven::SafeCurrency qw(money format_amount);
    use Proven::SafePhone qw(parse format normalize);
    use Proven::SafeHex qw(encode decode constant_time_equal);

=head1 DESCRIPTION

Proven is a verified safety library providing:

=over 4

=item * B<SafeMath> - Arithmetic without overflow/underflow/division-by-zero

=item * B<SafeString> - UTF-8 and escaping without exceptions

=item * B<SafePath> - Filesystem ops without traversal attacks

=item * B<SafeEmail> - Email validation without regex catastrophic backtracking

=item * B<SafeNetwork> - IP parsing and classification

=item * B<SafeCrypto> - Cryptographic primitives done right

=item * B<SafeUUID> - UUID parsing, validation, and generation

=item * B<SafeCurrency> - Currency codes and monetary arithmetic without float errors

=item * B<SafePhone> - Phone number parsing and formatting

=item * B<SafeHex> - Hexadecimal encoding with constant-time comparison

=back

=head1 MODULES

=head2 Proven::SafeMath

Safe arithmetic operations. All operations return undef on failure
instead of dying or producing incorrect results.

    use Proven::SafeMath qw(div add_checked mul_checked);

    my $result = div(10, 0);        # undef, not an error
    my $sum = add_checked($a, $b);  # undef on overflow

=head2 Proven::SafeString

String escaping and validation for security.

    use Proven::SafeString qw(escape_html escape_sql escape_js);

    my $html = escape_html('<script>alert(1)</script>');
    my $sql = escape_sql("O'Brien");  # Prefer parameterized queries!

=head2 Proven::SafePath

Path operations with traversal attack prevention.

    use Proven::SafePath qw(has_traversal safe_join sanitize_filename);

    if (has_traversal($user_input)) {
        die "Path traversal attempt detected";
    }

=head2 Proven::SafeEmail

Email validation and parsing.

    use Proven::SafeEmail qw(is_valid get_domain normalize);

    if (is_valid($email)) {
        my $domain = get_domain($email);
    }

=head2 Proven::SafeNetwork

IP address parsing and classification.

    use Proven::SafeNetwork qw(is_private is_loopback is_public);

    if (is_private($ip)) {
        # Internal network access
    }

=head2 Proven::SafeCrypto

Cryptographic operations with timing attack prevention.

    use Proven::SafeCrypto qw(constant_time_compare random_bytes);

    if (constant_time_compare($token, $expected)) {
        # Valid
    }

=head2 Proven::SafeUUID

UUID parsing, validation, and generation.

    use Proven::SafeUUID qw(parse format new_uuid is_valid);

    my $uuid = new_uuid();                    # Random v4 UUID
    my $parsed = parse('550e8400-e29b-41d4-a716-446655440000');
    print format($parsed);                    # Lowercase hyphenated

=head2 Proven::SafeCurrency

Currency handling without floating-point errors.
All amounts stored as integers in minor units (cents).

    use Proven::SafeCurrency qw(money format_amount add);

    my $price = money(1999, 'USD');           # $19.99
    my $total = add($price, $tax);
    print format_amount($total);              # "21.99"

=head2 Proven::SafePhone

Phone number parsing with international support.

    use Proven::SafePhone qw(parse format normalize);

    my $phone = parse('+1 (555) 123-4567', 'US');
    print format($phone, 'e164');             # +15551234567
    print format($phone, 'national');         # (555) 123-4567

=head2 Proven::SafeHex

Hexadecimal encoding with security features.

    use Proven::SafeHex qw(encode decode constant_time_equal);

    my $hex = encode($bytes);                 # To hex string
    my $data = decode($hex);                  # From hex string

    # Constant-time comparison prevents timing attacks
    if (constant_time_equal($token_hex, $expected_hex)) {
        # Valid token
    }

=head1 PHILOSOPHY

The crappier the language's default safety, the more it needs Proven.

Perl has many legacy footguns around string interpolation, taint mode,
and implicit type conversions. Proven provides safe alternatives.

=head1 AUTHOR

Hyperpolymath

=head1 LICENSE

PMPL-1.0

=cut

1;
