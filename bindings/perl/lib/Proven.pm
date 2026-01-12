# SPDX-License-Identifier: PMPL-1.0
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

=head1 DESCRIPTION

Proven is a verified safety library providing:

=over 4

=item * B<SafeMath> - Arithmetic without overflow/underflow/division-by-zero

=item * B<SafeString> - UTF-8 and escaping without exceptions

=item * B<SafePath> - Filesystem ops without traversal attacks

=item * B<SafeEmail> - Email validation without regex catastrophic backtracking

=item * B<SafeNetwork> - IP parsing and classification

=item * B<SafeCrypto> - Cryptographic primitives done right

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
