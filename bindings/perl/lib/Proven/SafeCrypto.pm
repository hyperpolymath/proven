# SPDX-License-Identifier: PMPL-1.0
# SPDX-FileCopyrightText: 2025 Hyperpolymath

package Proven::SafeCrypto;
use strict;
use warnings;
use Exporter 'import';

our $VERSION = '0.4.0';
our @EXPORT_OK = qw(
    constant_time_compare random_bytes random_int
    random_hex random_base64 secure_zero hmac verify_hmac
);

=head1 NAME

Proven::SafeCrypto - Cryptographic safety operations with constant-time guarantees

=head1 SYNOPSIS

    use Proven::SafeCrypto qw(constant_time_compare random_bytes);

    if (constant_time_compare($token, $expected)) {
        # Valid token
    }

    my $key = random_bytes(32);

=cut

=head2 constant_time_compare($a, $b)

Compare two strings in constant time to prevent timing attacks.

=cut

sub constant_time_compare {
    my ($a, $b) = @_;

    return 0 unless defined $a && defined $b;
    return 0 if length($a) != length($b);
    return 1 if length($a) == 0;

    my $result = 0;
    for my $i (0 .. length($a) - 1) {
        $result |= ord(substr($a, $i, 1)) ^ ord(substr($b, $i, 1));
    }

    return $result == 0 ? 1 : 0;
}

=head2 random_bytes($length)

Generate cryptographically secure random bytes.

=cut

sub random_bytes {
    my ($length) = @_;

    die "Length must be non-negative" if $length < 0;
    return '' if $length == 0;

    # Try Crypt::Random first
    eval {
        require Crypt::Random;
        return Crypt::Random::makerandom_octet(Length => $length);
    };

    # Fall back to /dev/urandom on Unix
    if (-r '/dev/urandom') {
        open my $fh, '<', '/dev/urandom' or die "Cannot open /dev/urandom: $!";
        binmode $fh;
        my $bytes;
        read($fh, $bytes, $length) == $length or die "Short read from /dev/urandom";
        close $fh;
        return $bytes;
    }

    die "No secure random source available";
}

=head2 random_int($min, $max)

Generate a cryptographically secure random integer.

=cut

sub random_int {
    my ($min, $max) = @_;

    die "Invalid range" if $min > $max;

    my $range = $max - $min + 1;
    my $bytes_needed = 8;  # 64-bit integer

    my $random = random_bytes($bytes_needed);
    my $value = 0;
    for my $i (0 .. $bytes_needed - 1) {
        $value = ($value << 8) | ord(substr($random, $i, 1));
    }

    return $min + ($value % $range);
}

=head2 random_hex($byte_length)

Generate a secure random hex string.
Output will be 2x the byte_length.

=cut

sub random_hex {
    my ($byte_length) = @_;
    return unpack('H*', random_bytes($byte_length));
}

=head2 random_base64($byte_length)

Generate a secure random base64 string.

=cut

sub random_base64 {
    my ($byte_length) = @_;

    require MIME::Base64;
    return MIME::Base64::encode_base64(random_bytes($byte_length), '');
}

=head2 secure_zero($length)

Create a string of null bytes (best-effort memory zeroing).

=cut

sub secure_zero {
    my ($length) = @_;
    return "\0" x $length;
}

=head2 hmac($data, $key, $algorithm)

Generate a HMAC. Default algorithm is SHA-256.

=cut

sub hmac {
    my ($data, $key, $algorithm) = @_;
    $algorithm //= 'SHA256';

    require Digest::HMAC;
    require Digest::SHA;

    my $hmac = Digest::HMAC->new($key, "Digest::$algorithm");
    $hmac->add($data);
    return $hmac->digest;
}

=head2 verify_hmac($data, $key, $expected_hmac, $algorithm)

Verify a HMAC using constant-time comparison.

=cut

sub verify_hmac {
    my ($data, $key, $expected_hmac, $algorithm) = @_;

    my $actual_hmac = hmac($data, $key, $algorithm);
    return constant_time_compare($actual_hmac, $expected_hmac);
}

1;

__END__

=head1 AUTHOR

Hyperpolymath

=head1 LICENSE

PMPL-1.0

=cut
