# SPDX-License-Identifier: PMPL-1.0
# SPDX-FileCopyrightText: 2025 Hyperpolymath

package Proven::SafeHex;
use strict;
use warnings;
use Exporter 'import';

our $VERSION = '0.4.0';
our @EXPORT_OK = qw(
    encode decode is_valid constant_time_equal
    encode_upper encode_lower bytes_to_hex hex_to_bytes
    xor_bytes pad_hex
);

=head1 NAME

Proven::SafeHex - Safe hexadecimal encoding and decoding operations

=head1 SYNOPSIS

    use Proven::SafeHex qw(encode decode constant_time_equal);

    my $hex = encode("Hello");           # "48656c6c6f"
    my $bytes = decode("48656c6c6f");    # "Hello"

    # Constant-time comparison for security
    if (constant_time_equal($token_hex, $expected_hex)) {
        # Valid token
    }

=head1 DESCRIPTION

Provides safe hexadecimal encoding/decoding operations without exceptions.
Includes constant-time comparison for preventing timing attacks.

=cut

=head2 encode($bytes)

Encode bytes to lowercase hexadecimal string.
Returns empty string for undef input.

=cut

sub encode {
    my ($bytes) = @_;
    return '' unless defined $bytes;
    return unpack('H*', $bytes);
}

=head2 encode_lower($bytes)

Alias for encode(). Returns lowercase hex.

=cut

sub encode_lower {
    my ($bytes) = @_;
    return encode($bytes);
}

=head2 encode_upper($bytes)

Encode bytes to uppercase hexadecimal string.

=cut

sub encode_upper {
    my ($bytes) = @_;
    return uc(encode($bytes));
}

=head2 decode($hex_string)

Decode a hexadecimal string to bytes.
Returns undef if the input is invalid hex.

=cut

sub decode {
    my ($hex_string) = @_;
    return undef unless defined $hex_string;

    # Remove optional 0x prefix
    $hex_string =~ s/^0[xX]//;

    # Remove whitespace
    $hex_string =~ s/\s//g;

    # Validate hex characters
    return undef unless $hex_string =~ /^[0-9a-fA-F]*$/;

    # Must have even length
    return undef unless length($hex_string) % 2 == 0;

    # Handle empty string
    return '' if length($hex_string) == 0;

    return pack('H*', $hex_string);
}

=head2 is_valid($hex_string)

Check if a string is valid hexadecimal.
Accepts optional 0x prefix and whitespace.

=cut

sub is_valid {
    my ($hex_string) = @_;
    return 0 unless defined $hex_string;

    # Remove optional prefix and whitespace for validation
    my $clean = $hex_string;
    $clean =~ s/^0[xX]//;
    $clean =~ s/\s//g;

    # Must be even length and all hex chars
    return 0 unless length($clean) % 2 == 0;
    return $clean =~ /^[0-9a-fA-F]*$/;
}

=head2 constant_time_equal($hex_a, $hex_b)

Compare two hex strings in constant time.
Prevents timing attacks by always comparing all characters.
Returns 1 if equal, 0 otherwise.

=cut

sub constant_time_equal {
    my ($hex_a, $hex_b) = @_;

    return 0 unless defined $hex_a && defined $hex_b;

    # Normalize: remove prefix and whitespace, lowercase
    my $clean_a = _normalize_hex($hex_a);
    my $clean_b = _normalize_hex($hex_b);

    return 0 unless defined $clean_a && defined $clean_b;

    # Length check (constant time: always check, but also compare lengths)
    my $len_a = length($clean_a);
    my $len_b = length($clean_b);

    # If lengths differ, still do comparison to prevent timing leak
    my $max_len = $len_a > $len_b ? $len_a : $len_b;
    return 0 if $max_len == 0 && ($len_a != $len_b);
    return 1 if $len_a == 0 && $len_b == 0;

    # Pad shorter string (this is intentional for constant-time)
    $clean_a .= '0' x ($max_len - $len_a) if $len_a < $max_len;
    $clean_b .= '0' x ($max_len - $len_b) if $len_b < $max_len;

    # Constant-time comparison
    my $result = 0;
    for my $i (0 .. $max_len - 1) {
        $result |= ord(substr($clean_a, $i, 1)) ^ ord(substr($clean_b, $i, 1));
    }

    # Also factor in length difference
    $result |= $len_a ^ $len_b;

    return $result == 0 ? 1 : 0;
}

sub _normalize_hex {
    my ($hex) = @_;
    return undef unless defined $hex;

    my $clean = lc($hex);
    $clean =~ s/^0x//;
    $clean =~ s/\s//g;

    # Validate
    return undef unless $clean =~ /^[0-9a-f]*$/;

    return $clean;
}

=head2 bytes_to_hex($bytes)

Alias for encode(). Convert bytes to hex string.

=cut

sub bytes_to_hex {
    my ($bytes) = @_;
    return encode($bytes);
}

=head2 hex_to_bytes($hex)

Alias for decode(). Convert hex string to bytes.

=cut

sub hex_to_bytes {
    my ($hex) = @_;
    return decode($hex);
}

=head2 xor_bytes($bytes_a, $bytes_b)

XOR two byte strings and return result as hex.
Strings must be same length. Returns undef if lengths differ.

=cut

sub xor_bytes {
    my ($bytes_a, $bytes_b) = @_;

    return undef unless defined $bytes_a && defined $bytes_b;
    return undef unless length($bytes_a) == length($bytes_b);

    my $result = '';
    for my $i (0 .. length($bytes_a) - 1) {
        $result .= chr(ord(substr($bytes_a, $i, 1)) ^ ord(substr($bytes_b, $i, 1)));
    }

    return encode($result);
}

=head2 pad_hex($hex, $target_length, $pad_char)

Pad a hex string to a target length.
Default pad character is '0'.
Pads on the left (for numbers) by default.
Returns undef if input is invalid hex.

=cut

sub pad_hex {
    my ($hex, $target_length, $pad_char) = @_;

    return undef unless defined $hex && defined $target_length;
    return undef unless is_valid($hex);

    $pad_char //= '0';
    $pad_char = substr($pad_char, 0, 1);  # Ensure single char

    # Normalize
    my $clean = _normalize_hex($hex);
    return undef unless defined $clean;

    my $current_len = length($clean);
    return $clean if $current_len >= $target_length;

    my $padding = $pad_char x ($target_length - $current_len);
    return $padding . $clean;
}

1;

__END__

=head1 SECURITY NOTES

The C<constant_time_equal> function is designed to prevent timing attacks
when comparing sensitive hex-encoded values like authentication tokens,
HMACs, or cryptographic digests.

Regular string comparison (C<eq>) can leak information about where
strings differ based on how long the comparison takes. This function
always takes the same amount of time regardless of input.

=head1 AUTHOR

Hyperpolymath

=head1 LICENSE

PMPL-1.0

=cut
