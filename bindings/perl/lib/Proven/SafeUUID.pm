# SPDX-License-Identifier: PMPL-1.0-or-later
# SPDX-FileCopyrightText: 2025 Hyperpolymath

package Proven::SafeUUID;
use strict;
use warnings;
use Exporter 'import';

our $VERSION = '0.4.0';
our @EXPORT_OK = qw(
    parse format is_valid new_uuid nil_uuid max_uuid
    get_version get_variant compare uuid_to_bytes bytes_to_uuid
);

=head1 NAME

Proven::SafeUUID - Safe UUID parsing, validation, and generation

=head1 SYNOPSIS

    use Proven::SafeUUID qw(parse format is_valid new_uuid);

    my $uuid = new_uuid();                          # Random v4 UUID
    my $parsed = parse('550e8400-e29b-41d4-a716-446655440000');
    my $string = format($parsed);

=head1 DESCRIPTION

Provides safe UUID operations without exceptions. Returns undef for
invalid input instead of dying.

=cut

# UUID class for OO interface
package Proven::SafeUUID::UUID;
use strict;
use warnings;

=head2 Proven::SafeUUID::UUID->new($bytes)

Create a new UUID object from 16 bytes.

=cut

sub new {
    my ($class, $bytes) = @_;
    return undef unless defined $bytes && length($bytes) == 16;
    return bless { bytes => $bytes }, $class;
}

=head2 $uuid->bytes()

Get the raw 16 bytes of the UUID.

=cut

sub bytes {
    my ($self) = @_;
    return $self->{bytes};
}

=head2 $uuid->to_string()

Format UUID as lowercase hyphenated string.

=cut

sub to_string {
    my ($self) = @_;
    return Proven::SafeUUID::format($self);
}

=head2 $uuid->version()

Get the UUID version (1-5, or 0 for nil/other).

=cut

sub version {
    my ($self) = @_;
    return Proven::SafeUUID::get_version($self);
}

=head2 $uuid->variant()

Get the UUID variant.

=cut

sub variant {
    my ($self) = @_;
    return Proven::SafeUUID::get_variant($self);
}

=head2 $uuid->is_nil()

Check if this is the nil UUID.

=cut

sub is_nil {
    my ($self) = @_;
    return $self->{bytes} eq ("\x00" x 16);
}

=head2 $uuid->equals($other)

Check equality with another UUID object.

=cut

sub equals {
    my ($self, $other) = @_;
    return 0 unless ref($other) && $other->isa('Proven::SafeUUID::UUID');
    return $self->{bytes} eq $other->{bytes};
}

package Proven::SafeUUID;

=head2 parse($string)

Parse a UUID string into a UUID object.
Accepts hyphenated format (xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx).
Returns undef if invalid.

=cut

sub parse {
    my ($string) = @_;
    return undef unless defined $string;

    # Remove hyphens and validate format
    my $hex = $string;
    $hex =~ s/-//g;

    # Must be exactly 32 hex characters
    return undef unless length($hex) == 32;
    return undef unless $hex =~ /^[0-9a-fA-F]{32}$/;

    # Convert to bytes
    my $bytes = pack('H*', $hex);
    return Proven::SafeUUID::UUID->new($bytes);
}

=head2 format($uuid)

Format a UUID object as a lowercase hyphenated string.
Returns undef if invalid.

=cut

sub format {
    my ($uuid) = @_;
    return undef unless ref($uuid) && $uuid->isa('Proven::SafeUUID::UUID');

    my $hex = unpack('H*', $uuid->bytes);
    return lc(substr($hex, 0, 8) . '-' .
              substr($hex, 8, 4) . '-' .
              substr($hex, 12, 4) . '-' .
              substr($hex, 16, 4) . '-' .
              substr($hex, 20, 12));
}

=head2 is_valid($string)

Check if a string is a valid UUID.

=cut

sub is_valid {
    my ($string) = @_;
    return defined parse($string);
}

=head2 new_uuid()

Generate a new random (version 4) UUID.

=cut

sub new_uuid {
    # Generate 16 random bytes
    my $bytes;

    # Try Crypt::Random first
    eval {
        require Crypt::Random;
        $bytes = Crypt::Random::makerandom_octet(Length => 16);
    };

    # Fall back to /dev/urandom
    if (!defined $bytes && -r '/dev/urandom') {
        open my $fh, '<', '/dev/urandom' or die "Cannot open /dev/urandom: $!";
        binmode $fh;
        read($fh, $bytes, 16) == 16 or die "Short read from /dev/urandom";
        close $fh;
    }

    die "No secure random source available" unless defined $bytes;

    # Set version 4 (random) in byte 6
    my @octets = unpack('C16', $bytes);
    $octets[6] = ($octets[6] & 0x0F) | 0x40;

    # Set variant (RFC 4122) in byte 8
    $octets[8] = ($octets[8] & 0x3F) | 0x80;

    $bytes = pack('C16', @octets);
    return Proven::SafeUUID::UUID->new($bytes);
}

=head2 nil_uuid()

Return the nil UUID (all zeros).

=cut

sub nil_uuid {
    return Proven::SafeUUID::UUID->new("\x00" x 16);
}

=head2 max_uuid()

Return the max UUID (all ones).

=cut

sub max_uuid {
    return Proven::SafeUUID::UUID->new("\xFF" x 16);
}

=head2 get_version($uuid)

Get the version number from a UUID object.
Returns 1-5 for standard versions, 0 for nil, undef for invalid.

=cut

sub get_version {
    my ($uuid) = @_;
    return undef unless ref($uuid) && $uuid->isa('Proven::SafeUUID::UUID');

    my @octets = unpack('C16', $uuid->bytes);
    return ($octets[6] >> 4) & 0x0F;
}

=head2 get_variant($uuid)

Get the variant from a UUID object.
Returns 'ncs', 'rfc4122', 'microsoft', or 'future'.

=cut

sub get_variant {
    my ($uuid) = @_;
    return undef unless ref($uuid) && $uuid->isa('Proven::SafeUUID::UUID');

    my @octets = unpack('C16', $uuid->bytes);
    my $variant_byte = $octets[8];

    if (($variant_byte & 0x80) == 0) {
        return 'ncs';
    } elsif (($variant_byte & 0xC0) == 0x80) {
        return 'rfc4122';
    } elsif (($variant_byte & 0xE0) == 0xC0) {
        return 'microsoft';
    } else {
        return 'future';
    }
}

=head2 compare($uuid_a, $uuid_b)

Compare two UUID objects.
Returns -1, 0, or 1 like cmp operator.
Returns undef if either argument is invalid.

=cut

sub compare {
    my ($uuid_a, $uuid_b) = @_;
    return undef unless ref($uuid_a) && $uuid_a->isa('Proven::SafeUUID::UUID');
    return undef unless ref($uuid_b) && $uuid_b->isa('Proven::SafeUUID::UUID');

    return $uuid_a->bytes cmp $uuid_b->bytes;
}

=head2 uuid_to_bytes($uuid)

Convert a UUID object to its 16-byte representation.

=cut

sub uuid_to_bytes {
    my ($uuid) = @_;
    return undef unless ref($uuid) && $uuid->isa('Proven::SafeUUID::UUID');
    return $uuid->bytes;
}

=head2 bytes_to_uuid($bytes)

Convert 16 bytes to a UUID object.

=cut

sub bytes_to_uuid {
    my ($bytes) = @_;
    return Proven::SafeUUID::UUID->new($bytes);
}

1;

__END__

=head1 AUTHOR

Hyperpolymath

=head1 LICENSE

PMPL-1.0

=cut
