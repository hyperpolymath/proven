# SPDX-License-Identifier: PMPL-1.0
# SPDX-FileCopyrightText: 2025 Hyperpolymath

package Proven::SafeNetwork;
use strict;
use warnings;
use Exporter 'import';

our $VERSION = '0.3.0';
our @EXPORT_OK = qw(
    parse_ipv4 is_valid_ipv4 is_private is_loopback
    is_public is_reserved format_ipv4 is_valid_ip
);

=head1 NAME

Proven::SafeNetwork - Safe network operations for IP address validation and classification

=head1 SYNOPSIS

    use Proven::SafeNetwork qw(parse_ipv4 is_private);

    if (is_private($ip_address)) {
        print "Private network\n";
    }

=cut

=head2 parse_ipv4($address)

Parse an IPv4 address string.
Returns hashref {a => ..., b => ..., c => ..., d => ...} or undef if invalid.

=cut

sub parse_ipv4 {
    my ($address) = @_;
    return undef unless defined $address;

    my @parts = split /\./, $address;
    return undef unless @parts == 4;

    for my $part (@parts) {
        return undef unless $part =~ /^\d+$/;
        my $num = int($part);
        return undef if $num < 0 || $num > 255;
        return undef if $part ne $num && $part =~ /^0/;  # No leading zeros
    }

    return {
        a => int($parts[0]),
        b => int($parts[1]),
        c => int($parts[2]),
        d => int($parts[3]),
    };
}

=head2 is_valid_ipv4($address)

Check if a string is a valid IPv4 address.

=cut

sub is_valid_ipv4 {
    my ($address) = @_;
    return defined parse_ipv4($address);
}

=head2 is_private($address)

Check if an IPv4 address is in a private range.

=cut

sub is_private {
    my ($address) = @_;
    my $ip = parse_ipv4($address);
    return 0 unless $ip;

    # 10.0.0.0/8
    return 1 if $ip->{a} == 10;

    # 172.16.0.0/12
    return 1 if $ip->{a} == 172 && $ip->{b} >= 16 && $ip->{b} <= 31;

    # 192.168.0.0/16
    return 1 if $ip->{a} == 192 && $ip->{b} == 168;

    return 0;
}

=head2 is_loopback($address)

Check if an IPv4 address is a loopback address (127.0.0.0/8).

=cut

sub is_loopback {
    my ($address) = @_;
    my $ip = parse_ipv4($address);
    return 0 unless $ip;
    return $ip->{a} == 127;
}

=head2 is_reserved($address)

Check if an IPv4 address is in a reserved range.

=cut

sub is_reserved {
    my ($address) = @_;
    my $ip = parse_ipv4($address);
    return 0 unless $ip;

    # 0.0.0.0/8 - Current network
    return 1 if $ip->{a} == 0;

    # 100.64.0.0/10 - Carrier-grade NAT
    return 1 if $ip->{a} == 100 && $ip->{b} >= 64 && $ip->{b} <= 127;

    # 169.254.0.0/16 - Link-local
    return 1 if $ip->{a} == 169 && $ip->{b} == 254;

    # 192.0.0.0/24 - IETF Protocol Assignments
    return 1 if $ip->{a} == 192 && $ip->{b} == 0 && $ip->{c} == 0;

    # 192.0.2.0/24 - TEST-NET-1
    return 1 if $ip->{a} == 192 && $ip->{b} == 0 && $ip->{c} == 2;

    # 198.51.100.0/24 - TEST-NET-2
    return 1 if $ip->{a} == 198 && $ip->{b} == 51 && $ip->{c} == 100;

    # 203.0.113.0/24 - TEST-NET-3
    return 1 if $ip->{a} == 203 && $ip->{b} == 0 && $ip->{c} == 113;

    # 224.0.0.0/4 - Multicast
    return 1 if $ip->{a} >= 224 && $ip->{a} <= 239;

    # 240.0.0.0/4 - Reserved for future use
    return 1 if $ip->{a} >= 240;

    return 0;
}

=head2 is_public($address)

Check if an IPv4 address is public (not private, loopback, or reserved).

=cut

sub is_public {
    my ($address) = @_;
    return is_valid_ipv4($address)
        && !is_private($address)
        && !is_loopback($address)
        && !is_reserved($address);
}

=head2 format_ipv4($ip)

Format an IPv4 address from octets hashref.

=cut

sub format_ipv4 {
    my ($ip) = @_;
    return sprintf('%d.%d.%d.%d', $ip->{a}, $ip->{b}, $ip->{c}, $ip->{d});
}

=head2 is_valid_ip($address)

Check if an address is valid (v4 or v6).

=cut

sub is_valid_ip {
    my ($address) = @_;
    return 1 if is_valid_ipv4($address);

    # Basic IPv6 check
    return 1 if $address =~ /^[\da-fA-F:]+$/
        && $address =~ /:/
        && length($address) <= 45;

    return 0;
}

1;

__END__

=head1 AUTHOR

Hyperpolymath

=head1 LICENSE

PMPL-1.0

=cut
