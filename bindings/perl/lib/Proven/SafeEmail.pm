# SPDX-License-Identifier: Apache-2.0
# SPDX-FileCopyrightText: 2025 Hyperpolymath

package Proven::SafeEmail;
use strict;
use warnings;
use Exporter 'import';

our $VERSION = '0.4.0';
our @EXPORT_OK = qw(
    is_valid split_email get_domain get_local_part
    normalize has_mx_record is_disposable
);

=head1 NAME

Proven::SafeEmail - Safe email validation and parsing operations

=head1 SYNOPSIS

    use Proven::SafeEmail qw(is_valid get_domain);

    if (is_valid($email)) {
        my $domain = get_domain($email);
    }

=cut

=head2 is_valid($email)

Check if an email address is valid (basic check).

=cut

sub is_valid {
    my ($email) = @_;
    return 0 unless defined $email;

    # Basic email validation
    my $at_pos = index($email, '@');
    return 0 if $at_pos < 0;

    # Check for multiple @ symbols
    return 0 if index($email, '@', $at_pos + 1) >= 0;

    my $local_part = substr($email, 0, $at_pos);
    my $domain = substr($email, $at_pos + 1);

    return 0 if length($local_part) == 0;
    return 0 if length($domain) < 3;
    return 0 if index($domain, '.') < 0;
    return 0 if substr($domain, 0, 1) eq '.';
    return 0 if substr($domain, -1) eq '.';

    return 1;
}

=head2 split_email($email)

Split an email into local part and domain.
Returns hashref {local_part => ..., domain => ...} or undef if invalid.

=cut

sub split_email {
    my ($email) = @_;
    return undef unless is_valid($email);

    my $at_pos = rindex($email, '@');
    return {
        local_part => substr($email, 0, $at_pos),
        domain => substr($email, $at_pos + 1),
    };
}

=head2 get_domain($email)

Extract the domain from an email address.

=cut

sub get_domain {
    my ($email) = @_;
    my $parts = split_email($email);
    return $parts ? $parts->{domain} : undef;
}

=head2 get_local_part($email)

Extract the local part from an email address.

=cut

sub get_local_part {
    my ($email) = @_;
    my $parts = split_email($email);
    return $parts ? $parts->{local_part} : undef;
}

=head2 normalize($email)

Normalize an email address (lowercase domain).

=cut

sub normalize {
    my ($email) = @_;
    my $parts = split_email($email);
    return undef unless $parts;
    return $parts->{local_part} . '@' . lc($parts->{domain});
}

=head2 has_mx_record($email)

Check if an email domain has MX records.
Note: This performs a DNS lookup and may be slow.

=cut

sub has_mx_record {
    my ($email) = @_;
    my $domain = get_domain($email);
    return 0 unless defined $domain;

    # Use Net::DNS if available, otherwise fall back to host command
    eval {
        require Net::DNS;
        my $resolver = Net::DNS::Resolver->new;
        my $mx = $resolver->query($domain, 'MX');
        return $mx ? 1 : 0;
    };

    # Fallback: use system command
    my $result = `host -t MX $domain 2>/dev/null`;
    return $result =~ /mail is handled by/ ? 1 : 0;
}

=head2 is_disposable($email)

Check if an email is from a disposable email service.
Note: This is a basic check against common disposable domains.

=cut

sub is_disposable {
    my ($email) = @_;

    my @disposable_domains = qw(
        tempmail.com throwaway.email guerrillamail.com
        mailinator.com 10minutemail.com temp-mail.org
        fakeinbox.com trashmail.com yopmail.com
    );

    my $domain = get_domain($email);
    return 0 unless defined $domain;

    $domain = lc($domain);
    for my $disposable (@disposable_domains) {
        return 1 if $domain eq $disposable;
    }

    return 0;
}

1;

__END__

=head1 AUTHOR

Hyperpolymath

=head1 LICENSE

PMPL-1.0

=cut
