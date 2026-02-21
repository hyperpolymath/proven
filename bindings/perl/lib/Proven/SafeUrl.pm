# SPDX-License-Identifier: PMPL-1.0-or-later
# SPDX-FileCopyrightText: 2025 Hyperpolymath

package Proven::SafeUrl;
use strict;
use warnings;
use Exporter 'import';

our $VERSION = '0.4.0';
our @EXPORT_OK = qw(
    parse is_valid normalize get_scheme get_host get_port
    get_path get_query get_fragment join_url encode_component
    decode_component is_absolute is_relative get_origin
);

=head1 NAME

Proven::SafeUrl - Safe URL parsing and manipulation without exceptions

=head1 SYNOPSIS

    use Proven::SafeUrl qw(parse is_valid get_host);

    my $url = parse('https://example.com:8080/path?query=value#frag');
    if (defined $url) {
        print get_host($url);  # example.com
        print get_port($url);  # 8080
    }

=head1 DESCRIPTION

Provides safe URL parsing and manipulation. All operations return undef
for invalid input instead of throwing exceptions.

=cut

# URL object for OO interface
package Proven::SafeUrl::Url;
use strict;
use warnings;

sub new {
    my ($class, $components) = @_;
    return undef unless ref($components) eq 'HASH';
    return bless $components, $class;
}

sub scheme   { shift->{scheme} }
sub host     { shift->{host} }
sub port     { shift->{port} }
sub path     { shift->{path} }
sub query    { shift->{query} }
sub fragment { shift->{fragment} }
sub userinfo { shift->{userinfo} }

sub to_string {
    my ($self) = @_;
    return Proven::SafeUrl::normalize($self);
}

sub origin {
    my ($self) = @_;
    return Proven::SafeUrl::get_origin($self);
}

sub is_absolute {
    my ($self) = @_;
    return defined $self->{scheme} && $self->{scheme} ne '';
}

sub equals {
    my ($self, $other) = @_;
    return 0 unless ref($other) && $other->isa('Proven::SafeUrl::Url');
    return $self->to_string eq $other->to_string;
}

package Proven::SafeUrl;

# Default ports for common schemes
my %DEFAULT_PORTS = (
    http   => 80,
    https  => 443,
    ftp    => 21,
    ssh    => 22,
    ws     => 80,
    wss    => 443,
    ldap   => 389,
    ldaps  => 636,
);

=head2 parse($url_string)

Parse a URL string into a Url object.
Returns undef if the URL is invalid.

=cut

sub parse {
    my ($url_string) = @_;
    return undef unless defined $url_string && $url_string ne '';

    my %components = (
        scheme   => undef,
        userinfo => undef,
        host     => undef,
        port     => undef,
        path     => '',
        query    => undef,
        fragment => undef,
    );

    my $remaining = $url_string;

    # Extract fragment
    if ($remaining =~ s/#(.*)$//) {
        $components{fragment} = $1;
    }

    # Extract query
    if ($remaining =~ s/\?(.*)$//) {
        $components{query} = $1;
    }

    # Extract scheme
    if ($remaining =~ s/^([a-zA-Z][a-zA-Z0-9+.-]*):(?=\/\/)//) {
        $components{scheme} = lc($1);
    }

    # Extract authority (//userinfo@host:port)
    if ($remaining =~ s/^\/\/([^\/]*)//) {
        my $authority = $1;

        # Extract userinfo
        if ($authority =~ s/^([^@]*)@//) {
            $components{userinfo} = $1;
        }

        # Extract port
        if ($authority =~ s/:(\d+)$//) {
            $components{port} = int($1);
            return undef if $components{port} > 65535;
        }

        # Remaining is host
        $components{host} = lc($authority) if $authority ne '';
    }

    # Remaining is path
    $components{path} = $remaining;

    # Validate: if we have a host, scheme should be present for absolute URLs
    # But allow relative URLs with just path

    return Proven::SafeUrl::Url->new(\%components);
}

=head2 is_valid($url_string)

Check if a string is a valid URL.

=cut

sub is_valid {
    my ($url_string) = @_;
    return defined parse($url_string);
}

=head2 normalize($url)

Normalize a URL object to its canonical string form.

=cut

sub normalize {
    my ($url) = @_;
    return undef unless ref($url) && $url->isa('Proven::SafeUrl::Url');

    my $result = '';

    if (defined $url->{scheme}) {
        $result .= $url->{scheme} . '://';

        if (defined $url->{userinfo}) {
            $result .= $url->{userinfo} . '@';
        }

        if (defined $url->{host}) {
            $result .= $url->{host};
        }

        if (defined $url->{port}) {
            my $default_port = $DEFAULT_PORTS{$url->{scheme}} // 0;
            if ($url->{port} != $default_port) {
                $result .= ':' . $url->{port};
            }
        }
    }

    $result .= $url->{path} // '';

    if (defined $url->{query}) {
        $result .= '?' . $url->{query};
    }

    if (defined $url->{fragment}) {
        $result .= '#' . $url->{fragment};
    }

    return $result;
}

=head2 get_scheme($url)

Get the scheme (protocol) from a URL object.

=cut

sub get_scheme {
    my ($url) = @_;
    return undef unless ref($url) && $url->isa('Proven::SafeUrl::Url');
    return $url->{scheme};
}

=head2 get_host($url)

Get the host from a URL object.

=cut

sub get_host {
    my ($url) = @_;
    return undef unless ref($url) && $url->isa('Proven::SafeUrl::Url');
    return $url->{host};
}

=head2 get_port($url)

Get the port from a URL object.
Returns the explicit port or the default port for the scheme.

=cut

sub get_port {
    my ($url) = @_;
    return undef unless ref($url) && $url->isa('Proven::SafeUrl::Url');

    return $url->{port} if defined $url->{port};
    return $DEFAULT_PORTS{$url->{scheme}} if defined $url->{scheme};
    return undef;
}

=head2 get_path($url)

Get the path from a URL object.

=cut

sub get_path {
    my ($url) = @_;
    return undef unless ref($url) && $url->isa('Proven::SafeUrl::Url');
    return $url->{path};
}

=head2 get_query($url)

Get the query string from a URL object.

=cut

sub get_query {
    my ($url) = @_;
    return undef unless ref($url) && $url->isa('Proven::SafeUrl::Url');
    return $url->{query};
}

=head2 get_fragment($url)

Get the fragment from a URL object.

=cut

sub get_fragment {
    my ($url) = @_;
    return undef unless ref($url) && $url->isa('Proven::SafeUrl::Url');
    return $url->{fragment};
}

=head2 get_origin($url)

Get the origin (scheme://host:port) from a URL object.

=cut

sub get_origin {
    my ($url) = @_;
    return undef unless ref($url) && $url->isa('Proven::SafeUrl::Url');
    return undef unless defined $url->{scheme} && defined $url->{host};

    my $origin = $url->{scheme} . '://' . $url->{host};

    if (defined $url->{port}) {
        my $default_port = $DEFAULT_PORTS{$url->{scheme}} // 0;
        if ($url->{port} != $default_port) {
            $origin .= ':' . $url->{port};
        }
    }

    return $origin;
}

=head2 join_url($base, $relative)

Join a relative URL with a base URL.
Returns undef if base is invalid.

=cut

sub join_url {
    my ($base_string, $relative_string) = @_;

    my $base = ref($base_string) ? $base_string : parse($base_string);
    return undef unless defined $base;
    return undef unless defined $relative_string;

    # If relative is absolute, return it
    my $relative = parse($relative_string);
    return $relative if defined $relative && defined $relative->{scheme};

    # Build new URL from base
    my %new_components = (
        scheme   => $base->{scheme},
        userinfo => $base->{userinfo},
        host     => $base->{host},
        port     => $base->{port},
        path     => '',
        query    => undef,
        fragment => undef,
    );

    if ($relative_string =~ /^\//) {
        # Absolute path
        $new_components{path} = $relative_string;
    } else {
        # Relative path - resolve against base
        my $base_path = $base->{path} // '';
        $base_path =~ s/[^\/]*$//;  # Remove filename
        $new_components{path} = $base_path . $relative_string;
    }

    # Extract query and fragment from relative
    if ($new_components{path} =~ s/#(.*)$//) {
        $new_components{fragment} = $1;
    }
    if ($new_components{path} =~ s/\?(.*)$//) {
        $new_components{query} = $1;
    }

    # Normalize path (remove . and ..)
    $new_components{path} = _normalize_path($new_components{path});

    return Proven::SafeUrl::Url->new(\%new_components);
}

sub _normalize_path {
    my ($path) = @_;
    return '' unless defined $path;

    my @segments = split /\//, $path, -1;
    my @normalized;

    for my $segment (@segments) {
        if ($segment eq '.') {
            next;
        } elsif ($segment eq '..') {
            pop @normalized if @normalized && $normalized[-1] ne '';
        } else {
            push @normalized, $segment;
        }
    }

    return join('/', @normalized);
}

=head2 encode_component($string)

Percent-encode a URL component.

=cut

sub encode_component {
    my ($string) = @_;
    return '' unless defined $string;

    $string =~ s/([^A-Za-z0-9\-_.~])/sprintf("%%%02X", ord($1))/ge;
    return $string;
}

=head2 decode_component($string)

Percent-decode a URL component.
Returns undef if encoding is invalid.

=cut

sub decode_component {
    my ($string) = @_;
    return '' unless defined $string;

    # Validate percent encoding
    return undef if $string =~ /%(?![0-9A-Fa-f]{2})/;

    $string =~ s/%([0-9A-Fa-f]{2})/chr(hex($1))/ge;
    return $string;
}

=head2 is_absolute($url)

Check if a URL is absolute (has a scheme).

=cut

sub is_absolute {
    my ($url) = @_;

    if (!ref($url)) {
        $url = parse($url);
        return 0 unless defined $url;
    }

    return defined $url->{scheme} && $url->{scheme} ne '';
}

=head2 is_relative($url)

Check if a URL is relative (no scheme).

=cut

sub is_relative {
    my ($url) = @_;
    return !is_absolute($url);
}

1;

__END__

=head1 AUTHOR

Hyperpolymath

=head1 LICENSE

PMPL-1.0

=cut
