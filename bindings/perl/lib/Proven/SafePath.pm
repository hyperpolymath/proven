# SPDX-License-Identifier: PMPL-1.0
# SPDX-FileCopyrightText: 2025 Hyperpolymath

package Proven::SafePath;
use strict;
use warnings;
use File::Spec;
use File::Basename;
use Cwd qw(abs_path);
use Exporter 'import';

our $VERSION = '0.4.0';
our @EXPORT_OK = qw(
    has_traversal is_safe sanitize_filename
    safe_join resolve_within safe_basename
    has_allowed_extension
);

=head1 NAME

Proven::SafePath - Safe filesystem path operations with traversal attack prevention

=head1 SYNOPSIS

    use Proven::SafePath qw(has_traversal safe_join);

    if (has_traversal($user_input)) {
        die "Path traversal detected";
    }
    my $path = safe_join('/uploads', $filename);

=cut

# Dangerous path sequences
my @TRAVERSAL_PATTERNS = ('..', '~');

# Characters dangerous in filenames
my @DANGEROUS_CHARS = ('/', '\\', '<', '>', ':', '"', '|', '?', '*', "\0");

=head2 has_traversal($path)

Check if a path contains directory traversal sequences.

=cut

sub has_traversal {
    my ($path) = @_;
    return 0 unless defined $path;

    for my $pattern (@TRAVERSAL_PATTERNS) {
        return 1 if index($path, $pattern) >= 0;
    }
    return 0;
}

=head2 is_safe($path)

Check if a path is safe (no traversal attacks).

=cut

sub is_safe {
    my ($path) = @_;
    return !has_traversal($path);
}

=head2 sanitize_filename($filename)

Sanitize a filename by removing dangerous characters.

=cut

sub sanitize_filename {
    my ($filename) = @_;
    return '' unless defined $filename;

    my $result = $filename;
    $result =~ s/\.\./_/g;

    for my $char (@DANGEROUS_CHARS) {
        $result =~ s/\Q$char\E/_/g;
    }

    return $result;
}

=head2 safe_join($base, @parts)

Safely join path components, rejecting traversal attempts.
Returns undef if traversal detected.

=cut

sub safe_join {
    my ($base, @parts) = @_;

    for my $part (@parts) {
        return undef if has_traversal($part);
    }

    my @sanitized = map { sanitize_filename($_) } @parts;

    return File::Spec->catfile($base, @sanitized);
}

=head2 resolve_within($base_path, $user_path)

Resolve a path to its canonical form and verify it's within a base directory.
Returns undef if outside base or invalid.

=cut

sub resolve_within {
    my ($base_path, $user_path) = @_;

    my $real_base = abs_path($base_path);
    return undef unless defined $real_base && -d $real_base;

    my $full_path = File::Spec->catfile($real_base, $user_path);
    my $real_path = abs_path($full_path);

    return undef unless defined $real_path;

    # Ensure resolved path is within base
    my $base_with_sep = $real_base;
    $base_with_sep .= File::Spec->catfile('', '') unless $base_with_sep =~ m{[/\\]$};

    unless ($real_path eq $real_base || index($real_path, $base_with_sep) == 0) {
        return undef;
    }

    return $real_path;
}

=head2 safe_basename($path)

Get a safe basename, stripping any directory components.

=cut

sub safe_basename {
    my ($path) = @_;
    return '' unless defined $path;

    # Handle both Unix and Windows separators
    $path =~ s{\\}{/}g;
    my $filename = basename($path);

    return sanitize_filename($filename);
}

=head2 has_allowed_extension($filename, @allowed_extensions)

Check if a filename has a safe extension.

=cut

sub has_allowed_extension {
    my ($filename, @allowed_extensions) = @_;
    return 0 unless defined $filename;

    my ($ext) = $filename =~ /\.([^.]+)$/;
    return 0 unless defined $ext;

    $ext = lc($ext);
    my %allowed = map { lc($_) => 1 } @allowed_extensions;

    return exists $allowed{$ext};
}

1;

__END__

=head1 AUTHOR

Hyperpolymath

=head1 LICENSE

PMPL-1.0

=cut
