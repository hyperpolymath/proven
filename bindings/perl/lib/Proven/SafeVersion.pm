# SPDX-License-Identifier: PMPL-1.0-or-later
# SPDX-FileCopyrightText: 2025 Hyperpolymath

package Proven::SafeVersion;
use strict;
use warnings;
use Exporter 'import';

our $VERSION = '0.4.0';
our @EXPORT_OK = qw(
    parse is_valid compare format
    is_prerelease is_stable get_major get_minor get_patch
    increment_major increment_minor increment_patch
    satisfies parse_constraint
);

=head1 NAME

Proven::SafeVersion - Safe semantic version parsing and comparison

=head1 SYNOPSIS

    use Proven::SafeVersion qw(parse compare satisfies);

    my $version = parse('1.2.3-beta.1+build.456');
    if (defined $version) {
        print get_major($version);  # 1
        if (satisfies($version, '>=1.0.0 <2.0.0')) {
            print "Version is compatible\n";
        }
    }

=head1 DESCRIPTION

Provides safe semantic versioning operations following SemVer 2.0.0.
All operations return undef for invalid input.

=cut

# Version object for OO interface
package Proven::SafeVersion::Version;
use strict;
use warnings;

sub new {
    my ($class, $components) = @_;
    return undef unless ref($components) eq 'HASH';

    my $major = $components->{major};
    my $minor = $components->{minor};
    my $patch = $components->{patch};

    return undef unless defined $major && $major =~ /^\d+$/;
    return undef unless defined $minor && $minor =~ /^\d+$/;
    return undef unless defined $patch && $patch =~ /^\d+$/;

    return bless {
        major      => int($major),
        minor      => int($minor),
        patch      => int($patch),
        prerelease => $components->{prerelease},
        build      => $components->{build},
    }, $class;
}

sub major      { shift->{major} }
sub minor      { shift->{minor} }
sub patch      { shift->{patch} }
sub prerelease { shift->{prerelease} }
sub build      { shift->{build} }

sub to_string {
    my ($self) = @_;
    return Proven::SafeVersion::format($self);
}

sub is_prerelease {
    my ($self) = @_;
    return defined $self->{prerelease} && $self->{prerelease} ne '';
}

sub is_stable {
    my ($self) = @_;
    return !$self->is_prerelease && $self->{major} > 0;
}

sub compare {
    my ($self, $other) = @_;
    return Proven::SafeVersion::compare($self, $other);
}

sub equals {
    my ($self, $other) = @_;
    my $cmp = Proven::SafeVersion::compare($self, $other);
    return defined $cmp && $cmp == 0;
}

package Proven::SafeVersion;

=head2 parse($version_string)

Parse a semantic version string.
Supports: major.minor.patch[-prerelease][+build]

=cut

sub parse {
    my ($version_string) = @_;
    return undef unless defined $version_string;

    # Remove leading 'v' if present
    $version_string =~ s/^v//i;

    # SemVer regex pattern
    if ($version_string =~ /^(\d+)\.(\d+)\.(\d+)(?:-([0-9A-Za-z\-\.]+))?(?:\+([0-9A-Za-z\-\.]+))?$/) {
        my ($major, $minor, $patch, $prerelease, $build) = ($1, $2, $3, $4, $5);

        return Proven::SafeVersion::Version->new({
            major      => $major,
            minor      => $minor,
            patch      => $patch,
            prerelease => $prerelease,
            build      => $build,
        });
    }

    # Allow short forms: major or major.minor
    if ($version_string =~ /^(\d+)(?:\.(\d+))?$/) {
        return Proven::SafeVersion::Version->new({
            major => $1,
            minor => $2 // 0,
            patch => 0,
        });
    }

    return undef;
}

=head2 is_valid($version_string)

Check if a string is a valid semantic version.

=cut

sub is_valid {
    my ($version_string) = @_;
    return defined parse($version_string);
}

=head2 compare($version_a, $version_b)

Compare two versions.
Returns -1 if a < b, 0 if a == b, 1 if a > b.
Returns undef if either version is invalid.

=cut

sub compare {
    my ($version_a, $version_b) = @_;

    # Parse strings if needed
    $version_a = parse($version_a) unless ref($version_a);
    $version_b = parse($version_b) unless ref($version_b);

    return undef unless defined $version_a && defined $version_b;
    return undef unless ref($version_a) && $version_a->isa('Proven::SafeVersion::Version');
    return undef unless ref($version_b) && $version_b->isa('Proven::SafeVersion::Version');

    # Compare major.minor.patch
    my $cmp = $version_a->{major} <=> $version_b->{major};
    return $cmp if $cmp != 0;

    $cmp = $version_a->{minor} <=> $version_b->{minor};
    return $cmp if $cmp != 0;

    $cmp = $version_a->{patch} <=> $version_b->{patch};
    return $cmp if $cmp != 0;

    # Prerelease handling (versions with prerelease have lower precedence)
    my $pre_a = $version_a->{prerelease};
    my $pre_b = $version_b->{prerelease};

    if (!defined $pre_a && !defined $pre_b) {
        return 0;
    } elsif (!defined $pre_a) {
        return 1;  # a is stable, b is prerelease
    } elsif (!defined $pre_b) {
        return -1;  # a is prerelease, b is stable
    }

    # Compare prerelease identifiers
    my @parts_a = split /\./, $pre_a;
    my @parts_b = split /\./, $pre_b;

    my $max_parts = @parts_a > @parts_b ? @parts_a : @parts_b;

    for my $i (0 .. $max_parts - 1) {
        my $part_a = $parts_a[$i];
        my $part_b = $parts_b[$i];

        # Missing part means lower precedence
        return -1 unless defined $part_a;
        return 1 unless defined $part_b;

        my $is_num_a = $part_a =~ /^\d+$/;
        my $is_num_b = $part_b =~ /^\d+$/;

        if ($is_num_a && $is_num_b) {
            # Numeric comparison
            $cmp = int($part_a) <=> int($part_b);
            return $cmp if $cmp != 0;
        } elsif ($is_num_a) {
            return -1;  # Numeric has lower precedence than alphanumeric
        } elsif ($is_num_b) {
            return 1;
        } else {
            # String comparison
            $cmp = $part_a cmp $part_b;
            return $cmp if $cmp != 0;
        }
    }

    return 0;
}

=head2 format($version)

Format a version object as a string.

=cut

sub format {
    my ($version) = @_;
    return undef unless ref($version) && $version->isa('Proven::SafeVersion::Version');

    my $result = sprintf('%d.%d.%d', $version->{major}, $version->{minor}, $version->{patch});

    if (defined $version->{prerelease} && $version->{prerelease} ne '') {
        $result .= '-' . $version->{prerelease};
    }

    if (defined $version->{build} && $version->{build} ne '') {
        $result .= '+' . $version->{build};
    }

    return $result;
}

=head2 is_prerelease($version)

Check if a version is a prerelease.

=cut

sub is_prerelease {
    my ($version) = @_;
    $version = parse($version) unless ref($version);
    return 0 unless defined $version;
    return defined $version->{prerelease} && $version->{prerelease} ne '';
}

=head2 is_stable($version)

Check if a version is stable (not prerelease and major > 0).

=cut

sub is_stable {
    my ($version) = @_;
    $version = parse($version) unless ref($version);
    return 0 unless defined $version;
    return !is_prerelease($version) && $version->{major} > 0;
}

=head2 get_major($version)

Get the major version number.

=cut

sub get_major {
    my ($version) = @_;
    $version = parse($version) unless ref($version);
    return undef unless defined $version;
    return $version->{major};
}

=head2 get_minor($version)

Get the minor version number.

=cut

sub get_minor {
    my ($version) = @_;
    $version = parse($version) unless ref($version);
    return undef unless defined $version;
    return $version->{minor};
}

=head2 get_patch($version)

Get the patch version number.

=cut

sub get_patch {
    my ($version) = @_;
    $version = parse($version) unless ref($version);
    return undef unless defined $version;
    return $version->{patch};
}

=head2 increment_major($version)

Increment the major version, reset minor and patch to 0.

=cut

sub increment_major {
    my ($version) = @_;
    $version = parse($version) unless ref($version);
    return undef unless defined $version;

    return Proven::SafeVersion::Version->new({
        major => $version->{major} + 1,
        minor => 0,
        patch => 0,
    });
}

=head2 increment_minor($version)

Increment the minor version, reset patch to 0.

=cut

sub increment_minor {
    my ($version) = @_;
    $version = parse($version) unless ref($version);
    return undef unless defined $version;

    return Proven::SafeVersion::Version->new({
        major => $version->{major},
        minor => $version->{minor} + 1,
        patch => 0,
    });
}

=head2 increment_patch($version)

Increment the patch version.

=cut

sub increment_patch {
    my ($version) = @_;
    $version = parse($version) unless ref($version);
    return undef unless defined $version;

    return Proven::SafeVersion::Version->new({
        major => $version->{major},
        minor => $version->{minor},
        patch => $version->{patch} + 1,
    });
}

=head2 satisfies($version, $constraint)

Check if a version satisfies a constraint.
Supports: =, >, <, >=, <=, ^, ~, ranges with || and &&

=cut

sub satisfies {
    my ($version, $constraint) = @_;
    $version = parse($version) unless ref($version);
    return 0 unless defined $version;
    return 1 unless defined $constraint;

    # Split on || for OR conditions
    my @or_parts = split /\s*\|\|\s*/, $constraint;
    for my $or_part (@or_parts) {
        # Split on && or space for AND conditions
        my @and_parts = split /\s+(?:&&\s*)?/, $or_part;
        my $all_match = 1;

        for my $part (@and_parts) {
            next if $part eq '';
            unless (_satisfies_single($version, $part)) {
                $all_match = 0;
                last;
            }
        }

        return 1 if $all_match;
    }

    return 0;
}

sub _satisfies_single {
    my ($version, $constraint) = @_;

    # Caret range: ^1.2.3 means >=1.2.3 <2.0.0
    if ($constraint =~ /^\^(.+)$/) {
        my $min_version = parse($1);
        return 0 unless defined $min_version;

        my $max_version;
        if ($min_version->{major} == 0) {
            if ($min_version->{minor} == 0) {
                # ^0.0.x means >=0.0.x <0.0.(x+1)
                $max_version = parse(sprintf('%d.%d.%d', 0, 0, $min_version->{patch} + 1));
            } else {
                # ^0.x.y means >=0.x.y <0.(x+1).0
                $max_version = parse(sprintf('%d.%d.0', 0, $min_version->{minor} + 1));
            }
        } else {
            # ^x.y.z means >=x.y.z <(x+1).0.0
            $max_version = parse(sprintf('%d.0.0', $min_version->{major} + 1));
        }

        return compare($version, $min_version) >= 0 && compare($version, $max_version) < 0;
    }

    # Tilde range: ~1.2.3 means >=1.2.3 <1.3.0
    if ($constraint =~ /^~(.+)$/) {
        my $min_version = parse($1);
        return 0 unless defined $min_version;

        my $max_version = parse(sprintf('%d.%d.0', $min_version->{major}, $min_version->{minor} + 1));
        return compare($version, $min_version) >= 0 && compare($version, $max_version) < 0;
    }

    # Comparison operators
    if ($constraint =~ /^(>=|<=|>|<|=)?(.+)$/) {
        my ($operator, $target_str) = ($1 // '=', $2);
        my $target = parse($target_str);
        return 0 unless defined $target;

        my $cmp = compare($version, $target);
        return 0 unless defined $cmp;

        if ($operator eq '=') {
            return $cmp == 0;
        } elsif ($operator eq '>') {
            return $cmp > 0;
        } elsif ($operator eq '<') {
            return $cmp < 0;
        } elsif ($operator eq '>=') {
            return $cmp >= 0;
        } elsif ($operator eq '<=') {
            return $cmp <= 0;
        }
    }

    return 0;
}

=head2 parse_constraint($constraint)

Parse a version constraint string for validation.
Returns a hashref with parsed constraints or undef if invalid.

=cut

sub parse_constraint {
    my ($constraint) = @_;
    return undef unless defined $constraint && $constraint ne '';

    my @parts;
    for my $part (split /\s*\|\|\s*/, $constraint) {
        for my $sub_part (split /\s+/, $part) {
            next if $sub_part eq '';

            if ($sub_part =~ /^([\^~])?(.+)$/) {
                my ($prefix, $version_str) = ($1, $2);
                if ($sub_part =~ /^(>=|<=|>|<|=)?(.+)$/) {
                    my ($op, $ver) = ($1, $2);
                    return undef unless is_valid($ver);
                    push @parts, { operator => $op // '=', version => $ver };
                } elsif ($prefix) {
                    return undef unless is_valid($version_str);
                    push @parts, { range => $prefix, version => $version_str };
                }
            }
        }
    }

    return @parts ? \@parts : undef;
}

1;

__END__

=head1 AUTHOR

Hyperpolymath

=head1 LICENSE

PMPL-1.0

=cut
