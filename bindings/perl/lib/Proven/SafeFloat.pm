# SPDX-License-Identifier: PMPL-1.0-or-later
# SPDX-FileCopyrightText: 2025 Hyperpolymath

package Proven::SafeFloat;
use strict;
use warnings;
use Exporter 'import';
use POSIX qw(floor ceil fmod);

our $VERSION = '0.4.0';
our @EXPORT_OK = qw(
    parse is_finite is_nan is_inf compare epsilon_equal
    round_to truncate_to floor_to ceil_to
    add sub mul div safe_div
    to_fixed to_precision from_string
    min max clamp abs sign
);

=head1 NAME

Proven::SafeFloat - Safe floating-point operations without exceptions

=head1 SYNOPSIS

    use Proven::SafeFloat qw(parse safe_div epsilon_equal);

    my $result = safe_div(1.0, 3.0);  # 0.333...
    my $dangerous = safe_div(1.0, 0.0);  # undef, not inf

    if (epsilon_equal(0.1 + 0.2, 0.3)) {
        print "Equal within epsilon\n";
    }

=head1 DESCRIPTION

Provides safe floating-point operations that handle edge cases
like NaN, infinity, and precision issues. Returns undef for
invalid operations instead of producing NaN or infinity.

=cut

# Machine epsilon for double precision
use constant EPSILON => 2.220446049250313e-16;

# Practical epsilon for comparisons
use constant COMPARISON_EPSILON => 1e-10;

=head2 parse($string)

Parse a string to a floating-point number.
Returns undef if the string is not a valid number.

=cut

sub parse {
    my ($string) = @_;
    return undef unless defined $string;

    # Remove whitespace
    $string =~ s/^\s+|\s+$//g;

    # Check for valid float format
    return undef unless $string =~ /^[+-]?(?:\d+\.?\d*|\.\d+)(?:[eE][+-]?\d+)?$/;

    my $value = $string + 0;

    # Check for overflow to infinity
    return undef unless is_finite($value);

    return $value;
}

=head2 is_finite($value)

Check if a value is a finite number (not NaN or infinity).

=cut

sub is_finite {
    my ($value) = @_;
    return 0 unless defined $value;

    # Perl's way to check for inf/nan
    return 0 if $value != $value;  # NaN check
    return 0 if $value == 9**9**9 || $value == -9**9**9;  # Infinity check

    return 1;
}

=head2 is_nan($value)

Check if a value is NaN.

=cut

sub is_nan {
    my ($value) = @_;
    return 0 unless defined $value;
    return $value != $value ? 1 : 0;
}

=head2 is_inf($value)

Check if a value is infinity (positive or negative).

=cut

sub is_inf {
    my ($value) = @_;
    return 0 unless defined $value;
    return 0 if is_nan($value);
    return !is_finite($value);
}

=head2 compare($a, $b)

Compare two floating-point numbers.
Returns -1, 0, or 1 like <=>.
Returns undef if either value is NaN.

=cut

sub compare {
    my ($a, $b) = @_;
    return undef unless defined $a && defined $b;
    return undef if is_nan($a) || is_nan($b);

    return $a <=> $b;
}

=head2 epsilon_equal($a, $b, $epsilon)

Check if two floating-point numbers are equal within epsilon.
Default epsilon is 1e-10.

=cut

sub epsilon_equal {
    my ($a, $b, $epsilon) = @_;
    return 0 unless defined $a && defined $b;
    return 0 if is_nan($a) || is_nan($b);

    $epsilon //= COMPARISON_EPSILON;

    my $diff = CORE::abs($a - $b);
    return $diff <= $epsilon ? 1 : 0;
}

=head2 round_to($value, $decimals)

Round a number to a specified number of decimal places.

=cut

sub round_to {
    my ($value, $decimals) = @_;
    return undef unless defined $value && is_finite($value);

    $decimals //= 0;
    $decimals = int($decimals);
    $decimals = 0 if $decimals < 0;

    my $factor = 10 ** $decimals;
    return floor($value * $factor + 0.5) / $factor;
}

=head2 truncate_to($value, $decimals)

Truncate a number to a specified number of decimal places.

=cut

sub truncate_to {
    my ($value, $decimals) = @_;
    return undef unless defined $value && is_finite($value);

    $decimals //= 0;
    $decimals = int($decimals);
    $decimals = 0 if $decimals < 0;

    my $factor = 10 ** $decimals;
    return int($value * $factor) / $factor;
}

=head2 floor_to($value, $decimals)

Floor a number to a specified number of decimal places.

=cut

sub floor_to {
    my ($value, $decimals) = @_;
    return undef unless defined $value && is_finite($value);

    $decimals //= 0;
    $decimals = int($decimals);
    $decimals = 0 if $decimals < 0;

    my $factor = 10 ** $decimals;
    return floor($value * $factor) / $factor;
}

=head2 ceil_to($value, $decimals)

Ceiling a number to a specified number of decimal places.

=cut

sub ceil_to {
    my ($value, $decimals) = @_;
    return undef unless defined $value && is_finite($value);

    $decimals //= 0;
    $decimals = int($decimals);
    $decimals = 0 if $decimals < 0;

    my $factor = 10 ** $decimals;
    return ceil($value * $factor) / $factor;
}

=head2 add($a, $b)

Safe addition. Returns undef if result is not finite.

=cut

sub add {
    my ($a, $b) = @_;
    return undef unless defined $a && defined $b;
    return undef unless is_finite($a) && is_finite($b);

    my $result = $a + $b;
    return is_finite($result) ? $result : undef;
}

=head2 sub($a, $b)

Safe subtraction. Returns undef if result is not finite.

=cut

sub sub {
    my ($a, $b) = @_;
    return undef unless defined $a && defined $b;
    return undef unless is_finite($a) && is_finite($b);

    my $result = $a - $b;
    return is_finite($result) ? $result : undef;
}

=head2 mul($a, $b)

Safe multiplication. Returns undef if result is not finite.

=cut

sub mul {
    my ($a, $b) = @_;
    return undef unless defined $a && defined $b;
    return undef unless is_finite($a) && is_finite($b);

    my $result = $a * $b;
    return is_finite($result) ? $result : undef;
}

=head2 div($a, $b)

Safe division. Returns undef if divisor is zero or result is not finite.

=cut

sub div {
    my ($a, $b) = @_;
    return undef unless defined $a && defined $b;
    return undef unless is_finite($a) && is_finite($b);
    return undef if $b == 0;

    my $result = $a / $b;
    return is_finite($result) ? $result : undef;
}

=head2 safe_div($a, $b, $default)

Safe division with a default value for division by zero.

=cut

sub safe_div {
    my ($a, $b, $default) = @_;
    my $result = div($a, $b);
    return defined $result ? $result : $default;
}

=head2 to_fixed($value, $decimals)

Format a number with a fixed number of decimal places.
Returns a string.

=cut

sub to_fixed {
    my ($value, $decimals) = @_;
    return undef unless defined $value && is_finite($value);

    $decimals //= 0;
    $decimals = int($decimals);
    $decimals = 0 if $decimals < 0;

    return sprintf("%.${decimals}f", $value);
}

=head2 to_precision($value, $significant_digits)

Format a number with a specified number of significant digits.
Returns a string.

=cut

sub to_precision {
    my ($value, $significant_digits) = @_;
    return undef unless defined $value && is_finite($value);

    $significant_digits //= 6;
    $significant_digits = int($significant_digits);
    $significant_digits = 1 if $significant_digits < 1;

    return sprintf("%.${significant_digits}g", $value);
}

=head2 from_string($string, $default)

Parse a string to float with a default value on failure.

=cut

sub from_string {
    my ($string, $default) = @_;
    my $result = parse($string);
    return defined $result ? $result : $default;
}

=head2 min(@values)

Get the minimum of a list of values.
Ignores NaN values. Returns undef if all values are NaN or list is empty.

=cut

sub min {
    my @values = grep { defined $_ && is_finite($_) } @_;
    return undef unless @values;

    my $minimum = $values[0];
    for my $value (@values[1..$#values]) {
        $minimum = $value if $value < $minimum;
    }
    return $minimum;
}

=head2 max(@values)

Get the maximum of a list of values.
Ignores NaN values. Returns undef if all values are NaN or list is empty.

=cut

sub max {
    my @values = grep { defined $_ && is_finite($_) } @_;
    return undef unless @values;

    my $maximum = $values[0];
    for my $value (@values[1..$#values]) {
        $maximum = $value if $value > $maximum;
    }
    return $maximum;
}

=head2 clamp($value, $min_value, $max_value)

Clamp a value to a range.

=cut

sub clamp {
    my ($value, $min_value, $max_value) = @_;
    return undef unless defined $value && is_finite($value);
    return undef unless defined $min_value && is_finite($min_value);
    return undef unless defined $max_value && is_finite($max_value);

    return $min_value if $value < $min_value;
    return $max_value if $value > $max_value;
    return $value;
}

=head2 abs($value)

Safe absolute value.

=cut

sub abs {
    my ($value) = @_;
    return undef unless defined $value && is_finite($value);
    return CORE::abs($value);
}

=head2 sign($value)

Get the sign of a number: -1, 0, or 1.

=cut

sub sign {
    my ($value) = @_;
    return undef unless defined $value && is_finite($value);

    return 0 if $value == 0;
    return $value > 0 ? 1 : -1;
}

1;

__END__

=head1 AUTHOR

Hyperpolymath

=head1 LICENSE

PMPL-1.0

=cut
