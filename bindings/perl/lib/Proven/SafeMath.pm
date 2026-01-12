# SPDX-License-Identifier: PMPL-1.0
# SPDX-FileCopyrightText: 2025 Hyperpolymath

package Proven::SafeMath;
use strict;
use warnings;
use Exporter 'import';

our $VERSION = '0.3.0';
our @EXPORT_OK = qw(
    div div_or mod
    add_checked sub_checked mul_checked
    abs_safe abs_clamped clamp
    pow_checked percent_of as_percent
);

# 64-bit integer limits (Perl uses native integers)
use constant MAX_INT => 9223372036854775807;
use constant MIN_INT => -9223372036854775808;

=head1 NAME

Proven::SafeMath - Safe arithmetic operations that cannot crash

=head1 SYNOPSIS

    use Proven::SafeMath qw(div add_checked);

    my $result = div(10, 0);  # undef, not an error
    my $sum = add_checked(MAX_INT, 1);  # undef (overflow)

=head1 DESCRIPTION

All operations handle edge cases like division by zero, overflow, and underflow
without dying. Operations return undef on failure.

=cut

=head2 div($numerator, $denominator)

Safe division that returns undef on division by zero.

=cut

sub div {
    my ($numerator, $denominator) = @_;
    return undef if $denominator == 0;
    return int($numerator / $denominator);
}

=head2 div_or($default, $numerator, $denominator)

Safe division with a default value for division by zero.

=cut

sub div_or {
    my ($default, $numerator, $denominator) = @_;
    my $result = div($numerator, $denominator);
    return defined($result) ? $result : $default;
}

=head2 mod($numerator, $denominator)

Safe modulo that returns undef on division by zero.

=cut

sub mod {
    my ($numerator, $denominator) = @_;
    return undef if $denominator == 0;
    return $numerator % $denominator;
}

=head2 add_checked($a, $b)

Addition with overflow detection. Returns undef if overflow would occur.

=cut

sub add_checked {
    my ($a, $b) = @_;

    # Check for overflow before performing operation
    if ($b > 0 && $a > MAX_INT - $b) {
        return undef;
    }
    if ($b < 0 && $a < MIN_INT - $b) {
        return undef;
    }
    return $a + $b;
}

=head2 sub_checked($a, $b)

Subtraction with underflow detection. Returns undef if underflow would occur.

=cut

sub sub_checked {
    my ($a, $b) = @_;

    # Check for underflow before performing operation
    if ($b > 0 && $a < MIN_INT + $b) {
        return undef;
    }
    if ($b < 0 && $a > MAX_INT + $b) {
        return undef;
    }
    return $a - $b;
}

=head2 mul_checked($a, $b)

Multiplication with overflow detection. Returns undef if overflow would occur.

=cut

sub mul_checked {
    my ($a, $b) = @_;

    return 0 if $a == 0 || $b == 0;

    # Check for overflow
    if ($a > 0) {
        if ($b > 0) {
            return undef if $a > int(MAX_INT / $b);
        } else {
            return undef if $b < int(MIN_INT / $a);
        }
    } else {
        if ($b > 0) {
            return undef if $a < int(MIN_INT / $b);
        } else {
            return undef if $a != 0 && $b < int(MAX_INT / $a);
        }
    }

    return $a * $b;
}

=head2 abs_safe($n)

Safe absolute value that handles MIN_INT correctly.
Returns undef if n is MIN_INT (cannot be represented).

=cut

sub abs_safe {
    my ($n) = @_;
    return undef if $n == MIN_INT;
    return abs($n);
}

=head2 abs_clamped($n)

Absolute value that clamps to MAX_INT instead of overflowing.

=cut

sub abs_clamped {
    my ($n) = @_;
    my $result = abs_safe($n);
    return defined($result) ? $result : MAX_INT;
}

=head2 clamp($lo, $hi, $value)

Clamp a value to range [$lo, $hi].

=cut

sub clamp {
    my ($lo, $hi, $value) = @_;
    return $lo if $value < $lo;
    return $hi if $value > $hi;
    return $value;
}

=head2 pow_checked($base, $exp)

Integer exponentiation with overflow detection.
Returns undef if overflow would occur.
Dies if exp is negative.

=cut

sub pow_checked {
    my ($base, $exp) = @_;

    die "Exponent must be non-negative" if $exp < 0;
    return 1 if $exp == 0;

    my $result = 1;
    my $current_base = $base;
    my $current_exp = $exp;

    while ($current_exp > 0) {
        if ($current_exp & 1) {
            my $new_result = mul_checked($result, $current_base);
            return undef unless defined($new_result);
            $result = $new_result;
        }
        $current_exp >>= 1;
        if ($current_exp > 0) {
            my $new_base = mul_checked($current_base, $current_base);
            return undef unless defined($new_base);
            $current_base = $new_base;
        }
    }

    return $result;
}

=head2 percent_of($percent, $total)

Calculate percentage safely. Returns undef on overflow/division-by-zero.

=cut

sub percent_of {
    my ($percent, $total) = @_;
    my $product = mul_checked($percent, $total);
    return undef unless defined($product);
    return div($product, 100);
}

=head2 as_percent($part, $whole)

Calculate what percentage part is of whole.
Returns undef on division by zero.

=cut

sub as_percent {
    my ($part, $whole) = @_;
    my $scaled = mul_checked($part, 100);
    return undef unless defined($scaled);
    return div($scaled, $whole);
}

1;

__END__

=head1 AUTHOR

Hyperpolymath

=head1 LICENSE

PMPL-1.0

=cut
