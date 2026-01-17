# SPDX-License-Identifier: PMPL-1.0
# SPDX-FileCopyrightText: 2025 Hyperpolymath

package Proven::SafeColor;
use strict;
use warnings;
use Exporter 'import';
use POSIX qw(floor);

our $VERSION = '0.4.0';
our @EXPORT_OK = qw(
    parse_hex parse_rgb parse_hsl parse_hsv
    to_hex to_rgb to_hsl to_hsv to_css
    lighten darken saturate desaturate
    mix complement invert grayscale
    is_valid_hex is_valid_rgb
);

=head1 NAME

Proven::SafeColor - Safe color parsing and manipulation

=head1 SYNOPSIS

    use Proven::SafeColor qw(parse_hex to_rgb lighten);

    my $color = parse_hex('#ff5500');
    my $rgb = to_rgb($color);  # { r => 255, g => 85, b => 0 }
    my $lighter = lighten($color, 0.2);

=head1 DESCRIPTION

Provides safe color operations for parsing and manipulating colors
in various formats (hex, RGB, HSL, HSV).

=cut

# Color object for OO interface
package Proven::SafeColor::Color;
use strict;
use warnings;

sub new {
    my ($class, $components) = @_;
    return undef unless ref($components) eq 'HASH';

    my $r = $components->{r};
    my $g = $components->{g};
    my $b = $components->{b};
    my $a = $components->{a} // 1.0;

    return undef unless defined $r && defined $g && defined $b;
    return undef if $r < 0 || $r > 255;
    return undef if $g < 0 || $g > 255;
    return undef if $b < 0 || $b > 255;
    return undef if $a < 0 || $a > 1;

    return bless {
        r => int($r),
        g => int($g),
        b => int($b),
        a => $a,
    }, $class;
}

sub r { shift->{r} }
sub g { shift->{g} }
sub b { shift->{b} }
sub a { shift->{a} }

sub to_hex { Proven::SafeColor::to_hex(shift) }
sub to_rgb { Proven::SafeColor::to_rgb(shift) }
sub to_hsl { Proven::SafeColor::to_hsl(shift) }
sub to_css { Proven::SafeColor::to_css(shift) }

sub equals {
    my ($self, $other) = @_;
    return 0 unless ref($other) && $other->isa('Proven::SafeColor::Color');
    return $self->{r} == $other->{r}
        && $self->{g} == $other->{g}
        && $self->{b} == $other->{b}
        && $self->{a} == $other->{a};
}

package Proven::SafeColor;

=head2 parse_hex($hex_string)

Parse a hex color string (#RGB, #RRGGBB, #RRGGBBAA).
Returns a Color object or undef if invalid.

=cut

sub parse_hex {
    my ($hex_string) = @_;
    return undef unless defined $hex_string;

    # Remove # prefix if present
    $hex_string =~ s/^#//;

    my ($r, $g, $b, $a);

    if (length($hex_string) == 3) {
        # #RGB -> #RRGGBB
        if ($hex_string =~ /^([0-9a-fA-F])([0-9a-fA-F])([0-9a-fA-F])$/) {
            $r = hex($1 . $1);
            $g = hex($2 . $2);
            $b = hex($3 . $3);
            $a = 1.0;
        } else {
            return undef;
        }
    } elsif (length($hex_string) == 4) {
        # #RGBA -> #RRGGBBAA
        if ($hex_string =~ /^([0-9a-fA-F])([0-9a-fA-F])([0-9a-fA-F])([0-9a-fA-F])$/) {
            $r = hex($1 . $1);
            $g = hex($2 . $2);
            $b = hex($3 . $3);
            $a = hex($4 . $4) / 255;
        } else {
            return undef;
        }
    } elsif (length($hex_string) == 6) {
        # #RRGGBB
        if ($hex_string =~ /^([0-9a-fA-F]{2})([0-9a-fA-F]{2})([0-9a-fA-F]{2})$/) {
            $r = hex($1);
            $g = hex($2);
            $b = hex($3);
            $a = 1.0;
        } else {
            return undef;
        }
    } elsif (length($hex_string) == 8) {
        # #RRGGBBAA
        if ($hex_string =~ /^([0-9a-fA-F]{2})([0-9a-fA-F]{2})([0-9a-fA-F]{2})([0-9a-fA-F]{2})$/) {
            $r = hex($1);
            $g = hex($2);
            $b = hex($3);
            $a = hex($4) / 255;
        } else {
            return undef;
        }
    } else {
        return undef;
    }

    return Proven::SafeColor::Color->new({ r => $r, g => $g, b => $b, a => $a });
}

=head2 parse_rgb($r, $g, $b, $a)

Create a color from RGB values (0-255).

=cut

sub parse_rgb {
    my ($r, $g, $b, $a) = @_;
    return Proven::SafeColor::Color->new({ r => $r, g => $g, b => $b, a => $a // 1.0 });
}

=head2 parse_hsl($h, $s, $l, $a)

Create a color from HSL values.
H: 0-360, S: 0-1, L: 0-1

=cut

sub parse_hsl {
    my ($h, $s, $l, $a) = @_;
    return undef unless defined $h && defined $s && defined $l;

    $a //= 1.0;

    # Normalize hue to 0-360
    $h = $h % 360;
    $h += 360 if $h < 0;

    # Clamp saturation and lightness
    $s = _clamp($s, 0, 1);
    $l = _clamp($l, 0, 1);

    my ($r, $g, $b);

    if ($s == 0) {
        $r = $g = $b = int($l * 255);
    } else {
        my $q = $l < 0.5 ? $l * (1 + $s) : $l + $s - $l * $s;
        my $p = 2 * $l - $q;

        $r = int(_hue_to_rgb($p, $q, $h / 360 + 1/3) * 255);
        $g = int(_hue_to_rgb($p, $q, $h / 360) * 255);
        $b = int(_hue_to_rgb($p, $q, $h / 360 - 1/3) * 255);
    }

    return Proven::SafeColor::Color->new({ r => $r, g => $g, b => $b, a => $a });
}

sub _hue_to_rgb {
    my ($p, $q, $t) = @_;
    $t += 1 if $t < 0;
    $t -= 1 if $t > 1;

    return $p + ($q - $p) * 6 * $t if $t < 1/6;
    return $q if $t < 1/2;
    return $p + ($q - $p) * (2/3 - $t) * 6 if $t < 2/3;
    return $p;
}

=head2 parse_hsv($h, $s, $v, $a)

Create a color from HSV values.
H: 0-360, S: 0-1, V: 0-1

=cut

sub parse_hsv {
    my ($h, $s, $v, $a) = @_;
    return undef unless defined $h && defined $s && defined $v;

    $a //= 1.0;

    # Normalize hue
    $h = $h % 360;
    $h += 360 if $h < 0;

    $s = _clamp($s, 0, 1);
    $v = _clamp($v, 0, 1);

    my $c = $v * $s;
    my $x = $c * (1 - CORE::abs(($h / 60) % 2 - 1));
    my $m = $v - $c;

    my ($r_prime, $g_prime, $b_prime);

    if ($h < 60) {
        ($r_prime, $g_prime, $b_prime) = ($c, $x, 0);
    } elsif ($h < 120) {
        ($r_prime, $g_prime, $b_prime) = ($x, $c, 0);
    } elsif ($h < 180) {
        ($r_prime, $g_prime, $b_prime) = (0, $c, $x);
    } elsif ($h < 240) {
        ($r_prime, $g_prime, $b_prime) = (0, $x, $c);
    } elsif ($h < 300) {
        ($r_prime, $g_prime, $b_prime) = ($x, 0, $c);
    } else {
        ($r_prime, $g_prime, $b_prime) = ($c, 0, $x);
    }

    my $r = int(($r_prime + $m) * 255);
    my $g = int(($g_prime + $m) * 255);
    my $b = int(($b_prime + $m) * 255);

    return Proven::SafeColor::Color->new({ r => $r, g => $g, b => $b, a => $a });
}

=head2 to_hex($color)

Convert a color to hex string (#RRGGBB or #RRGGBBAA).

=cut

sub to_hex {
    my ($color) = @_;
    return undef unless ref($color) && $color->isa('Proven::SafeColor::Color');

    if ($color->{a} == 1.0) {
        return sprintf('#%02x%02x%02x', $color->{r}, $color->{g}, $color->{b});
    } else {
        return sprintf('#%02x%02x%02x%02x', $color->{r}, $color->{g}, $color->{b}, int($color->{a} * 255));
    }
}

=head2 to_rgb($color)

Convert a color to RGB hashref.

=cut

sub to_rgb {
    my ($color) = @_;
    return undef unless ref($color) && $color->isa('Proven::SafeColor::Color');

    return {
        r => $color->{r},
        g => $color->{g},
        b => $color->{b},
        a => $color->{a},
    };
}

=head2 to_hsl($color)

Convert a color to HSL hashref.

=cut

sub to_hsl {
    my ($color) = @_;
    return undef unless ref($color) && $color->isa('Proven::SafeColor::Color');

    my $r = $color->{r} / 255;
    my $g = $color->{g} / 255;
    my $b = $color->{b} / 255;

    my $max_val = _max($r, $g, $b);
    my $min_val = _min($r, $g, $b);
    my $l = ($max_val + $min_val) / 2;

    my ($h, $s);

    if ($max_val == $min_val) {
        $h = $s = 0;
    } else {
        my $d = $max_val - $min_val;
        $s = $l > 0.5 ? $d / (2 - $max_val - $min_val) : $d / ($max_val + $min_val);

        if ($max_val == $r) {
            $h = ($g - $b) / $d + ($g < $b ? 6 : 0);
        } elsif ($max_val == $g) {
            $h = ($b - $r) / $d + 2;
        } else {
            $h = ($r - $g) / $d + 4;
        }

        $h /= 6;
    }

    return {
        h => $h * 360,
        s => $s,
        l => $l,
        a => $color->{a},
    };
}

=head2 to_hsv($color)

Convert a color to HSV hashref.

=cut

sub to_hsv {
    my ($color) = @_;
    return undef unless ref($color) && $color->isa('Proven::SafeColor::Color');

    my $r = $color->{r} / 255;
    my $g = $color->{g} / 255;
    my $b = $color->{b} / 255;

    my $max_val = _max($r, $g, $b);
    my $min_val = _min($r, $g, $b);
    my $d = $max_val - $min_val;

    my $v = $max_val;
    my $s = $max_val == 0 ? 0 : $d / $max_val;
    my $h = 0;

    if ($max_val != $min_val) {
        if ($max_val == $r) {
            $h = ($g - $b) / $d + ($g < $b ? 6 : 0);
        } elsif ($max_val == $g) {
            $h = ($b - $r) / $d + 2;
        } else {
            $h = ($r - $g) / $d + 4;
        }
        $h /= 6;
    }

    return {
        h => $h * 360,
        s => $s,
        v => $v,
        a => $color->{a},
    };
}

=head2 to_css($color)

Convert a color to CSS string (rgb/rgba/hex).

=cut

sub to_css {
    my ($color) = @_;
    return undef unless ref($color) && $color->isa('Proven::SafeColor::Color');

    if ($color->{a} == 1.0) {
        return to_hex($color);
    } else {
        return sprintf('rgba(%d, %d, %d, %.3f)', $color->{r}, $color->{g}, $color->{b}, $color->{a});
    }
}

=head2 lighten($color, $amount)

Lighten a color by a given amount (0-1).

=cut

sub lighten {
    my ($color, $amount) = @_;
    return undef unless ref($color) && $color->isa('Proven::SafeColor::Color');
    return undef unless defined $amount;

    my $hsl = to_hsl($color);
    $hsl->{l} = _clamp($hsl->{l} + $amount, 0, 1);

    return parse_hsl($hsl->{h}, $hsl->{s}, $hsl->{l}, $hsl->{a});
}

=head2 darken($color, $amount)

Darken a color by a given amount (0-1).

=cut

sub darken {
    my ($color, $amount) = @_;
    return lighten($color, -$amount);
}

=head2 saturate($color, $amount)

Increase saturation by a given amount (0-1).

=cut

sub saturate {
    my ($color, $amount) = @_;
    return undef unless ref($color) && $color->isa('Proven::SafeColor::Color');
    return undef unless defined $amount;

    my $hsl = to_hsl($color);
    $hsl->{s} = _clamp($hsl->{s} + $amount, 0, 1);

    return parse_hsl($hsl->{h}, $hsl->{s}, $hsl->{l}, $hsl->{a});
}

=head2 desaturate($color, $amount)

Decrease saturation by a given amount (0-1).

=cut

sub desaturate {
    my ($color, $amount) = @_;
    return saturate($color, -$amount);
}

=head2 mix($color1, $color2, $weight)

Mix two colors. Weight is 0-1 where 0 is all color1 and 1 is all color2.

=cut

sub mix {
    my ($color1, $color2, $weight) = @_;
    return undef unless ref($color1) && $color1->isa('Proven::SafeColor::Color');
    return undef unless ref($color2) && $color2->isa('Proven::SafeColor::Color');

    $weight //= 0.5;
    $weight = _clamp($weight, 0, 1);

    my $r = int($color1->{r} * (1 - $weight) + $color2->{r} * $weight);
    my $g = int($color1->{g} * (1 - $weight) + $color2->{g} * $weight);
    my $b = int($color1->{b} * (1 - $weight) + $color2->{b} * $weight);
    my $a = $color1->{a} * (1 - $weight) + $color2->{a} * $weight;

    return Proven::SafeColor::Color->new({ r => $r, g => $g, b => $b, a => $a });
}

=head2 complement($color)

Get the complementary color (opposite on color wheel).

=cut

sub complement {
    my ($color) = @_;
    return undef unless ref($color) && $color->isa('Proven::SafeColor::Color');

    my $hsl = to_hsl($color);
    $hsl->{h} = ($hsl->{h} + 180) % 360;

    return parse_hsl($hsl->{h}, $hsl->{s}, $hsl->{l}, $hsl->{a});
}

=head2 invert($color)

Invert a color.

=cut

sub invert {
    my ($color) = @_;
    return undef unless ref($color) && $color->isa('Proven::SafeColor::Color');

    return Proven::SafeColor::Color->new({
        r => 255 - $color->{r},
        g => 255 - $color->{g},
        b => 255 - $color->{b},
        a => $color->{a},
    });
}

=head2 grayscale($color)

Convert a color to grayscale.

=cut

sub grayscale {
    my ($color) = @_;
    return undef unless ref($color) && $color->isa('Proven::SafeColor::Color');

    # Use luminosity method
    my $gray = int(0.2126 * $color->{r} + 0.7152 * $color->{g} + 0.0722 * $color->{b});

    return Proven::SafeColor::Color->new({
        r => $gray,
        g => $gray,
        b => $gray,
        a => $color->{a},
    });
}

=head2 is_valid_hex($hex_string)

Check if a string is a valid hex color.

=cut

sub is_valid_hex {
    my ($hex_string) = @_;
    return defined parse_hex($hex_string);
}

=head2 is_valid_rgb($r, $g, $b)

Check if RGB values are valid.

=cut

sub is_valid_rgb {
    my ($r, $g, $b) = @_;
    return 0 unless defined $r && defined $g && defined $b;
    return 0 if $r < 0 || $r > 255;
    return 0 if $g < 0 || $g > 255;
    return 0 if $b < 0 || $b > 255;
    return 1;
}

# Utility functions
sub _clamp {
    my ($value, $min_val, $max_val) = @_;
    return $min_val if $value < $min_val;
    return $max_val if $value > $max_val;
    return $value;
}

sub _min {
    my @values = @_;
    my $minimum = $values[0];
    for (@values[1..$#values]) {
        $minimum = $_ if $_ < $minimum;
    }
    return $minimum;
}

sub _max {
    my @values = @_;
    my $maximum = $values[0];
    for (@values[1..$#values]) {
        $maximum = $_ if $_ > $maximum;
    }
    return $maximum;
}

1;

__END__

=head1 AUTHOR

Hyperpolymath

=head1 LICENSE

PMPL-1.0

=cut
