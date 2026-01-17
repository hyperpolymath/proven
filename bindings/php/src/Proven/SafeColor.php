<?php
// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

declare(strict_types=1);

namespace Proven;

/**
 * RGB color with values 0-255.
 */
readonly class Rgb implements \Stringable
{
    public function __construct(
        public int $r,
        public int $g,
        public int $b,
    ) {}

    /**
     * Create a new RGB color with validation.
     *
     * @return Result<Rgb>
     */
    public static function new(int $r, int $g, int $b): Result
    {
        if ($r < 0 || $r > 255 || $g < 0 || $g > 255 || $b < 0 || $b > 255) {
            return Result::err(ProvenError::outOfRange('RGB values must be 0-255'));
        }
        return Result::ok(new self($r, $g, $b));
    }

    /**
     * Create RGB from hex string (e.g., "#FF0000" or "FF0000").
     *
     * @return Result<Rgb>
     */
    public static function fromHex(string $hex): Result
    {
        $clean = ltrim(trim($hex), '#');

        if (strlen($clean) === 3) {
            // Expand shorthand (e.g., "F00" -> "FF0000")
            $clean = $clean[0] . $clean[0] . $clean[1] . $clean[1] . $clean[2] . $clean[2];
        }

        if (strlen($clean) !== 6 || !ctype_xdigit($clean)) {
            return Result::err(ProvenError::invalidInput('Hex color must be 6 characters'));
        }

        $r = hexdec(substr($clean, 0, 2));
        $g = hexdec(substr($clean, 2, 2));
        $b = hexdec(substr($clean, 4, 2));

        return Result::ok(new self((int)$r, (int)$g, (int)$b));
    }

    /**
     * Create RGB from HSL values.
     *
     * @param float $h Hue (0-360)
     * @param float $s Saturation (0-1)
     * @param float $l Lightness (0-1)
     */
    public static function fromHsl(float $h, float $s, float $l): self
    {
        $h = fmod($h, 360);
        if ($h < 0) {
            $h += 360;
        }
        $s = max(0, min(1, $s));
        $l = max(0, min(1, $l));

        $c = (1 - abs(2 * $l - 1)) * $s;
        $x = $c * (1 - abs(fmod($h / 60, 2) - 1));
        $m = $l - $c / 2;

        [$r, $g, $b] = match (true) {
            $h < 60 => [$c, $x, 0],
            $h < 120 => [$x, $c, 0],
            $h < 180 => [0, $c, $x],
            $h < 240 => [0, $x, $c],
            $h < 300 => [$x, 0, $c],
            default => [$c, 0, $x],
        };

        return new self(
            (int)round(($r + $m) * 255),
            (int)round(($g + $m) * 255),
            (int)round(($b + $m) * 255),
        );
    }

    /**
     * Convert to hex string.
     */
    public function toHex(): string
    {
        return sprintf('#%02X%02X%02X', $this->r, $this->g, $this->b);
    }

    /**
     * Convert to lowercase hex string.
     */
    public function toHexLower(): string
    {
        return sprintf('#%02x%02x%02x', $this->r, $this->g, $this->b);
    }

    /**
     * Convert to CSS rgb() notation.
     */
    public function toCss(): string
    {
        return sprintf('rgb(%d, %d, %d)', $this->r, $this->g, $this->b);
    }

    /**
     * Convert to HSL values.
     *
     * @return array{h: float, s: float, l: float}
     */
    public function toHsl(): array
    {
        $r = $this->r / 255;
        $g = $this->g / 255;
        $b = $this->b / 255;

        $max = max($r, $g, $b);
        $min = min($r, $g, $b);
        $l = ($max + $min) / 2;

        if ($max === $min) {
            return ['h' => 0.0, 's' => 0.0, 'l' => $l];
        }

        $d = $max - $min;
        $s = $l > 0.5 ? $d / (2 - $max - $min) : $d / ($max + $min);

        $h = match ($max) {
            $r => (($g - $b) / $d + ($g < $b ? 6 : 0)) * 60,
            $g => (($b - $r) / $d + 2) * 60,
            default => (($r - $g) / $d + 4) * 60,
        };

        return ['h' => $h, 's' => $s, 'l' => $l];
    }

    /**
     * Calculate relative luminance (WCAG formula).
     */
    public function luminance(): float
    {
        $r = self::gammaCorrect($this->r / 255);
        $g = self::gammaCorrect($this->g / 255);
        $b = self::gammaCorrect($this->b / 255);
        return 0.2126 * $r + 0.7152 * $g + 0.0722 * $b;
    }

    private static function gammaCorrect(float $value): float
    {
        return $value <= 0.03928
            ? $value / 12.92
            : pow(($value + 0.055) / 1.055, 2.4);
    }

    /**
     * Check if this is a dark color (luminance < 0.5).
     */
    public function isDark(): bool
    {
        return $this->luminance() < 0.5;
    }

    /**
     * Check if this is a light color (luminance >= 0.5).
     */
    public function isLight(): bool
    {
        return $this->luminance() >= 0.5;
    }

    /**
     * Get a contrasting text color (black or white).
     */
    public function contrastingTextColor(): self
    {
        return $this->isDark() ? self::white() : self::black();
    }

    /**
     * Invert the color.
     */
    public function invert(): self
    {
        return new self(255 - $this->r, 255 - $this->g, 255 - $this->b);
    }

    /**
     * Lighten the color by a percentage.
     *
     * @param float $amount Amount to lighten (0-1)
     */
    public function lighten(float $amount): self
    {
        $hsl = $this->toHsl();
        $hsl['l'] = min(1, $hsl['l'] + $amount);
        return self::fromHsl($hsl['h'], $hsl['s'], $hsl['l']);
    }

    /**
     * Darken the color by a percentage.
     *
     * @param float $amount Amount to darken (0-1)
     */
    public function darken(float $amount): self
    {
        $hsl = $this->toHsl();
        $hsl['l'] = max(0, $hsl['l'] - $amount);
        return self::fromHsl($hsl['h'], $hsl['s'], $hsl['l']);
    }

    /**
     * Mix with another color.
     *
     * @param float $weight Weight of this color (0-1)
     */
    public function mix(self $other, float $weight = 0.5): self
    {
        $weight = max(0, min(1, $weight));
        $invWeight = 1 - $weight;

        return new self(
            (int)round($this->r * $weight + $other->r * $invWeight),
            (int)round($this->g * $weight + $other->g * $invWeight),
            (int)round($this->b * $weight + $other->b * $invWeight),
        );
    }

    // Common color constants
    public static function black(): self { return new self(0, 0, 0); }
    public static function white(): self { return new self(255, 255, 255); }
    public static function red(): self { return new self(255, 0, 0); }
    public static function green(): self { return new self(0, 255, 0); }
    public static function blue(): self { return new self(0, 0, 255); }
    public static function yellow(): self { return new self(255, 255, 0); }
    public static function cyan(): self { return new self(0, 255, 255); }
    public static function magenta(): self { return new self(255, 0, 255); }

    public function __toString(): string
    {
        return $this->toHex();
    }
}

/**
 * RGBA color with alpha channel.
 */
readonly class Rgba implements \Stringable
{
    public function __construct(
        public int $r,
        public int $g,
        public int $b,
        public int $a,
    ) {}

    /**
     * Create RGBA from RGB with alpha.
     */
    public static function fromRgb(Rgb $rgb, int $alpha = 255): self
    {
        return new self($rgb->r, $rgb->g, $rgb->b, $alpha);
    }

    /**
     * Create from hex with alpha (e.g., "#FF0000FF" or "FF0000FF").
     *
     * @return Result<Rgba>
     */
    public static function fromHex(string $hex): Result
    {
        $clean = ltrim(trim($hex), '#');

        if (strlen($clean) === 8 && ctype_xdigit($clean)) {
            $r = hexdec(substr($clean, 0, 2));
            $g = hexdec(substr($clean, 2, 2));
            $b = hexdec(substr($clean, 4, 2));
            $a = hexdec(substr($clean, 6, 2));
            return Result::ok(new self((int)$r, (int)$g, (int)$b, (int)$a));
        }

        // Try parsing as RGB
        $rgbResult = Rgb::fromHex($hex);
        if ($rgbResult->isOk()) {
            return Result::ok(self::fromRgb($rgbResult->unwrap()));
        }

        return Result::err(ProvenError::invalidInput('Invalid RGBA hex color'));
    }

    /**
     * Convert to RGB (discarding alpha).
     */
    public function toRgb(): Rgb
    {
        return new Rgb($this->r, $this->g, $this->b);
    }

    /**
     * Get alpha as a float (0-1).
     */
    public function alphaFloat(): float
    {
        return $this->a / 255;
    }

    /**
     * Convert to hex string with alpha.
     */
    public function toHex(): string
    {
        return sprintf('#%02X%02X%02X%02X', $this->r, $this->g, $this->b, $this->a);
    }

    /**
     * Convert to CSS rgba() notation.
     */
    public function toCss(): string
    {
        return sprintf('rgba(%d, %d, %d, %.3f)', $this->r, $this->g, $this->b, $this->alphaFloat());
    }

    /**
     * Blend with a background color.
     */
    public function blend(Rgb $background): Rgb
    {
        $alpha = $this->alphaFloat();
        $invAlpha = 1 - $alpha;

        return new Rgb(
            (int)round($this->r * $alpha + $background->r * $invAlpha),
            (int)round($this->g * $alpha + $background->g * $invAlpha),
            (int)round($this->b * $alpha + $background->b * $invAlpha),
        );
    }

    public function __toString(): string
    {
        return $this->toHex();
    }
}

/**
 * Safe color operations with validation and WCAG contrast calculations.
 */
class SafeColor
{
    /**
     * Calculate WCAG contrast ratio between two colors.
     *
     * @return float Contrast ratio (1.0 to 21.0)
     */
    public static function contrastRatio(Rgb $color1, Rgb $color2): float
    {
        $l1 = $color1->luminance();
        $l2 = $color2->luminance();
        $lighter = max($l1, $l2);
        $darker = min($l1, $l2);
        return ($lighter + 0.05) / ($darker + 0.05);
    }

    /**
     * Check if contrast meets WCAG AA standard (4.5:1 for normal text).
     */
    public static function meetsWcagAA(Rgb $color1, Rgb $color2): bool
    {
        return self::contrastRatio($color1, $color2) >= 4.5;
    }

    /**
     * Check if contrast meets WCAG AA Large standard (3:1 for large text).
     */
    public static function meetsWcagAALarge(Rgb $color1, Rgb $color2): bool
    {
        return self::contrastRatio($color1, $color2) >= 3.0;
    }

    /**
     * Check if contrast meets WCAG AAA standard (7:1 for normal text).
     */
    public static function meetsWcagAAA(Rgb $color1, Rgb $color2): bool
    {
        return self::contrastRatio($color1, $color2) >= 7.0;
    }

    /**
     * Check if contrast meets WCAG AAA Large standard (4.5:1 for large text).
     */
    public static function meetsWcagAAALarge(Rgb $color1, Rgb $color2): bool
    {
        return self::contrastRatio($color1, $color2) >= 4.5;
    }

    /**
     * Parse a color string (supports #RGB, #RRGGBB, rgb(), hsl()).
     *
     * @return Result<Rgb>
     */
    public static function parse(string $color): Result
    {
        $color = trim($color);

        // Hex format
        if (str_starts_with($color, '#')) {
            return Rgb::fromHex($color);
        }

        // rgb() format
        if (preg_match('/^rgb\s*\(\s*(\d+)\s*,\s*(\d+)\s*,\s*(\d+)\s*\)$/i', $color, $matches)) {
            return Rgb::new((int)$matches[1], (int)$matches[2], (int)$matches[3]);
        }

        // hsl() format
        if (preg_match('/^hsl\s*\(\s*([\d.]+)\s*,\s*([\d.]+)%?\s*,\s*([\d.]+)%?\s*\)$/i', $color, $matches)) {
            $h = (float)$matches[1];
            $s = (float)$matches[2] / 100;
            $l = (float)$matches[3] / 100;
            return Result::ok(Rgb::fromHsl($h, $s, $l));
        }

        // Named colors
        $named = self::namedColor($color);
        if ($named !== null) {
            return Result::ok($named);
        }

        return Result::err(ProvenError::invalidInput("Unknown color format: $color"));
    }

    /**
     * Get a named CSS color.
     */
    public static function namedColor(string $name): ?Rgb
    {
        $colors = [
            'black' => [0, 0, 0],
            'white' => [255, 255, 255],
            'red' => [255, 0, 0],
            'green' => [0, 128, 0],
            'blue' => [0, 0, 255],
            'yellow' => [255, 255, 0],
            'cyan' => [0, 255, 255],
            'magenta' => [255, 0, 255],
            'gray' => [128, 128, 128],
            'grey' => [128, 128, 128],
            'orange' => [255, 165, 0],
            'pink' => [255, 192, 203],
            'purple' => [128, 0, 128],
            'brown' => [165, 42, 42],
            'navy' => [0, 0, 128],
            'teal' => [0, 128, 128],
            'lime' => [0, 255, 0],
            'aqua' => [0, 255, 255],
            'silver' => [192, 192, 192],
            'maroon' => [128, 0, 0],
            'olive' => [128, 128, 0],
            'fuchsia' => [255, 0, 255],
        ];

        $lower = strtolower($name);
        if (isset($colors[$lower])) {
            [$r, $g, $b] = $colors[$lower];
            return new Rgb($r, $g, $b);
        }

        return null;
    }

    /**
     * Generate a color palette from a base color.
     *
     * @param int $count Number of colors
     * @return array<Rgb>
     */
    public static function generatePalette(Rgb $base, int $count = 5): array
    {
        $hsl = $base->toHsl();
        $palette = [];

        for ($i = 0; $i < $count; $i++) {
            $lightness = ($i + 1) / ($count + 1);
            $palette[] = Rgb::fromHsl($hsl['h'], $hsl['s'], $lightness);
        }

        return $palette;
    }

    /**
     * Generate complementary colors.
     *
     * @return array{primary: Rgb, complementary: Rgb}
     */
    public static function complementary(Rgb $color): array
    {
        $hsl = $color->toHsl();
        $complementaryHue = fmod($hsl['h'] + 180, 360);

        return [
            'primary' => $color,
            'complementary' => Rgb::fromHsl($complementaryHue, $hsl['s'], $hsl['l']),
        ];
    }
}
