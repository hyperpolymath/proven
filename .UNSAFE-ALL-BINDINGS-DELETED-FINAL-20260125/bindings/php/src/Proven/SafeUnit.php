<?php
// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

declare(strict_types=1);

namespace Proven;

/**
 * Length units.
 */
enum LengthUnit: string
{
    case Meter = 'm';
    case Kilometer = 'km';
    case Centimeter = 'cm';
    case Millimeter = 'mm';
    case Mile = 'mi';
    case Yard = 'yd';
    case Foot = 'ft';
    case Inch = 'in';
    case NauticalMile = 'nmi';

    /**
     * Get conversion factor to meters.
     */
    public function toMeters(): float
    {
        return match ($this) {
            self::Meter => 1.0,
            self::Kilometer => 1000.0,
            self::Centimeter => 0.01,
            self::Millimeter => 0.001,
            self::Mile => 1609.344,
            self::Yard => 0.9144,
            self::Foot => 0.3048,
            self::Inch => 0.0254,
            self::NauticalMile => 1852.0,
        };
    }
}

/**
 * Mass units.
 */
enum MassUnit: string
{
    case Kilogram = 'kg';
    case Gram = 'g';
    case Milligram = 'mg';
    case Tonne = 't';
    case Pound = 'lb';
    case Ounce = 'oz';
    case Stone = 'st';

    /**
     * Get conversion factor to kilograms.
     */
    public function toKilograms(): float
    {
        return match ($this) {
            self::Kilogram => 1.0,
            self::Gram => 0.001,
            self::Milligram => 0.000001,
            self::Tonne => 1000.0,
            self::Pound => 0.45359237,
            self::Ounce => 0.028349523125,
            self::Stone => 6.35029318,
        };
    }
}

/**
 * Temperature units.
 */
enum TemperatureUnit: string
{
    case Celsius = 'C';
    case Fahrenheit = 'F';
    case Kelvin = 'K';
}

/**
 * Time units.
 */
enum TimeUnit: string
{
    case Second = 's';
    case Millisecond = 'ms';
    case Microsecond = 'us';
    case Nanosecond = 'ns';
    case Minute = 'min';
    case Hour = 'h';
    case Day = 'd';
    case Week = 'w';

    /**
     * Get conversion factor to seconds.
     */
    public function toSeconds(): float
    {
        return match ($this) {
            self::Second => 1.0,
            self::Millisecond => 0.001,
            self::Microsecond => 0.000001,
            self::Nanosecond => 0.000000001,
            self::Minute => 60.0,
            self::Hour => 3600.0,
            self::Day => 86400.0,
            self::Week => 604800.0,
        };
    }
}

/**
 * Data size units.
 */
enum DataUnit: string
{
    case Byte = 'B';
    case Kilobyte = 'KB';
    case Megabyte = 'MB';
    case Gigabyte = 'GB';
    case Terabyte = 'TB';
    case Petabyte = 'PB';
    case Kibibyte = 'KiB';
    case Mebibyte = 'MiB';
    case Gibibyte = 'GiB';
    case Tebibyte = 'TiB';
    case Pebibyte = 'PiB';

    /**
     * Get conversion factor to bytes.
     */
    public function toBytes(): float
    {
        return match ($this) {
            self::Byte => 1.0,
            self::Kilobyte => 1000.0,
            self::Megabyte => 1000000.0,
            self::Gigabyte => 1000000000.0,
            self::Terabyte => 1000000000000.0,
            self::Petabyte => 1000000000000000.0,
            self::Kibibyte => 1024.0,
            self::Mebibyte => 1048576.0,
            self::Gibibyte => 1073741824.0,
            self::Tebibyte => 1099511627776.0,
            self::Pebibyte => 1125899906842624.0,
        };
    }
}

/**
 * Safe unit conversion operations.
 */
class SafeUnit
{
    /**
     * Convert a length value between units.
     */
    public static function convertLength(float $value, LengthUnit $from, LengthUnit $to): float
    {
        $inMeters = $value * $from->toMeters();
        return $inMeters / $to->toMeters();
    }

    /**
     * Convert a mass value between units.
     */
    public static function convertMass(float $value, MassUnit $from, MassUnit $to): float
    {
        $inKg = $value * $from->toKilograms();
        return $inKg / $to->toKilograms();
    }

    /**
     * Convert a temperature value between units.
     */
    public static function convertTemperature(float $value, TemperatureUnit $from, TemperatureUnit $to): float
    {
        // Convert to Celsius first
        $celsius = match ($from) {
            TemperatureUnit::Celsius => $value,
            TemperatureUnit::Fahrenheit => ($value - 32) * 5 / 9,
            TemperatureUnit::Kelvin => $value - 273.15,
        };

        // Convert from Celsius to target
        return match ($to) {
            TemperatureUnit::Celsius => $celsius,
            TemperatureUnit::Fahrenheit => $celsius * 9 / 5 + 32,
            TemperatureUnit::Kelvin => $celsius + 273.15,
        };
    }

    /**
     * Convert a time value between units.
     */
    public static function convertTime(float $value, TimeUnit $from, TimeUnit $to): float
    {
        $inSeconds = $value * $from->toSeconds();
        return $inSeconds / $to->toSeconds();
    }

    /**
     * Convert a data size value between units.
     */
    public static function convertData(float $value, DataUnit $from, DataUnit $to): float
    {
        $inBytes = $value * $from->toBytes();
        return $inBytes / $to->toBytes();
    }

    /**
     * Format a byte count as human-readable string (binary units).
     */
    public static function formatBytes(float $bytes, int $precision = 2): string
    {
        $units = ['B', 'KiB', 'MiB', 'GiB', 'TiB', 'PiB'];
        $absBytes = abs($bytes);
        $sign = $bytes < 0 ? '-' : '';

        if ($absBytes < 1) {
            return $sign . '0 B';
        }

        $exp = min((int)floor(log($absBytes, 1024)), count($units) - 1);
        $value = $absBytes / pow(1024, $exp);

        return $sign . number_format($value, $precision) . ' ' . $units[$exp];
    }

    /**
     * Format a byte count as human-readable string (SI units).
     */
    public static function formatBytesSI(float $bytes, int $precision = 2): string
    {
        $units = ['B', 'KB', 'MB', 'GB', 'TB', 'PB'];
        $absBytes = abs($bytes);
        $sign = $bytes < 0 ? '-' : '';

        if ($absBytes < 1) {
            return $sign . '0 B';
        }

        $exp = min((int)floor(log($absBytes, 1000)), count($units) - 1);
        $value = $absBytes / pow(1000, $exp);

        return $sign . number_format($value, $precision) . ' ' . $units[$exp];
    }

    /**
     * Format a duration in seconds as human-readable string.
     */
    public static function formatDuration(float $seconds, bool $short = false): string
    {
        $seconds = abs($seconds);

        if ($seconds < 0.001) {
            $value = $seconds * 1000000;
            return number_format($value, 2) . ($short ? 'us' : ' microseconds');
        }
        if ($seconds < 1) {
            $value = $seconds * 1000;
            return number_format($value, 2) . ($short ? 'ms' : ' milliseconds');
        }
        if ($seconds < 60) {
            return number_format($seconds, 2) . ($short ? 's' : ' seconds');
        }
        if ($seconds < 3600) {
            $minutes = $seconds / 60;
            return number_format($minutes, 2) . ($short ? 'min' : ' minutes');
        }
        if ($seconds < 86400) {
            $hours = $seconds / 3600;
            return number_format($hours, 2) . ($short ? 'h' : ' hours');
        }

        $days = $seconds / 86400;
        return number_format($days, 2) . ($short ? 'd' : ' days');
    }

    /**
     * Parse a duration string (e.g., "1h30m", "2d", "500ms").
     *
     * @return Result<float> Duration in seconds
     */
    public static function parseDuration(string $input): Result
    {
        $input = trim(strtolower($input));
        if ($input === '') {
            return Result::err(ProvenError::emptyInput());
        }

        $total = 0.0;
        $pattern = '/(\d+(?:\.\d+)?)\s*(d|h|m|min|s|ms|us|ns)/';

        if (preg_match_all($pattern, $input, $matches, PREG_SET_ORDER) === 0) {
            // Try parsing as plain number (assumed seconds)
            if (is_numeric($input)) {
                return Result::ok((float)$input);
            }
            return Result::err(ProvenError::parseError('Invalid duration format'));
        }

        foreach ($matches as $match) {
            $value = (float)$match[1];
            $unit = $match[2];

            $multiplier = match ($unit) {
                'd' => 86400.0,
                'h' => 3600.0,
                'm', 'min' => 60.0,
                's' => 1.0,
                'ms' => 0.001,
                'us' => 0.000001,
                'ns' => 0.000000001,
                default => null,
            };

            if ($multiplier === null) {
                return Result::err(ProvenError::parseError("Unknown unit: $unit"));
            }

            $total += $value * $multiplier;
        }

        return Result::ok($total);
    }

    /**
     * Parse a byte size string (e.g., "1GB", "500MiB").
     *
     * @return Result<float> Size in bytes
     */
    public static function parseByteSize(string $input): Result
    {
        $input = trim($input);
        if ($input === '') {
            return Result::err(ProvenError::emptyInput());
        }

        $pattern = '/^(\d+(?:\.\d+)?)\s*(B|KB|MB|GB|TB|PB|KiB|MiB|GiB|TiB|PiB)?$/i';
        if (!preg_match($pattern, $input, $matches)) {
            return Result::err(ProvenError::parseError('Invalid byte size format'));
        }

        $value = (float)$matches[1];
        $unit = strtoupper($matches[2] ?? 'B');

        $multiplier = match ($unit) {
            'B' => 1.0,
            'KB' => 1000.0,
            'MB' => 1000000.0,
            'GB' => 1000000000.0,
            'TB' => 1000000000000.0,
            'PB' => 1000000000000000.0,
            'KIB' => 1024.0,
            'MIB' => 1048576.0,
            'GIB' => 1073741824.0,
            'TIB' => 1099511627776.0,
            'PIB' => 1125899906842624.0,
            default => null,
        };

        if ($multiplier === null) {
            return Result::err(ProvenError::parseError("Unknown unit: $unit"));
        }

        return Result::ok($value * $multiplier);
    }
}
