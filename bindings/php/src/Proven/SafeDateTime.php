<?php
// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

declare(strict_types=1);

namespace Proven;

/**
 * Parsed date components.
 */
readonly class Date
{
    public function __construct(
        /** Year (e.g., 2025) */
        public int $year,
        /** Month (1-12) */
        public int $month,
        /** Day (1-31) */
        public int $day,
    ) {}

    /**
     * Format as ISO 8601 date string.
     */
    public function format(): string
    {
        return sprintf('%04d-%02d-%02d', $this->year, $this->month, $this->day);
    }

    /**
     * Get the day of week (1=Monday, 7=Sunday).
     */
    public function dayOfWeek(): int
    {
        $date = \DateTimeImmutable::createFromFormat('Y-m-d', $this->format());
        if ($date === false) {
            return 1;
        }
        return (int)$date->format('N');
    }

    /**
     * Get the day of year (1-366).
     */
    public function dayOfYear(): int
    {
        $date = \DateTimeImmutable::createFromFormat('Y-m-d', $this->format());
        if ($date === false) {
            return 1;
        }
        return (int)$date->format('z') + 1;
    }

    /**
     * Check if this date is in a leap year.
     */
    public function isLeapYear(): bool
    {
        return SafeDateTime::isLeapYear($this->year);
    }

    public function __toString(): string
    {
        return $this->format();
    }
}

/**
 * Parsed time components.
 */
readonly class Time
{
    public function __construct(
        /** Hour (0-23) */
        public int $hour,
        /** Minute (0-59) */
        public int $minute,
        /** Second (0-59) */
        public int $second,
        /** Nanoseconds (0-999999999) */
        public int $nanos = 0,
    ) {}

    /**
     * Format as ISO 8601 time string.
     */
    public function format(bool $includeNanos = false): string
    {
        $base = sprintf('%02d:%02d:%02d', $this->hour, $this->minute, $this->second);
        if ($includeNanos && $this->nanos > 0) {
            $base .= '.' . sprintf('%09d', $this->nanos);
        }
        return $base;
    }

    /**
     * Get total seconds since midnight.
     */
    public function toSeconds(): int
    {
        return $this->hour * 3600 + $this->minute * 60 + $this->second;
    }

    public function __toString(): string
    {
        return $this->format();
    }
}

/**
 * Combined date and time.
 */
readonly class DateTime
{
    public function __construct(
        public Date $date,
        public Time $time,
        /** Timezone offset in minutes (e.g., -300 for UTC-5) */
        public ?int $tzOffset = null,
    ) {}

    /**
     * Format as ISO 8601 datetime string.
     */
    public function format(): string
    {
        $result = $this->date->format() . 'T' . $this->time->format();

        if ($this->tzOffset !== null) {
            if ($this->tzOffset === 0) {
                $result .= 'Z';
            } else {
                $sign = $this->tzOffset >= 0 ? '+' : '-';
                $absOffset = abs($this->tzOffset);
                $hours = intdiv($absOffset, 60);
                $minutes = $absOffset % 60;
                $result .= sprintf('%s%02d:%02d', $sign, $hours, $minutes);
            }
        }

        return $result;
    }

    /**
     * Convert to Unix timestamp.
     */
    public function toTimestamp(): int
    {
        $dt = new \DateTimeImmutable($this->format());
        return $dt->getTimestamp();
    }

    public function __toString(): string
    {
        return $this->format();
    }
}

/**
 * Safe datetime parsing and validation.
 *
 * Provides ISO 8601 compliant datetime parsing with proper validation.
 */
class SafeDateTime
{
    /**
     * Check if a year is a leap year.
     */
    public static function isLeapYear(int $year): bool
    {
        return ($year % 4 === 0 && $year % 100 !== 0) || ($year % 400 === 0);
    }

    /**
     * Get the number of days in a month.
     *
     * @return Option<int>
     */
    public static function daysInMonth(int $year, int $month): Option
    {
        return match ($month) {
            1, 3, 5, 7, 8, 10, 12 => Option::some(31),
            4, 6, 9, 11 => Option::some(30),
            2 => Option::some(self::isLeapYear($year) ? 29 : 28),
            default => Option::none(),
        };
    }

    /**
     * Validate and create a date.
     *
     * @return Result<Date>
     */
    public static function validateDate(int $year, int $month, int $day): Result
    {
        if ($month < 1 || $month > 12) {
            return Result::err(ProvenError::validationError("Invalid month: $month"));
        }

        $maxDay = self::daysInMonth($year, $month)->unwrapOr(0);
        if ($day < 1 || $day > $maxDay) {
            return Result::err(ProvenError::validationError("Invalid day $day for month $month"));
        }

        return Result::ok(new Date($year, $month, $day));
    }

    /**
     * Validate and create a time.
     *
     * @return Result<Time>
     */
    public static function validateTime(int $hour, int $minute, int $second, int $nanos = 0): Result
    {
        if ($hour < 0 || $hour > 23) {
            return Result::err(ProvenError::validationError("Invalid hour: $hour"));
        }
        if ($minute < 0 || $minute > 59) {
            return Result::err(ProvenError::validationError("Invalid minute: $minute"));
        }
        if ($second < 0 || $second > 59) {
            return Result::err(ProvenError::validationError("Invalid second: $second"));
        }
        if ($nanos < 0 || $nanos > 999999999) {
            return Result::err(ProvenError::validationError("Invalid nanoseconds: $nanos"));
        }

        return Result::ok(new Time($hour, $minute, $second, $nanos));
    }

    /**
     * Parse an ISO 8601 date (YYYY-MM-DD).
     *
     * @return Result<Date>
     */
    public static function parseIsoDate(string $s): Result
    {
        $s = trim($s);
        $parts = explode('-', $s);

        if (count($parts) !== 3) {
            return Result::err(ProvenError::parseError('Invalid date format, expected YYYY-MM-DD'));
        }

        $year = filter_var($parts[0], FILTER_VALIDATE_INT);
        $month = filter_var($parts[1], FILTER_VALIDATE_INT);
        $day = filter_var($parts[2], FILTER_VALIDATE_INT);

        if ($year === false || $month === false || $day === false) {
            return Result::err(ProvenError::parseError('Invalid date components'));
        }

        return self::validateDate($year, $month, $day);
    }

    /**
     * Parse an ISO 8601 time (HH:MM:SS or HH:MM:SS.nnn).
     *
     * @return Result<Time>
     */
    public static function parseIsoTime(string $s): Result
    {
        $s = trim($s);

        // Handle fractional seconds
        $nanos = 0;
        $dotPos = strpos($s, '.');
        if ($dotPos !== false) {
            $fracPart = substr($s, $dotPos + 1);
            $s = substr($s, 0, $dotPos);
            $fracPart = str_pad(substr($fracPart, 0, 9), 9, '0');
            $nanos = (int)$fracPart;
        }

        $parts = explode(':', $s);
        if (count($parts) < 2 || count($parts) > 3) {
            return Result::err(ProvenError::parseError('Invalid time format, expected HH:MM or HH:MM:SS'));
        }

        $hour = filter_var($parts[0], FILTER_VALIDATE_INT);
        $minute = filter_var($parts[1], FILTER_VALIDATE_INT);
        $second = count($parts) > 2 ? filter_var($parts[2], FILTER_VALIDATE_INT) : 0;

        if ($hour === false || $minute === false || $second === false) {
            return Result::err(ProvenError::parseError('Invalid time components'));
        }

        return self::validateTime($hour, $minute, $second, $nanos);
    }

    /**
     * Parse an ISO 8601 datetime (YYYY-MM-DDTHH:MM:SS[.nnn][Z|+HH:MM]).
     *
     * @return Result<DateTime>
     */
    public static function parseIsoDateTime(string $s): Result
    {
        $s = trim($s);

        // Handle timezone
        $tzOffset = null;
        if (str_ends_with($s, 'Z')) {
            $tzOffset = 0;
            $s = substr($s, 0, -1);
        } elseif (preg_match('/([+-])(\d{2}):?(\d{2})$/', $s, $matches)) {
            $sign = $matches[1] === '+' ? 1 : -1;
            $hours = (int)$matches[2];
            $minutes = (int)$matches[3];
            $tzOffset = $sign * ($hours * 60 + $minutes);
            $s = substr($s, 0, -strlen($matches[0]));
        }

        // Split date and time
        $tPos = stripos($s, 'T');
        if ($tPos === false) {
            $tPos = strpos($s, ' ');
        }

        if ($tPos === false) {
            return Result::err(ProvenError::parseError('Invalid datetime format, expected YYYY-MM-DDTHH:MM:SS'));
        }

        $datePart = substr($s, 0, $tPos);
        $timePart = substr($s, $tPos + 1);

        $dateResult = self::parseIsoDate($datePart);
        if ($dateResult->isErr()) {
            return $dateResult;
        }

        $timeResult = self::parseIsoTime($timePart);
        if ($timeResult->isErr()) {
            return $timeResult;
        }

        return Result::ok(new DateTime(
            $dateResult->unwrap(),
            $timeResult->unwrap(),
            $tzOffset,
        ));
    }

    /**
     * Get the current date.
     */
    public static function today(): Date
    {
        $now = new \DateTimeImmutable();
        return new Date(
            (int)$now->format('Y'),
            (int)$now->format('n'),
            (int)$now->format('j'),
        );
    }

    /**
     * Get the current time.
     */
    public static function now(): Time
    {
        $now = new \DateTimeImmutable();
        return new Time(
            (int)$now->format('G'),
            (int)$now->format('i'),
            (int)$now->format('s'),
            (int)$now->format('u') * 1000,
        );
    }

    /**
     * Get the current datetime.
     */
    public static function nowDateTime(): DateTime
    {
        $now = new \DateTimeImmutable();
        return new DateTime(
            self::today(),
            self::now(),
            (int)($now->getOffset() / 60),
        );
    }

    /**
     * Create a DateTime from a Unix timestamp.
     */
    public static function fromTimestamp(int $timestamp, ?int $tzOffset = null): DateTime
    {
        $dt = (new \DateTimeImmutable())->setTimestamp($timestamp);

        return new DateTime(
            new Date(
                (int)$dt->format('Y'),
                (int)$dt->format('n'),
                (int)$dt->format('j'),
            ),
            new Time(
                (int)$dt->format('G'),
                (int)$dt->format('i'),
                (int)$dt->format('s'),
            ),
            $tzOffset,
        );
    }

    /**
     * Add days to a date.
     */
    public static function addDays(Date $date, int $days): Date
    {
        $dt = \DateTimeImmutable::createFromFormat('Y-m-d', $date->format());
        if ($dt === false) {
            return $date;
        }
        $dt = $dt->modify("$days days");
        return new Date(
            (int)$dt->format('Y'),
            (int)$dt->format('n'),
            (int)$dt->format('j'),
        );
    }

    /**
     * Calculate the difference in days between two dates.
     */
    public static function diffDays(Date $from, Date $to): int
    {
        $dtFrom = \DateTimeImmutable::createFromFormat('Y-m-d', $from->format());
        $dtTo = \DateTimeImmutable::createFromFormat('Y-m-d', $to->format());

        if ($dtFrom === false || $dtTo === false) {
            return 0;
        }

        $diff = $dtTo->diff($dtFrom);
        return $diff->invert ? $diff->days : -$diff->days;
    }
}
