// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath

package com.hyperpolymath.proven

import java.time.*
import java.time.format.DateTimeFormatter
import java.time.format.DateTimeParseException
import java.time.temporal.ChronoUnit

/**
 * Safe date/time wrapper.
 */
data class SafeDateTime(
    val year: Int,
    val month: Int,
    val day: Int,
    val hour: Int = 0,
    val minute: Int = 0,
    val second: Int = 0,
    val millisecond: Int = 0,
    val timezone: String = "UTC"
) : Comparable<SafeDateTime> {

    /**
     * Convert to epoch milliseconds.
     */
    fun toEpochMillis(): Long {
        val zdt = toZonedDateTime()
        return zdt.toInstant().toEpochMilli()
    }

    /**
     * Convert to Java ZonedDateTime.
     */
    fun toZonedDateTime(): ZonedDateTime {
        val zone = ZoneId.of(timezone)
        return ZonedDateTime.of(year, month, day, hour, minute, second, millisecond * 1_000_000, zone)
    }

    /**
     * Convert to ISO 8601 string.
     */
    fun toISOString(): String {
        return toZonedDateTime().format(DateTimeFormatter.ISO_OFFSET_DATE_TIME)
    }

    /**
     * Add duration.
     */
    fun add(duration: SafeDuration): SafeDateTime {
        val zdt = toZonedDateTime()
            .plusYears(duration.years.toLong())
            .plusMonths(duration.months.toLong())
            .plusDays(duration.days.toLong())
            .plusHours(duration.hours.toLong())
            .plusMinutes(duration.minutes.toLong())
            .plusSeconds(duration.seconds.toLong())
            .plusNanos(duration.milliseconds * 1_000_000L)

        return fromZonedDateTime(zdt)
    }

    /**
     * Subtract duration.
     */
    fun subtract(duration: SafeDuration): SafeDateTime {
        val zdt = toZonedDateTime()
            .minusYears(duration.years.toLong())
            .minusMonths(duration.months.toLong())
            .minusDays(duration.days.toLong())
            .minusHours(duration.hours.toLong())
            .minusMinutes(duration.minutes.toLong())
            .minusSeconds(duration.seconds.toLong())
            .minusNanos(duration.milliseconds * 1_000_000L)

        return fromZonedDateTime(zdt)
    }

    /**
     * Get difference between two dates.
     */
    fun diff(other: SafeDateTime): SafeDuration {
        val millis = this.toEpochMillis() - other.toEpochMillis()
        return SafeDuration.fromMilliseconds(millis)
    }

    /**
     * Convert to different timezone.
     */
    fun inTimezone(tz: String): SafeDateTime {
        val zdt = toZonedDateTime().withZoneSameInstant(ZoneId.of(tz))
        return fromZonedDateTime(zdt)
    }

    /**
     * Start of day.
     */
    fun startOfDay(): SafeDateTime = copy(hour = 0, minute = 0, second = 0, millisecond = 0)

    /**
     * End of day.
     */
    fun endOfDay(): SafeDateTime = copy(hour = 23, minute = 59, second = 59, millisecond = 999)

    /**
     * Get day of week (1 = Monday, 7 = Sunday).
     */
    fun dayOfWeek(): Int = toZonedDateTime().dayOfWeek.value

    /**
     * Get day of year.
     */
    fun dayOfYear(): Int = toZonedDateTime().dayOfYear

    /**
     * Check if in leap year.
     */
    fun isLeapYear(): Boolean = Year.isLeap(year.toLong())

    /**
     * Get week of year.
     */
    fun weekOfYear(): Int = toZonedDateTime().get(java.time.temporal.WeekFields.ISO.weekOfYear())

    override fun compareTo(other: SafeDateTime): Int {
        return toEpochMillis().compareTo(other.toEpochMillis())
    }

    companion object {
        /**
         * Create from epoch milliseconds.
         */
        fun fromEpochMillis(millis: Long, timezone: String = "UTC"): SafeDateTime {
            val instant = Instant.ofEpochMilli(millis)
            val zdt = instant.atZone(ZoneId.of(timezone))
            return fromZonedDateTime(zdt)
        }

        /**
         * Create from ISO 8601 string.
         */
        fun fromISO(iso: String): Result<SafeDateTime> {
            return try {
                val zdt = ZonedDateTime.parse(iso)
                Result.success(fromZonedDateTime(zdt))
            } catch (e: DateTimeParseException) {
                try {
                    val ldt = LocalDateTime.parse(iso)
                    val zdt = ldt.atZone(ZoneId.of("UTC"))
                    Result.success(fromZonedDateTime(zdt))
                } catch (e2: Exception) {
                    Result.failure(IllegalArgumentException("Invalid ISO date: $iso"))
                }
            }
        }

        /**
         * Get current date/time.
         */
        fun now(timezone: String = "UTC"): SafeDateTime {
            return fromZonedDateTime(ZonedDateTime.now(ZoneId.of(timezone)))
        }

        /**
         * Create from ZonedDateTime.
         */
        fun fromZonedDateTime(zdt: ZonedDateTime): SafeDateTime {
            return SafeDateTime(
                year = zdt.year,
                month = zdt.monthValue,
                day = zdt.dayOfMonth,
                hour = zdt.hour,
                minute = zdt.minute,
                second = zdt.second,
                millisecond = zdt.nano / 1_000_000,
                timezone = zdt.zone.id
            )
        }
    }
}

/**
 * Duration representation.
 */
data class SafeDuration(
    val years: Int = 0,
    val months: Int = 0,
    val days: Int = 0,
    val hours: Int = 0,
    val minutes: Int = 0,
    val seconds: Int = 0,
    val milliseconds: Int = 0
) {
    /**
     * Total milliseconds (approximate for years/months).
     */
    fun toMilliseconds(): Long {
        return (years * 365L * 24 * 60 * 60 * 1000) +
                (months * 30L * 24 * 60 * 60 * 1000) +
                (days * 24L * 60 * 60 * 1000) +
                (hours * 60L * 60 * 1000) +
                (minutes * 60L * 1000) +
                (seconds * 1000L) +
                milliseconds
    }

    /**
     * Add two durations.
     */
    operator fun plus(other: SafeDuration): SafeDuration {
        return SafeDuration(
            years = years + other.years,
            months = months + other.months,
            days = days + other.days,
            hours = hours + other.hours,
            minutes = minutes + other.minutes,
            seconds = seconds + other.seconds,
            milliseconds = milliseconds + other.milliseconds
        )
    }

    companion object {
        fun fromMilliseconds(millis: Long): SafeDuration {
            var remaining = millis
            val days = (remaining / (24 * 60 * 60 * 1000)).toInt()
            remaining %= (24 * 60 * 60 * 1000)
            val hours = (remaining / (60 * 60 * 1000)).toInt()
            remaining %= (60 * 60 * 1000)
            val minutes = (remaining / (60 * 1000)).toInt()
            remaining %= (60 * 1000)
            val seconds = (remaining / 1000).toInt()
            remaining %= 1000

            return SafeDuration(
                days = days,
                hours = hours,
                minutes = minutes,
                seconds = seconds,
                milliseconds = remaining.toInt()
            )
        }

        val ZERO = SafeDuration()
        fun ofDays(days: Int) = SafeDuration(days = days)
        fun ofHours(hours: Int) = SafeDuration(hours = hours)
        fun ofMinutes(minutes: Int) = SafeDuration(minutes = minutes)
        fun ofSeconds(seconds: Int) = SafeDuration(seconds = seconds)
        fun ofMilliseconds(ms: Int) = SafeDuration(milliseconds = ms)
    }
}
