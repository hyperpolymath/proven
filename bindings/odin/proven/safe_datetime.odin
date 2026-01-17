// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

package proven

import "core:strings"
import "core:strconv"
import "core:fmt"

// Parsed date components.
Date :: struct {
    year:  i32,
    month: u8,
    day:   u8,
}

// Parsed time components.
Time :: struct {
    hour:   u8,
    minute: u8,
    second: u8,
    nanos:  u32,
}

// Combined datetime.
DateTime :: struct {
    date: Date,
    time: Time,
}

// Check if a year is a leap year.
is_leap_year :: proc(year: i32) -> bool {
    return (year % 4 == 0 && year % 100 != 0) || (year % 400 == 0)
}

// Get days in a month.
days_in_month :: proc(year: i32, month: u8) -> (days: u8, ok: bool) {
    switch month {
    case 1, 3, 5, 7, 8, 10, 12:
        return 31, true
    case 4, 6, 9, 11:
        return 30, true
    case 2:
        return is_leap_year(year) ? 29 : 28, true
    case:
        return 0, false
    }
}

// Validate a date.
validate_date :: proc(year: i32, month, day: u8) -> (date: Date, ok: bool) {
    if month < 1 || month > 12 {
        return {}, false
    }

    max_day, _ := days_in_month(year, month)
    if day < 1 || day > max_day {
        return {}, false
    }

    return Date{year = year, month = month, day = day}, true
}

// Validate a time.
validate_time :: proc(hour, minute, second: u8) -> (time: Time, ok: bool) {
    if hour > 23 {
        return {}, false
    }
    if minute > 59 {
        return {}, false
    }
    if second > 59 {
        return {}, false
    }

    return Time{hour = hour, minute = minute, second = second, nanos = 0}, true
}

// Parse ISO 8601 date (YYYY-MM-DD).
parse_iso_date :: proc(s: string) -> (date: Date, ok: bool) {
    parts := strings.split(s, "-")
    defer delete(parts)

    if len(parts) != 3 {
        return {}, false
    }

    year, year_ok := strconv.parse_int(parts[0])
    if !year_ok {
        return {}, false
    }

    month, month_ok := strconv.parse_int(parts[1])
    if !month_ok || month < 1 || month > 12 {
        return {}, false
    }

    day, day_ok := strconv.parse_int(parts[2])
    if !day_ok || day < 1 || day > 31 {
        return {}, false
    }

    return validate_date(i32(year), u8(month), u8(day))
}

// Parse ISO 8601 time (HH:MM:SS or HH:MM).
parse_iso_time :: proc(s: string) -> (time: Time, ok: bool) {
    parts := strings.split(s, ":")
    defer delete(parts)

    if len(parts) < 2 || len(parts) > 3 {
        return {}, false
    }

    hour, hour_ok := strconv.parse_int(parts[0])
    if !hour_ok || hour < 0 || hour > 23 {
        return {}, false
    }

    minute, minute_ok := strconv.parse_int(parts[1])
    if !minute_ok || minute < 0 || minute > 59 {
        return {}, false
    }

    second: int = 0
    if len(parts) > 2 {
        // Handle optional fractional seconds
        sec_part := parts[2]
        dot_idx := strings.index(sec_part, ".")
        if dot_idx >= 0 {
            sec_part = sec_part[:dot_idx]
        }
        second_val, second_ok := strconv.parse_int(sec_part)
        if !second_ok || second_val < 0 || second_val > 59 {
            return {}, false
        }
        second = second_val
    }

    return validate_time(u8(hour), u8(minute), u8(second))
}

// Parse ISO 8601 datetime (YYYY-MM-DDTHH:MM:SS).
parse_iso_datetime :: proc(s: string) -> (dt: DateTime, ok: bool) {
    // Handle both 'T' and space separators
    t_idx := strings.index(s, "T")
    if t_idx < 0 {
        t_idx = strings.index(s, " ")
    }

    if t_idx < 0 {
        // Date only
        date, date_ok := parse_iso_date(s)
        if !date_ok {
            return {}, false
        }
        return DateTime{date = date, time = {}}, true
    }

    date_part := s[:t_idx]
    time_part := s[t_idx + 1:]

    // Remove timezone suffix if present
    plus_idx := strings.index(time_part, "+")
    if plus_idx >= 0 {
        time_part = time_part[:plus_idx]
    }
    z_idx := strings.index(time_part, "Z")
    if z_idx >= 0 {
        time_part = time_part[:z_idx]
    }

    date, date_ok := parse_iso_date(date_part)
    if !date_ok {
        return {}, false
    }

    time, time_ok := parse_iso_time(time_part)
    if !time_ok {
        return {}, false
    }

    return DateTime{date = date, time = time}, true
}

// Format date as ISO 8601.
format_iso_date :: proc(date: Date, allocator := context.allocator) -> string {
    return fmt.aprintf("%04d-%02d-%02d", date.year, date.month, date.day)
}

// Format time as ISO 8601.
format_iso_time :: proc(time: Time, allocator := context.allocator) -> string {
    return fmt.aprintf("%02d:%02d:%02d", time.hour, time.minute, time.second)
}

// Format datetime as ISO 8601.
format_iso_datetime :: proc(dt: DateTime, allocator := context.allocator) -> string {
    return fmt.aprintf("%04d-%02d-%02dT%02d:%02d:%02d",
        dt.date.year, dt.date.month, dt.date.day,
        dt.time.hour, dt.time.minute, dt.time.second)
}

// Compare two dates.
// Returns -1 if a < b, 0 if a == b, 1 if a > b.
compare_dates :: proc(a, b: Date) -> int {
    if a.year < b.year { return -1 }
    if a.year > b.year { return 1 }
    if a.month < b.month { return -1 }
    if a.month > b.month { return 1 }
    if a.day < b.day { return -1 }
    if a.day > b.day { return 1 }
    return 0
}

// Compare two times.
// Returns -1 if a < b, 0 if a == b, 1 if a > b.
compare_times :: proc(a, b: Time) -> int {
    if a.hour < b.hour { return -1 }
    if a.hour > b.hour { return 1 }
    if a.minute < b.minute { return -1 }
    if a.minute > b.minute { return 1 }
    if a.second < b.second { return -1 }
    if a.second > b.second { return 1 }
    if a.nanos < b.nanos { return -1 }
    if a.nanos > b.nanos { return 1 }
    return 0
}

// Compare two datetimes.
compare_datetimes :: proc(a, b: DateTime) -> int {
    date_cmp := compare_dates(a.date, b.date)
    if date_cmp != 0 {
        return date_cmp
    }
    return compare_times(a.time, b.time)
}

// Get the day of the week (0 = Sunday, 6 = Saturday).
// Uses Zeller's congruence.
day_of_week :: proc(date: Date) -> int {
    y := int(date.year)
    m := int(date.month)
    d := int(date.day)

    if m < 3 {
        m += 12
        y -= 1
    }

    k := y % 100
    j := y / 100

    h := (d + (13 * (m + 1)) / 5 + k + k / 4 + j / 4 - 2 * j) % 7
    return ((h + 6) % 7)  // Convert to 0 = Sunday
}

// Check if a date is a weekend.
is_weekend :: proc(date: Date) -> bool {
    dow := day_of_week(date)
    return dow == 0 || dow == 6
}

// Add days to a date.
add_days :: proc(date: Date, days: int) -> Date {
    // Simple implementation - add one day at a time
    result := date
    remaining := days

    for remaining > 0 {
        max_day, _ := days_in_month(result.year, result.month)
        if result.day < max_day {
            result.day += 1
        } else {
            result.day = 1
            if result.month < 12 {
                result.month += 1
            } else {
                result.month = 1
                result.year += 1
            }
        }
        remaining -= 1
    }

    for remaining < 0 {
        if result.day > 1 {
            result.day -= 1
        } else {
            if result.month > 1 {
                result.month -= 1
            } else {
                result.month = 12
                result.year -= 1
            }
            max_day, _ := days_in_month(result.year, result.month)
            result.day = max_day
        }
        remaining += 1
    }

    return result
}

// Days between two dates.
days_between :: proc(a, b: Date) -> int {
    // Simple implementation - count days
    count := 0
    current := a
    target := b
    forward := compare_dates(a, b) < 0

    for compare_dates(current, target) != 0 {
        if forward {
            current = add_days(current, 1)
            count += 1
        } else {
            current = add_days(current, -1)
            count -= 1
        }
    }

    return count
}
