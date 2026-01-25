// SPDX-License-Identifier: PMPL-1.0
// Safe datetime handling for V.
//
// Provides datetime parsing, validation, and manipulation with
// protection against invalid dates and timezone issues.

module proven

import time

// Safe datetime wrapper
pub struct SafeDateTime {
pub:
	year        int
	month       int
	day         int
	hour        int
	minute      int
	second      int
	nanosecond  int
	unix_time   i64
	is_utc      bool
}

// Create a datetime from components with validation
pub fn make_datetime(year int, month int, day int, hour int, minute int, second int) ?SafeDateTime {
	// Validate year range
	if year < 1 || year > 9999 {
		return none
	}

	// Validate month
	if month < 1 || month > 12 {
		return none
	}

	// Validate day based on month
	max_day := days_in_month(year, month)
	if day < 1 || day > max_day {
		return none
	}

	// Validate time components
	if hour < 0 || hour > 23 {
		return none
	}
	if minute < 0 || minute > 59 {
		return none
	}
	if second < 0 || second > 59 {
		return none
	}

	t := time.Time{
		year:   year
		month:  month
		day:    day
		hour:   hour
		minute: minute
		second: second
	}

	return SafeDateTime{
		year:       year
		month:      month
		day:        day
		hour:       hour
		minute:     minute
		second:     second
		nanosecond: 0
		unix_time:  t.unix()
		is_utc:     true
	}
}

// Get current UTC time
pub fn now_utc() SafeDateTime {
	t := time.utc()
	return SafeDateTime{
		year:       t.year
		month:      t.month
		day:        t.day
		hour:       t.hour
		minute:     t.minute
		second:     t.second
		nanosecond: t.nanosecond
		unix_time:  t.unix()
		is_utc:     true
	}
}

// Get current local time
pub fn now_local() SafeDateTime {
	t := time.now()
	return SafeDateTime{
		year:       t.year
		month:      t.month
		day:        t.day
		hour:       t.hour
		minute:     t.minute
		second:     t.second
		nanosecond: t.nanosecond
		unix_time:  t.unix()
		is_utc:     false
	}
}

// Create datetime from Unix timestamp
pub fn from_unix(timestamp i64) ?SafeDateTime {
	// Validate reasonable range
	if timestamp < -62135596800 || timestamp > 253402300799 {
		return none
	}

	t := time.unix(timestamp)
	return SafeDateTime{
		year:       t.year
		month:      t.month
		day:        t.day
		hour:       t.hour
		minute:     t.minute
		second:     t.second
		nanosecond: 0
		unix_time:  timestamp
		is_utc:     true
	}
}

// Parse ISO 8601 datetime string
pub fn parse_iso8601(s string) ?SafeDateTime {
	if s.len < 10 {
		return none
	}

	// Basic format: YYYY-MM-DD or YYYY-MM-DDTHH:MM:SS
	year := s[0..4].int()
	if s[4] != `-` {
		return none
	}
	month := s[5..7].int()
	if s[7] != `-` {
		return none
	}
	day := s[8..10].int()

	mut hour := 0
	mut minute := 0
	mut second := 0

	if s.len >= 19 && (s[10] == `T` || s[10] == ` `) {
		hour = s[11..13].int()
		if s[13] != `:` {
			return none
		}
		minute = s[14..16].int()
		if s[16] != `:` {
			return none
		}
		second = s[17..19].int()
	}

	return make_datetime(year, month, day, hour, minute, second)
}

// Format datetime to ISO 8601
pub fn format_iso8601(dt SafeDateTime) string {
	suffix := if dt.is_utc { 'Z' } else { '' }
	return '${dt.year:04d}-${dt.month:02d}-${dt.day:02d}T${dt.hour:02d}:${dt.minute:02d}:${dt.second:02d}${suffix}'
}

// Format datetime to RFC 2822
pub fn format_rfc2822(dt SafeDateTime) string {
	days := ['Sun', 'Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat']
	months := ['Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec']

	dow := day_of_week(dt.year, dt.month, dt.day)
	tz := if dt.is_utc { '+0000' } else { '+0000' }

	return '${days[dow]}, ${dt.day:02d} ${months[dt.month - 1]} ${dt.year} ${dt.hour:02d}:${dt.minute:02d}:${dt.second:02d} ${tz}'
}

// Get days in month
pub fn days_in_month(year int, month int) int {
	days := [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]

	if month < 1 || month > 12 {
		return 0
	}

	if month == 2 && is_leap_year(year) {
		return 29
	}

	return days[month - 1]
}

// Check if year is a leap year
pub fn is_leap_year(year int) bool {
	return (year % 4 == 0 && year % 100 != 0) || (year % 400 == 0)
}

// Get day of week (0 = Sunday)
pub fn day_of_week(year int, month int, day int) int {
	// Zeller's congruence
	mut m := month
	mut y := year

	if m < 3 {
		m += 12
		y -= 1
	}

	k := y % 100
	j := y / 100

	h := (day + (13 * (m + 1)) / 5 + k + k / 4 + j / 4 - 2 * j) % 7
	return ((h + 6) % 7)
}

// Add days to datetime
pub fn add_days(dt SafeDateTime, days int) ?SafeDateTime {
	new_timestamp := dt.unix_time + i64(days) * 86400
	return from_unix(new_timestamp)
}

// Add hours to datetime
pub fn add_hours(dt SafeDateTime, hours int) ?SafeDateTime {
	new_timestamp := dt.unix_time + i64(hours) * 3600
	return from_unix(new_timestamp)
}

// Add minutes to datetime
pub fn add_minutes(dt SafeDateTime, minutes int) ?SafeDateTime {
	new_timestamp := dt.unix_time + i64(minutes) * 60
	return from_unix(new_timestamp)
}

// Add seconds to datetime
pub fn add_seconds(dt SafeDateTime, seconds int) ?SafeDateTime {
	new_timestamp := dt.unix_time + i64(seconds)
	return from_unix(new_timestamp)
}

// Calculate difference in seconds between two datetimes
pub fn diff_seconds(dt1 SafeDateTime, dt2 SafeDateTime) i64 {
	return dt1.unix_time - dt2.unix_time
}

// Calculate difference in days between two datetimes
pub fn diff_days(dt1 SafeDateTime, dt2 SafeDateTime) i64 {
	return diff_seconds(dt1, dt2) / 86400
}

// Check if datetime is in the past
pub fn is_past(dt SafeDateTime) bool {
	now := now_utc()
	return dt.unix_time < now.unix_time
}

// Check if datetime is in the future
pub fn is_future(dt SafeDateTime) bool {
	now := now_utc()
	return dt.unix_time > now.unix_time
}

// Check if two datetimes are on the same day
pub fn same_day(dt1 SafeDateTime, dt2 SafeDateTime) bool {
	return dt1.year == dt2.year && dt1.month == dt2.month && dt1.day == dt2.day
}

// Get start of day (midnight)
pub fn start_of_day(dt SafeDateTime) ?SafeDateTime {
	return make_datetime(dt.year, dt.month, dt.day, 0, 0, 0)
}

// Get end of day (23:59:59)
pub fn end_of_day(dt SafeDateTime) ?SafeDateTime {
	return make_datetime(dt.year, dt.month, dt.day, 23, 59, 59)
}

// Get start of month
pub fn start_of_month(dt SafeDateTime) ?SafeDateTime {
	return make_datetime(dt.year, dt.month, 1, 0, 0, 0)
}

// Get end of month
pub fn end_of_month(dt SafeDateTime) ?SafeDateTime {
	last_day := days_in_month(dt.year, dt.month)
	return make_datetime(dt.year, dt.month, last_day, 23, 59, 59)
}

// Duration representation
pub struct Duration {
pub:
	days    i64
	hours   i64
	minutes i64
	seconds i64
}

// Calculate duration between two datetimes
pub fn duration_between(start SafeDateTime, end SafeDateTime) Duration {
	mut total_seconds := end.unix_time - start.unix_time

	if total_seconds < 0 {
		total_seconds = -total_seconds
	}

	days := total_seconds / 86400
	total_seconds %= 86400

	hours := total_seconds / 3600
	total_seconds %= 3600

	minutes := total_seconds / 60
	seconds := total_seconds % 60

	return Duration{
		days:    days
		hours:   hours
		minutes: minutes
		seconds: seconds
	}
}

// Format duration as string
pub fn format_duration(d Duration) string {
	if d.days > 0 {
		return '${d.days}d ${d.hours}h ${d.minutes}m ${d.seconds}s'
	}
	if d.hours > 0 {
		return '${d.hours}h ${d.minutes}m ${d.seconds}s'
	}
	if d.minutes > 0 {
		return '${d.minutes}m ${d.seconds}s'
	}
	return '${d.seconds}s'
}
