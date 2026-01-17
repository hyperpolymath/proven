// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

package proven

import (
	"time"
)

// DateTime represents a safe date/time wrapper.
type DateTime struct {
	time time.Time
}

// NewDateTime creates a DateTime from time.Time.
func NewDateTime(t time.Time) DateTime {
	return DateTime{time: t}
}

// Now returns the current time as DateTime.
func Now() DateTime {
	return DateTime{time: time.Now()}
}

// NowUTC returns the current UTC time as DateTime.
func NowUTC() DateTime {
	return DateTime{time: time.Now().UTC()}
}

// ParseISO8601 parses an ISO 8601 formatted string.
func ParseISO8601(s string) (DateTime, bool) {
	formats := []string{
		time.RFC3339,
		time.RFC3339Nano,
		"2006-01-02T15:04:05",
		"2006-01-02",
		"2006-01-02 15:04:05",
	}

	for _, format := range formats {
		if t, err := time.Parse(format, s); err == nil {
			return DateTime{time: t}, true
		}
	}
	return DateTime{}, false
}

// ParseRFC3339 parses an RFC 3339 formatted string.
func ParseRFC3339(s string) (DateTime, bool) {
	t, err := time.Parse(time.RFC3339, s)
	if err != nil {
		return DateTime{}, false
	}
	return DateTime{time: t}, true
}

// ToISO8601 formats as ISO 8601.
func (dt DateTime) ToISO8601() string {
	return dt.time.Format(time.RFC3339)
}

// ToRFC3339 formats as RFC 3339.
func (dt DateTime) ToRFC3339() string {
	return dt.time.Format(time.RFC3339)
}

// ToDate formats as YYYY-MM-DD.
func (dt DateTime) ToDate() string {
	return dt.time.Format("2006-01-02")
}

// ToTime formats as HH:MM:SS.
func (dt DateTime) ToTime() string {
	return dt.time.Format("15:04:05")
}

// Year returns the year.
func (dt DateTime) Year() int {
	return dt.time.Year()
}

// Month returns the month (1-12).
func (dt DateTime) Month() int {
	return int(dt.time.Month())
}

// Day returns the day of month (1-31).
func (dt DateTime) Day() int {
	return dt.time.Day()
}

// Hour returns the hour (0-23).
func (dt DateTime) Hour() int {
	return dt.time.Hour()
}

// Minute returns the minute (0-59).
func (dt DateTime) Minute() int {
	return dt.time.Minute()
}

// Second returns the second (0-59).
func (dt DateTime) Second() int {
	return dt.time.Second()
}

// Weekday returns the day of week (0=Sunday).
func (dt DateTime) Weekday() int {
	return int(dt.time.Weekday())
}

// Unix returns Unix timestamp in seconds.
func (dt DateTime) Unix() int64 {
	return dt.time.Unix()
}

// UnixMilli returns Unix timestamp in milliseconds.
func (dt DateTime) UnixMilli() int64 {
	return dt.time.UnixMilli()
}

// AddDays adds days to the datetime.
func (dt DateTime) AddDays(days int) DateTime {
	return DateTime{time: dt.time.AddDate(0, 0, days)}
}

// AddMonths adds months to the datetime.
func (dt DateTime) AddMonths(months int) DateTime {
	return DateTime{time: dt.time.AddDate(0, months, 0)}
}

// AddYears adds years to the datetime.
func (dt DateTime) AddYears(years int) DateTime {
	return DateTime{time: dt.time.AddDate(years, 0, 0)}
}

// AddHours adds hours to the datetime.
func (dt DateTime) AddHours(hours int) DateTime {
	return DateTime{time: dt.time.Add(time.Duration(hours) * time.Hour)}
}

// AddMinutes adds minutes to the datetime.
func (dt DateTime) AddMinutes(minutes int) DateTime {
	return DateTime{time: dt.time.Add(time.Duration(minutes) * time.Minute)}
}

// AddSeconds adds seconds to the datetime.
func (dt DateTime) AddSeconds(seconds int) DateTime {
	return DateTime{time: dt.time.Add(time.Duration(seconds) * time.Second)}
}

// Before checks if this datetime is before another.
func (dt DateTime) Before(other DateTime) bool {
	return dt.time.Before(other.time)
}

// After checks if this datetime is after another.
func (dt DateTime) After(other DateTime) bool {
	return dt.time.After(other.time)
}

// Equal checks if two datetimes are equal.
func (dt DateTime) Equal(other DateTime) bool {
	return dt.time.Equal(other.time)
}

// DiffDays returns difference in days.
func (dt DateTime) DiffDays(other DateTime) int {
	diff := dt.time.Sub(other.time)
	return int(diff.Hours() / 24)
}

// DiffHours returns difference in hours.
func (dt DateTime) DiffHours(other DateTime) int {
	diff := dt.time.Sub(other.time)
	return int(diff.Hours())
}

// ToUTC converts to UTC timezone.
func (dt DateTime) ToUTC() DateTime {
	return DateTime{time: dt.time.UTC()}
}

// ToLocal converts to local timezone.
func (dt DateTime) ToLocal() DateTime {
	return DateTime{time: dt.time.Local()}
}

// Inner returns the underlying time.Time.
func (dt DateTime) Inner() time.Time {
	return dt.time
}

// FromUnix creates DateTime from Unix timestamp.
func FromUnix(seconds int64) DateTime {
	return DateTime{time: time.Unix(seconds, 0)}
}

// FromUnixMilli creates DateTime from Unix milliseconds.
func FromUnixMilli(millis int64) DateTime {
	return DateTime{time: time.UnixMilli(millis)}
}
