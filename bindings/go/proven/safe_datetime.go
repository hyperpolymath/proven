// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

// SafeDateTime provides ISO 8601 date/time handling via the Proven FFI.
// All computation is performed in Idris 2 with formal verification.

package proven

// #include <stdint.h>
// #include <stdbool.h>
// #include <stdlib.h>
import "C"

// ProvenDateTime represents the components of a parsed date/time.
type ProvenDateTime struct {
	Year            int32
	Month           uint8
	Day             uint8
	Hour            uint8
	Minute          uint8
	Second          uint8
	Nanosecond      uint32
	TZOffsetMinutes int16
}

// DateTimeParse parses an ISO 8601 date string.
func DateTimeParse(input string) (*ProvenDateTime, error) {
	cs, length := cString(input)
	defer unsafeFree(cs)

	result := C.proven_datetime_parse(cs, length)
	if int(result.status) != StatusOK {
		return nil, newError(int(result.status))
	}
	return &ProvenDateTime{
		Year:            int32(result.datetime.year),
		Month:           uint8(result.datetime.month),
		Day:             uint8(result.datetime.day),
		Hour:            uint8(result.datetime.hour),
		Minute:          uint8(result.datetime.minute),
		Second:          uint8(result.datetime.second),
		Nanosecond:      uint32(result.datetime.nanosecond),
		TZOffsetMinutes: int16(result.datetime.tz_offset_minutes),
	}, nil
}

// DateTimeFormatISO8601 formats a ProvenDateTime as an ISO 8601 string.
func DateTimeFormatISO8601(dt *ProvenDateTime) (string, error) {
	cDT := C.DateTime{
		year:              C.int32_t(dt.Year),
		month:             C.uint8_t(dt.Month),
		day:               C.uint8_t(dt.Day),
		hour:              C.uint8_t(dt.Hour),
		minute:            C.uint8_t(dt.Minute),
		second:            C.uint8_t(dt.Second),
		nanosecond:        C.uint32_t(dt.Nanosecond),
		tz_offset_minutes: C.int16_t(dt.TZOffsetMinutes),
	}
	return goStringResult(C.proven_datetime_format_iso8601(cDT))
}

// DateTimeIsLeapYear checks whether a year is a leap year.
func DateTimeIsLeapYear(year int32) bool {
	return bool(C.proven_datetime_is_leap_year(C.int32_t(year)))
}

// DateTimeDaysInMonth returns the number of days in a given month.
func DateTimeDaysInMonth(year int32, month uint8) uint8 {
	return uint8(C.proven_datetime_days_in_month(C.int32_t(year), C.uint8_t(month)))
}
