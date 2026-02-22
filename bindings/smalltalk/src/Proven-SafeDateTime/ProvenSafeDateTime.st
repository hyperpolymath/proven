" SPDX-License-Identifier: PMPL-1.0-or-later
  Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk> "

"
ProvenSafeDateTime - Safe ISO 8601 date/time handling.

All computation is performed by formally verified Idris 2 code via the
Zig FFI bridge (libproven). This class is a thin wrapper that marshals
Smalltalk strings and integers to the C ABI. No logic is reimplemented here.

Parsed datetimes are returned as a Dictionary with keys:
  #year, #month, #day, #hour, #minute, #second, #nanosecond, #tzOffsetMinutes.

Methods return nil on error (Smalltalk idiom).
"

Class {
	#name : #ProvenSafeDateTime,
	#superclass : #Object,
	#category : #'Proven-SafeDateTime'
}

{ #category : #private }
ProvenSafeDateTime class >> library [
	"Return the shared library instance."

	^ ProvenLibrary uniqueInstance
]

{ #category : #private }
ProvenSafeDateTime class >> withUtf8BytesOf: aString do: aBlock [
	"Convert a Smalltalk String to UTF-8 bytes in external memory and
	 pass (pointer, length) to aBlock. Frees external memory on return."

	| utf8Bytes externalAddress |
	utf8Bytes := aString utf8Encoded.
	externalAddress := ExternalAddress allocate: utf8Bytes size.
	[
		externalAddress replaceFrom: 1 to: utf8Bytes size with: utf8Bytes startingAt: 1.
		^ aBlock value: externalAddress value: utf8Bytes size
	] ensure: [
		externalAddress free
	]
]

{ #category : #parsing }
ProvenSafeDateTime class >> parse: anIso8601String [
	"Parse an ISO 8601 date/time string.
	 Delegates to proven_datetime_parse() in libproven.

	 Supports formats: YYYY-MM-DD, YYYY-MM-DDTHH:MM:SS,
	 YYYY-MM-DDTHH:MM:SSZ, YYYY-MM-DDTHH:MM:SS+HH:MM.

	 Returns a Dictionary with keys:
	   #year (Integer), #month (1-12), #day (1-31),
	   #hour (0-23), #minute (0-59), #second (0-59),
	   #nanosecond (Integer), #tzOffsetMinutes (Integer).
	 Returns nil on parse failure."

	^ self withUtf8BytesOf: anIso8601String do: [ :ptr :len |
		| result handle status dict offset |
		result := self library provenDatetimeParse: ptr len: len.
		handle := result getHandle.
		status := handle signedLongAt: 1.
		status = 0 ifFalse: [ nil ] ifTrue: [
			"ProvenDateTimeResult layout:
			   int32  status       (4 bytes, offset 1)
			   ProvenDateTime:
			     int32  year       (4 bytes, offset 5)
			     uint8  month      (1 byte,  offset 9)
			     uint8  day        (1 byte,  offset 10)
			     uint8  hour       (1 byte,  offset 11)
			     uint8  minute     (1 byte,  offset 12)
			     uint8  second     (1 byte,  offset 13)
			     uint32 nanosecond (4 bytes, offset 14 -- may have padding)
			     int16  tz_offset  (2 bytes)"
			dict := Dictionary new.
			dict at: #year put: (handle signedLongAt: 5).
			dict at: #month put: (handle unsignedByteAt: 9).
			dict at: #day put: (handle unsignedByteAt: 10).
			dict at: #hour put: (handle unsignedByteAt: 11).
			dict at: #minute put: (handle unsignedByteAt: 12).
			dict at: #second put: (handle unsignedByteAt: 13).
			"Nanosecond may be at offset 16 due to alignment padding after 3 uint8s"
			dict at: #nanosecond put: (handle unsignedLongAt: 16).
			"tz_offset_minutes (int16) follows the uint32 nanosecond"
			dict at: #tzOffsetMinutes put: (handle signedShortAt: 20).
			dict ] ]
]

{ #category : #'leap year' }
ProvenSafeDateTime class >> isLeapYear: aYear [
	"Check if a year is a leap year.
	 Delegates to proven_datetime_is_leap_year() in libproven.
	 Returns true or false."

	^ self library provenDatetimeIsLeapYear: aYear
]

{ #category : #'days in month' }
ProvenSafeDateTime class >> daysInMonth: aMonth year: aYear [
	"Get the number of days in a given month.
	 Delegates to proven_datetime_days_in_month() in libproven.
	 Returns an Integer (1-31), or 0 if month is invalid.

	 aMonth is 1-12, aYear is the full year (e.g. 2026)."

	^ self library provenDatetimeDaysInMonth: aYear month: aMonth
]

{ #category : #formatting }
ProvenSafeDateTime class >> formatIso8601: aDateTimeDictionary [
	"Format a DateTime dictionary as an ISO 8601 string.
	 Delegates to proven_datetime_format_iso8601() in libproven.

	 aDateTimeDictionary should contain keys:
	   #year, #month, #day, #hour, #minute, #second, #nanosecond, #tzOffsetMinutes.

	 Returns a String, or nil on error.
	 The C-allocated string is freed automatically after copying."

	| dtStruct result handle |
	"Build the ProvenDateTime struct in external memory.
	 Layout must match the C struct exactly."
	dtStruct := ExternalAddress allocate: 24.
	[
		handle := dtStruct.
		handle signedLongAt: 1 put: (aDateTimeDictionary at: #year).
		handle unsignedByteAt: 5 put: (aDateTimeDictionary at: #month).
		handle unsignedByteAt: 6 put: (aDateTimeDictionary at: #day).
		handle unsignedByteAt: 7 put: (aDateTimeDictionary at: #hour).
		handle unsignedByteAt: 8 put: (aDateTimeDictionary at: #minute).
		handle unsignedByteAt: 9 put: (aDateTimeDictionary at: #second).
		"Padding byte at offset 10-11 may exist; write nanosecond at aligned offset 12"
		handle unsignedLongAt: 12 put: (aDateTimeDictionary at: #nanosecond ifAbsent: [ 0 ]).
		handle signedShortAt: 16 put: (aDateTimeDictionary at: #tzOffsetMinutes ifAbsent: [ 0 ]).
		result := self library provenDatetimeFormatIso8601: dtStruct.
		^ result valueOrNil
	] ensure: [
		dtStruct free
	]
]
