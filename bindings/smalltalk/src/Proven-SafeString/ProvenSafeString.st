" SPDX-License-Identifier: PMPL-1.0-or-later
  Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk> "

"
ProvenSafeString - Safe string operations that handle encoding safely.

All computation is performed by formally verified Idris 2 code via the
Zig FFI bridge (libproven). This class is a thin wrapper that marshals
Smalltalk strings/byte arrays to the C ABI. No logic is reimplemented here.

String results are automatically copied to Smalltalk Strings and the
C-allocated memory is freed via proven_free_string().

Methods return nil on error (Smalltalk idiom).
"

Class {
	#name : #ProvenSafeString,
	#superclass : #Object,
	#category : #'Proven-SafeString'
}

{ #category : #private }
ProvenSafeString class >> library [
	"Return the shared library instance."

	^ ProvenLibrary uniqueInstance
]

{ #category : #private }
ProvenSafeString class >> withUtf8BytesOf: aString do: aBlock [
	"Convert a Smalltalk String to a null-terminated UTF-8 ByteArray,
	 pin it in external memory, and pass (externalPointer, length) to aBlock.
	 Returns the result of aBlock."

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

{ #category : #validation }
ProvenSafeString class >> isValidUtf8: aByteArrayOrString [
	"Check if bytes are valid UTF-8.
	 Delegates to proven_string_is_valid_utf8() in libproven.
	 Returns true/false, or nil on error.

	 aByteArrayOrString may be a ByteArray or a String (which is converted to
	 UTF-8 bytes before being passed to the C function)."

	| bytes result |
	bytes := aByteArrayOrString isString
		ifTrue: [ aByteArrayOrString utf8Encoded ]
		ifFalse: [ aByteArrayOrString ].
	^ ExternalAddress allocate: bytes size during: [ :addr |
		addr replaceFrom: 1 to: bytes size with: bytes startingAt: 1.
		result := self library provenStringIsValidUtf8: addr len: bytes size.
		result valueOrNil ]
]

{ #category : #escaping }
ProvenSafeString class >> escapeSql: aString [
	"Escape a string for safe SQL interpolation (single quotes).
	 Delegates to proven_string_escape_sql() in libproven.
	 Returns the escaped String, or nil on error.

	 Note: Prefer parameterized queries over string escaping."

	^ self withUtf8BytesOf: aString do: [ :ptr :len |
		| result |
		result := self library provenStringEscapeSql: ptr len: len.
		result valueOrNil ]
]

{ #category : #escaping }
ProvenSafeString class >> escapeHtml: aString [
	"Escape a string for safe HTML insertion (prevents XSS).
	 Delegates to proven_string_escape_html() in libproven.
	 Returns the escaped String, or nil on error."

	^ self withUtf8BytesOf: aString do: [ :ptr :len |
		| result |
		result := self library provenStringEscapeHtml: ptr len: len.
		result valueOrNil ]
]

{ #category : #escaping }
ProvenSafeString class >> escapeJs: aString [
	"Escape a string for safe JavaScript string literal insertion.
	 Delegates to proven_string_escape_js() in libproven.
	 Returns the escaped String, or nil on error."

	^ self withUtf8BytesOf: aString do: [ :ptr :len |
		| result |
		result := self library provenStringEscapeJs: ptr len: len.
		result valueOrNil ]
]
