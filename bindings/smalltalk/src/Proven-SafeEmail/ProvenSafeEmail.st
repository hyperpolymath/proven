" SPDX-License-Identifier: PMPL-1.0-or-later
  Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk> "

"
ProvenSafeEmail - Safe email validation backed by formally verified code.

All validation is performed by formally verified Idris 2 code via the
Zig FFI bridge (libproven). This class is a thin wrapper that marshals
Smalltalk strings to the C ABI. No logic is reimplemented here.

Methods return nil on error (Smalltalk idiom).
"

Class {
	#name : #ProvenSafeEmail,
	#superclass : #Object,
	#category : #'Proven-SafeEmail'
}

{ #category : #private }
ProvenSafeEmail class >> library [
	"Return the shared library instance."

	^ ProvenLibrary uniqueInstance
]

{ #category : #private }
ProvenSafeEmail class >> withUtf8BytesOf: aString do: aBlock [
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

{ #category : #validation }
ProvenSafeEmail class >> isValid: anEmailString [
	"Validate an email address (RFC 5321 simplified).
	 Delegates to proven_email_is_valid() in libproven.
	 Returns true if valid, false if invalid, or nil on error."

	^ self withUtf8BytesOf: anEmailString do: [ :ptr :len |
		| result |
		result := self library provenEmailIsValid: ptr len: len.
		result valueOrNil ]
]
