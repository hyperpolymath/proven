" SPDX-License-Identifier: PMPL-1.0-or-later
  Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk> "

"
ProvenSafeUrl - Safe URL parsing and validation backed by formally verified code.

All parsing is performed by formally verified Idris 2 code via the
Zig FFI bridge (libproven). This class is a thin wrapper that marshals
Smalltalk strings to the C ABI. No logic is reimplemented here.

Parsed URL components are returned as a Dictionary with keys:
  #scheme, #host, #port, #path, #query, #fragment.
Port is an Integer or nil. Query and fragment may be nil.

Memory for ProvenUrlComponents is freed via proven_url_free() after
extracting the components into Smalltalk objects.

Methods return nil on error (Smalltalk idiom).
"

Class {
	#name : #ProvenSafeUrl,
	#superclass : #Object,
	#category : #'Proven-SafeUrl'
}

{ #category : #private }
ProvenSafeUrl class >> library [
	"Return the shared library instance."

	^ ProvenLibrary uniqueInstance
]

{ #category : #private }
ProvenSafeUrl class >> withUtf8BytesOf: aString do: aBlock [
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

{ #category : #private }
ProvenSafeUrl class >> extractStringFrom: pointer length: length [
	"Extract a Smalltalk String from a C string pointer and length.
	 Returns nil if the pointer is null or length is 0."

	(pointer isNil or: [ pointer isNull ]) ifTrue: [ ^ nil ].
	length = 0 ifTrue: [ ^ nil ].
	^ (pointer copyFrom: 1 to: length) utf8Decoded
]

{ #category : #parsing }
ProvenSafeUrl class >> parse: aUrlString [
	"Parse a URL string into its components.
	 Delegates to proven_url_parse() in libproven.
	 Returns a Dictionary with keys #scheme, #host, #port, #path, #query, #fragment,
	 or nil on parse failure.

	 The C-allocated URL components are freed via proven_url_free() after extraction."

	^ self withUtf8BytesOf: aUrlString do: [ :ptr :len |
		| result status components |
		result := self library provenUrlParse: ptr len: len.
		status := result getHandle signedLongAt: 1.
		status = 0
			ifFalse: [ nil ]
			ifTrue: [
				"Extract components from the FFI result struct.
				 The ProvenUrlResult has: int32 status, then ProvenUrlComponents inline.
				 ProvenUrlComponents contains pairs of (char* ptr, size_t len) for each field,
				 plus uint16 port and bool has_port.
				 We use the raw handle offsets to read the fields."
				| handle ptrSize dict schemePtr schemeLen hostPtr hostLen
				  port hasPort pathPtr pathLen queryPtr queryLen fragPtr fragLen
				  offset |
				handle := result getHandle.
				ptrSize := FFIExternalType pointerSize.

				"Skip past status (4 bytes) to reach ProvenUrlComponents"
				offset := 5.

				"scheme: char* + size_t"
				schemePtr := handle pointerAt: offset.
				offset := offset + ptrSize.
				schemeLen := handle platformSizeTAt: offset.
				offset := offset + ptrSize.

				"host: char* + size_t"
				hostPtr := handle pointerAt: offset.
				offset := offset + ptrSize.
				hostLen := handle platformSizeTAt: offset.
				offset := offset + ptrSize.

				"port: uint16 + bool has_port"
				port := handle unsignedShortAt: offset.
				offset := offset + 2.
				hasPort := (handle unsignedByteAt: offset) ~= 0.
				offset := offset + 1.

				"path: char* + size_t"
				pathPtr := handle pointerAt: offset.
				offset := offset + ptrSize.
				pathLen := handle platformSizeTAt: offset.
				offset := offset + ptrSize.

				"query: char* + size_t"
				queryPtr := handle pointerAt: offset.
				offset := offset + ptrSize.
				queryLen := handle platformSizeTAt: offset.
				offset := offset + ptrSize.

				"fragment: char* + size_t"
				fragPtr := handle pointerAt: offset.
				offset := offset + ptrSize.
				fragLen := handle platformSizeTAt: offset.

				dict := Dictionary new.
				dict at: #scheme put: (self extractStringFrom: schemePtr length: schemeLen).
				dict at: #host put: (self extractStringFrom: hostPtr length: hostLen).
				dict at: #port put: (hasPort ifTrue: [ port ] ifFalse: [ nil ]).
				dict at: #path put: (self extractStringFrom: pathPtr length: pathLen).
				dict at: #query put: (self extractStringFrom: queryPtr length: queryLen).
				dict at: #fragment put: (self extractStringFrom: fragPtr length: fragLen).

				"Free the C-allocated URL components.
				 We pass the address of the components portion (after status)."
				self library provenUrlFree: (result getHandle + 4).

				dict ] ]
]

{ #category : #validation }
ProvenSafeUrl class >> isValid: aUrlString [
	"Check if a URL string is valid (can be parsed).
	 Returns true if valid, false otherwise."

	^ (self parse: aUrlString) notNil
]

{ #category : #convenience }
ProvenSafeUrl class >> hostOf: aUrlString [
	"Extract the host component from a URL.
	 Returns the host String, or nil on parse failure."

	| parsed |
	parsed := self parse: aUrlString.
	parsed ifNil: [ ^ nil ].
	^ parsed at: #host
]

{ #category : #convenience }
ProvenSafeUrl class >> schemeOf: aUrlString [
	"Extract the scheme component from a URL.
	 Returns the scheme String, or nil on parse failure."

	| parsed |
	parsed := self parse: aUrlString.
	parsed ifNil: [ ^ nil ].
	^ parsed at: #scheme
]

{ #category : #convenience }
ProvenSafeUrl class >> isHttps: aUrlString [
	"Check if a URL uses HTTPS.
	 Returns true if the scheme is 'https', false otherwise."

	| scheme |
	scheme := self schemeOf: aUrlString.
	scheme ifNil: [ ^ false ].
	^ scheme asLowercase = 'https'
]
