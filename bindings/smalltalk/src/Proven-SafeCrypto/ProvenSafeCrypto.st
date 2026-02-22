" SPDX-License-Identifier: PMPL-1.0-or-later
  Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk> "

"
ProvenSafeCrypto - Safe cryptographic primitives backed by formally verified code.

All computation is performed by formally verified Idris 2 code via the
Zig FFI bridge (libproven). This class is a thin wrapper that marshals
Smalltalk byte arrays to the C ABI. No logic is reimplemented here.

Methods return nil on error (Smalltalk idiom).
"

Class {
	#name : #ProvenSafeCrypto,
	#superclass : #Object,
	#category : #'Proven-SafeCrypto'
}

{ #category : #private }
ProvenSafeCrypto class >> library [
	"Return the shared library instance."

	^ ProvenLibrary uniqueInstance
]

{ #category : #private }
ProvenSafeCrypto class >> withExternalBytesOf: aByteArray do: aBlock [
	"Copy a Smalltalk ByteArray into external memory, pass (pointer, length)
	 to aBlock, and free the external memory on return."

	| externalAddress |
	externalAddress := ExternalAddress allocate: aByteArray size.
	[
		externalAddress replaceFrom: 1 to: aByteArray size with: aByteArray startingAt: 1.
		^ aBlock value: externalAddress value: aByteArray size
	] ensure: [
		externalAddress free
	]
]

{ #category : #comparison }
ProvenSafeCrypto class >> constantTimeEquals: aByteArray with: anotherByteArray [
	"Compare two byte arrays in constant time (prevents timing attacks).
	 Delegates to proven_crypto_constant_time_eq() in libproven.
	 Returns true if equal, false if not, or nil on error.

	 Both arguments must be ByteArrays."

	| ext1 ext2 result |
	ext1 := ExternalAddress allocate: aByteArray size.
	ext2 := ExternalAddress allocate: anotherByteArray size.
	[
		ext1 replaceFrom: 1 to: aByteArray size with: aByteArray startingAt: 1.
		ext2 replaceFrom: 1 to: anotherByteArray size with: anotherByteArray startingAt: 1.
		result := self library
			provenCryptoConstantTimeEq: ext1 len1: aByteArray size
			ptr2: ext2 len2: anotherByteArray size.
		^ result valueOrNil
	] ensure: [
		ext1 free.
		ext2 free
	]
]

{ #category : #generation }
ProvenSafeCrypto class >> randomBytes: anInteger [
	"Generate cryptographically secure random bytes.
	 Delegates to proven_crypto_random_bytes() in libproven.
	 Returns a ByteArray of the requested length, or nil on error.

	 anInteger is the number of random bytes to generate."

	| buffer status result |
	anInteger < 0 ifTrue: [ ^ nil ].
	anInteger = 0 ifTrue: [ ^ ByteArray new ].
	buffer := ExternalAddress allocate: anInteger.
	[
		status := self library provenCryptoRandomBytes: buffer len: anInteger.
		(ProvenLibrary isSuccess: status)
			ifTrue: [
				result := ByteArray new: anInteger.
				1 to: anInteger do: [ :i |
					result at: i put: (buffer unsignedByteAt: i) ].
				^ result ]
			ifFalse: [ ^ nil ]
	] ensure: [
		buffer free
	]
]

{ #category : #'hex encoding' }
ProvenSafeCrypto class >> hexEncode: aByteArray [
	"Encode bytes to lowercase hex string.
	 Delegates to proven_hex_encode() in libproven.
	 Returns a hex String, or nil on error."

	^ self hexEncode: aByteArray uppercase: false
]

{ #category : #'hex encoding' }
ProvenSafeCrypto class >> hexEncode: aByteArray uppercase: aBoolean [
	"Encode bytes to hex string with specified case.
	 Delegates to proven_hex_encode() in libproven.
	 Returns a hex String, or nil on error."

	^ self withExternalBytesOf: aByteArray do: [ :ptr :len |
		| result |
		result := self library provenHexEncode: ptr len: len uppercase: aBoolean.
		result valueOrNil ]
]

{ #category : #'checksum' }
ProvenSafeCrypto class >> crc32: aByteArray [
	"Calculate CRC32 checksum.
	 Delegates to proven_checksum_crc32() in libproven.
	 Returns the CRC32 value as an Integer, or nil on error."

	^ self withExternalBytesOf: aByteArray do: [ :ptr :len |
		| result |
		result := self library provenChecksumCrc32: ptr len: len.
		result valueOrNil ]
]

{ #category : #'checksum' }
ProvenSafeCrypto class >> verifyCrc32: aByteArray expected: anInteger [
	"Verify CRC32 matches expected value.
	 Delegates to proven_checksum_verify_crc32() in libproven.
	 Returns true if match, false if mismatch, or nil on error."

	^ self withExternalBytesOf: aByteArray do: [ :ptr :len |
		| result |
		result := self library provenChecksumVerifyCrc32: ptr len: len expected: anInteger.
		result valueOrNil ]
]
