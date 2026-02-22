" SPDX-License-Identifier: PMPL-1.0-or-later
  Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk> "

"
ProvenSafeNetwork - Safe network operations (IPv4 parsing and classification).

All computation is performed by formally verified Idris 2 code via the
Zig FFI bridge (libproven). This class is a thin wrapper that marshals
Smalltalk strings to the C ABI. No logic is reimplemented here.

IPv4 addresses are returned as an Array of 4 integers (octets).

Methods return nil on error (Smalltalk idiom).
"

Class {
	#name : #ProvenSafeNetwork,
	#superclass : #Object,
	#category : #'Proven-SafeNetwork'
}

{ #category : #private }
ProvenSafeNetwork class >> library [
	"Return the shared library instance."

	^ ProvenLibrary uniqueInstance
]

{ #category : #private }
ProvenSafeNetwork class >> withUtf8BytesOf: aString do: aBlock [
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
ProvenSafeNetwork class >> parseIpv4Raw: anAddressString [
	"Parse an IPv4 address and return the raw FFI result.
	 Used internally to obtain the ProvenIPv4Address for further operations.
	 Returns the result struct, or nil if parsing failed."

	^ self withUtf8BytesOf: anAddressString do: [ :ptr :len |
		| result status |
		result := self library provenNetworkParseIpv4: ptr len: len.
		status := result getHandle signedLongAt: 1.
		status = 0
			ifTrue: [ result ]
			ifFalse: [ nil ] ]
]

{ #category : #parsing }
ProvenSafeNetwork class >> parseIpv4: anAddressString [
	"Parse an IPv4 address string (e.g. '192.168.1.1').
	 Delegates to proven_network_parse_ipv4() in libproven.
	 Returns an Array of 4 octets (#(192 168 1 1)), or nil on parse failure."

	| result handle |
	result := self parseIpv4Raw: anAddressString.
	result ifNil: [ ^ nil ].

	"ProvenIPv4Result: int32 status (4 bytes), then ProvenIPv4Address (4 bytes: octets[4])"
	handle := result getHandle.
	^ Array
		with: (handle unsignedByteAt: 5)
		with: (handle unsignedByteAt: 6)
		with: (handle unsignedByteAt: 7)
		with: (handle unsignedByteAt: 8)
]

{ #category : #validation }
ProvenSafeNetwork class >> isValidIpv4: anAddressString [
	"Check if a string is a valid IPv4 address.
	 Returns true if valid, false otherwise."

	^ (self parseIpv4: anAddressString) notNil
]

{ #category : #classification }
ProvenSafeNetwork class >> isPrivate: anAddressString [
	"Check if an IPv4 address is in a private range (RFC 1918).
	 Delegates to proven_network_ipv4_is_private() in libproven.
	 Returns true if private (10.x.x.x, 172.16-31.x.x, 192.168.x.x),
	 false if not private, or nil if the address cannot be parsed."

	| result handle addrStruct |
	result := self parseIpv4Raw: anAddressString.
	result ifNil: [ ^ nil ].

	"Create a ProvenIPv4Address struct from the parsed octets.
	 The address bytes are at offset 5 (after the int32 status)."
	handle := result getHandle.
	addrStruct := ExternalAddress allocate: 4.
	[
		addrStruct unsignedByteAt: 1 put: (handle unsignedByteAt: 5).
		addrStruct unsignedByteAt: 2 put: (handle unsignedByteAt: 6).
		addrStruct unsignedByteAt: 3 put: (handle unsignedByteAt: 7).
		addrStruct unsignedByteAt: 4 put: (handle unsignedByteAt: 8).
		^ self library provenNetworkIpv4IsPrivate: addrStruct
	] ensure: [
		addrStruct free
	]
]

{ #category : #classification }
ProvenSafeNetwork class >> isLoopback: anAddressString [
	"Check if an IPv4 address is a loopback address (127.0.0.0/8).
	 Delegates to proven_network_ipv4_is_loopback() in libproven.
	 Returns true if loopback, false otherwise, or nil on parse failure."

	| result handle addrStruct |
	result := self parseIpv4Raw: anAddressString.
	result ifNil: [ ^ nil ].

	handle := result getHandle.
	addrStruct := ExternalAddress allocate: 4.
	[
		addrStruct unsignedByteAt: 1 put: (handle unsignedByteAt: 5).
		addrStruct unsignedByteAt: 2 put: (handle unsignedByteAt: 6).
		addrStruct unsignedByteAt: 3 put: (handle unsignedByteAt: 7).
		addrStruct unsignedByteAt: 4 put: (handle unsignedByteAt: 8).
		^ self library provenNetworkIpv4IsLoopback: addrStruct
	] ensure: [
		addrStruct free
	]
]
