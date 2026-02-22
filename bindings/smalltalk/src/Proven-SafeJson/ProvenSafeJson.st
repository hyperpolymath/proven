" SPDX-License-Identifier: PMPL-1.0-or-later
  Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk> "

"
ProvenSafeJson - Safe JSON validation and type detection.

All computation is performed by formally verified Idris 2 code via the
Zig FFI bridge (libproven). This class is a thin wrapper that marshals
Smalltalk strings to the C ABI. No logic is reimplemented here.

JSON type constants:
  0 = null, 1 = bool, 2 = number, 3 = string, 4 = array, 5 = object, -1 = invalid.

Methods return nil on error (Smalltalk idiom).
"

Class {
	#name : #ProvenSafeJson,
	#superclass : #Object,
	#classVars : [
		'TypeNull',
		'TypeBool',
		'TypeNumber',
		'TypeString',
		'TypeArray',
		'TypeObject',
		'TypeInvalid'
	],
	#category : #'Proven-SafeJson'
}

{ #category : #'class initialization' }
ProvenSafeJson class >> initialize [
	"Initialize JSON type constants matching ProvenJsonType enum in libproven."

	TypeNull := 0.
	TypeBool := 1.
	TypeNumber := 2.
	TypeString := 3.
	TypeArray := 4.
	TypeObject := 5.
	TypeInvalid := -1
]

{ #category : #constants }
ProvenSafeJson class >> typeNull [
	^ TypeNull
]

{ #category : #constants }
ProvenSafeJson class >> typeBool [
	^ TypeBool
]

{ #category : #constants }
ProvenSafeJson class >> typeNumber [
	^ TypeNumber
]

{ #category : #constants }
ProvenSafeJson class >> typeString [
	^ TypeString
]

{ #category : #constants }
ProvenSafeJson class >> typeArray [
	^ TypeArray
]

{ #category : #constants }
ProvenSafeJson class >> typeObject [
	^ TypeObject
]

{ #category : #constants }
ProvenSafeJson class >> typeInvalid [
	^ TypeInvalid
]

{ #category : #private }
ProvenSafeJson class >> library [
	"Return the shared library instance."

	^ ProvenLibrary uniqueInstance
]

{ #category : #private }
ProvenSafeJson class >> withUtf8BytesOf: aString do: aBlock [
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
ProvenSafeJson class >> isValid: aJsonString [
	"Check if a string is valid JSON.
	 Delegates to proven_json_is_valid() in libproven.
	 Returns true if valid, false if invalid, or nil on error."

	^ self withUtf8BytesOf: aJsonString do: [ :ptr :len |
		| result |
		result := self library provenJsonIsValid: ptr len: len.
		result valueOrNil ]
]

{ #category : #'type detection' }
ProvenSafeJson class >> getType: aJsonString [
	"Get the JSON value type at root level.
	 Delegates to proven_json_get_type() in libproven.
	 Returns an integer type code (see type* class methods), or -1 for invalid."

	^ self withUtf8BytesOf: aJsonString do: [ :ptr :len |
		self library provenJsonGetType: ptr len: len ]
]

{ #category : #'type detection' }
ProvenSafeJson class >> typeNameFor: aTypeCode [
	"Return a human-readable name for a JSON type code."

	aTypeCode = TypeNull ifTrue: [ ^ 'null' ].
	aTypeCode = TypeBool ifTrue: [ ^ 'boolean' ].
	aTypeCode = TypeNumber ifTrue: [ ^ 'number' ].
	aTypeCode = TypeString ifTrue: [ ^ 'string' ].
	aTypeCode = TypeArray ifTrue: [ ^ 'array' ].
	aTypeCode = TypeObject ifTrue: [ ^ 'object' ].
	^ 'invalid'
]

{ #category : #convenience }
ProvenSafeJson class >> isObject: aJsonString [
	"Check if a JSON string represents an object at root level."

	^ (self getType: aJsonString) = TypeObject
]

{ #category : #convenience }
ProvenSafeJson class >> isArray: aJsonString [
	"Check if a JSON string represents an array at root level."

	^ (self getType: aJsonString) = TypeArray
]
