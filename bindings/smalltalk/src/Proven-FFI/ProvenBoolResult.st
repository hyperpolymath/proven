" SPDX-License-Identifier: PMPL-1.0-or-later
  Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk> "

"
ProvenBoolResult - FFI struct for boolean result from libproven.

Maps to the C struct:
  typedef struct { int32_t status; bool value; } ProvenBoolResult;

The status field is 0 (PROVEN_OK) on success, negative on error.
The value field contains the boolean result when status is 0.

Note: C bool is typically 1 byte. Struct packing means value is at offset 4.
"

Class {
	#name : #ProvenBoolResult,
	#superclass : #FFIExternalStructure,
	#category : #'Proven-FFI'
}

{ #category : #'field definition' }
ProvenBoolResult class >> fieldsDesc [
	"Define the C struct layout:
	   int32_t status  -- ProvenStatus code (0 = OK, negative = error)
	   bool    value   -- The boolean result on success"

	^ #(
		int32 status;
		bool value;
	)
]

{ #category : #accessing }
ProvenBoolResult >> status [
	"Return the status code. Zero means success."

	^ self getHandle signedLongAt: 1
]

{ #category : #accessing }
ProvenBoolResult >> value [
	"Return the boolean value. Only meaningful when status is 0."

	^ (self getHandle unsignedByteAt: 5) ~= 0
]

{ #category : #testing }
ProvenBoolResult >> isSuccess [
	"Return true if this result indicates success (status = 0)."

	^ self status = 0
]

{ #category : #testing }
ProvenBoolResult >> isError [
	"Return true if this result indicates an error (status ~= 0)."

	^ self status ~= 0
]

{ #category : #accessing }
ProvenBoolResult >> errorDescription [
	"Return a human-readable error description, or nil if successful."

	self isSuccess ifTrue: [ ^ nil ].
	^ ProvenLibrary descriptionForStatus: self status
]

{ #category : #converting }
ProvenBoolResult >> valueOrNil [
	"Return the boolean value if successful, or nil on error.
	 This is the idiomatic Smalltalk way to handle fallible results."

	self isSuccess ifTrue: [ ^ self value ].
	^ nil
]

{ #category : #printing }
ProvenBoolResult >> printOn: aStream [
	"Print a readable representation of this result."

	aStream nextPutAll: 'ProvenBoolResult('.
	self isSuccess
		ifTrue: [
			aStream
				nextPutAll: 'ok: ';
				print: self value ]
		ifFalse: [
			aStream
				nextPutAll: 'error: ';
				nextPutAll: self errorDescription ].
	aStream nextPut: $)
]
