" SPDX-License-Identifier: PMPL-1.0-or-later
  Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk> "

"
ProvenIntResult - FFI struct for integer result from libproven.

Maps to the C struct:
  typedef struct { int32_t status; int64_t value; } ProvenIntResult;

The status field is 0 (PROVEN_OK) on success, negative on error.
The value field contains the computation result when status is 0.
"

Class {
	#name : #ProvenIntResult,
	#superclass : #FFIExternalStructure,
	#category : #'Proven-FFI'
}

{ #category : #'field definition' }
ProvenIntResult class >> fieldsDesc [
	"Define the C struct layout:
	   int32_t status  -- ProvenStatus code (0 = OK, negative = error)
	   int64_t value   -- The integer result on success"

	^ #(
		int32 status;
		int64 value;
	)
]

{ #category : #accessing }
ProvenIntResult >> status [
	"Return the status code. Zero means success."

	^ self getHandle signedLongAt: 1
]

{ #category : #accessing }
ProvenIntResult >> value [
	"Return the integer value. Only meaningful when status is 0."

	^ self getHandle signedLongLongAt: 5
]

{ #category : #testing }
ProvenIntResult >> isSuccess [
	"Return true if this result indicates success (status = 0)."

	^ self status = 0
]

{ #category : #testing }
ProvenIntResult >> isError [
	"Return true if this result indicates an error (status ~= 0)."

	^ self status ~= 0
]

{ #category : #accessing }
ProvenIntResult >> errorDescription [
	"Return a human-readable error description, or nil if successful."

	self isSuccess ifTrue: [ ^ nil ].
	^ ProvenLibrary descriptionForStatus: self status
]

{ #category : #converting }
ProvenIntResult >> valueOrNil [
	"Return the integer value if successful, or nil on error.
	 This is the idiomatic Smalltalk way to handle fallible results."

	self isSuccess ifTrue: [ ^ self value ].
	^ nil
]

{ #category : #printing }
ProvenIntResult >> printOn: aStream [
	"Print a readable representation of this result."

	aStream nextPutAll: 'ProvenIntResult('.
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
