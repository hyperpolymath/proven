" SPDX-License-Identifier: PMPL-1.0-or-later
  Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk> "

"
ProvenFloatResult - FFI struct for floating-point result from libproven.

Maps to the C struct:
  typedef struct { int32_t status; double value; } ProvenFloatResult;

The status field is 0 (PROVEN_OK) on success, negative on error.
The value field contains the double-precision float result when status is 0.

Note: Due to alignment, the double is at offset 8 (4 bytes status + 4 padding).
"

Class {
	#name : #ProvenFloatResult,
	#superclass : #FFIExternalStructure,
	#category : #'Proven-FFI'
}

{ #category : #'field definition' }
ProvenFloatResult class >> fieldsDesc [
	"Define the C struct layout:
	   int32_t status  -- ProvenStatus code (0 = OK, negative = error)
	   double  value   -- The floating-point result on success"

	^ #(
		int32  status;
		double value;
	)
]

{ #category : #accessing }
ProvenFloatResult >> status [
	"Return the status code. Zero means success."

	^ self getHandle signedLongAt: 1
]

{ #category : #accessing }
ProvenFloatResult >> value [
	"Return the double value. Only meaningful when status is 0."

	^ self getHandle doubleAt: 9
]

{ #category : #testing }
ProvenFloatResult >> isSuccess [
	"Return true if this result indicates success (status = 0)."

	^ self status = 0
]

{ #category : #testing }
ProvenFloatResult >> isError [
	"Return true if this result indicates an error (status ~= 0)."

	^ self status ~= 0
]

{ #category : #accessing }
ProvenFloatResult >> errorDescription [
	"Return a human-readable error description, or nil if successful."

	self isSuccess ifTrue: [ ^ nil ].
	^ ProvenLibrary descriptionForStatus: self status
]

{ #category : #converting }
ProvenFloatResult >> valueOrNil [
	"Return the float value if successful, or nil on error.
	 This is the idiomatic Smalltalk way to handle fallible results."

	self isSuccess ifTrue: [ ^ self value ].
	^ nil
]

{ #category : #printing }
ProvenFloatResult >> printOn: aStream [
	"Print a readable representation of this result."

	aStream nextPutAll: 'ProvenFloatResult('.
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
