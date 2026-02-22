" SPDX-License-Identifier: PMPL-1.0-or-later
  Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk> "

"
ProvenStringResult - FFI struct for string result from libproven.

Maps to the C struct:
  typedef struct { int32_t status; char* value; size_t length; } ProvenStringResult;

The status field is 0 (PROVEN_OK) on success, negative on error.
The value field is a pointer to a C string allocated by libproven.
The length field indicates the byte length of the string.

IMPORTANT: The caller MUST free the value pointer using proven_free_string()
when status is 0 and the string is no longer needed. Wrappers handle this
automatically by copying to a Smalltalk String and freeing immediately.
"

Class {
	#name : #ProvenStringResult,
	#superclass : #FFIExternalStructure,
	#category : #'Proven-FFI'
}

{ #category : #'field definition' }
ProvenStringResult class >> fieldsDesc [
	"Define the C struct layout:
	   int32_t status  -- ProvenStatus code (0 = OK, negative = error)
	   char*   value   -- Pointer to the string data (must be freed)
	   size_t  length  -- Byte length of the string"

	^ #(
		int32  status;
		void*  value;
		size_t length;
	)
]

{ #category : #accessing }
ProvenStringResult >> status [
	"Return the status code. Zero means success."

	^ self getHandle signedLongAt: 1
]

{ #category : #accessing }
ProvenStringResult >> valuePointer [
	"Return the raw pointer to the C string. The caller is responsible for
	 freeing this pointer via proven_free_string(). Prefer valueOrNil which
	 copies and frees automatically."

	^ self getHandle pointerAt: 5
]

{ #category : #accessing }
ProvenStringResult >> length [
	"Return the byte length of the string."

	| pointerSize |
	pointerSize := FFIExternalType pointerSize.
	^ self getHandle platformSizeTAt: 5 + pointerSize
]

{ #category : #testing }
ProvenStringResult >> isSuccess [
	"Return true if this result indicates success (status = 0)."

	^ self status = 0
]

{ #category : #testing }
ProvenStringResult >> isError [
	"Return true if this result indicates an error (status ~= 0)."

	^ self status ~= 0
]

{ #category : #accessing }
ProvenStringResult >> errorDescription [
	"Return a human-readable error description, or nil if successful."

	self isSuccess ifTrue: [ ^ nil ].
	^ ProvenLibrary descriptionForStatus: self status
]

{ #category : #converting }
ProvenStringResult >> valueOrNil [
	"Return a Smalltalk String copy if successful, or nil on error.
	 Automatically frees the C string after copying.
	 This is the idiomatic Smalltalk way to handle fallible results."

	| ptr len result lib |
	self isSuccess ifFalse: [ ^ nil ].
	ptr := self valuePointer.
	ptr isNull ifTrue: [ ^ nil ].
	len := self length.
	len = 0 ifTrue: [
		lib := ProvenLibrary uniqueInstance.
		lib provenFreeString: ptr.
		^ '' ].
	result := (ptr copyFrom: 1 to: len) utf8Decoded.
	lib := ProvenLibrary uniqueInstance.
	lib provenFreeString: ptr.
	^ result
]

{ #category : #printing }
ProvenStringResult >> printOn: aStream [
	"Print a readable representation of this result.
	 Note: This does NOT extract the value (to avoid accidental memory management)."

	aStream nextPutAll: 'ProvenStringResult('.
	self isSuccess
		ifTrue: [
			aStream
				nextPutAll: 'ok, length: ';
				print: self length ]
		ifFalse: [
			aStream
				nextPutAll: 'error: ';
				nextPutAll: self errorDescription ].
	aStream nextPut: $)
]
