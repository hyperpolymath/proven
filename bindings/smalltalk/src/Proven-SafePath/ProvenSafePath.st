" SPDX-License-Identifier: PMPL-1.0-or-later
  Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk> "

"
ProvenSafePath - Safe filesystem path operations that prevent traversal attacks.

All computation is performed by formally verified Idris 2 code via the
Zig FFI bridge (libproven). This class is a thin wrapper that marshals
Smalltalk strings to the C ABI. No logic is reimplemented here.

Methods return nil on error (Smalltalk idiom).
"

Class {
	#name : #ProvenSafePath,
	#superclass : #Object,
	#category : #'Proven-SafePath'
}

{ #category : #private }
ProvenSafePath class >> library [
	"Return the shared library instance."

	^ ProvenLibrary uniqueInstance
]

{ #category : #private }
ProvenSafePath class >> withUtf8BytesOf: aString do: aBlock [
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
ProvenSafePath class >> hasTraversal: aPathString [
	"Check if a path contains directory traversal sequences ('..').
	 Delegates to proven_path_has_traversal() in libproven.
	 Returns true if traversal was detected, false if safe, or nil on error."

	^ self withUtf8BytesOf: aPathString do: [ :ptr :len |
		| result |
		result := self library provenPathHasTraversal: ptr len: len.
		result valueOrNil ]
]

{ #category : #sanitization }
ProvenSafePath class >> sanitizeFilename: aFilenameString [
	"Sanitize a filename by removing dangerous characters.
	 Delegates to proven_path_sanitize_filename() in libproven.
	 Returns the sanitized filename String, or nil on error.
	 The C-allocated string is automatically freed after copying."

	^ self withUtf8BytesOf: aFilenameString do: [ :ptr :len |
		| result |
		result := self library provenPathSanitizeFilename: ptr len: len.
		result valueOrNil ]
]

{ #category : #validation }
ProvenSafePath class >> isSafe: aPathString [
	"Check if a path is safe (does NOT contain traversal sequences).
	 This is a convenience wrapper around hasTraversal:.
	 Returns true if safe, false if traversal detected, nil on error."

	| hasTraversal |
	hasTraversal := self hasTraversal: aPathString.
	hasTraversal ifNil: [ ^ nil ].
	^ hasTraversal not
]
