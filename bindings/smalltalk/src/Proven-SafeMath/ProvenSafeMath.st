" SPDX-License-Identifier: PMPL-1.0-or-later
  Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk> "

"
ProvenSafeMath - Safe arithmetic operations that cannot crash.

All computation is performed by formally verified Idris 2 code via the
Zig FFI bridge (libproven). This class is a thin wrapper that marshals
Smalltalk integers to/from the C ABI. No logic is reimplemented here.

Methods return nil on error (Smalltalk idiom) unless stated otherwise.
"

Class {
	#name : #ProvenSafeMath,
	#superclass : #Object,
	#category : #'Proven-SafeMath'
}

{ #category : #private }
ProvenSafeMath class >> library [
	"Return the shared library instance."

	^ ProvenLibrary uniqueInstance
]

{ #category : #arithmetic }
ProvenSafeMath class >> add: a to: b [
	"Safe addition with overflow detection.
	 Delegates to proven_math_add_checked() in libproven.
	 Returns the sum as an Integer, or nil on overflow."

	| result |
	result := self library provenMathAddChecked: a b: b.
	^ result valueOrNil
]

{ #category : #arithmetic }
ProvenSafeMath class >> subtract: b from: a [
	"Safe subtraction with underflow detection.
	 Delegates to proven_math_sub_checked() in libproven.
	 Returns the difference (a - b), or nil on underflow."

	| result |
	result := self library provenMathSubChecked: a b: b.
	^ result valueOrNil
]

{ #category : #arithmetic }
ProvenSafeMath class >> multiply: a by: b [
	"Safe multiplication with overflow detection.
	 Delegates to proven_math_mul_checked() in libproven.
	 Returns the product, or nil on overflow."

	| result |
	result := self library provenMathMulChecked: a b: b.
	^ result valueOrNil
]

{ #category : #arithmetic }
ProvenSafeMath class >> divide: numerator by: denominator [
	"Safe integer division with zero-divisor detection.
	 Delegates to proven_math_div() in libproven.
	 Returns the quotient, or nil on division by zero or overflow."

	| result |
	result := self library provenMathDiv: numerator denominator: denominator.
	^ result valueOrNil
]

{ #category : #arithmetic }
ProvenSafeMath class >> mod: numerator by: denominator [
	"Safe modulo with zero-divisor detection.
	 Delegates to proven_math_mod() in libproven.
	 Returns the remainder, or nil on division by zero."

	| result |
	result := self library provenMathMod: numerator denominator: denominator.
	^ result valueOrNil
]

{ #category : #arithmetic }
ProvenSafeMath class >> abs: n [
	"Safe absolute value that handles INT64_MIN correctly.
	 Delegates to proven_math_abs_safe() in libproven.
	 Returns the absolute value, or nil if n is INT64_MIN."

	| result |
	result := self library provenMathAbsSafe: n.
	^ result valueOrNil
]

{ #category : #arithmetic }
ProvenSafeMath class >> clamp: value between: lo and: hi [
	"Clamp value to [lo, hi] range.
	 Delegates to proven_math_clamp() in libproven.
	 Always returns a valid integer (no error case)."

	^ self library provenMathClampLo: lo hi: hi value: value
]

{ #category : #arithmetic }
ProvenSafeMath class >> pow: base exp: exponent [
	"Safe integer exponentiation with overflow checking.
	 Delegates to proven_math_pow_checked() in libproven.
	 Returns the result, or nil on overflow or negative exponent."

	| result |
	result := self library provenMathPowChecked: base exp: exponent.
	^ result valueOrNil
]

{ #category : #convenience }
ProvenSafeMath class >> percentOf: percent total: total [
	"Calculate percent% of total safely using FFI-backed multiply and divide.
	 Returns the result, or nil on overflow or division by zero."

	| product |
	product := self multiply: percent by: total.
	product ifNil: [ ^ nil ].
	^ self divide: product by: 100
]

{ #category : #convenience }
ProvenSafeMath class >> asPercent: part of: whole [
	"Calculate what percentage part is of whole.
	 Returns the percentage (0-100+), or nil on division by zero or overflow."

	| scaled |
	scaled := self multiply: part by: 100.
	scaled ifNil: [ ^ nil ].
	^ self divide: scaled by: whole
]
