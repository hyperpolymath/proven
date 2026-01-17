// SPDX-License-Identifier: PMPL-1.0
// Safe semantic version handling for V.
//
// Provides SemVer parsing, validation, comparison, and version
// range matching with protection against malformed versions.

module proven

// Semantic version representation
pub struct SemVer {
pub:
	major      int
	minor      int
	patch      int
	prerelease string
	build      string
}

// Parse a semantic version string
pub fn parse_semver(s string) ?SemVer {
	if s.len == 0 || s.len > 256 {
		return none
	}

	mut working := s.trim_space()

	// Remove optional 'v' prefix
	if working.starts_with('v') || working.starts_with('V') {
		working = working[1..]
	}

	mut build := ''
	mut prerelease := ''

	// Extract build metadata
	if plus_idx := working.index('+') {
		build = working[plus_idx + 1..]
		working = working[..plus_idx]

		if !is_valid_identifier(build) {
			return none
		}
	}

	// Extract prerelease
	if dash_idx := working.index('-') {
		prerelease = working[dash_idx + 1..]
		working = working[..dash_idx]

		if !is_valid_prerelease(prerelease) {
			return none
		}
	}

	// Parse version numbers
	parts := working.split('.')
	if parts.len < 1 || parts.len > 3 {
		return none
	}

	major := parse_version_number(parts[0]) or { return none }
	minor := if parts.len > 1 { parse_version_number(parts[1]) or { return none } } else { 0 }
	patch := if parts.len > 2 { parse_version_number(parts[2]) or { return none } } else { 0 }

	return SemVer{
		major:      major
		minor:      minor
		patch:      patch
		prerelease: prerelease
		build:      build
	}
}

// Parse a version number component
fn parse_version_number(s string) ?int {
	if s.len == 0 {
		return none
	}

	// No leading zeros (except for zero itself)
	if s.len > 1 && s[0] == `0` {
		return none
	}

	for c in s {
		if c < `0` || c > `9` {
			return none
		}
	}

	return s.int()
}

// Validate prerelease identifier
fn is_valid_prerelease(s string) bool {
	if s.len == 0 {
		return false
	}

	parts := s.split('.')
	for part in parts {
		if part.len == 0 {
			return false
		}

		// Numeric parts can't have leading zeros
		if is_all_digits(part) && part.len > 1 && part[0] == `0` {
			return false
		}

		// Must be alphanumeric plus hyphen
		for c in part {
			if !is_alnum_or_hyphen(c) {
				return false
			}
		}
	}

	return true
}

// Validate build metadata identifier
fn is_valid_identifier(s string) bool {
	if s.len == 0 {
		return false
	}

	parts := s.split('.')
	for part in parts {
		if part.len == 0 {
			return false
		}

		for c in part {
			if !is_alnum_or_hyphen(c) {
				return false
			}
		}
	}

	return true
}

// Check if string is all digits
fn is_all_digits(s string) bool {
	for c in s {
		if c < `0` || c > `9` {
			return false
		}
	}
	return s.len > 0
}

// Check if character is alphanumeric or hyphen
fn is_alnum_or_hyphen(c u8) bool {
	return (c >= `a` && c <= `z`) || (c >= `A` && c <= `Z`) || (c >= `0` && c <= `9`) || c == `-`
}

// Format version to string
pub fn format_semver(v SemVer) string {
	mut result := '${v.major}.${v.minor}.${v.patch}'

	if v.prerelease.len > 0 {
		result += '-${v.prerelease}'
	}

	if v.build.len > 0 {
		result += '+${v.build}'
	}

	return result
}

// Compare two versions (-1, 0, 1)
pub fn compare_semver(a SemVer, b SemVer) int {
	// Compare major
	if a.major != b.major {
		return if a.major < b.major { -1 } else { 1 }
	}

	// Compare minor
	if a.minor != b.minor {
		return if a.minor < b.minor { -1 } else { 1 }
	}

	// Compare patch
	if a.patch != b.patch {
		return if a.patch < b.patch { -1 } else { 1 }
	}

	// Compare prerelease (no prerelease > prerelease)
	if a.prerelease.len == 0 && b.prerelease.len > 0 {
		return 1
	}
	if a.prerelease.len > 0 && b.prerelease.len == 0 {
		return -1
	}
	if a.prerelease.len > 0 && b.prerelease.len > 0 {
		return compare_prerelease(a.prerelease, b.prerelease)
	}

	return 0
}

// Compare prerelease strings
fn compare_prerelease(a string, b string) int {
	parts_a := a.split('.')
	parts_b := b.split('.')

	min_len := if parts_a.len < parts_b.len { parts_a.len } else { parts_b.len }

	for i in 0 .. min_len {
		cmp := compare_prerelease_part(parts_a[i], parts_b[i])
		if cmp != 0 {
			return cmp
		}
	}

	// More parts = later version
	if parts_a.len != parts_b.len {
		return if parts_a.len < parts_b.len { -1 } else { 1 }
	}

	return 0
}

// Compare single prerelease part
fn compare_prerelease_part(a string, b string) int {
	a_numeric := is_all_digits(a)
	b_numeric := is_all_digits(b)

	// Numeric < alphanumeric
	if a_numeric && !b_numeric {
		return -1
	}
	if !a_numeric && b_numeric {
		return 1
	}

	// Both numeric: compare as numbers
	if a_numeric && b_numeric {
		a_int := a.int()
		b_int := b.int()
		if a_int != b_int {
			return if a_int < b_int { -1 } else { 1 }
		}
		return 0
	}

	// Both alphanumeric: compare as strings
	if a < b {
		return -1
	}
	if a > b {
		return 1
	}
	return 0
}

// Check if version a is less than b
pub fn semver_lt(a SemVer, b SemVer) bool {
	return compare_semver(a, b) < 0
}

// Check if version a is less than or equal to b
pub fn semver_le(a SemVer, b SemVer) bool {
	return compare_semver(a, b) <= 0
}

// Check if version a is greater than b
pub fn semver_gt(a SemVer, b SemVer) bool {
	return compare_semver(a, b) > 0
}

// Check if version a is greater than or equal to b
pub fn semver_ge(a SemVer, b SemVer) bool {
	return compare_semver(a, b) >= 0
}

// Check if version a equals b (ignoring build metadata)
pub fn semver_eq(a SemVer, b SemVer) bool {
	return compare_semver(a, b) == 0
}

// Check if version satisfies range constraint
pub fn satisfies_range(v SemVer, range_str string) bool {
	constraint := parse_constraint(range_str) or { return false }
	return constraint.matches(v)
}

// Version constraint
struct VersionConstraint {
	op      string
	version SemVer
}

fn (c VersionConstraint) matches(v SemVer) bool {
	return match c.op {
		'=' { semver_eq(v, c.version) }
		'>' { semver_gt(v, c.version) }
		'>=' { semver_ge(v, c.version) }
		'<' { semver_lt(v, c.version) }
		'<=' { semver_le(v, c.version) }
		'^' { matches_caret(v, c.version) }
		'~' { matches_tilde(v, c.version) }
		else { false }
	}
}

// Parse a constraint string
fn parse_constraint(s string) ?VersionConstraint {
	mut working := s.trim_space()

	mut op := '='

	if working.starts_with('>=') {
		op = '>='
		working = working[2..]
	} else if working.starts_with('<=') {
		op = '<='
		working = working[2..]
	} else if working.starts_with('>') {
		op = '>'
		working = working[1..]
	} else if working.starts_with('<') {
		op = '<'
		working = working[1..]
	} else if working.starts_with('^') {
		op = '^'
		working = working[1..]
	} else if working.starts_with('~') {
		op = '~'
		working = working[1..]
	} else if working.starts_with('=') {
		working = working[1..]
	}

	version := parse_semver(working.trim_space())?

	return VersionConstraint{
		op:      op
		version: version
	}
}

// Caret range matching (^1.2.3 matches >=1.2.3 <2.0.0)
fn matches_caret(v SemVer, base SemVer) bool {
	if base.major == 0 {
		if base.minor == 0 {
			// ^0.0.x matches only 0.0.x
			return v.major == 0 && v.minor == 0 && v.patch == base.patch
		}
		// ^0.y.z matches >=0.y.z <0.(y+1).0
		return v.major == 0 && v.minor == base.minor && semver_ge(v, base)
	}

	// ^x.y.z matches >=x.y.z <(x+1).0.0
	return v.major == base.major && semver_ge(v, base)
}

// Tilde range matching (~1.2.3 matches >=1.2.3 <1.3.0)
fn matches_tilde(v SemVer, base SemVer) bool {
	return v.major == base.major && v.minor == base.minor && semver_ge(v, base)
}

// Increment major version
pub fn bump_major(v SemVer) SemVer {
	return SemVer{
		major:      v.major + 1
		minor:      0
		patch:      0
		prerelease: ''
		build:      ''
	}
}

// Increment minor version
pub fn bump_minor(v SemVer) SemVer {
	return SemVer{
		major:      v.major
		minor:      v.minor + 1
		patch:      0
		prerelease: ''
		build:      ''
	}
}

// Increment patch version
pub fn bump_patch(v SemVer) SemVer {
	return SemVer{
		major:      v.major
		minor:      v.minor
		patch:      v.patch + 1
		prerelease: ''
		build:      ''
	}
}

// Check if version is stable (no prerelease)
pub fn is_stable(v SemVer) bool {
	return v.prerelease.len == 0
}

// Check if version is prerelease
pub fn is_prerelease(v SemVer) bool {
	return v.prerelease.len > 0
}

// Check if version is valid
pub fn is_valid_semver(s string) bool {
	return parse_semver(s) != none
}
