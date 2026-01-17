// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

package proven

import (
	"regexp"
	"strconv"
	"strings"
)

// SemVer represents a semantic version.
type SemVer struct {
	Major      int
	Minor      int
	Patch      int
	Prerelease string
	Build      string
}

var semverRegex = regexp.MustCompile(`^v?(\d+)\.(\d+)\.(\d+)(?:-([0-9A-Za-z-.]+))?(?:\+([0-9A-Za-z-.]+))?$`)

// ParseSemVer parses a semantic version string.
func ParseSemVer(s string) (SemVer, bool) {
	matches := semverRegex.FindStringSubmatch(strings.TrimSpace(s))
	if matches == nil {
		return SemVer{}, false
	}

	major, _ := strconv.Atoi(matches[1])
	minor, _ := strconv.Atoi(matches[2])
	patch, _ := strconv.Atoi(matches[3])

	return SemVer{
		Major:      major,
		Minor:      minor,
		Patch:      patch,
		Prerelease: matches[4],
		Build:      matches[5],
	}, true
}

// String returns version string.
func (v SemVer) String() string {
	s := strconv.Itoa(v.Major) + "." + strconv.Itoa(v.Minor) + "." + strconv.Itoa(v.Patch)
	if v.Prerelease != "" {
		s += "-" + v.Prerelease
	}
	if v.Build != "" {
		s += "+" + v.Build
	}
	return s
}

// Compare compares two versions.
// Returns -1 if v < other, 0 if equal, 1 if v > other.
func (v SemVer) Compare(other SemVer) int {
	if v.Major != other.Major {
		if v.Major < other.Major {
			return -1
		}
		return 1
	}
	if v.Minor != other.Minor {
		if v.Minor < other.Minor {
			return -1
		}
		return 1
	}
	if v.Patch != other.Patch {
		if v.Patch < other.Patch {
			return -1
		}
		return 1
	}

	// Prerelease comparison
	if v.Prerelease == "" && other.Prerelease != "" {
		return 1 // no prerelease > prerelease
	}
	if v.Prerelease != "" && other.Prerelease == "" {
		return -1
	}
	if v.Prerelease != other.Prerelease {
		if v.Prerelease < other.Prerelease {
			return -1
		}
		return 1
	}

	return 0
}

// Lt checks if v < other.
func (v SemVer) Lt(other SemVer) bool {
	return v.Compare(other) < 0
}

// Le checks if v <= other.
func (v SemVer) Le(other SemVer) bool {
	return v.Compare(other) <= 0
}

// Gt checks if v > other.
func (v SemVer) Gt(other SemVer) bool {
	return v.Compare(other) > 0
}

// Ge checks if v >= other.
func (v SemVer) Ge(other SemVer) bool {
	return v.Compare(other) >= 0
}

// Eq checks if v == other.
func (v SemVer) Eq(other SemVer) bool {
	return v.Compare(other) == 0
}

// IsStable checks if version is stable (no prerelease).
func (v SemVer) IsStable() bool {
	return v.Prerelease == ""
}

// IsPrerelease checks if version has prerelease.
func (v SemVer) IsPrerelease() bool {
	return v.Prerelease != ""
}

// BumpMajor returns next major version.
func (v SemVer) BumpMajor() SemVer {
	return SemVer{Major: v.Major + 1, Minor: 0, Patch: 0}
}

// BumpMinor returns next minor version.
func (v SemVer) BumpMinor() SemVer {
	return SemVer{Major: v.Major, Minor: v.Minor + 1, Patch: 0}
}

// BumpPatch returns next patch version.
func (v SemVer) BumpPatch() SemVer {
	return SemVer{Major: v.Major, Minor: v.Minor, Patch: v.Patch + 1}
}

// VersionConstraint represents a version constraint.
type VersionConstraint struct {
	operator string
	version  SemVer
}

// ParseVersionConstraint parses a constraint like ">=1.0.0" or "^2.0".
func ParseVersionConstraint(s string) (VersionConstraint, bool) {
	s = strings.TrimSpace(s)

	operators := []string{">=", "<=", ">", "<", "=", "^", "~"}
	var op string
	var verStr string

	for _, o := range operators {
		if strings.HasPrefix(s, o) {
			op = o
			verStr = strings.TrimSpace(s[len(o):])
			break
		}
	}

	if op == "" {
		op = "="
		verStr = s
	}

	ver, ok := ParseSemVer(verStr)
	if !ok {
		return VersionConstraint{}, false
	}

	return VersionConstraint{operator: op, version: ver}, true
}

// Matches checks if version satisfies constraint.
func (c VersionConstraint) Matches(v SemVer) bool {
	switch c.operator {
	case "=":
		return v.Eq(c.version)
	case ">":
		return v.Gt(c.version)
	case ">=":
		return v.Ge(c.version)
	case "<":
		return v.Lt(c.version)
	case "<=":
		return v.Le(c.version)
	case "^":
		// Caret: allows changes that don't modify left-most non-zero
		if c.version.Major == 0 {
			if c.version.Minor == 0 {
				return v.Major == 0 && v.Minor == 0 && v.Patch == c.version.Patch
			}
			return v.Major == 0 && v.Minor == c.version.Minor && v.Ge(c.version)
		}
		return v.Major == c.version.Major && v.Ge(c.version)
	case "~":
		// Tilde: allows patch-level changes
		return v.Major == c.version.Major &&
			v.Minor == c.version.Minor &&
			v.Ge(c.version)
	}
	return false
}

// String returns constraint string.
func (c VersionConstraint) String() string {
	if c.operator == "=" {
		return c.version.String()
	}
	return c.operator + c.version.String()
}

// SortVersions sorts versions in ascending order.
func SortVersions(versions []SemVer) {
	for i := 0; i < len(versions)-1; i++ {
		for j := i + 1; j < len(versions); j++ {
			if versions[j].Lt(versions[i]) {
				versions[i], versions[j] = versions[j], versions[i]
			}
		}
	}
}

// LatestVersion returns the latest version from a list.
func LatestVersion(versions []SemVer) (SemVer, bool) {
	if len(versions) == 0 {
		return SemVer{}, false
	}

	latest := versions[0]
	for _, v := range versions[1:] {
		if v.Gt(latest) {
			latest = v
		}
	}
	return latest, true
}

// LatestStableVersion returns the latest stable version.
func LatestStableVersion(versions []SemVer) (SemVer, bool) {
	var stable []SemVer
	for _, v := range versions {
		if v.IsStable() {
			stable = append(stable, v)
		}
	}
	return LatestVersion(stable)
}
