# SPDX-License-Identifier: PMPL-1.0-or-later
# SPDX-FileCopyrightText: 2025 Hyperpolymath
#
# SafeVersion - Semantic versioning parsing and comparison.
#

package provide proven::version 0.4.0
package require Tcl 8.6

namespace eval ::proven::version {
    namespace export parse format compare is_stable is_prerelease
    namespace export bump_major bump_minor bump_patch satisfies
    namespace export new with_prerelease with_build
    namespace ensemble create

    # Parse version string
    # Returns dict with {major minor patch prerelease build_metadata ok error}
    proc parse {versionString} {
        set s [string trim $versionString]

        # Strip leading v
        if {[string index $s 0] eq "v"} {
            set s [string range $s 1 end]
        }

        # Extract build metadata
        set buildMetadata ""
        set plusPos [string first "+" $s]
        if {$plusPos >= 0} {
            set buildMetadata [string range $s [expr {$plusPos + 1}] end]
            set s [string range $s 0 [expr {$plusPos - 1}]]
        }

        # Extract prerelease
        set prerelease ""
        set dashPos [string first "-" $s]
        if {$dashPos >= 0} {
            set prerelease [string range $s [expr {$dashPos + 1}] end]
            set s [string range $s 0 [expr {$dashPos - 1}]]
        }

        # Parse major.minor.patch
        set parts [split $s "."]
        if {[llength $parts] != 3} {
            return [dict create major 0 minor 0 patch 0 prerelease "" build_metadata "" \
                ok 0 error "Version must have major.minor.patch"]
        }

        foreach {major minor patch} $parts break

        if {![string is integer -strict $major] || $major < 0} {
            return [dict create major 0 minor 0 patch 0 prerelease "" build_metadata "" \
                ok 0 error "Invalid major version"]
        }

        if {![string is integer -strict $minor] || $minor < 0} {
            return [dict create major 0 minor 0 patch 0 prerelease "" build_metadata "" \
                ok 0 error "Invalid minor version"]
        }

        if {![string is integer -strict $patch] || $patch < 0} {
            return [dict create major 0 minor 0 patch 0 prerelease "" build_metadata "" \
                ok 0 error "Invalid patch version"]
        }

        return [dict create \
            major $major \
            minor $minor \
            patch $patch \
            prerelease $prerelease \
            build_metadata $buildMetadata \
            ok 1 \
            error ""]
    }

    # Create new version
    proc new {major minor patch} {
        return [dict create \
            major $major \
            minor $minor \
            patch $patch \
            prerelease "" \
            build_metadata ""]
    }

    # Add prerelease to version
    proc with_prerelease {versionDict prerelease} {
        dict set versionDict prerelease $prerelease
        return $versionDict
    }

    # Add build metadata to version
    proc with_build {versionDict build} {
        dict set versionDict build_metadata $build
        return $versionDict
    }

    # Format version as string
    proc format {versionDict} {
        set s "[dict get $versionDict major].[dict get $versionDict minor].[dict get $versionDict patch]"

        set prerelease [dict get $versionDict prerelease]
        if {$prerelease ne ""} {
            append s "-$prerelease"
        }

        set build [dict get $versionDict build_metadata]
        if {$build ne ""} {
            append s "+$build"
        }

        return $s
    }

    # Compare two versions
    # Returns -1 if v1 < v2, 0 if equal, 1 if v1 > v2
    proc compare {v1 v2} {
        # Compare major
        set cmp [_compare_int [dict get $v1 major] [dict get $v2 major]]
        if {$cmp != 0} { return $cmp }

        # Compare minor
        set cmp [_compare_int [dict get $v1 minor] [dict get $v2 minor]]
        if {$cmp != 0} { return $cmp }

        # Compare patch
        set cmp [_compare_int [dict get $v1 patch] [dict get $v2 patch]]
        if {$cmp != 0} { return $cmp }

        # Compare prerelease
        set pre1 [dict get $v1 prerelease]
        set pre2 [dict get $v2 prerelease]

        # No prerelease > prerelease
        if {$pre1 eq "" && $pre2 ne ""} { return 1 }
        if {$pre1 ne "" && $pre2 eq ""} { return -1 }

        # Both have prerelease - compare lexicographically
        return [string compare $pre1 $pre2]
    }

    # Check if version is stable (>= 1.0.0 and no prerelease)
    proc is_stable {versionDict} {
        set major [dict get $versionDict major]
        set prerelease [dict get $versionDict prerelease]
        return [expr {$major >= 1 && $prerelease eq ""}]
    }

    # Check if version is prerelease
    proc is_prerelease {versionDict} {
        return [expr {[dict get $versionDict prerelease] ne ""}]
    }

    # Bump major version
    proc bump_major {versionDict} {
        return [new [expr {[dict get $versionDict major] + 1}] 0 0]
    }

    # Bump minor version
    proc bump_minor {versionDict} {
        return [new [dict get $versionDict major] [expr {[dict get $versionDict minor] + 1}] 0]
    }

    # Bump patch version
    proc bump_patch {versionDict} {
        return [new [dict get $versionDict major] [dict get $versionDict minor] \
            [expr {[dict get $versionDict patch] + 1}]]
    }

    # Check if version satisfies constraint
    # Supports: >=, <=, >, <, =, ^, ~
    proc satisfies {versionDict constraint} {
        set constraint [string trim $constraint]

        # Parse constraint
        if {[string match ">=*" $constraint]} {
            set target [parse [string range $constraint 2 end]]
            if {![dict get $target ok]} { return 0 }
            return [expr {[compare $versionDict $target] >= 0}]
        }

        if {[string match "<=*" $constraint]} {
            set target [parse [string range $constraint 2 end]]
            if {![dict get $target ok]} { return 0 }
            return [expr {[compare $versionDict $target] <= 0}]
        }

        if {[string match ">*" $constraint]} {
            set target [parse [string range $constraint 1 end]]
            if {![dict get $target ok]} { return 0 }
            return [expr {[compare $versionDict $target] > 0}]
        }

        if {[string match "<*" $constraint]} {
            set target [parse [string range $constraint 1 end]]
            if {![dict get $target ok]} { return 0 }
            return [expr {[compare $versionDict $target] < 0}]
        }

        if {[string match "=*" $constraint]} {
            set target [parse [string range $constraint 1 end]]
            if {![dict get $target ok]} { return 0 }
            return [expr {[compare $versionDict $target] == 0}]
        }

        if {[string match "^*" $constraint]} {
            # Caret: compatible with version (same major)
            set target [parse [string range $constraint 1 end]]
            if {![dict get $target ok]} { return 0 }

            set targetMajor [dict get $target major]
            if {$targetMajor == 0} {
                # For 0.x, must match major and minor
                return [expr {[dict get $versionDict major] == 0 &&
                    [dict get $versionDict minor] == [dict get $target minor] &&
                    [compare $versionDict $target] >= 0}]
            }
            return [expr {[dict get $versionDict major] == $targetMajor &&
                [compare $versionDict $target] >= 0}]
        }

        if {[string match "~*" $constraint]} {
            # Tilde: same major.minor
            set target [parse [string range $constraint 1 end]]
            if {![dict get $target ok]} { return 0 }

            return [expr {[dict get $versionDict major] == [dict get $target major] &&
                [dict get $versionDict minor] == [dict get $target minor] &&
                [compare $versionDict $target] >= 0}]
        }

        # Exact match
        set target [parse $constraint]
        if {![dict get $target ok]} { return 0 }
        return [expr {[compare $versionDict $target] == 0}]
    }

    # Internal: compare integers
    proc _compare_int {a b} {
        if {$a < $b} { return -1 }
        if {$a > $b} { return 1 }
        return 0
    }
}
