// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath

package com.hyperpolymath.proven

/**
 * Semantic version (SemVer) representation.
 */
data class SemVer(
    val major: Int,
    val minor: Int,
    val patch: Int,
    val prerelease: String? = null,
    val build: String? = null
) : Comparable<SemVer> {

    init {
        require(major >= 0) { "Major version must be non-negative" }
        require(minor >= 0) { "Minor version must be non-negative" }
        require(patch >= 0) { "Patch version must be non-negative" }
    }

    override fun compareTo(other: SemVer): Int {
        // Compare major.minor.patch
        major.compareTo(other.major).let { if (it != 0) return it }
        minor.compareTo(other.minor).let { if (it != 0) return it }
        patch.compareTo(other.patch).let { if (it != 0) return it }

        // Pre-release versions have lower precedence
        return when {
            prerelease == null && other.prerelease == null -> 0
            prerelease == null -> 1  // No prerelease > has prerelease
            other.prerelease == null -> -1
            else -> comparePrerelease(prerelease, other.prerelease)
        }
    }

    private fun comparePrerelease(a: String, b: String): Int {
        val aParts = a.split(".")
        val bParts = b.split(".")

        for (i in 0 until maxOf(aParts.size, bParts.size)) {
            if (i >= aParts.size) return -1
            if (i >= bParts.size) return 1

            val aNum = aParts[i].toIntOrNull()
            val bNum = bParts[i].toIntOrNull()

            val cmp = when {
                aNum != null && bNum != null -> aNum.compareTo(bNum)
                aNum != null -> -1  // Numeric < alphanumeric
                bNum != null -> 1
                else -> aParts[i].compareTo(bParts[i])
            }

            if (cmp != 0) return cmp
        }

        return 0
    }

    override fun toString(): String {
        val base = "$major.$minor.$patch"
        val pre = prerelease?.let { "-$it" } ?: ""
        val bld = build?.let { "+$it" } ?: ""
        return "$base$pre$bld"
    }

    /**
     * Bump major version.
     */
    fun bumpMajor(): SemVer = SemVer(major + 1, 0, 0)

    /**
     * Bump minor version.
     */
    fun bumpMinor(): SemVer = SemVer(major, minor + 1, 0)

    /**
     * Bump patch version.
     */
    fun bumpPatch(): SemVer = SemVer(major, minor, patch + 1)

    /**
     * Check if this is a prerelease version.
     */
    fun isPrerelease(): Boolean = prerelease != null

    /**
     * Check if this is stable (>= 1.0.0 and no prerelease).
     */
    fun isStable(): Boolean = major >= 1 && prerelease == null

    /**
     * Check if compatible with another version (same major).
     */
    fun isCompatibleWith(other: SemVer): Boolean = major == other.major

    /**
     * Check if satisfies version range.
     */
    fun satisfies(range: String): Boolean {
        return SafeVersion.satisfies(this, range)
    }

    companion object {
        /**
         * Parse a version string.
         */
        fun parse(version: String): Result<SemVer> = SafeVersion.parse(version)
    }
}

/**
 * Version utilities.
 */
object SafeVersion {
    private val SEMVER_REGEX = Regex(
        """^(0|[1-9]\d*)\.(0|[1-9]\d*)\.(0|[1-9]\d*)(?:-([\da-zA-Z-]+(?:\.[\da-zA-Z-]+)*))?(?:\+([\da-zA-Z-]+(?:\.[\da-zA-Z-]+)*))?$"""
    )

    /**
     * Parse a semantic version string.
     */
    fun parse(version: String): Result<SemVer> {
        val match = SEMVER_REGEX.matchEntire(version.trim())
            ?: return Result.failure(IllegalArgumentException("Invalid semantic version: $version"))

        val (major, minor, patch, prerelease, build) = match.destructured

        return Result.success(
            SemVer(
                major = major.toInt(),
                minor = minor.toInt(),
                patch = patch.toInt(),
                prerelease = prerelease.takeIf { it.isNotEmpty() },
                build = build.takeIf { it.isNotEmpty() }
            )
        )
    }

    /**
     * Check if a string is a valid semantic version.
     */
    fun isValid(version: String): Boolean = SEMVER_REGEX.matches(version.trim())

    /**
     * Compare two version strings.
     */
    fun compare(a: String, b: String): Result<Int> {
        val verA = parse(a).getOrElse { return Result.failure(it) }
        val verB = parse(b).getOrElse { return Result.failure(it) }
        return Result.success(verA.compareTo(verB))
    }

    /**
     * Check if version satisfies a range.
     */
    fun satisfies(version: SemVer, range: String): Boolean {
        val trimmed = range.trim()

        return when {
            trimmed.startsWith(">=") -> {
                val target = parse(trimmed.drop(2).trim()).getOrNull() ?: return false
                version >= target
            }
            trimmed.startsWith("<=") -> {
                val target = parse(trimmed.drop(2).trim()).getOrNull() ?: return false
                version <= target
            }
            trimmed.startsWith(">") -> {
                val target = parse(trimmed.drop(1).trim()).getOrNull() ?: return false
                version > target
            }
            trimmed.startsWith("<") -> {
                val target = parse(trimmed.drop(1).trim()).getOrNull() ?: return false
                version < target
            }
            trimmed.startsWith("=") -> {
                val target = parse(trimmed.drop(1).trim()).getOrNull() ?: return false
                version == target
            }
            trimmed.startsWith("^") -> {
                // Caret range: compatible with version
                val target = parse(trimmed.drop(1).trim()).getOrNull() ?: return false
                version >= target && version.major == target.major
            }
            trimmed.startsWith("~") -> {
                // Tilde range: approximately equivalent
                val target = parse(trimmed.drop(1).trim()).getOrNull() ?: return false
                version >= target && version.major == target.major && version.minor == target.minor
            }
            trimmed.contains(" - ") -> {
                // Range
                val parts = trimmed.split(" - ")
                if (parts.size != 2) return false
                val min = parse(parts[0].trim()).getOrNull() ?: return false
                val max = parse(parts[1].trim()).getOrNull() ?: return false
                version >= min && version <= max
            }
            else -> {
                // Exact match
                val target = parse(trimmed).getOrNull() ?: return false
                version == target
            }
        }
    }

    /**
     * Sort versions.
     */
    fun sort(versions: List<String>): Result<List<String>> {
        val parsed = versions.map { parse(it).getOrElse { return Result.failure(it) } to it }
        return Result.success(parsed.sortedBy { it.first }.map { it.second })
    }

    /**
     * Get latest version from list.
     */
    fun latest(versions: List<String>): Result<String> {
        if (versions.isEmpty()) return Result.failure(IllegalArgumentException("Version list is empty"))
        return sort(versions).map { it.last() }
    }
}
