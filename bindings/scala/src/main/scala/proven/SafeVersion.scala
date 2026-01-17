// SPDX-License-Identifier: PMPL-1.0

package proven

import scala.util.Try

/**
 * Semantic version following SemVer 2.0.0 specification.
 */
case class Version(
    major: Long,
    minor: Long,
    patch: Long,
    prerelease: Option[String] = None,
    buildMetadata: Option[String] = None
) extends Ordered[Version]:

  /**
   * Format as standard version string.
   */
  def format: String =
    val base = s"$major.$minor.$patch"
    val pre = prerelease.map(p => s"-$p").getOrElse("")
    val build = buildMetadata.map(b => s"+$b").getOrElse("")
    base + pre + build

  /**
   * Check if this is a prerelease version.
   */
  def isPrerelease: Boolean = prerelease.isDefined

  /**
   * Check if this is a stable release (>= 1.0.0 and no prerelease).
   */
  def isStable: Boolean = major >= 1 && prerelease.isEmpty

  /**
   * Increment major version (resets minor and patch).
   */
  def bumpMajor: Version = Version(major + 1, 0, 0)

  /**
   * Increment minor version (resets patch).
   */
  def bumpMinor: Version = Version(major, minor + 1, 0)

  /**
   * Increment patch version.
   */
  def bumpPatch: Version = Version(major, minor, patch + 1)

  /**
   * Create a prerelease version.
   */
  def withPrerelease(pre: String): Version = copy(prerelease = Some(pre))

  /**
   * Create a version with build metadata.
   */
  def withBuild(build: String): Version = copy(buildMetadata = Some(build))

  /**
   * Compare versions (build metadata is ignored per SemVer spec).
   */
  override def compare(that: Version): Int =
    // Compare major.minor.patch
    val majorCmp = major.compare(that.major)
    if majorCmp != 0 then return majorCmp

    val minorCmp = minor.compare(that.minor)
    if minorCmp != 0 then return minorCmp

    val patchCmp = patch.compare(that.patch)
    if patchCmp != 0 then return patchCmp

    // Prerelease comparison
    (prerelease, that.prerelease) match
      case (None, None) => 0
      case (None, Some(_)) => 1  // No prerelease > prerelease
      case (Some(_), None) => -1
      case (Some(a), Some(b)) => a.compare(b)

  override def toString: String = format

object Version:
  /**
   * Parse a version string.
   */
  def parse(s: String): Option[Version] =
    val cleaned = s.trim.stripPrefix("v").stripPrefix("V")

    // Split off build metadata
    val (versionPre, build) = cleaned.indexOf('+') match
      case -1 => (cleaned, None)
      case i => (cleaned.substring(0, i), Some(cleaned.substring(i + 1)))

    // Split off prerelease
    val (version, prerelease) = versionPre.indexOf('-') match
      case -1 => (versionPre, None)
      case i => (versionPre.substring(0, i), Some(versionPre.substring(i + 1)))

    // Parse major.minor.patch
    val parts = version.split('.')
    if parts.length != 3 then return None

    for
      major <- Try(parts(0).toLong).toOption.filter(_ >= 0)
      minor <- Try(parts(1).toLong).toOption.filter(_ >= 0)
      patch <- Try(parts(2).toLong).toOption.filter(_ >= 0)
    yield Version(major, minor, patch, prerelease, build)

  /**
   * Create a version from components.
   */
  def apply(major: Long, minor: Long, patch: Long): Version =
    new Version(major, minor, patch)

/**
 * Safe semantic version operations.
 */
object SafeVersion:

  /**
   * Parse a version string.
   */
  def parse(s: String): Option[Version] = Version.parse(s)

  /**
   * Check if a string is a valid semantic version.
   */
  def isValid(s: String): Boolean = parse(s).isDefined

  /**
   * Compare two version strings.
   * Returns -1 if v1 < v2, 0 if equal, 1 if v1 > v2, None if invalid.
   */
  def compare(v1: String, v2: String): Option[Int] =
    for
      version1 <- parse(v1)
      version2 <- parse(v2)
    yield version1.compare(version2)

  /**
   * Check if version satisfies a constraint.
   * Supports: >=, <=, >, <, =, ^, ~
   */
  def satisfies(version: String, constraint: String): Option[Boolean] =
    val c = constraint.trim
    parse(version).flatMap { v =>
      if c.startsWith(">=") then
        parse(c.substring(2)).map(target => v >= target)
      else if c.startsWith("<=") then
        parse(c.substring(2)).map(target => v <= target)
      else if c.startsWith(">") then
        parse(c.substring(1)).map(target => v > target)
      else if c.startsWith("<") then
        parse(c.substring(1)).map(target => v < target)
      else if c.startsWith("=") then
        parse(c.substring(1)).map(target => v == target)
      else if c.startsWith("^") then
        // Caret: compatible with version (same major, if major > 0)
        parse(c.substring(1)).map { target =>
          if target.major == 0 then
            v.major == 0 && v.minor == target.minor && v >= target
          else
            v.major == target.major && v >= target
        }
      else if c.startsWith("~") then
        // Tilde: same major.minor
        parse(c.substring(1)).map { target =>
          v.major == target.major && v.minor == target.minor && v >= target
        }
      else
        // Exact match
        parse(c).map(target => v == target)
    }

  /**
   * Get the maximum version from a list.
   */
  def max(versions: Seq[String]): Option[Version] =
    versions.flatMap(parse).sorted.lastOption

  /**
   * Get the minimum version from a list.
   */
  def min(versions: Seq[String]): Option[Version] =
    versions.flatMap(parse).sorted.headOption

  /**
   * Filter versions by constraint.
   */
  def filter(versions: Seq[String], constraint: String): Seq[Version] =
    versions.flatMap { v =>
      parse(v).filter(_ => satisfies(v, constraint).getOrElse(false))
    }

  /**
   * Sort versions in ascending order.
   */
  def sort(versions: Seq[String]): Seq[Version] =
    versions.flatMap(parse).sorted

  /**
   * Sort versions in descending order.
   */
  def sortDescending(versions: Seq[String]): Seq[Version] =
    versions.flatMap(parse).sorted.reverse

  /**
   * Check if two versions are compatible (same major version, if major > 0).
   */
  def areCompatible(v1: String, v2: String): Option[Boolean] =
    for
      version1 <- parse(v1)
      version2 <- parse(v2)
    yield
      if version1.major == 0 || version2.major == 0 then
        version1.major == version2.major && version1.minor == version2.minor
      else
        version1.major == version2.major
