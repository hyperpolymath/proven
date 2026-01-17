// SPDX-License-Identifier: PMPL-1.0

package proven

import java.io.File
import java.nio.file.{Files, Path, Paths}
import scala.util.Try

/**
 * Result of a path operation.
 */
sealed trait PathResult {
  def isOk: Boolean
  def isError: Boolean = !isOk
  def getPath: Option[String]
  def getError: Option[String]
}

object PathResult {
  case class Ok(path: String) extends PathResult {
    override def isOk: Boolean = true
    override def getPath: Option[String] = Some(path)
    override def getError: Option[String] = None
  }

  case class Error(error: String) extends PathResult {
    override def isOk: Boolean = false
    override def getPath: Option[String] = None
    override def getError: Option[String] = Some(error)
  }

  def ok(path: String): PathResult = Ok(path)
  def error(msg: String): PathResult = Error(msg)
}

/**
 * Safe filesystem path operations with traversal protection.
 */
object SafePath {

  private val DangerousChars = Set('<', '>', ':', '"', '/', '\\', '|', '?', '*')

  /**
   * Check if a path contains directory traversal sequences.
   */
  def hasTraversal(path: String): Boolean = {
    path.contains("..") || path.startsWith("~")
  }

  /**
   * Check if a path is safe (no traversal).
   */
  def isSafePath(path: String): Boolean = !hasTraversal(path)

  /**
   * Sanitize a filename by removing dangerous characters.
   */
  def sanitizeFilename(filename: String): String = {
    var safe = filename
      .replace("..", "_")

    // Remove dangerous characters
    DangerousChars.foreach { c =>
      safe = safe.replace(c.toString, "_")
    }

    // Remove null bytes
    safe = safe.replace("\u0000", "_")

    // Remove leading/trailing dots and spaces
    safe = safe.stripPrefix(".").stripSuffix(".")
    safe = safe.trim

    safe
  }

  /**
   * Safely join path components, checking for traversal.
   */
  def join(base: String, parts: String*): PathResult = {
    val separator = File.separator

    var path = base.stripSuffix(separator)

    for (part <- parts) {
      if (hasTraversal(part)) {
        return PathResult.error("traversal_detected")
      }

      val safePart = sanitizeFilename(part)
      path = s"$path$separator$safePart"
    }

    PathResult.ok(path)
  }

  /**
   * Resolve path and verify it's within a base directory.
   */
  def resolveWithin(basePath: String, userPath: String): PathResult = {
    Try {
      val base = Paths.get(basePath).toRealPath()

      // Construct full path and resolve
      val full = base.resolve(userPath).normalize()

      // Check if resolved path exists and get real path
      val resolved = if (Files.exists(full)) {
        full.toRealPath()
      } else {
        full
      }

      // Verify the resolved path is within base
      if (!resolved.startsWith(base)) {
        PathResult.error("path_escapes_base")
      } else {
        PathResult.ok(resolved.toString)
      }
    }.getOrElse(PathResult.error("resolution_failed"))
  }

  /**
   * Get safe basename (strip directory components).
   */
  def safeBasename(path: String): String = {
    val file = new File(path)
    sanitizeFilename(file.getName)
  }

  /**
   * Check if filename has an allowed extension.
   */
  def hasAllowedExtension(filename: String, allowedExtensions: Seq[String]): Boolean = {
    val lastDot = filename.lastIndexOf('.')
    if (lastDot == -1 || lastDot == filename.length - 1) {
      false
    } else {
      val ext = filename.substring(lastDot + 1).toLowerCase
      allowedExtensions.exists(_.toLowerCase == ext)
    }
  }

  /**
   * Get file extension (lowercase).
   */
  def getExtension(filename: String): Option[String] = {
    val lastDot = filename.lastIndexOf('.')
    if (lastDot == -1 || lastDot == filename.length - 1) {
      None
    } else {
      Some(filename.substring(lastDot + 1).toLowerCase)
    }
  }

  /**
   * Check if path is absolute.
   */
  def isAbsolutePath(path: String): Boolean = {
    Paths.get(path).isAbsolute
  }

  /**
   * Check if path exists and is readable.
   */
  def isReadable(path: String): Boolean = {
    Try {
      val p = Paths.get(path)
      Files.exists(p) && Files.isReadable(p)
    }.getOrElse(false)
  }

  /**
   * Check if path is a regular file (not symlink, device, etc).
   */
  def isRegularFile(path: String): Boolean = {
    Try {
      val p = Paths.get(path)
      Files.isRegularFile(p)
    }.getOrElse(false)
  }

  /**
   * Check if path is a directory.
   */
  def isDirectory(path: String): Boolean = {
    Try {
      Files.isDirectory(Paths.get(path))
    }.getOrElse(false)
  }

  /**
   * Check if path is a symbolic link.
   */
  def isSymlink(path: String): Boolean = {
    Try {
      Files.isSymbolicLink(Paths.get(path))
    }.getOrElse(false)
  }

  /**
   * Create directory safely (checks for traversal first).
   */
  def safeMkdir(base: String, name: String): Boolean = {
    if (hasTraversal(name)) {
      false
    } else {
      Try {
        val safeName = sanitizeFilename(name)
        val fullPath = Paths.get(base, safeName)
        Files.createDirectories(fullPath)
        true
      }.getOrElse(false)
    }
  }

  /**
   * Normalize a path by resolving redundant separators.
   */
  def normalizePath(path: String): String = {
    Paths.get(path).normalize().toString
  }
}
