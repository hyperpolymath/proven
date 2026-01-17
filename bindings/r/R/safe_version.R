# SPDX-License-Identifier: PMPL-1.0
# SPDX-FileCopyrightText: 2025 Hyperpolymath

#' Safe semantic versioning parsing and comparison.
#'
#' Provides SemVer parsing with validation and safe comparison
#' operations following the Semantic Versioning 2.0.0 specification.

#' Create a Version object.
#'
#' @param major Major version number
#' @param minor Minor version number
#' @param patch Patch version number
#' @param prerelease Optional prerelease string
#' @param build_metadata Optional build metadata string
#' @return Version S3 object
#' @keywords internal
new_version <- function(major, minor, patch, prerelease = NULL, build_metadata = NULL) {
  structure(
    list(
      major = as.integer(major),
      minor = as.integer(minor),
      patch = as.integer(patch),
      prerelease = prerelease,
      build_metadata = build_metadata
    ),
    class = "Version"
  )
}

#' Check if object is a Version.
#'
#' @param x Object to check
#' @return TRUE if x is a Version object
#' @export
is_version <- function(x) {
  inherits(x, "Version")
}

#' Format Version object.
#'
#' @param x Version object
#' @param ... Additional arguments (ignored)
#' @return Formatted version string
#' @export
format.Version <- function(x, ...) {
  version_to_string(x)
}

#' Print Version object.
#'
#' @param x Version object
#' @param ... Additional arguments (ignored)
#' @export
print.Version <- function(x, ...) {
  cat("Version:", version_to_string(x), "\n")
  invisible(x)
}

#' Create a new version.
#'
#' @param major Major version number
#' @param minor Minor version number
#' @param patch Patch version number
#' @return Version object
#' @export
version <- function(major, minor, patch) {
  new_version(major, minor, patch)
}

#' Add prerelease to version.
#'
#' @param ver Version object
#' @param prerelease Prerelease string
#' @return New Version object with prerelease
#' @export
version_with_prerelease <- function(ver, prerelease) {
  if (!is_version(ver)) return(NULL)
  new_version(ver$major, ver$minor, ver$patch, prerelease, ver$build_metadata)
}

#' Add build metadata to version.
#'
#' @param ver Version object
#' @param build Build metadata string
#' @return New Version object with build metadata
#' @export
version_with_build <- function(ver, build) {
  if (!is_version(ver)) return(NULL)
  new_version(ver$major, ver$minor, ver$patch, ver$prerelease, build)
}

#' Parse a version string.
#'
#' @param version_string Version string (e.g., "1.2.3-alpha+build")
#' @return Version object or NULL on parse error
#' @export
parse_version <- function(version_string) {
  if (is.na(version_string) || is.null(version_string)) {
    return(NULL)
  }

  s <- trimws(version_string)

  # Remove optional 'v' prefix
  if (startsWith(tolower(s), "v")) {
    s <- substr(s, 2, nchar(s))
  }

  # Split off build metadata
  build_metadata <- NULL
  plus_pos <- regexpr("+", s, fixed = TRUE)
  if (plus_pos > 0) {
    build_metadata <- substr(s, plus_pos + 1, nchar(s))
    s <- substr(s, 1, plus_pos - 1)
  }

  # Split off prerelease
  prerelease <- NULL
  dash_pos <- regexpr("-", s, fixed = TRUE)
  if (dash_pos > 0) {
    prerelease <- substr(s, dash_pos + 1, nchar(s))
    s <- substr(s, 1, dash_pos - 1)
  }

  # Parse major.minor.patch
  parts <- strsplit(s, ".", fixed = TRUE)[[1]]
  if (length(parts) != 3) {
    return(NULL)
  }

  major <- suppressWarnings(as.integer(parts[1]))
  minor <- suppressWarnings(as.integer(parts[2]))
  patch <- suppressWarnings(as.integer(parts[3]))

  if (is.na(major) || is.na(minor) || is.na(patch)) {
    return(NULL)
  }

  if (major < 0 || minor < 0 || patch < 0) {
    return(NULL)
  }

  new_version(major, minor, patch, prerelease, build_metadata)
}

#' Convert version to string.
#'
#' @param ver Version object
#' @return Version string or NA
#' @export
version_to_string <- function(ver) {
  if (is.null(ver) || !is_version(ver)) {
    return(NA_character_)
  }

  s <- sprintf("%d.%d.%d", ver$major, ver$minor, ver$patch)

  if (!is.null(ver$prerelease)) {
    s <- paste0(s, "-", ver$prerelease)
  }

  if (!is.null(ver$build_metadata)) {
    s <- paste0(s, "+", ver$build_metadata)
  }

  s
}

#' Check if version is a prerelease.
#'
#' @param ver Version object or string
#' @return TRUE if prerelease
#' @export
version_is_prerelease <- function(ver) {
  if (is.character(ver)) {
    ver <- parse_version(ver)
  }

  if (is.null(ver) || !is_version(ver)) {
    return(NA)
  }

  !is.null(ver$prerelease)
}

#' Check if version is stable (>= 1.0.0 and no prerelease).
#'
#' @param ver Version object or string
#' @return TRUE if stable
#' @export
version_is_stable <- function(ver) {
  if (is.character(ver)) {
    ver <- parse_version(ver)
  }

  if (is.null(ver) || !is_version(ver)) {
    return(NA)
  }

  ver$major >= 1 && is.null(ver$prerelease)
}

#' Increment major version.
#'
#' @param ver Version object
#' @return New Version object
#' @export
version_bump_major <- function(ver) {
  if (!is_version(ver)) return(NULL)
  new_version(ver$major + 1, 0, 0)
}

#' Increment minor version.
#'
#' @param ver Version object
#' @return New Version object
#' @export
version_bump_minor <- function(ver) {
  if (!is_version(ver)) return(NULL)
  new_version(ver$major, ver$minor + 1, 0)
}

#' Increment patch version.
#'
#' @param ver Version object
#' @return New Version object
#' @export
version_bump_patch <- function(ver) {
  if (!is_version(ver)) return(NULL)
  new_version(ver$major, ver$minor, ver$patch + 1)
}

#' Compare two versions.
#'
#' @param ver1 First Version object or string
#' @param ver2 Second Version object or string
#' @return -1 if ver1 < ver2, 0 if equal, 1 if ver1 > ver2, NA if invalid
#' @export
version_compare <- function(ver1, ver2) {
  if (is.character(ver1)) ver1 <- parse_version(ver1)
  if (is.character(ver2)) ver2 <- parse_version(ver2)

  if (is.null(ver1) || is.null(ver2) || !is_version(ver1) || !is_version(ver2)) {
    return(NA_integer_)
  }

  # Compare major.minor.patch
  if (ver1$major != ver2$major) {
    return(if (ver1$major < ver2$major) -1L else 1L)
  }

  if (ver1$minor != ver2$minor) {
    return(if (ver1$minor < ver2$minor) -1L else 1L)
  }

  if (ver1$patch != ver2$patch) {
    return(if (ver1$patch < ver2$patch) -1L else 1L)
  }

  # Prerelease comparison
  pre1 <- ver1$prerelease
  pre2 <- ver2$prerelease

  if (is.null(pre1) && is.null(pre2)) return(0L)
  if (is.null(pre1)) return(1L)  # No prerelease > prerelease
  if (is.null(pre2)) return(-1L)

  # Compare prerelease strings
  if (pre1 == pre2) return(0L)
  if (pre1 < pre2) return(-1L)
  return(1L)
}

#' Check if version satisfies a constraint.
#'
#' Supports: >=, <=, >, <, =, ^, ~ operators.
#'
#' @param ver Version object or string
#' @param constraint Constraint string (e.g., ">=1.0.0", "^1.2.0")
#' @return TRUE if satisfies, FALSE otherwise, NA on error
#' @export
version_satisfies <- function(ver, constraint) {
  if (is.character(ver)) ver <- parse_version(ver)
  if (is.null(ver) || !is_version(ver)) return(NA)

  constraint <- trimws(constraint)

  # Parse operator and target version
  if (startsWith(constraint, ">=")) {
    target <- parse_version(substr(constraint, 3, nchar(constraint)))
    if (is.null(target)) return(NA)
    return(version_compare(ver, target) >= 0)
  }

  if (startsWith(constraint, "<=")) {
    target <- parse_version(substr(constraint, 3, nchar(constraint)))
    if (is.null(target)) return(NA)
    return(version_compare(ver, target) <= 0)
  }

  if (startsWith(constraint, ">") && !startsWith(constraint, ">=")) {
    target <- parse_version(substr(constraint, 2, nchar(constraint)))
    if (is.null(target)) return(NA)
    return(version_compare(ver, target) > 0)
  }

  if (startsWith(constraint, "<") && !startsWith(constraint, "<=")) {
    target <- parse_version(substr(constraint, 2, nchar(constraint)))
    if (is.null(target)) return(NA)
    return(version_compare(ver, target) < 0)
  }

  if (startsWith(constraint, "=")) {
    target <- parse_version(substr(constraint, 2, nchar(constraint)))
    if (is.null(target)) return(NA)
    return(version_compare(ver, target) == 0)
  }

  if (startsWith(constraint, "^")) {
    # Caret: compatible with version (same major, if major > 0)
    target <- parse_version(substr(constraint, 2, nchar(constraint)))
    if (is.null(target)) return(NA)

    if (target$major == 0) {
      return(ver$major == 0 && ver$minor == target$minor &&
             version_compare(ver, target) >= 0)
    }
    return(ver$major == target$major && version_compare(ver, target) >= 0)
  }

  if (startsWith(constraint, "~")) {
    # Tilde: same major.minor
    target <- parse_version(substr(constraint, 2, nchar(constraint)))
    if (is.null(target)) return(NA)
    return(ver$major == target$major && ver$minor == target$minor &&
           version_compare(ver, target) >= 0)
  }

  # Exact match
  target <- parse_version(constraint)
  if (is.null(target)) return(NA)
  version_compare(ver, target) == 0
}

#' Equality operator for Version objects.
#'
#' @param e1 First Version
#' @param e2 Second Version
#' @return TRUE if equal
#' @export
`==.Version` <- function(e1, e2) {
  version_compare(e1, e2) == 0
}

#' Less than operator for Version objects.
#'
#' @param e1 First Version
#' @param e2 Second Version
#' @return TRUE if e1 < e2
#' @export
`<.Version` <- function(e1, e2) {
  version_compare(e1, e2) < 0
}

#' Greater than operator for Version objects.
#'
#' @param e1 First Version
#' @param e2 Second Version
#' @return TRUE if e1 > e2
#' @export
`>.Version` <- function(e1, e2) {
  version_compare(e1, e2) > 0
}
