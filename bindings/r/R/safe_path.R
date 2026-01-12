# SPDX-License-Identifier: PMPL-1.0
# SPDX-FileCopyrightText: 2025 Hyperpolymath

#' Safe path operations for directory traversal prevention.

# Dangerous path patterns
.TRAVERSAL_PATTERNS <- c("..", "./", ".\\", "%2e%2e", "%2e.", ".%2e", "%00")

#' Create a successful PathResult.
#'
#' @param path The valid path
#' @return A PathResult list
path_ok <- function(path) {
  list(ok = TRUE, path = path, error = NULL)
}

#' Create an error PathResult.
#'
#' @param message Error message
#' @return A PathResult list
path_error <- function(message) {
  list(ok = FALSE, path = NULL, error = message)
}

#' Check if path contains traversal sequences.
#'
#' @param path Path to check
#' @return TRUE if traversal detected
#' @export
has_traversal <- function(path) {
  if (is.na(path)) return(TRUE)

  normalized <- tolower(path)
  for (pattern in .TRAVERSAL_PATTERNS) {
    if (grepl(pattern, normalized, fixed = TRUE)) {
      return(TRUE)
    }
  }
  FALSE
}

#' Sanitize a filename.
#'
#' @param filename Filename to sanitize
#' @return Sanitized filename
#' @export
sanitize_filename <- function(filename) {
  if (is.na(filename)) return("unnamed")

  result <- filename

  # Remove directory separators
  result <- gsub("[/\\\\]", "_", result)

  # Remove null bytes
  result <- gsub("\x00", "", result)

  # Remove leading dots
  result <- gsub("^\\.+", "", result)

  # Replace dangerous chars
  result <- gsub('[<>:"|?*]', "_", result)

  # Collapse multiple underscores
  result <- gsub("_+", "_", result)

  # Trim underscores
  result <- gsub("^_+|_+$", "", result)

  if (nchar(result) == 0) {
    return("unnamed")
  }

  result
}

#' Join paths safely.
#'
#' @param base Base path
#' @param ... Components to join
#' @return PathResult list
#' @export
path_join <- function(base, ...) {
  components <- list(...)

  for (component in components) {
    if (has_traversal(component)) {
      return(path_error(paste("Path traversal detected in component:", component)))
    }
  }

  result <- base
  for (component in components) {
    # Remove leading slashes
    clean <- gsub("^[/\\\\]+", "", component)
    result <- file.path(result, clean)
  }

  path_ok(result)
}

#' Resolve path within base directory.
#'
#' @param base Base directory
#' @param path Path to resolve
#' @return PathResult list
#' @export
resolve_within <- function(base, path) {
  if (has_traversal(path)) {
    return(path_error("Path traversal detected"))
  }

  # Get absolute paths
  abs_base <- normalizePath(base, mustWork = FALSE)

  # Clean the path
  clean_path <- gsub("^[/\\\\]+", "", path)
  full_path <- normalizePath(file.path(abs_base, clean_path), mustWork = FALSE)

  # Verify it's within base
  if (!startsWith(full_path, abs_base)) {
    return(path_error("Path escapes base directory"))
  }

  path_ok(full_path)
}

#' Get file extension safely.
#'
#' @param path Path to get extension from
#' @return Extension or NA
#' @export
get_extension <- function(path) {
  if (is.na(path) || nchar(path) == 0) return(NA)

  base <- basename(path)
  if (startsWith(base, ".")) return(NA)

  ext <- tools::file_ext(path)
  if (nchar(ext) == 0) return(NA)

  paste0(".", ext)
}

#' Check if extension is allowed.
#'
#' @param path Path to check
#' @param allowed Vector of allowed extensions
#' @return TRUE if allowed
#' @export
extension_allowed <- function(path, allowed) {
  ext <- get_extension(path)
  if (is.na(ext)) return(FALSE)

  tolower(ext) %in% tolower(allowed)
}

#' Normalize path separators to forward slash.
#'
#' @param path Path to normalize
#' @return Normalized path
#' @export
normalize_separators <- function(path) {
  if (is.na(path)) return(NA)
  gsub("\\\\", "/", path)
}

#' Check if path is absolute.
#'
#' @param path Path to check
#' @return TRUE if absolute
#' @export
is_absolute <- function(path) {
  if (is.na(path) || nchar(path) == 0) return(FALSE)

  # Unix absolute
  if (startsWith(path, "/")) return(TRUE)

  # Windows absolute (e.g., C:)
  if (nchar(path) >= 2 && substr(path, 2, 2) == ":") return(TRUE)

  FALSE
}

#' Check if path is relative.
#'
#' @param path Path to check
#' @return TRUE if relative
#' @export
is_relative <- function(path) {
  !is_absolute(path)
}

#' Get parent directory.
#'
#' @param path Path to get parent from
#' @return Parent directory or NA
#' @export
get_parent <- function(path) {
  if (is.na(path) || nchar(path) == 0) return(NA)
  dirname(path)
}

#' Get filename from path.
#'
#' @param path Path to get filename from
#' @return Filename
#' @export
get_filename <- function(path) {
  if (is.na(path)) return(NA)
  basename(path)
}

#' Check if filename is hidden (starts with dot).
#'
#' @param path Path to check
#' @return TRUE if hidden
#' @export
is_hidden <- function(path) {
  if (is.na(path)) return(FALSE)
  base <- basename(path)
  nchar(base) > 0 && startsWith(base, ".")
}
