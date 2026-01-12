# SPDX-License-Identifier: AGPL-3.0-or-later
# SPDX-FileCopyrightText: 2025 Hyperpolymath

#' Safe UUID validation, parsing, and generation.
#'
#' Provides an S3 class for UUIDs with parsing, formatting, and version detection.

#' UUID version constants.
#' @export
UUID_VERSION_1 <- 1L
#' @export
UUID_VERSION_3 <- 3L
#' @export
UUID_VERSION_4 <- 4L
#' @export
UUID_VERSION_5 <- 5L

#' Nil UUID constant (all zeros).
#' @export
UUID_NIL <- "00000000-0000-0000-0000-000000000000"

#' Create a UUID object.
#'
#' @param bytes Raw vector of 16 bytes
#' @return UUID S3 object
#' @keywords internal
new_uuid <- function(bytes) {

  if (length(bytes) != 16) {
    stop("UUID must be exactly 16 bytes")
  }
  structure(
    list(bytes = bytes),
    class = "UUID"
  )
}

#' Check if object is a UUID.
#'
#' @param x Object to check
#' @return TRUE if x is a UUID object
#' @export
is_uuid <- function(x) {
  inherits(x, "UUID")
}

#' Parse a UUID string.
#'
#' Parses standard UUID format (8-4-4-4-12 hex digits with hyphens).
#'
#' @param uuid_string UUID string to parse
#' @return UUID object or NULL on invalid input
#' @export
parse_uuid <- function(uuid_string) {
  if (is.na(uuid_string) || is.null(uuid_string)) {
    return(NULL)
  }

  # Remove braces if present (Microsoft format)
  uuid_string <- gsub("^\\{|\\}$", "", uuid_string)

  # Validate format: 8-4-4-4-12 hex digits
  pattern <- "^[0-9a-fA-F]{8}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{12}$"
  if (!grepl(pattern, uuid_string)) {
    return(NULL)
  }

  # Remove hyphens and convert to bytes
  hex_string <- gsub("-", "", uuid_string)

  bytes <- tryCatch({
    n <- nchar(hex_string)
    raw_bytes <- sapply(seq(1, n, by = 2), function(i) {
      strtoi(substr(hex_string, i, i + 1), base = 16L)
    })
    as.raw(raw_bytes)
  }, error = function(e) NULL)

  if (is.null(bytes) || length(bytes) != 16) {
    return(NULL)
  }

  new_uuid(bytes)
}

#' Format UUID object as string.
#'
#' @param x UUID object
#' @param ... Additional arguments (ignored)
#' @return Formatted UUID string in lowercase
#' @export
format.UUID <- function(x, ...) {
  format_uuid(x)
}

#' Print UUID object.
#'
#' @param x UUID object
#' @param ... Additional arguments (ignored)
#' @export
print.UUID <- function(x, ...) {
  cat("UUID:", format_uuid(x), "\n")
  invisible(x)
}

#' Format a UUID object as a string.
#'
#' @param uuid UUID object
#' @param uppercase Use uppercase hex digits (default FALSE)
#' @return Formatted UUID string
#' @export
format_uuid <- function(uuid, uppercase = FALSE) {
  if (is.null(uuid) || !is_uuid(uuid)) {
    return(NA_character_)
  }

  hex_chars <- format(as.hexmode(as.integer(uuid$bytes)), width = 2)
  hex_string <- paste0(hex_chars, collapse = "")

  # Insert hyphens at positions 8, 12, 16, 20
  formatted <- paste0(
    substr(hex_string, 1, 8), "-",
    substr(hex_string, 9, 12), "-",
    substr(hex_string, 13, 16), "-",
    substr(hex_string, 17, 20), "-",
    substr(hex_string, 21, 32)
  )

  if (uppercase) {
    toupper(formatted)
  } else {
    tolower(formatted)
  }
}

#' Validate a UUID string format.
#'
#' @param uuid_string UUID string to validate
#' @return TRUE if valid UUID format
#' @export
is_valid_uuid <- function(uuid_string) {
  !is.null(parse_uuid(uuid_string))
}

#' Get the version of a UUID.
#'
#' @param uuid UUID object or string
#' @return UUID version (1, 3, 4, 5) or NA if invalid
#' @export
uuid_version <- function(uuid) {
  if (is.character(uuid)) {
    uuid <- parse_uuid(uuid)
  }

  if (is.null(uuid) || !is_uuid(uuid)) {
    return(NA_integer_)
  }

  # Version is in bits 4-7 of byte 7 (index 7, 0-indexed)
  version_byte <- as.integer(uuid$bytes[7])
  bitwAnd(bitwShiftR(version_byte, 4), 0x0F)
}

#' Get the variant of a UUID.
#'
#' @param uuid UUID object or string
#' @return UUID variant string or NA if invalid
#' @export
uuid_variant <- function(uuid) {
  if (is.character(uuid)) {
    uuid <- parse_uuid(uuid)
  }

  if (is.null(uuid) || !is_uuid(uuid)) {
    return(NA_character_)
  }

  # Variant is in the high bits of byte 9 (index 8, 0-indexed)
  variant_byte <- as.integer(uuid$bytes[9])

  if (bitwAnd(variant_byte, 0x80) == 0x00) {
    "NCS"
  } else if (bitwAnd(variant_byte, 0xC0) == 0x80) {
    "RFC4122"
  } else if (bitwAnd(variant_byte, 0xE0) == 0xC0) {
    "Microsoft"
  } else {
    "Future"
  }
}

#' Check if UUID is the nil UUID.
#'
#' @param uuid UUID object or string
#' @return TRUE if nil UUID
#' @export
is_nil_uuid <- function(uuid) {
  if (is.character(uuid)) {
    uuid <- parse_uuid(uuid)
  }

  if (is.null(uuid) || !is_uuid(uuid)) {
    return(FALSE)
  }

  all(uuid$bytes == as.raw(0))
}

#' Generate a random UUID (version 4).
#'
#' Uses cryptographically secure random bytes from openssl.
#'
#' @return UUID object
#' @export
generate_uuid <- function() {
  # Generate 16 random bytes
  bytes <- openssl::rand_bytes(16)

  # Set version to 4 (random): byte 7, bits 4-7
  bytes[7] <- as.raw(bitwOr(bitwAnd(as.integer(bytes[7]), 0x0F), 0x40))

  # Set variant to RFC4122: byte 9, bits 6-7

  bytes[9] <- as.raw(bitwOr(bitwAnd(as.integer(bytes[9]), 0x3F), 0x80))

  new_uuid(bytes)
}

#' Generate a UUID v4 as a string.
#'
#' @return UUID string
#' @export
generate_uuid_string <- function() {
  format_uuid(generate_uuid())
}

#' Create a nil UUID.
#'
#' @return UUID object with all zero bytes
#' @export
nil_uuid <- function() {
  new_uuid(as.raw(rep(0, 16)))
}

#' Compare two UUIDs for equality.
#'
#' @param uuid1 First UUID (object or string)
#' @param uuid2 Second UUID (object or string)
#' @return TRUE if equal
#' @export
uuids_equal <- function(uuid1, uuid2) {
  if (is.character(uuid1)) {
    uuid1 <- parse_uuid(uuid1)
  }
  if (is.character(uuid2)) {
    uuid2 <- parse_uuid(uuid2)
  }

  if (is.null(uuid1) || is.null(uuid2)) {
    return(FALSE)
  }

  all(uuid1$bytes == uuid2$bytes)
}

#' Equality operator for UUID objects.
#'
#' @param e1 First UUID
#' @param e2 Second UUID
#' @return TRUE if equal
#' @export
`==.UUID` <- function(e1, e2) {
  uuids_equal(e1, e2)
}

#' Get raw bytes of a UUID.
#'
#' @param uuid UUID object or string
#' @return Raw vector of 16 bytes or NULL
#' @export
uuid_bytes <- function(uuid) {
  if (is.character(uuid)) {
    uuid <- parse_uuid(uuid)
  }

  if (is.null(uuid) || !is_uuid(uuid)) {
    return(NULL)
  }

  uuid$bytes
}

#' Create UUID from raw bytes.
#'
#' @param bytes Raw vector of 16 bytes
#' @return UUID object or NULL if invalid
#' @export
uuid_from_bytes <- function(bytes) {
  if (!is.raw(bytes) || length(bytes) != 16) {
    return(NULL)
  }

  new_uuid(bytes)
}

#' Parse UUID without hyphens.
#'
#' @param hex_string 32-character hex string
#' @return UUID object or NULL on invalid input
#' @export
parse_uuid_hex <- function(hex_string) {
  if (is.na(hex_string) || nchar(hex_string) != 32) {
    return(NULL)
  }

  if (!grepl("^[0-9a-fA-F]{32}$", hex_string)) {
    return(NULL)
  }

  # Insert hyphens and parse normally
  formatted <- paste0(
    substr(hex_string, 1, 8), "-",
    substr(hex_string, 9, 12), "-",
    substr(hex_string, 13, 16), "-",
    substr(hex_string, 17, 20), "-",
    substr(hex_string, 21, 32)
  )

  parse_uuid(formatted)
}

#' Format UUID without hyphens.
#'
#' @param uuid UUID object or string
#' @return 32-character hex string or NA
#' @export
format_uuid_hex <- function(uuid) {
  if (is.character(uuid)) {
    uuid <- parse_uuid(uuid)
  }

  if (is.null(uuid) || !is_uuid(uuid)) {
    return(NA_character_)
  }

  hex_chars <- format(as.hexmode(as.integer(uuid$bytes)), width = 2)
  tolower(paste0(hex_chars, collapse = ""))
}
