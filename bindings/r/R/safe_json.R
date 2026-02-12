# SPDX-License-Identifier: Apache-2.0
# SPDX-FileCopyrightText: 2025 Hyperpolymath

#' Safe JSON parsing and validation.
#'
#' Provides JSON syntax validation without full parsing,
#' safe access to nested values, and JSON path operations.

#' Validate JSON syntax without full parsing.
#'
#' Checks for balanced braces/brackets and proper string handling.
#'
#' @param json_string JSON string to validate
#' @return TRUE if valid JSON syntax
#' @export
is_valid_json <- function(json_string) {
  if (is.na(json_string) || is.null(json_string) || nchar(json_string) == 0) {
    return(FALSE)
  }

  depth_brace <- 0L
  depth_bracket <- 0L
  in_string <- FALSE
  escape <- FALSE

  chars <- strsplit(json_string, "")[[1]]

  for (char in chars) {
    if (escape) {
      escape <- FALSE
      next
    }

    if (char == "\\" && in_string) {
      escape <- TRUE
      next
    }

    if (char == '"') {
      in_string <- !in_string
      next
    }

    if (!in_string) {
      if (char == "{") {
        depth_brace <- depth_brace + 1L
      } else if (char == "}") {
        depth_brace <- depth_brace - 1L
      } else if (char == "[") {
        depth_bracket <- depth_bracket + 1L
      } else if (char == "]") {
        depth_bracket <- depth_bracket - 1L
      }

      if (depth_brace < 0L || depth_bracket < 0L) {
        return(FALSE)
      }
    }
  }

  depth_brace == 0L && depth_bracket == 0L && !in_string
}

#' Parse JSON string safely.
#'
#' Returns NULL on parse error instead of throwing.
#'
#' @param json_string JSON string to parse
#' @return Parsed R object or NULL on error
#' @export
safe_json_parse <- function(json_string) {
  if (!is_valid_json(json_string)) {
    return(NULL)
  }

  tryCatch({
    jsonlite::fromJSON(json_string, simplifyVector = FALSE)
  }, error = function(e) NULL)
}

#' Safely get a value from a nested structure.
#'
#' @param object Parsed JSON object (list)
#' @param path Vector of keys/indices for path
#' @param default Default value if path not found
#' @return Value at path or default
#' @export
json_get <- function(object, path, default = NULL) {
  if (is.null(object) || length(path) == 0) {
    return(default)
  }

  current <- object

  for (key in path) {
    if (is.null(current)) {
      return(default)
    }

    if (is.numeric(key)) {
      # Array index (1-based for R)
      idx <- as.integer(key)
      if (is.list(current) && idx >= 1 && idx <= length(current)) {
        current <- current[[idx]]
      } else {
        return(default)
      }
    } else {
      # Object key
      if (is.list(current) && !is.null(names(current)) && key %in% names(current)) {
        current <- current[[key]]
      } else {
        return(default)
      }
    }
  }

  current
}

#' Check if a path exists in a JSON object.
#'
#' @param object Parsed JSON object (list)
#' @param path Vector of keys/indices for path
#' @return TRUE if path exists
#' @export
json_has_path <- function(object, path) {
  sentinel <- structure(list(), class = "json_sentinel")
  result <- json_get(object, path, sentinel)
  !inherits(result, "json_sentinel")
}

#' Get the type of a JSON value.
#'
#' @param value R value from parsed JSON
#' @return Type string: "object", "array", "string", "number", "boolean", "null"
#' @export
json_type <- function(value) {
  if (is.null(value)) {
    return("null")
  }

  if (is.logical(value)) {
    return("boolean")
  }

  if (is.numeric(value)) {
    return("number")
  }

  if (is.character(value)) {
    return("string")
  }

  if (is.list(value)) {
    if (is.null(names(value)) || length(names(value)) == 0 ||
        (length(value) > 0 && all(names(value) == ""))) {
      return("array")
    } else {
      return("object")
    }
  }

  "unknown"
}

#' Safely stringify an R object to JSON.
#'
#' @param object R object to stringify
#' @param pretty Use pretty printing (default FALSE)
#' @return JSON string or NA on error
#' @export
safe_json_stringify <- function(object, pretty = FALSE) {
  tryCatch({
    jsonlite::toJSON(object, auto_unbox = TRUE, pretty = pretty, null = "null")
  }, error = function(e) NA_character_)
}

#' Validate JSON against a simple schema.
#'
#' Schema is a list with "type" and optionally "properties" for objects.
#'
#' @param object Parsed JSON object
#' @param schema Schema definition
#' @return TRUE if valid, FALSE otherwise
#' @export
json_validate_schema <- function(object, schema) {
  if (!is.list(schema) || is.null(schema$type)) {
    return(FALSE)
  }

  actual_type <- json_type(object)

  if (actual_type != schema$type) {
    return(FALSE)
  }

  if (schema$type == "object" && !is.null(schema$properties)) {
    for (prop_name in names(schema$properties)) {
      prop_schema <- schema$properties[[prop_name]]
      if (!is.null(prop_schema$required) && prop_schema$required) {
        if (!prop_name %in% names(object)) {
          return(FALSE)
        }
      }
      if (prop_name %in% names(object)) {
        if (!json_validate_schema(object[[prop_name]], prop_schema)) {
          return(FALSE)
        }
      }
    }
  }

  if (schema$type == "array" && !is.null(schema$items)) {
    for (item in object) {
      if (!json_validate_schema(item, schema$items)) {
        return(FALSE)
      }
    }
  }

  TRUE
}

#' Merge two JSON objects.
#'
#' @param base Base object
#' @param overlay Overlay object (takes precedence)
#' @param deep Perform deep merge (default TRUE)
#' @return Merged object
#' @export
json_merge <- function(base, overlay, deep = TRUE) {
  if (!is.list(base) || !is.list(overlay)) {
    return(overlay)
  }

  # If both are arrays (no names), concatenate
  if ((is.null(names(base)) || all(names(base) == "")) &&
      (is.null(names(overlay)) || all(names(overlay) == ""))) {
    return(c(base, overlay))
  }

  # Object merge
  result <- base

  for (key in names(overlay)) {
    if (deep && key %in% names(result) &&
        is.list(result[[key]]) && is.list(overlay[[key]])) {
      result[[key]] <- json_merge(result[[key]], overlay[[key]], deep = TRUE)
    } else {
      result[[key]] <- overlay[[key]]
    }
  }

  result
}

#' Extract all keys from a JSON object.
#'
#' @param object Parsed JSON object
#' @return Vector of keys or NULL
#' @export
json_keys <- function(object) {
  if (!is.list(object) || is.null(names(object))) {
    return(NULL)
  }

  names(object)
}

#' Extract all values from a JSON object.
#'
#' @param object Parsed JSON object
#' @return List of values or NULL
#' @export
json_values <- function(object) {
  if (!is.list(object)) {
    return(NULL)
  }

  unname(object)
}

#' Count elements in a JSON array or object.
#'
#' @param object Parsed JSON object or array
#' @return Length or NA
#' @export
json_length <- function(object) {
  if (is.null(object) || !is.list(object)) {
    return(NA_integer_)
  }

  length(object)
}

#' Flatten a nested JSON object.
#'
#' @param object Parsed JSON object
#' @param separator Key separator (default ".")
#' @param prefix Current prefix (internal use)
#' @return Flattened list with dotted keys
#' @export
json_flatten <- function(object, separator = ".", prefix = "") {
  if (!is.list(object)) {
    result <- list()
    result[[prefix]] <- object
    return(result)
  }

  result <- list()

  if (!is.null(names(object)) && any(names(object) != "")) {
    # Object
    for (key in names(object)) {
      new_prefix <- if (nchar(prefix) == 0) key else paste0(prefix, separator, key)
      sub_result <- json_flatten(object[[key]], separator, new_prefix)
      result <- c(result, sub_result)
    }
  } else {
    # Array
    for (i in seq_along(object)) {
      new_prefix <- if (nchar(prefix) == 0) {
        as.character(i - 1)
      } else {
        paste0(prefix, separator, i - 1)
      }
      sub_result <- json_flatten(object[[i]], separator, new_prefix)
      result <- c(result, sub_result)
    }
  }

  result
}
