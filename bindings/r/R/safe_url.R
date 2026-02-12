# SPDX-License-Identifier: Apache-2.0
# SPDX-FileCopyrightText: 2025 Hyperpolymath

#' Safe URL parsing and validation.
#'
#' Provides URL parsing, validation, and component extraction with
#' proper handling of edge cases and security concerns.

#' Create a successful UrlResult.
#'
#' @param url_object ParsedUrl object
#' @return A UrlResult list
#' @keywords internal
url_ok <- function(url_object) {
  list(ok = TRUE, url = url_object, error = NULL)
}

#' Create an error UrlResult.
#'
#' @param message Error message
#' @return A UrlResult list
#' @keywords internal
url_error <- function(message) {
  list(ok = FALSE, url = NULL, error = message)
}

#' Create a ParsedUrl object.
#'
#' @param scheme URL scheme (e.g., "https")
#' @param username Optional username
#' @param password Optional password
#' @param host Host (domain or IP)
#' @param port Optional port
#' @param path Path component
#' @param query Optional query string
#' @param fragment Optional fragment
#' @return ParsedUrl S3 object
#' @keywords internal
new_parsed_url <- function(scheme, username, password, host, port, path, query, fragment) {
  structure(
    list(
      scheme = scheme,
      username = username,
      password = password,
      host = host,
      port = port,
      path = path,
      query = query,
      fragment = fragment
    ),
    class = "ParsedUrl"
  )
}

#' Check if object is a ParsedUrl.
#'
#' @param x Object to check
#' @return TRUE if x is a ParsedUrl object
#' @export
is_parsed_url <- function(x) {
  inherits(x, "ParsedUrl")
}

#' Format ParsedUrl object.
#'
#' @param x ParsedUrl object
#' @param ... Additional arguments (ignored)
#' @return Formatted URL string
#' @export
format.ParsedUrl <- function(x, ...) {
  format_url(x)
}

#' Print ParsedUrl object.
#'
#' @param x ParsedUrl object
#' @param ... Additional arguments (ignored)
#' @export
print.ParsedUrl <- function(x, ...) {
  cat("ParsedUrl:", format_url(x), "\n")
  cat("  Scheme:", x$scheme, "\n")
  cat("  Host:", x$host, "\n")
  if (!is.null(x$port)) cat("  Port:", x$port, "\n")
  cat("  Path:", x$path, "\n")
  if (!is.null(x$query)) cat("  Query:", x$query, "\n")
  if (!is.null(x$fragment)) cat("  Fragment:", x$fragment, "\n")
  invisible(x)
}

#' Parse a URL string.
#'
#' @param url URL string to parse
#' @return ParsedUrl object or NULL on invalid input
#' @export
parse_url <- function(url) {
  if (is.na(url) || is.null(url) || nchar(trimws(url)) == 0) {
    return(NULL)
  }

  url <- trimws(url)

  # Extract scheme
  scheme_match <- regexpr("^[a-zA-Z][a-zA-Z0-9+.-]*://", url)
  if (scheme_match == -1) {
    return(NULL)
  }

  scheme_end <- attr(scheme_match, "match.length")
  scheme <- tolower(substr(url, 1, scheme_end - 3))  # Remove ://
  rest <- substr(url, scheme_end + 1, nchar(url))

  # Extract fragment
  fragment <- NULL
  fragment_pos <- regexpr("#", rest, fixed = TRUE)
  if (fragment_pos > 0) {
    fragment <- substr(rest, fragment_pos + 1, nchar(rest))
    rest <- substr(rest, 1, fragment_pos - 1)
  }

  # Extract query
  query <- NULL
  query_pos <- regexpr("?", rest, fixed = TRUE)
  if (query_pos > 0) {
    query <- substr(rest, query_pos + 1, nchar(rest))
    rest <- substr(rest, 1, query_pos - 1)
  }

  # Extract path
  path_pos <- regexpr("/", rest, fixed = TRUE)
  if (path_pos > 0) {
    path <- substr(rest, path_pos, nchar(rest))
    authority <- substr(rest, 1, path_pos - 1)
  } else {
    path <- "/"
    authority <- rest
  }

  # Extract userinfo
  username <- NULL
  password <- NULL
  at_pos <- regexpr("@", authority, fixed = TRUE)
  if (at_pos > 0) {
    userinfo <- substr(authority, 1, at_pos - 1)
    authority <- substr(authority, at_pos + 1, nchar(authority))

    colon_pos <- regexpr(":", userinfo, fixed = TRUE)
    if (colon_pos > 0) {
      username <- substr(userinfo, 1, colon_pos - 1)
      password <- substr(userinfo, colon_pos + 1, nchar(userinfo))
    } else {
      username <- userinfo
    }
  }

  # Extract port
  port <- NULL
  if (startsWith(authority, "[")) {
    # IPv6
    bracket_end <- regexpr("]", authority, fixed = TRUE)
    if (bracket_end == -1) {
      return(NULL)
    }
    host <- substr(authority, 2, bracket_end - 1)
    rest_authority <- substr(authority, bracket_end + 1, nchar(authority))
    if (startsWith(rest_authority, ":")) {
      port_str <- substr(rest_authority, 2, nchar(rest_authority))
      port <- suppressWarnings(as.integer(port_str))
      if (is.na(port)) port <- NULL
    }
  } else {
    colon_pos <- regexpr(":", authority, fixed = TRUE)
    if (colon_pos > 0) {
      host <- substr(authority, 1, colon_pos - 1)
      port_str <- substr(authority, colon_pos + 1, nchar(authority))
      port <- suppressWarnings(as.integer(port_str))
      if (is.na(port)) {
        host <- authority
        port <- NULL
      }
    } else {
      host <- authority
    }
  }

  new_parsed_url(scheme, username, password, host, port, path, query, fragment)
}

#' Format a ParsedUrl object back to a URL string.
#'
#' @param url ParsedUrl object
#' @return URL string or NA
#' @export
format_url <- function(url) {
  if (is.null(url) || !is_parsed_url(url)) {
    return(NA_character_)
  }

  result <- paste0(url$scheme, "://")

  if (!is.null(url$username)) {
    result <- paste0(result, url$username)
    if (!is.null(url$password)) {
      result <- paste0(result, ":", url$password)
    }
    result <- paste0(result, "@")
  }

  result <- paste0(result, url$host)

  if (!is.null(url$port)) {
    result <- paste0(result, ":", url$port)
  }

  result <- paste0(result, url$path)

  if (!is.null(url$query)) {
    result <- paste0(result, "?", url$query)
  }

  if (!is.null(url$fragment)) {
    result <- paste0(result, "#", url$fragment)
  }

  result
}

#' Check if a URL string is valid.
#'
#' @param url URL string to validate
#' @return TRUE if valid
#' @export
is_valid_url_format <- function(url) {
  !is.null(parse_url(url))
}

#' Get the scheme from a URL.
#'
#' @param url URL string or ParsedUrl object
#' @return Scheme string or NA
#' @export
url_scheme <- function(url) {
  if (is.character(url)) {
    url <- parse_url(url)
  }

  if (is.null(url) || !is_parsed_url(url)) {
    return(NA_character_)
  }

  url$scheme
}

#' Get the host from a URL.
#'
#' @param url URL string or ParsedUrl object
#' @return Host string or NA
#' @export
url_host <- function(url) {
  if (is.character(url)) {
    url <- parse_url(url)
  }

  if (is.null(url) || !is_parsed_url(url)) {
    return(NA_character_)
  }

  url$host
}

#' Get the port from a URL.
#'
#' @param url URL string or ParsedUrl object
#' @return Port number or NA
#' @export
url_port <- function(url) {
  if (is.character(url)) {
    url <- parse_url(url)
  }

  if (is.null(url) || !is_parsed_url(url)) {
    return(NA_integer_)
  }

  if (is.null(url$port)) {
    # Return default port for scheme
    switch(url$scheme,
      "http" = 80L,
      "https" = 443L,
      "ftp" = 21L,
      "ssh" = 22L,
      NA_integer_
    )
  } else {
    as.integer(url$port)
  }
}

#' Get the path from a URL.
#'
#' @param url URL string or ParsedUrl object
#' @return Path string or NA
#' @export
url_path <- function(url) {
  if (is.character(url)) {
    url <- parse_url(url)
  }

  if (is.null(url) || !is_parsed_url(url)) {
    return(NA_character_)
  }

  url$path
}

#' Get the query string from a URL.
#'
#' @param url URL string or ParsedUrl object
#' @return Query string or NA
#' @export
url_query <- function(url) {
  if (is.character(url)) {
    url <- parse_url(url)
  }

  if (is.null(url) || !is_parsed_url(url)) {
    return(NA_character_)
  }

  if (is.null(url$query)) NA_character_ else url$query
}

#' Parse query string into named list.
#'
#' @param query_string Query string (without leading ?)
#' @return Named list of parameters
#' @export
parse_query_string <- function(query_string) {
  if (is.na(query_string) || is.null(query_string) || nchar(query_string) == 0) {
    return(list())
  }

  pairs <- strsplit(query_string, "&", fixed = TRUE)[[1]]
  result <- list()

  for (pair in pairs) {
    if (nchar(pair) == 0) next

    eq_pos <- regexpr("=", pair, fixed = TRUE)
    if (eq_pos > 0) {
      key <- utils::URLdecode(substr(pair, 1, eq_pos - 1))
      value <- utils::URLdecode(substr(pair, eq_pos + 1, nchar(pair)))
    } else {
      key <- utils::URLdecode(pair)
      value <- ""
    }
    result[[key]] <- value
  }

  result
}

#' Build query string from named list.
#'
#' @param params Named list of parameters
#' @return Query string
#' @export
build_query_string <- function(params) {
  if (length(params) == 0) {
    return("")
  }

  pairs <- mapply(function(key, value) {
    paste0(utils::URLencode(key, reserved = TRUE), "=",
           utils::URLencode(as.character(value), reserved = TRUE))
  }, names(params), params, SIMPLIFY = TRUE, USE.NAMES = FALSE)

  paste(pairs, collapse = "&")
}

#' Check if URL uses HTTPS.
#'
#' @param url URL string or ParsedUrl object
#' @return TRUE if HTTPS
#' @export
is_https <- function(url) {
  scheme <- url_scheme(url)
  !is.na(scheme) && scheme == "https"
}

#' Check if URL uses HTTP.
#'
#' @param url URL string or ParsedUrl object
#' @return TRUE if HTTP
#' @export
is_http <- function(url) {
  scheme <- url_scheme(url)
  !is.na(scheme) && scheme == "http"
}

#' Get the origin of a URL (scheme + host + port).
#'
#' @param url URL string or ParsedUrl object
#' @return Origin string or NA
#' @export
url_origin <- function(url) {
  if (is.character(url)) {
    url <- parse_url(url)
  }

  if (is.null(url) || !is_parsed_url(url)) {
    return(NA_character_)
  }

  result <- paste0(url$scheme, "://", url$host)

  if (!is.null(url$port)) {
    result <- paste0(result, ":", url$port)
  }

  result
}

#' Join a base URL with a relative path.
#'
#' @param base_url Base URL string
#' @param relative_path Relative path
#' @return Combined URL string or NA
#' @export
url_join <- function(base_url, relative_path) {
  base <- parse_url(base_url)
  if (is.null(base)) {
    return(NA_character_)
  }

  # If relative_path is absolute, parse it
  if (startsWith(relative_path, "http://") || startsWith(relative_path, "https://")) {
    return(relative_path)
  }

  # If starts with //, use base scheme

if (startsWith(relative_path, "//")) {
    return(paste0(base$scheme, ":", relative_path))
  }

  # If starts with /, replace path
  if (startsWith(relative_path, "/")) {
    base$path <- relative_path
    base$query <- NULL
    base$fragment <- NULL
    return(format_url(base))
  }

  # Relative path - combine with base path
  base_path <- base$path
  if (!endsWith(base_path, "/")) {
    base_path <- dirname(base_path)
    if (base_path == ".") base_path <- "/"
    if (!endsWith(base_path, "/")) base_path <- paste0(base_path, "/")
  }

  base$path <- paste0(base_path, relative_path)
  base$query <- NULL
  base$fragment <- NULL

  format_url(base)
}
