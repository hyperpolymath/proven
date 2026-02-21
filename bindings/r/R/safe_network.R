# SPDX-License-Identifier: PMPL-1.0-or-later
# SPDX-FileCopyrightText: 2025 Hyperpolymath

#' Safe network validation and operations.

#' IP address classification values.
#' @export
IP_LOOPBACK <- "loopback"
#' @export
IP_PRIVATE <- "private"
#' @export
IP_RESERVED <- "reserved"
#' @export
IP_PUBLIC <- "public"
#' @export
IP_INVALID <- "invalid"

#' Create an IPv4Address.
#'
#' @param a First octet
#' @param b Second octet
#' @param c Third octet
#' @param d Fourth octet
#' @return IPv4Address list
ipv4_address <- function(a, b, c, d) {
  list(a = a, b = b, c = c, d = d)
}

#' Parse IPv4 address string.
#'
#' @param address Address string
#' @return IPv4Address list or NULL
#' @export
parse_ipv4 <- function(address) {
  if (is.na(address)) return(NULL)

  parts <- strsplit(address, ".", fixed = TRUE)[[1]]
  if (length(parts) != 4) return(NULL)

  octets <- integer(4)
  for (i in 1:4) {
    part <- parts[i]

    # Check for empty or leading zeros
    if (nchar(part) == 0) return(NULL)
    if (nchar(part) > 1 && startsWith(part, "0")) return(NULL)

    # Check all digits
    if (!grepl("^[0-9]+$", part)) return(NULL)

    value <- as.integer(part)
    if (is.na(value) || value < 0 || value > 255) return(NULL)

    octets[i] <- value
  }

  ipv4_address(octets[1], octets[2], octets[3], octets[4])
}

#' Format IPv4Address as string.
#'
#' @param ip IPv4Address list
#' @return Formatted string
#' @export
format_ipv4 <- function(ip) {
  if (is.null(ip)) return(NA)
  paste(ip$a, ip$b, ip$c, ip$d, sep = ".")
}

#' Convert IPv4Address to integer.
#'
#' @param ip IPv4Address list
#' @return Integer representation
#' @export
ipv4_to_int <- function(ip) {
  if (is.null(ip)) return(NA)
  bitwOr(bitwOr(bitwOr(bitwShiftL(ip$a, 24), bitwShiftL(ip$b, 16)), bitwShiftL(ip$c, 8)), ip$d)
}

#' Check if IPv4 is a loopback address.
#'
#' @param ip IPv4Address list
#' @return TRUE if loopback
#' @export
is_loopback <- function(ip) {
  if (is.null(ip)) return(FALSE)
  ip$a == 127
}

#' Check if IPv4 is a private address (RFC 1918).
#'
#' @param ip IPv4Address list
#' @return TRUE if private
#' @export
is_private_ip <- function(ip) {
  if (is.null(ip)) return(FALSE)

  ip$a == 10 ||
    (ip$a == 172 && ip$b >= 16 && ip$b <= 31) ||
    (ip$a == 192 && ip$b == 168)
}

#' Check if IPv4 is a reserved address.
#'
#' @param ip IPv4Address list
#' @return TRUE if reserved
#' @export
is_reserved <- function(ip) {
  if (is.null(ip)) return(FALSE)

  ip$a == 0 ||
    (ip$a == 100 && ip$b >= 64 && ip$b <= 127) ||
    (ip$a == 169 && ip$b == 254) ||
    (ip$a == 192 && ip$b == 0 && ip$c == 0) ||
    (ip$a == 192 && ip$b == 0 && ip$c == 2) ||
    (ip$a == 198 && ip$b == 51 && ip$c == 100) ||
    (ip$a == 203 && ip$b == 0 && ip$c == 113) ||
    (ip$a >= 224 && ip$a <= 239) ||
    ip$a >= 240
}

#' Check if IPv4 is a public address.
#'
#' @param ip IPv4Address list
#' @return TRUE if public
#' @export
is_public_ip <- function(ip) {
  if (is.null(ip)) return(FALSE)
  !is_loopback(ip) && !is_private_ip(ip) && !is_reserved(ip)
}

#' Classify an IPv4 address.
#'
#' @param ip IPv4Address list
#' @return Classification string
#' @export
classify_ip <- function(ip) {
  if (is.null(ip)) return(IP_INVALID)

  if (is_loopback(ip)) return(IP_LOOPBACK)
  if (is_private_ip(ip)) return(IP_PRIVATE)
  if (is_reserved(ip)) return(IP_RESERVED)
  IP_PUBLIC
}

#' Classify IPv4 from string.
#'
#' @param address Address string
#' @return Classification string
#' @export
classify_ip_string <- function(address) {
  ip <- parse_ipv4(address)
  classify_ip(ip)
}

#' Check if IP is in CIDR range.
#'
#' @param ip IPv4Address list
#' @param network Network IPv4Address list
#' @param prefix_length CIDR prefix length
#' @return TRUE if in range
#' @export
ip_in_range <- function(ip, network, prefix_length) {
  if (is.null(ip) || is.null(network)) return(FALSE)
  if (prefix_length < 0 || prefix_length > 32) return(FALSE)

  if (prefix_length == 0) {
    mask <- 0
  } else {
    mask <- bitwShiftL(0xFFFFFFFF, 32 - prefix_length)
  }

  bitwAnd(ipv4_to_int(ip), mask) == bitwAnd(ipv4_to_int(network), mask)
}

#' Check if string is valid IPv4.
#'
#' @param address Address string
#' @return TRUE if valid
#' @export
is_valid_ipv4 <- function(address) {
  !is.null(parse_ipv4(address))
}

#' Check if port number is valid.
#'
#' @param port Port number
#' @return TRUE if valid
#' @export
is_valid_port <- function(port) {
  !is.na(port) && port >= 1 && port <= 65535
}

#' Check if port is privileged.
#'
#' @param port Port number
#' @return TRUE if privileged
#' @export
is_privileged_port <- function(port) {
  !is.na(port) && port >= 1 && port < 1024
}

#' Check if hostname is valid.
#'
#' @param hostname Hostname to check
#' @return TRUE if valid
#' @export
is_valid_hostname <- function(hostname) {
  if (is.na(hostname) || nchar(hostname) == 0 || nchar(hostname) > 253) {
    return(FALSE)
  }

  labels <- strsplit(hostname, ".", fixed = TRUE)[[1]]

  for (label in labels) {
    if (nchar(label) == 0 || nchar(label) > 63) return(FALSE)
    if (!grepl("^[a-zA-Z0-9]([a-zA-Z0-9-]*[a-zA-Z0-9])?$", label)) return(FALSE)
  }

  TRUE
}

#' Check if URL is valid (basic check).
#'
#' @param url URL to check
#' @return TRUE if valid
#' @export
is_valid_url <- function(url) {
  if (is.na(url)) return(FALSE)
  startsWith(url, "http://") || startsWith(url, "https://")
}

#' Check if URL host is a private IP (SSRF protection).
#'
#' @param url URL to check
#' @return TRUE if private
#' @export
is_private_url <- function(url) {
  if (is.na(url)) return(FALSE)

  # Extract host from URL
  host <- url

  # Remove protocol
  if (startsWith(host, "http://")) {
    host <- substr(host, 8, nchar(host))
  } else if (startsWith(host, "https://")) {
    host <- substr(host, 9, nchar(host))
  }

  # Remove path
  slash_pos <- regexpr("/", host, fixed = TRUE)
  if (slash_pos > 0) {
    host <- substr(host, 1, slash_pos - 1)
  }

  # Remove port
  colon_pos <- regexpr(":", host, fixed = TRUE)
  if (colon_pos > 0) {
    host <- substr(host, 1, colon_pos - 1)
  }

  # Check common private hosts
  if (host == "localhost" || host == "127.0.0.1" || host == "::1") {
    return(TRUE)
  }

  # Check if it's a private IP
  ip <- parse_ipv4(host)
  if (is.null(ip)) return(FALSE)

  is_private_ip(ip) || is_loopback(ip) || is_reserved(ip)
}
