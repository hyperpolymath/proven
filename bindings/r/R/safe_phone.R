# SPDX-License-Identifier: AGPL-3.0-or-later
# SPDX-FileCopyrightText: 2025 Hyperpolymath

#' Safe phone number validation, parsing, and formatting.
#'
#' Provides phone number parsing, validation, and E.164 formatting
#' with country code support.

#' Country calling codes and phone number patterns.
#'
#' Named list with country code, country name, and phone pattern info.
#' @export
country_codes <- list(
  # North America (NANP)
  US = list(code = "+1", name = "United States", pattern = "^[2-9][0-9]{9}$", length = 10L),
  CA = list(code = "+1", name = "Canada", pattern = "^[2-9][0-9]{9}$", length = 10L),
  # Europe
  GB = list(code = "+44", name = "United Kingdom", pattern = "^[1-9][0-9]{9,10}$", length = c(10L, 11L)),
  DE = list(code = "+49", name = "Germany", pattern = "^[1-9][0-9]{5,14}$", length = c(6L, 15L)),
  FR = list(code = "+33", name = "France", pattern = "^[1-9][0-9]{8}$", length = 9L),
  IT = list(code = "+39", name = "Italy", pattern = "^[0-9]{6,12}$", length = c(6L, 12L)),
  ES = list(code = "+34", name = "Spain", pattern = "^[6-9][0-9]{8}$", length = 9L),
  NL = list(code = "+31", name = "Netherlands", pattern = "^[1-9][0-9]{8}$", length = 9L),
  BE = list(code = "+32", name = "Belgium", pattern = "^[1-9][0-9]{7,8}$", length = c(8L, 9L)),
  CH = list(code = "+41", name = "Switzerland", pattern = "^[1-9][0-9]{8}$", length = 9L),
  AT = list(code = "+43", name = "Austria", pattern = "^[1-9][0-9]{3,12}$", length = c(4L, 13L)),
  SE = list(code = "+46", name = "Sweden", pattern = "^[1-9][0-9]{6,12}$", length = c(7L, 13L)),
  NO = list(code = "+47", name = "Norway", pattern = "^[2-9][0-9]{7}$", length = 8L),
  DK = list(code = "+45", name = "Denmark", pattern = "^[2-9][0-9]{7}$", length = 8L),
  FI = list(code = "+358", name = "Finland", pattern = "^[1-9][0-9]{4,11}$", length = c(5L, 12L)),
  PL = list(code = "+48", name = "Poland", pattern = "^[1-9][0-9]{8}$", length = 9L),
  PT = list(code = "+351", name = "Portugal", pattern = "^[2-9][0-9]{8}$", length = 9L),
  IE = list(code = "+353", name = "Ireland", pattern = "^[1-9][0-9]{6,9}$", length = c(7L, 10L)),
  # Asia Pacific
  CN = list(code = "+86", name = "China", pattern = "^1[0-9]{10}$", length = 11L),
  JP = list(code = "+81", name = "Japan", pattern = "^[1-9][0-9]{8,9}$", length = c(9L, 10L)),
  KR = list(code = "+82", name = "South Korea", pattern = "^[1-9][0-9]{7,10}$", length = c(8L, 11L)),
  IN = list(code = "+91", name = "India", pattern = "^[6-9][0-9]{9}$", length = 10L),
  AU = list(code = "+61", name = "Australia", pattern = "^[2-9][0-9]{8}$", length = 9L),
  NZ = list(code = "+64", name = "New Zealand", pattern = "^[2-9][0-9]{7,9}$", length = c(8L, 10L)),
  SG = list(code = "+65", name = "Singapore", pattern = "^[3689][0-9]{7}$", length = 8L),
  HK = list(code = "+852", name = "Hong Kong", pattern = "^[2-9][0-9]{7}$", length = 8L),
  TW = list(code = "+886", name = "Taiwan", pattern = "^[2-9][0-9]{7,8}$", length = c(8L, 9L)),
  PH = list(code = "+63", name = "Philippines", pattern = "^[2-9][0-9]{7,10}$", length = c(8L, 11L)),
  MY = list(code = "+60", name = "Malaysia", pattern = "^[1-9][0-9]{7,9}$", length = c(8L, 10L)),
  TH = list(code = "+66", name = "Thailand", pattern = "^[2-9][0-9]{7,8}$", length = c(8L, 9L)),
  ID = list(code = "+62", name = "Indonesia", pattern = "^[1-9][0-9]{7,12}$", length = c(8L, 13L)),
  VN = list(code = "+84", name = "Vietnam", pattern = "^[1-9][0-9]{8,9}$", length = c(9L, 10L)),
  # Middle East
  AE = list(code = "+971", name = "United Arab Emirates", pattern = "^[1-9][0-9]{8}$", length = 9L),
  SA = list(code = "+966", name = "Saudi Arabia", pattern = "^[1-9][0-9]{8}$", length = 9L),
  IL = list(code = "+972", name = "Israel", pattern = "^[2-9][0-9]{7,8}$", length = c(8L, 9L)),
  # Americas
  MX = list(code = "+52", name = "Mexico", pattern = "^[1-9][0-9]{9}$", length = 10L),
  BR = list(code = "+55", name = "Brazil", pattern = "^[1-9][0-9]{9,10}$", length = c(10L, 11L)),
  AR = list(code = "+54", name = "Argentina", pattern = "^[1-9][0-9]{9,10}$", length = c(10L, 11L)),
  CO = list(code = "+57", name = "Colombia", pattern = "^[1-9][0-9]{9}$", length = 10L),
  CL = list(code = "+56", name = "Chile", pattern = "^[2-9][0-9]{8}$", length = 9L),
  # Africa
  ZA = list(code = "+27", name = "South Africa", pattern = "^[1-9][0-9]{8}$", length = 9L),
  NG = list(code = "+234", name = "Nigeria", pattern = "^[7-9][0-9]{9}$", length = 10L),
  EG = list(code = "+20", name = "Egypt", pattern = "^[1-2][0-9]{8,9}$", length = c(9L, 10L)),
  KE = list(code = "+254", name = "Kenya", pattern = "^[1-9][0-9]{8}$", length = 9L)
)

#' Get country info by ISO country code.
#'
#' @param iso_code Two-letter ISO country code
#' @return Country info list or NULL
#' @export
get_country_info <- function(iso_code) {
  if (is.na(iso_code) || is.null(iso_code)) {
    return(NULL)
  }

  code <- toupper(iso_code)
  if (code %in% names(country_codes)) {
    country_codes[[code]]
  } else {
    NULL
  }
}

#' Get calling code for a country.
#'
#' @param iso_code Two-letter ISO country code
#' @return Calling code string or NA
#' @export
get_calling_code <- function(iso_code) {
  info <- get_country_info(iso_code)
  if (is.null(info)) {
    NA_character_
  } else {
    info$code
  }
}

#' Create a PhoneNumber object.
#'
#' @param national_number The national phone number (digits only)
#' @param country_code Two-letter ISO country code
#' @return PhoneNumber S3 object or NULL on invalid input
#' @keywords internal
new_phone_number <- function(national_number, country_code) {
  country_info <- get_country_info(country_code)
  if (is.null(country_info)) {
    return(NULL)
  }

  structure(
    list(
      national_number = national_number,
      country_code = toupper(country_code),
      calling_code = country_info$code
    ),
    class = "PhoneNumber"
  )
}

#' Check if object is a PhoneNumber.
#'
#' @param x Object to check
#' @return TRUE if x is a PhoneNumber object
#' @export
is_phone_number <- function(x) {
  inherits(x, "PhoneNumber")
}

#' Format PhoneNumber object.
#'
#' @param x PhoneNumber object
#' @param ... Additional arguments (ignored)
#' @return Formatted string
#' @export
format.PhoneNumber <- function(x, ...) {
  format_phone_e164(x)
}

#' Print PhoneNumber object.
#'
#' @param x PhoneNumber object
#' @param ... Additional arguments (ignored)
#' @export
print.PhoneNumber <- function(x, ...) {
  cat("PhoneNumber:", format_phone_e164(x), "\n")
  cat("  Country:", x$country_code, "\n")
  invisible(x)
}

#' Extract digits from a phone number string.
#'
#' @param phone_string Phone number string
#' @return String of digits only
#' @keywords internal
extract_digits <- function(phone_string) {
  gsub("[^0-9]", "", phone_string)
}

#' Parse a phone number string.
#'
#' @param phone_string Phone number string
#' @param default_country Default country code if not specified
#' @return PhoneNumber object or NULL on invalid input
#' @export
parse_phone <- function(phone_string, default_country = "US") {
  if (is.na(phone_string) || is.null(phone_string) || nchar(phone_string) == 0) {
    return(NULL)
  }

  # Check if it starts with + (international format)
  is_international <- startsWith(trimws(phone_string), "+")

  # Extract just the digits
  digits <- extract_digits(phone_string)

  if (nchar(digits) == 0) {
    return(NULL)
  }

  if (is_international) {
    # Try to match against known country codes
    for (country_name in names(country_codes)) {
      info <- country_codes[[country_name]]
      code_digits <- extract_digits(info$code)

      if (startsWith(digits, code_digits)) {
        national <- substr(digits, nchar(code_digits) + 1, nchar(digits))

        # Validate length
        expected_length <- info$length
        if (is.numeric(expected_length) && length(expected_length) == 1) {
          if (nchar(national) == expected_length) {
            return(new_phone_number(national, country_name))
          }
        } else if (length(expected_length) == 2) {
          if (nchar(national) >= expected_length[1] && nchar(national) <= expected_length[2]) {
            return(new_phone_number(national, country_name))
          }
        }
      }
    }

    return(NULL)
  } else {
    # Use default country
    country_info <- get_country_info(default_country)
    if (is.null(country_info)) {
      return(NULL)
    }

    # Remove leading 0 if present (common in many countries)
    if (startsWith(digits, "0")) {
      digits <- substr(digits, 2, nchar(digits))
    }

    # Validate length
    expected_length <- country_info$length
    if (is.numeric(expected_length) && length(expected_length) == 1) {
      if (nchar(digits) != expected_length) {
        return(NULL)
      }
    } else if (length(expected_length) == 2) {
      if (nchar(digits) < expected_length[1] || nchar(digits) > expected_length[2]) {
        return(NULL)
      }
    }

    new_phone_number(digits, default_country)
  }
}

#' Format phone number in E.164 format.
#'
#' @param phone PhoneNumber object or string
#' @param default_country Default country for string input
#' @return E.164 formatted string or NA
#' @export
format_phone_e164 <- function(phone, default_country = "US") {
  if (is.character(phone)) {
    phone <- parse_phone(phone, default_country)
  }

  if (is.null(phone) || !is_phone_number(phone)) {
    return(NA_character_)
  }

  paste0(phone$calling_code, phone$national_number)
}

#' Format phone number for display (national format).
#'
#' @param phone PhoneNumber object or string
#' @param default_country Default country for string input
#' @return Formatted string or NA
#' @export
format_phone_national <- function(phone, default_country = "US") {
  if (is.character(phone)) {
    phone <- parse_phone(phone, default_country)
  }

  if (is.null(phone) || !is_phone_number(phone)) {
    return(NA_character_)
  }

  digits <- phone$national_number

  # US/CA formatting
  if (phone$country_code %in% c("US", "CA") && nchar(digits) == 10) {
    return(paste0(
      "(", substr(digits, 1, 3), ") ",
      substr(digits, 4, 6), "-",
      substr(digits, 7, 10)
    ))
  }

  # UK formatting
  if (phone$country_code == "GB") {
    if (nchar(digits) == 10) {
      return(paste0(
        substr(digits, 1, 4), " ",
        substr(digits, 5, 7), " ",
        substr(digits, 8, 10)
      ))
    } else if (nchar(digits) == 11) {
      return(paste0(
        substr(digits, 1, 5), " ",
        substr(digits, 6, 8), " ",
        substr(digits, 9, 11)
      ))
    }
  }

  # Default: just return the national number
  digits
}

#' Format phone number for display (international format).
#'
#' @param phone PhoneNumber object or string
#' @param default_country Default country for string input
#' @return Formatted string or NA
#' @export
format_phone_international <- function(phone, default_country = "US") {
  if (is.character(phone)) {
    phone <- parse_phone(phone, default_country)
  }

  if (is.null(phone) || !is_phone_number(phone)) {
    return(NA_character_)
  }

  paste0(phone$calling_code, " ", format_phone_national(phone))
}

#' Validate a phone number string.
#'
#' @param phone_string Phone number string
#' @param country Expected country code (optional)
#' @return TRUE if valid
#' @export
is_valid_phone <- function(phone_string, country = NULL) {
  if (is.null(country)) {
    # Try to parse as international
    phone <- parse_phone(phone_string, "US")
    return(!is.null(phone))
  }

  phone <- parse_phone(phone_string, country)

  if (is.null(phone)) {
    return(FALSE)
  }

  # Verify country matches
  toupper(country) == phone$country_code
}

#' Get the country of a phone number.
#'
#' @param phone PhoneNumber object or string
#' @return Country code or NA
#' @export
phone_country <- function(phone) {
  if (is.character(phone)) {
    phone <- parse_phone(phone, "US")
  }

  if (is.null(phone) || !is_phone_number(phone)) {
    return(NA_character_)
  }

  phone$country_code
}

#' Get the national number of a phone.
#'
#' @param phone PhoneNumber object or string
#' @return National number string or NA
#' @export
phone_national_number <- function(phone) {
  if (is.character(phone)) {
    phone <- parse_phone(phone, "US")
  }

  if (is.null(phone) || !is_phone_number(phone)) {
    return(NA_character_)
  }

  phone$national_number
}

#' Compare two phone numbers for equality.
#'
#' @param phone1 First phone (object or string)
#' @param phone2 Second phone (object or string)
#' @return TRUE if equal
#' @export
phones_equal <- function(phone1, phone2) {
  if (is.character(phone1)) {
    phone1 <- parse_phone(phone1, "US")
  }
  if (is.character(phone2)) {
    phone2 <- parse_phone(phone2, "US")
  }

  if (is.null(phone1) || is.null(phone2)) {
    return(FALSE)
  }

  format_phone_e164(phone1) == format_phone_e164(phone2)
}

#' Equality operator for PhoneNumber objects.
#'
#' @param e1 First PhoneNumber
#' @param e2 Second PhoneNumber
#' @return TRUE if equal
#' @export
`==.PhoneNumber` <- function(e1, e2) {
  phones_equal(e1, e2)
}

#' Mask a phone number for display.
#'
#' @param phone PhoneNumber object or string
#' @param visible_digits Number of digits to show at end (default 4)
#' @return Masked phone string or NA
#' @export
mask_phone <- function(phone, visible_digits = 4L) {
  if (is.character(phone)) {
    phone <- parse_phone(phone, "US")
  }

  if (is.null(phone) || !is_phone_number(phone)) {
    return(NA_character_)
  }

  national <- phone$national_number
  len <- nchar(national)

  if (len <= visible_digits) {
    return(paste0(phone$calling_code, " ", national))
  }

  masked_part <- paste0(rep("*", len - visible_digits), collapse = "")
  visible_part <- substr(national, len - visible_digits + 1, len)

  paste0(phone$calling_code, " ", masked_part, visible_part)
}

#' Check if phone number is a mobile number.
#'
#' This is a heuristic based on common mobile prefixes.
#'
#' @param phone PhoneNumber object or string
#' @return TRUE if likely mobile, NA if unknown
#' @export
is_mobile_phone <- function(phone) {
  if (is.character(phone)) {
    phone <- parse_phone(phone, "US")
  }

  if (is.null(phone) || !is_phone_number(phone)) {
    return(NA)
  }

  national <- phone$national_number

  # Country-specific mobile detection
  switch(phone$country_code,
    # US/CA: All area codes can be mobile
    "US" = TRUE,
    "CA" = TRUE,
    # UK: Mobile starts with 7
    "GB" = startsWith(national, "7"),
    # Germany: Mobile starts with 15, 16, 17
    "DE" = grepl("^1[567]", national),
    # France: Mobile starts with 6 or 7
    "FR" = grepl("^[67]", national),
    # China: Mobile starts with 1
    "CN" = startsWith(national, "1"),
    # India: Mobile starts with 6-9
    "IN" = grepl("^[6-9]", national),
    # Australia: Mobile starts with 4
    "AU" = startsWith(national, "4"),
    # Unknown
    NA
  )
}
