// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

//// SafeEmail - Email validation that cannot crash.

import gleam/option.{type Option, None, Some}
import gleam/string

/// Email parts after splitting.
pub type EmailParts {
  EmailParts(local: String, domain: String)
}

/// Check if an email address is valid (basic check).
pub fn is_valid(email: String) -> Bool {
  case string.split(email, "@") {
    [local, domain] ->
      string.length(local) > 0
      && string.length(domain) > 2
      && string.contains(domain, ".")
      && !string.starts_with(domain, ".")
      && !string.ends_with(domain, ".")
    _ -> False
  }
}

/// Split an email into local part and domain.
pub fn split(email: String) -> Option(EmailParts) {
  case is_valid(email) {
    False -> None
    True ->
      case string.split(email, "@") {
        [local, domain] -> Some(EmailParts(local: local, domain: domain))
        _ -> None
      }
  }
}

/// Extract the domain from an email address.
pub fn get_domain(email: String) -> Option(String) {
  case split(email) {
    Some(parts) -> Some(parts.domain)
    None -> None
  }
}

/// Extract the local part from an email address.
pub fn get_local_part(email: String) -> Option(String) {
  case split(email) {
    Some(parts) -> Some(parts.local)
    None -> None
  }
}

/// Normalize an email address (lowercase domain).
pub fn normalize(email: String) -> Option(String) {
  case split(email) {
    Some(parts) ->
      Some(parts.local <> "@" <> string.lowercase(parts.domain))
    None -> None
  }
}
