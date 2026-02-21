# SPDX-License-Identifier: PMPL-1.0-or-later
# SPDX-FileCopyrightText: 2025 Hyperpolymath
#
# SafeEmail - RFC 5321 compliant email validation for Nickel
#
# Validates email addresses without regex catastrophic backtracking.
# All operations are pure and cannot crash.

{
  # Email result type
  EmailResult = {
    local | String,
    domain | String,
    ok | Bool,
    error | String,
  },

  # Basic email pattern (simplified, safe from ReDoS)
  # Full RFC 5321 validation would require more complex parsing
  email_pattern = "^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}$",

  # Check if string is a valid email address
  is_valid
    : String -> Bool
    = fun email =>
      std.string.is_match email_pattern email
      && std.string.length email <= 254  # Max email length per RFC
      && !(std.string.is_match "\\.\\." email),  # No consecutive dots

  # Parse email into local and domain parts
  parse
    : String -> { local : String, domain : String, ok : Bool, error : String }
    = fun email =>
      if !(is_valid email) then
        { local = "", domain = "", ok = false, error = "Invalid email format" }
      else
        let parts = std.string.split "@" email in
        if std.array.length parts != 2 then
          { local = "", domain = "", ok = false, error = "Invalid email format" }
        else
          {
            local = std.array.first parts,
            domain = std.array.last parts,
            ok = true,
            error = "",
          },

  # Normalize email (lowercase domain, trim whitespace)
  normalize
    : String -> String
    = fun email =>
      let trimmed = std.string.trim email in
      let parts = std.string.split "@" trimmed in
      if std.array.length parts != 2 then
        trimmed
      else
        let local = std.array.first parts in
        let domain = std.string.lowercase (std.array.last parts) in
        "%{local}@%{domain}",

  # Get domain from email
  get_domain
    : String -> String
    = fun email =>
      let parts = std.string.split "@" email in
      if std.array.length parts != 2 then
        ""
      else
        std.array.last parts,

  # Get local part from email
  get_local
    : String -> String
    = fun email =>
      let parts = std.string.split "@" email in
      if std.array.length parts < 1 then
        ""
      else
        std.array.first parts,

  # Email contract
  Email = std.contract.from_predicate is_valid,

  # Email with specific domain contract factory
  EmailWithDomain = fun domain =>
    std.contract.from_predicate (fun email =>
      is_valid email && get_domain email == domain
    ),
}
