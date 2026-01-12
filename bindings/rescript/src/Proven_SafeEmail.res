// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

/**
 * SafeEmail - Email validation that cannot crash.
 */

/** Basic email validation regex */
let emailRegex = %re("/^[^\s@]+@[^\s@]+\.[^\s@]+$/")

/** Check if an email address is valid */
let isValid = (email: string): bool => {
  Js.Re.test_(emailRegex, email)
}

/** Split an email into local part and domain */
let split = (email: string): option<(string, string)> => {
  if !isValid(email) {
    None
  } else {
    switch Js.String2.lastIndexOf(email, "@") {
    | -1 => None
    | atPos =>
      let localPart = Js.String2.slice(email, ~from=0, ~to_=atPos)
      let domain = Js.String2.sliceToEnd(email, ~from=atPos + 1)
      Some((localPart, domain))
    }
  }
}

/** Extract the domain from an email address */
let getDomain = (email: string): option<string> => {
  switch split(email) {
  | Some((_, domain)) => Some(domain)
  | None => None
  }
}

/** Extract the local part from an email address */
let getLocalPart = (email: string): option<string> => {
  switch split(email) {
  | Some((localPart, _)) => Some(localPart)
  | None => None
  }
}

/** Normalize an email address (lowercase domain) */
let normalize = (email: string): option<string> => {
  switch split(email) {
  | Some((localPart, domain)) =>
    Some(`${localPart}@${Js.String2.toLowerCase(domain)}`)
  | None => None
  }
}
