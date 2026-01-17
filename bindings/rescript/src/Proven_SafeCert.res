// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

/**
 * SafeCert - X.509 certificate validation and parsing that cannot crash.
 *
 * Provides certificate validation functions that always return explicit
 * error types. Handles common certificate parsing tasks including validity
 * period checks, issuer/subject extraction, and basic constraint validation.
 */

/** Certificate error types */
type certError =
  | InvalidFormat
  | InvalidPem
  | InvalidDer
  | ExpiredCertificate
  | NotYetValid
  | InvalidSignature
  | InvalidIssuer
  | InvalidSubject
  | MissingRequiredField
  | UnsupportedVersion
  | ChainValidationFailed
  | SelfSignedNotAllowed

/** Certificate validity period */
type validityPeriod = {
  notBefore: float,
  notAfter: float,
}

/** Distinguished Name (DN) components */
type distinguishedName = {
  commonName: option<string>,
  organization: option<string>,
  organizationalUnit: option<string>,
  country: option<string>,
  state: option<string>,
  locality: option<string>,
}

/** Basic constraints extension */
type basicConstraints = {
  isCa: bool,
  pathLenConstraint: option<int>,
}

/** Parsed certificate information (subset of X.509 fields) */
type certificateInfo = {
  version: int,
  serialNumber: string,
  issuer: distinguishedName,
  subject: distinguishedName,
  validity: validityPeriod,
  basicConstraints: option<basicConstraints>,
  isSelfSigned: bool,
}

/** Check if validity period is currently valid */
let isValidAtTime = (validity: validityPeriod, currentTime: float): bool => {
  currentTime >= validity.notBefore && currentTime <= validity.notAfter
}

/** Check if validity period has expired */
let isExpiredAtTime = (validity: validityPeriod, currentTime: float): bool => {
  currentTime > validity.notAfter
}

/** Check if validity period is not yet valid */
let isNotYetValidAtTime = (validity: validityPeriod, currentTime: float): bool => {
  currentTime < validity.notBefore
}

/** Get remaining validity in seconds (negative if expired) */
let getRemainingSeconds = (validity: validityPeriod, currentTime: float): float => {
  validity.notAfter -. currentTime
}

/** Check if data appears to be PEM encoded */
let isPemEncoded = (data: string): bool => {
  Js.String2.startsWith(data, "-----BEGIN ")
}

/** Check if data appears to be DER encoded (starts with SEQUENCE tag 0x30) */
let isDerEncoded = (data: string): bool => {
  Js.String2.length(data) >= 2 && Js.String2.charCodeAt(data, 0) == 48.0
}

/** Validate PEM format structure (does not validate certificate contents) */
let validatePemFormat = (pem: string): result<unit, certError> => {
  let beginMarker = "-----BEGIN CERTIFICATE-----"
  let endMarker = "-----END CERTIFICATE-----"

  let beginPos = Js.String2.indexOf(pem, beginMarker)
  if beginPos == -1 {
    Error(InvalidPem)
  } else {
    let endPos = Js.String2.indexOf(pem, endMarker)
    if endPos == -1 {
      Error(InvalidPem)
    } else if beginPos >= endPos {
      Error(InvalidPem)
    } else {
      let contentStart = beginPos + Js.String2.length(beginMarker)
      if contentStart >= endPos {
        Error(InvalidPem)
      } else {
        Ok()
      }
    }
  }
}

/** Extract the base64 content from a PEM-encoded certificate */
let extractPemContent = (pem: string): result<string, certError> => {
  let beginMarker = "-----BEGIN CERTIFICATE-----"
  let endMarker = "-----END CERTIFICATE-----"

  let beginPos = Js.String2.indexOf(pem, beginMarker)
  if beginPos == -1 {
    Error(InvalidPem)
  } else {
    let endPos = Js.String2.indexOf(pem, endMarker)
    if endPos == -1 {
      Error(InvalidPem)
    } else {
      let contentStart = beginPos + Js.String2.length(beginMarker)
      if contentStart >= endPos {
        Error(InvalidPem)
      } else {
        let content = Js.String2.slice(pem, ~from=contentStart, ~to_=endPos)
        // Strip whitespace and newlines
        let cleaned =
          content
          ->Js.String2.replaceByRe(%re("/\\n/g"), "")
          ->Js.String2.replaceByRe(%re("/\\r/g"), "")
          ->Js.String2.replaceByRe(%re("/ /g"), "")
          ->Js.String2.replaceByRe(%re("/\\t/g"), "")
        Ok(cleaned)
      }
    }
  }
}

/** Validate certificate validity period against current time */
let validateValidityPeriod = (validity: validityPeriod, currentTime: float): result<
  unit,
  certError,
> => {
  if isNotYetValidAtTime(validity, currentTime) {
    Error(NotYetValid)
  } else if isExpiredAtTime(validity, currentTime) {
    Error(ExpiredCertificate)
  } else {
    Ok()
  }
}

/** Check if a certificate chain depth is acceptable */
let validateChainDepth = (depth: int, maxDepth: int): result<unit, certError> => {
  if depth > maxDepth {
    Error(ChainValidationFailed)
  } else {
    Ok()
  }
}

/** Validate basic constraints for CA certificates */
let validateBasicConstraints = (
  constraints: option<basicConstraints>,
  isCaExpected: bool,
): result<unit, certError> => {
  if isCaExpected {
    switch constraints {
    | None => Error(MissingRequiredField)
    | Some(bc) =>
      if !bc.isCa {
        Error(ChainValidationFailed)
      } else {
        Ok()
      }
    }
  } else {
    Ok()
  }
}

/** Compare two optional strings for equality */
let optionalStrEquals = (a: option<string>, b: option<string>): bool => {
  switch (a, b) {
  | (None, None) => true
  | (Some(aVal), Some(bVal)) => aVal == bVal
  | _ => false
  }
}

/** Safely compare two distinguished names for equality */
let dnEquals = (a: distinguishedName, b: distinguishedName): bool => {
  optionalStrEquals(a.commonName, b.commonName) &&
  optionalStrEquals(a.organization, b.organization) &&
  optionalStrEquals(a.organizationalUnit, b.organizationalUnit) &&
  optionalStrEquals(a.country, b.country) &&
  optionalStrEquals(a.state, b.state) &&
  optionalStrEquals(a.locality, b.locality)
}

/** Check if a certificate appears to be self-signed (issuer == subject) */
let isSelfSigned = (issuer: distinguishedName, subject: distinguishedName): bool => {
  dnEquals(issuer, subject)
}

/** Validate that a certificate is not self-signed (for chain validation) */
let validateNotSelfSigned = (
  issuer: distinguishedName,
  subject: distinguishedName,
): result<unit, certError> => {
  if isSelfSigned(issuer, subject) {
    Error(SelfSignedNotAllowed)
  } else {
    Ok()
  }
}

/** Count the number of certificates in a PEM bundle */
let countCertificatesInBundle = (pemBundle: string): int => {
  let marker = "-----BEGIN CERTIFICATE-----"
  let rec countMarkers = (pos: int, count: int): int => {
    let idx = Js.String2.indexOfFrom(pemBundle, marker, pos)
    if idx == -1 {
      count
    } else {
      countMarkers(idx + Js.String2.length(marker), count + 1)
    }
  }
  countMarkers(0, 0)
}

/** Validate certificate version (X.509 v1, v2, or v3) */
let validateVersion = (version: int): result<unit, certError> => {
  if version < 1 || version > 3 {
    Error(UnsupportedVersion)
  } else {
    Ok()
  }
}

/** Create an empty distinguished name */
let emptyDn: distinguishedName = {
  commonName: None,
  organization: None,
  organizationalUnit: None,
  country: None,
  state: None,
  locality: None,
}

/** Create a distinguished name with common name only */
let dnFromCommonName = (cn: string): distinguishedName => {
  ...emptyDn,
  commonName: Some(cn),
}

/** Format a distinguished name as a string */
let formatDn = (dn: distinguishedName): string => {
  let parts = []

  let parts = switch dn.commonName {
  | Some(cn) => Js.Array2.concat(parts, ["CN=" ++ cn])
  | None => parts
  }

  let parts = switch dn.organization {
  | Some(o) => Js.Array2.concat(parts, ["O=" ++ o])
  | None => parts
  }

  let parts = switch dn.organizationalUnit {
  | Some(ou) => Js.Array2.concat(parts, ["OU=" ++ ou])
  | None => parts
  }

  let parts = switch dn.country {
  | Some(c) => Js.Array2.concat(parts, ["C=" ++ c])
  | None => parts
  }

  let parts = switch dn.state {
  | Some(st) => Js.Array2.concat(parts, ["ST=" ++ st])
  | None => parts
  }

  let parts = switch dn.locality {
  | Some(l) => Js.Array2.concat(parts, ["L=" ++ l])
  | None => parts
  }

  Js.Array2.joinWith(parts, ", ")
}

/** Check if certificate info has valid basic constraints for CA */
let isValidCa = (info: certificateInfo): bool => {
  switch info.basicConstraints {
  | Some(bc) => bc.isCa
  | None => false
  }
}

/** Get the path length constraint if present */
let getPathLenConstraint = (info: certificateInfo): option<int> => {
  switch info.basicConstraints {
  | Some(bc) => bc.pathLenConstraint
  | None => None
  }
}

/** Validate a complete certificate chain structure (without cryptographic verification) */
let validateChainStructure = (
  chain: array<certificateInfo>,
  currentTime: float,
): result<unit, certError> => {
  if Js.Array2.length(chain) == 0 {
    Error(InvalidFormat)
  } else {
    // Validate each certificate's validity period
    let validityResults = Js.Array2.map(chain, cert =>
      validateValidityPeriod(cert.validity, currentTime)
    )

    let hasValidityError = Js.Array2.some(validityResults, r =>
      switch r {
      | Error(_) => true
      | Ok(_) => false
      }
    )

    if hasValidityError {
      // Return first error
      Js.Array2.find(validityResults, r =>
        switch r {
        | Error(_) => true
        | Ok(_) => false
        }
      )->Belt.Option.getWithDefault(Ok())
    } else {
      // Check chain depth
      validateChainDepth(Js.Array2.length(chain), 10)
    }
  }
}

/** Common certificate expiry warning threshold (30 days in seconds) */
let expiryWarningThreshold = 30.0 *. 24.0 *. 60.0 *. 60.0

/** Check if certificate is expiring soon */
let isExpiringSoon = (validity: validityPeriod, currentTime: float): bool => {
  let remaining = getRemainingSeconds(validity, currentTime)
  remaining > 0.0 && remaining < expiryWarningThreshold
}

/** Get certificate status as a human-readable string */
let getValidityStatus = (validity: validityPeriod, currentTime: float): string => {
  if isNotYetValidAtTime(validity, currentTime) {
    "not_yet_valid"
  } else if isExpiredAtTime(validity, currentTime) {
    "expired"
  } else if isExpiringSoon(validity, currentTime) {
    "expiring_soon"
  } else {
    "valid"
  }
}
