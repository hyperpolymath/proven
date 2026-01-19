// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

/**
 * SafeJwt - JWT parsing and validation that cannot crash.
 *
 * Provides validation and parsing of JWT tokens without performing
 * cryptographic signature verification. Focuses on structural
 * validation, decoding, and safe access to token components.
 */

/** JWT algorithm types */
type algorithm =
  | HS256
  | HS384
  | HS512
  | RS256
  | RS384
  | RS512
  | ES256
  | ES384
  | ES512
  | PS256
  | PS384
  | PS512
  | None
  | Unknown(string)

/** JWT error types */
type jwtError =
  | InvalidFormat
  | InvalidBase64
  | InvalidJson
  | InvalidHeader
  | InvalidPayload
  | MissingRequiredClaim
  | TokenExpired
  | TokenNotYetValid

/** JWT header structure */
type header = {
  algorithm: algorithm,
  tokenType: string,
  keyId: option<string>,
  contentType: option<string>,
}

/** Standard JWT claims */
type standardClaims = {
  issuer: option<string>,
  subject: option<string>,
  audience: option<string>,
  expirationTime: option<float>,
  notBefore: option<float>,
  issuedAt: option<float>,
  jwtId: option<string>,
}

/** Parsed JWT token */
type token = {
  rawHeader: string,
  rawPayload: string,
  rawSignature: string,
  headerJson: string,
  payloadJson: string,
  header: header,
  claims: standardClaims,
}

/** Parse algorithm string to variant */
let algorithmFromString = (algorithmString: string): algorithm => {
  switch algorithmString {
  | "HS256" => HS256
  | "HS384" => HS384
  | "HS512" => HS512
  | "RS256" => RS256
  | "RS384" => RS384
  | "RS512" => RS512
  | "ES256" => ES256
  | "ES384" => ES384
  | "ES512" => ES512
  | "PS256" => PS256
  | "PS384" => PS384
  | "PS512" => PS512
  | "none" => None
  | other => Unknown(other)
  }
}

/** Convert algorithm to string */
let algorithmToString = (alg: algorithm): string => {
  switch alg {
  | HS256 => "HS256"
  | HS384 => "HS384"
  | HS512 => "HS512"
  | RS256 => "RS256"
  | RS384 => "RS384"
  | RS512 => "RS512"
  | ES256 => "ES256"
  | ES384 => "ES384"
  | ES512 => "ES512"
  | PS256 => "PS256"
  | PS384 => "PS384"
  | PS512 => "PS512"
  | None => "none"
  | Unknown(s) => s
  }
}

/** Check if algorithm uses symmetric key (HMAC) */
let isSymmetricAlgorithm = (alg: algorithm): bool => {
  switch alg {
  | HS256 | HS384 | HS512 => true
  | _ => false
  }
}

/** Check if algorithm uses asymmetric key (RSA/ECDSA) */
let isAsymmetricAlgorithm = (alg: algorithm): bool => {
  switch alg {
  | RS256 | RS384 | RS512 | ES256 | ES384 | ES512 | PS256 | PS384 | PS512 => true
  | _ => false
  }
}

/** Check if algorithm is none (unsigned) */
let isNoneAlgorithm = (alg: algorithm): bool => {
  alg == None
}

/** Check if character is valid base64url */
let isBase64UrlChar = (charCode: float): bool => {
  // A-Z: 65-90, a-z: 97-122, 0-9: 48-57, -: 45, _: 95
  (charCode >= 65.0 && charCode <= 90.0) ||
  (charCode >= 97.0 && charCode <= 122.0) ||
  (charCode >= 48.0 && charCode <= 57.0) ||
  charCode == 45.0 ||
  charCode == 95.0
}

/** Validate base64url string */
let isValidBase64Url = (input: string): bool => {
  if Js.String2.length(input) == 0 {
    false
  } else {
    let chars = Js.String2.split(input, "")
    Js.Array2.every(chars, char => {
      let code = Js.String2.charCodeAt(char, 0)
      isBase64UrlChar(code)
    })
  }
}

/** Check if a string looks like a valid JWT format (3 base64url parts) */
let isValidFormat = (tokenString: string): bool => {
  let parts = Js.String2.split(tokenString, ".")
  if Js.Array2.length(parts) != 3 {
    false
  } else {
    Js.Array2.every(parts, part => {
      Js.String2.length(part) > 0 && isValidBase64Url(part)
    })
  }
}

/** Base64url decode */
@module("./proven_ffi.js")
external base64UrlDecode: string => Js.Nullable.t<string> = "base64UrlDecode"

/** External atob for base64 decoding */
@val external atob: string => string = "atob"

/** Fallback base64url decode using atob */
let decodeBase64Url = (input: string): option<string> => {
  // Convert base64url to base64
  let base64 =
    input
    ->Js.String2.replaceByRe(%re("/-/g"), "+")
    ->Js.String2.replaceByRe(%re("/_/g"), "/")

  // Add padding if needed
  let padded = switch mod(Js.String2.length(base64), 4) {
  | 2 => base64 ++ "=="
  | 3 => base64 ++ "="
  | _ => base64
  }

  try {
    Some(atob(padded))
  } catch {
  | _ => Js.Nullable.toOption(base64UrlDecode(input))
  }
}

/** Extract string value from simple JSON */
let extractJsonString = (json: string, key: string): option<string> => {
  let pattern = "\"" ++ key ++ "\""
  switch Js.String2.indexOf(json, pattern) {
  | -1 => None
  | keyPos =>
    let afterKey = Js.String2.sliceToEnd(json, ~from=keyPos + Js.String2.length(pattern))
    // Find the colon and opening quote
    let colonPos = Js.String2.indexOf(afterKey, ":")
    if colonPos == -1 {
      None
    } else {
      let afterColon = Js.String2.sliceToEnd(afterKey, ~from=colonPos + 1)
      let trimmed = Js.String2.trim(afterColon)
      if !Js.String2.startsWith(trimmed, "\"") {
        None
      } else {
        let valueStart = Js.String2.sliceToEnd(trimmed, ~from=1)
        // Find closing quote (handle escaped quotes)
        let rec findEndQuote = (s: string, idx: int): option<int> => {
          if idx >= Js.String2.length(s) {
            None
          } else {
            let char = Js.String2.charAt(s, idx)
            if char == "\\" && idx + 1 < Js.String2.length(s) {
              findEndQuote(s, idx + 2)
            } else if char == "\"" {
              Some(idx)
            } else {
              findEndQuote(s, idx + 1)
            }
          }
        }
        switch findEndQuote(valueStart, 0) {
        | None => None
        | Some(endPos) => Some(Js.String2.slice(valueStart, ~from=0, ~to_=endPos))
        }
      }
    }
  }
}

/** Extract number value from simple JSON */
let extractJsonNumber = (json: string, key: string): option<float> => {
  let pattern = "\"" ++ key ++ "\""
  switch Js.String2.indexOf(json, pattern) {
  | -1 => None
  | keyPos =>
    let afterKey = Js.String2.sliceToEnd(json, ~from=keyPos + Js.String2.length(pattern))
    let colonPos = Js.String2.indexOf(afterKey, ":")
    if colonPos == -1 {
      None
    } else {
      let afterColon = Js.String2.sliceToEnd(afterKey, ~from=colonPos + 1)
      let trimmed = Js.String2.trim(afterColon)
      // Extract digits and minus sign
      let rec extractNumber = (s: string, idx: int, acc: string): string => {
        if idx >= Js.String2.length(s) {
          acc
        } else {
          let char = Js.String2.charAt(s, idx)
          let code = Js.String2.charCodeAt(char, 0)
          if (code >= 48.0 && code <= 57.0) || (idx == 0 && char == "-") {
            extractNumber(s, idx + 1, acc ++ char)
          } else {
            acc
          }
        }
      }
      let numStr = extractNumber(trimmed, 0, "")
      Belt.Float.fromString(numStr)
    }
  }
}

/** Parse JWT header */
let parseHeader = (headerJson: string): option<header> => {
  switch extractJsonString(headerJson, "alg") {
  | None => None
  | Some(alg) =>
    let tokenType = Belt.Option.getWithDefault(extractJsonString(headerJson, "typ"), "JWT")
    let keyId = extractJsonString(headerJson, "kid")
    let contentType = extractJsonString(headerJson, "cty")
    Some({
      algorithm: algorithmFromString(alg),
      tokenType,
      keyId,
      contentType,
    })
  }
}

/** Parse standard claims from payload JSON */
let parseClaims = (payloadJson: string): standardClaims => {
  {
    issuer: extractJsonString(payloadJson, "iss"),
    subject: extractJsonString(payloadJson, "sub"),
    audience: extractJsonString(payloadJson, "aud"),
    expirationTime: extractJsonNumber(payloadJson, "exp"),
    notBefore: extractJsonNumber(payloadJson, "nbf"),
    issuedAt: extractJsonNumber(payloadJson, "iat"),
    jwtId: extractJsonString(payloadJson, "jti"),
  }
}

/** Parse a JWT token string into its components */
let parse = (tokenString: string): result<token, jwtError> => {
  if !isValidFormat(tokenString) {
    Error(InvalidFormat)
  } else {
    let parts = Js.String2.split(tokenString, ".")
    let rawHeader = Belt.Array.getUnsafe(parts, 0)
    let rawPayload = Belt.Array.getUnsafe(parts, 1)
    let rawSignature = Belt.Array.getUnsafe(parts, 2)

    switch decodeBase64Url(rawHeader) {
    | None => Error(InvalidHeader)
    | Some(headerJson) =>
      switch decodeBase64Url(rawPayload) {
      | None => Error(InvalidPayload)
      | Some(payloadJson) =>
        switch parseHeader(headerJson) {
        | None => Error(InvalidHeader)
        | Some(header) =>
          Ok({
            rawHeader,
            rawPayload,
            rawSignature,
            headerJson,
            payloadJson,
            header,
            claims: parseClaims(payloadJson),
          })
        }
      }
    }
  }
}

/** Check if token claims have expired based on current timestamp */
let isExpired = (claims: standardClaims, currentTimestamp: float): bool => {
  switch claims.expirationTime {
  | Some(exp) => currentTimestamp > exp
  | None => false
  }
}

/** Check if token is valid yet based on current timestamp */
let isValidYet = (claims: standardClaims, currentTimestamp: float): bool => {
  switch claims.notBefore {
  | Some(nbf) => currentTimestamp >= nbf
  | None => true
  }
}

/** Check if token is currently valid (not expired and valid yet) */
let isCurrentlyValid = (claims: standardClaims, currentTimestamp: float): bool => {
  !isExpired(claims, currentTimestamp) && isValidYet(claims, currentTimestamp)
}

/** Validate token timing with leeway */
let validateTiming = (token: token, currentTimestamp: float, leewaySeconds: float): result<
  unit,
  jwtError,
> => {
  switch token.claims.expirationTime {
  | Some(exp) if currentTimestamp > exp +. leewaySeconds => Error(TokenExpired)
  | _ =>
    switch token.claims.notBefore {
    | Some(nbf) if currentTimestamp < nbf -. leewaySeconds => Error(TokenNotYetValid)
    | _ => Ok()
    }
  }
}

/** Get signing input (header.payload) for verification */
let getSigningInput = (token: token): string => {
  token.rawHeader ++ "." ++ token.rawPayload
}

/** Check if header indicates a standard JWT */
let isStandardJwt = (header: header): bool => {
  header.tokenType == "JWT"
}

/** Get remaining validity time in seconds (negative if expired) */
let getRemainingTime = (claims: standardClaims, currentTimestamp: float): option<float> => {
  switch claims.expirationTime {
  | Some(exp) => Some(exp -. currentTimestamp)
  | None => None
  }
}

/** Check if issuer matches expected value */
let hasIssuer = (claims: standardClaims, expectedIssuer: string): bool => {
  claims.issuer == Some(expectedIssuer)
}

/** Check if audience matches expected value */
let hasAudience = (claims: standardClaims, expectedAudience: string): bool => {
  claims.audience == Some(expectedAudience)
}

/** Check if subject matches expected value */
let hasSubject = (claims: standardClaims, expectedSubject: string): bool => {
  claims.subject == Some(expectedSubject)
}

/** Validate all required claims are present */
let hasRequiredClaims = (
  claims: standardClaims,
  ~issuer: bool=false,
  ~subject: bool=false,
  ~audience: bool=false,
  ~expiration: bool=false,
): bool => {
  let issuerOk = !issuer || Belt.Option.isSome(claims.issuer)
  let subjectOk = !subject || Belt.Option.isSome(claims.subject)
  let audienceOk = !audience || Belt.Option.isSome(claims.audience)
  let expirationOk = !expiration || Belt.Option.isSome(claims.expirationTime)
  issuerOk && subjectOk && audienceOk && expirationOk
}
