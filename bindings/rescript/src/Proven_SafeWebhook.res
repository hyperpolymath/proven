// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

/**
 * SafeWebhook - Webhook URL and signature validation that cannot crash.
 *
 * Provides secure validation of webhook URLs and cryptographic signature
 * verification for common webhook providers (GitHub, Stripe, Slack, etc.).
 * All operations use constant-time comparison to prevent timing attacks.
 */

/** Supported webhook signature algorithms */
type signatureAlgorithm =
  | HmacSha256
  | HmacSha1
  | HmacSha512

/** Webhook provider presets with their specific signature formats */
type webhookProvider =
  | GitHub
  | Stripe
  | Slack
  | Twilio
  | Shopify
  | Custom

/** Validated webhook URL */
type webhookUrl = {
  scheme: string,
  host: string,
  port: option<int>,
  path: string,
  query: option<string>,
}

/** Webhook validation error types */
type webhookError =
  | InvalidUrl
  | InsecureScheme
  | InvalidSignature
  | SignatureMismatch
  | MissingParameter
  | TimestampOutOfRange
  | InvalidPayload
  | UnsupportedAlgorithm

/** Parsed Stripe signature components */
type stripeSignatureComponents = {
  timestamp: string,
  signature: string,
}

/** Slack signature result */
type slackSignatureResult = {
  slackSignature: string,
  slackTimestamp: string,
}

/** Get the signature header name for a provider */
let getSignatureHeader = (provider: webhookProvider): string => {
  switch provider {
  | GitHub => "X-Hub-Signature-256"
  | Stripe => "Stripe-Signature"
  | Slack => "X-Slack-Signature"
  | Twilio => "X-Twilio-Signature"
  | Shopify => "X-Shopify-Hmac-SHA256"
  | Custom => "X-Signature"
  }
}

/** Get the default algorithm for a provider */
let getDefaultAlgorithm = (provider: webhookProvider): signatureAlgorithm => {
  switch provider {
  | GitHub => HmacSha256
  | Stripe => HmacSha256
  | Slack => HmacSha256
  | Twilio => HmacSha1
  | Shopify => HmacSha256
  | Custom => HmacSha256
  }
}

/** Check if the URL uses a secure scheme */
let isSecure = (url: webhookUrl): bool => {
  url.scheme == "https"
}

/** Get the effective port (explicit or default for scheme) */
let effectivePort = (url: webhookUrl): int => {
  switch url.port {
  | Some(p) => p
  | None =>
    if url.scheme == "https" {
      443
    } else if url.scheme == "http" {
      80
    } else {
      443
    }
  }
}

/** Format the URL as a string */
let urlToString = (url: webhookUrl): string => {
  let portStr = switch url.port {
  | Some(p) => `:${Belt.Int.toString(p)}`
  | None => ""
  }
  let queryStr = switch url.query {
  | Some(q) => `?${q}`
  | None => ""
  }
  `${url.scheme}://${url.host}${portStr}${url.path}${queryStr}`
}

/** Parse and validate a webhook URL (HTTPS only by default) */
let parseUrl = (
  input: string,
  ~allowHttp: bool=false,
  ~allowedHosts: option<array<string>>=?,
  (),
): result<webhookUrl, webhookError> => {
  if Js.String2.length(input) == 0 {
    Error(InvalidUrl)
  } else {
    // Find scheme
    switch Js.String2.indexOf(input, "://") {
    | -1 => Error(InvalidUrl)
    | schemeEnd =>
      let scheme = Js.String2.slice(input, ~from=0, ~to_=schemeEnd)
      let isHttps = scheme == "https"
      let isHttp = scheme == "http"

      if !isHttps && !isHttp {
        Error(InvalidUrl)
      } else if !isHttps && !allowHttp {
        Error(InsecureScheme)
      } else {
        let remaining = Js.String2.sliceToEnd(input, ~from=schemeEnd + 3)

        // Find path start
        let pathStart = switch Js.String2.indexOf(remaining, "/") {
        | -1 => Js.String2.length(remaining)
        | idx => idx
        }

        let authority = Js.String2.slice(remaining, ~from=0, ~to_=pathStart)
        let pathAndQuery =
          pathStart < Js.String2.length(remaining)
            ? Js.String2.sliceToEnd(remaining, ~from=pathStart)
            : "/"

        // Parse host and port
        let (host, port) = switch Js.String2.lastIndexOf(authority, ":") {
        | -1 => (authority, None)
        | colonPos =>
          let hostPart = Js.String2.slice(authority, ~from=0, ~to_=colonPos)
          let portStr = Js.String2.sliceToEnd(authority, ~from=colonPos + 1)
          switch Belt.Int.fromString(portStr) {
          | Some(p) => (hostPart, Some(p))
          | None => (authority, None)
          }
        }

        if Js.String2.length(host) == 0 {
          Error(InvalidUrl)
        } else {
          // Check allowed hosts
          let hostAllowed = switch allowedHosts {
          | None => true
          | Some(hosts) => Js.Array2.includes(hosts, host)
          }

          if !hostAllowed {
            Error(InvalidUrl)
          } else {
            // Parse path and query
            let (path, query) = switch Js.String2.indexOf(pathAndQuery, "?") {
            | -1 => (pathAndQuery, None)
            | queryPos => (
                Js.String2.slice(pathAndQuery, ~from=0, ~to_=queryPos),
                Some(Js.String2.sliceToEnd(pathAndQuery, ~from=queryPos + 1)),
              )
            }

            Ok({scheme, host, port, path, query})
          }
        }
      }
    }
  }
}

/** Check if a URL is a valid webhook endpoint */
let isValidWebhookUrl = (input: string): bool => {
  switch parseUrl(input, ()) {
  | Ok(_) => true
  | Error(_) => false
  }
}

/** External binding to crypto for HMAC operations */
@module("crypto")
external createHmac: (string, string) => 'hmacInstance = "createHmac"

@send
external hmacUpdate: ('hmacInstance, string) => 'hmacInstance = "update"

@send
external hmacDigest: ('hmacInstance, string) => string = "digest"

/** Compute HMAC-SHA256 signature as hex string */
let computeHmacSha256 = (payload: string, secret: string): string => {
  createHmac("sha256", secret)->hmacUpdate(payload)->hmacDigest("hex")
}

/** Compute HMAC-SHA1 signature as hex string */
let computeHmacSha1 = (payload: string, secret: string): string => {
  createHmac("sha1", secret)->hmacUpdate(payload)->hmacDigest("hex")
}

/** Compute HMAC-SHA512 signature as hex string */
let computeHmacSha512 = (payload: string, secret: string): string => {
  createHmac("sha512", secret)->hmacUpdate(payload)->hmacDigest("hex")
}

/** Constant-time string comparison to prevent timing attacks */
@module("crypto")
external timingSafeEqual: (Js.TypedArray2.Uint8Array.t, Js.TypedArray2.Uint8Array.t) => bool =
  "timingSafeEqual"

@new
external createTextEncoder: unit => 'textEncoder = "TextEncoder"

@send
external encode: ('textEncoder, string) => Js.TypedArray2.Uint8Array.t = "encode"

let constantTimeCompare = (a: string, b: string): bool => {
  if Js.String2.length(a) != Js.String2.length(b) {
    false
  } else {
    let encoder = createTextEncoder()
    try {
      timingSafeEqual(encode(encoder, a), encode(encoder, b))
    } catch {
    | _ => false
    }
  }
}

/** Strip common signature prefixes (sha256=, v0=, etc.) */
let stripSignaturePrefix = (signature: string): string => {
  if Js.String2.startsWith(signature, "sha256=") {
    Js.String2.sliceToEnd(signature, ~from=7)
  } else if Js.String2.startsWith(signature, "sha1=") {
    Js.String2.sliceToEnd(signature, ~from=5)
  } else if Js.String2.startsWith(signature, "v0=") {
    Js.String2.sliceToEnd(signature, ~from=3)
  } else {
    signature
  }
}

/** Verify an HMAC-SHA256 signature */
let verifyHmacSha256 = (
  payload: string,
  secret: string,
  signature: string,
): result<bool, webhookError> => {
  let computed = computeHmacSha256(payload, secret)
  let provided = stripSignaturePrefix(signature)

  if Js.String2.length(provided) != 64 {
    Error(InvalidSignature)
  } else {
    Ok(constantTimeCompare(computed, provided))
  }
}

/** Verify a GitHub webhook signature */
let verifyGitHub = (
  payload: string,
  secret: string,
  signatureHeader: string,
): result<bool, webhookError> => {
  verifyHmacSha256(payload, secret, signatureHeader)
}

/** Parse Stripe signature header */
let parseStripeSignature = (
  header: string,
): result<stripeSignatureComponents, webhookError> => {
  let timestamp = ref(None)
  let signature = ref(None)

  Js.String2.split(header, ",")->Js.Array2.forEach(part => {
    if Js.String2.startsWith(part, "t=") {
      timestamp := Some(Js.String2.sliceToEnd(part, ~from=2))
    } else if Js.String2.startsWith(part, "v1=") {
      signature := Some(Js.String2.sliceToEnd(part, ~from=3))
    }
  })

  switch (timestamp.contents, signature.contents) {
  | (Some(t), Some(s)) => Ok({timestamp: t, signature: s})
  | _ => Error(MissingParameter)
  }
}

/** Verify a Stripe webhook signature with timestamp validation */
let verifyStripe = (
  payload: string,
  secret: string,
  signatureHeader: string,
  toleranceSeconds: int,
): result<bool, webhookError> => {
  switch parseStripeSignature(signatureHeader) {
  | Error(e) => Error(e)
  | Ok({timestamp, signature}) =>
    switch Belt.Int.fromString(timestamp) {
    | None => Error(InvalidPayload)
    | Some(ts) =>
      let currentTime = Belt.Float.toInt(Js.Date.now() /. 1000.0)
      let diff = abs(currentTime - ts)

      if diff > toleranceSeconds {
        Error(TimestampOutOfRange)
      } else {
        let signedPayload = `${timestamp}.${payload}`
        let computed = computeHmacSha256(signedPayload, secret)

        if Js.String2.length(signature) != 64 {
          Error(InvalidSignature)
        } else {
          Ok(constantTimeCompare(computed, signature))
        }
      }
    }
  }
}

/** Verify a Slack webhook signature */
let verifySlack = (
  payload: string,
  secret: string,
  signatureHeader: string,
  timestampHeader: string,
  toleranceSeconds: int,
): result<bool, webhookError> => {
  switch Belt.Int.fromString(timestampHeader) {
  | None => Error(InvalidPayload)
  | Some(ts) =>
    let currentTime = Belt.Float.toInt(Js.Date.now() /. 1000.0)
    let diff = abs(currentTime - ts)

    if diff > toleranceSeconds {
      Error(TimestampOutOfRange)
    } else {
      let signedPayload = `v0:${timestampHeader}:${payload}`
      verifyHmacSha256(signedPayload, secret, signatureHeader)
    }
  }
}

/** Generate a webhook signature for testing or sending webhooks */
let generateSignature = (
  payload: string,
  secret: string,
  algorithm: signatureAlgorithm,
): string => {
  switch algorithm {
  | HmacSha256 => computeHmacSha256(payload, secret)
  | HmacSha1 => computeHmacSha1(payload, secret)
  | HmacSha512 => computeHmacSha512(payload, secret)
  }
}

/** Generate a GitHub-style signature header */
let generateGitHubSignature = (payload: string, secret: string): string => {
  `sha256=${computeHmacSha256(payload, secret)}`
}

/** Generate a Stripe-style signature header */
let generateStripeSignature = (payload: string, secret: string): string => {
  let timestamp = Belt.Float.toInt(Js.Date.now() /. 1000.0)
  let signedPayload = `${Belt.Int.toString(timestamp)}.${payload}`
  let signature = computeHmacSha256(signedPayload, secret)
  `t=${Belt.Int.toString(timestamp)},v1=${signature}`
}

/** Generate a Slack-style signature */
let generateSlackSignature = (
  payload: string,
  secret: string,
): slackSignatureResult => {
  let timestamp = Belt.Int.toString(Belt.Float.toInt(Js.Date.now() /. 1000.0))
  let signedPayload = `v0:${timestamp}:${payload}`
  let signature = `v0=${computeHmacSha256(signedPayload, secret)}`
  {slackSignature: signature, slackTimestamp: timestamp}
}

/** Default tolerance for timestamp validation (5 minutes) */
let defaultToleranceSeconds = 300
