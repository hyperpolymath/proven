-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
||| FFI exports for SafeCert operations
|||
||| This module exports X.509 certificate validation to the C ABI via Idris2's RefC backend.
||| All functions are proven total and validate certificate properties.
|||
||| Return conventions:
||| - Bool checks -> Int (0 = false, 1 = true)
||| - Algorithm info -> Int (encoded IDs)
||| - Validation -> (status: Int, issues: String)
module Proven.FFI.SafeCert

import Proven.SafeCert

%default total

--------------------------------------------------------------------------------
-- FFI Result Encoding
--------------------------------------------------------------------------------

||| Encode Bool as Int
encodeBool : Bool -> Int
encodeBool False = 0
encodeBool True = 1

||| Encode CertAlgorithm as Int
encodeAlgorithm : CertAlgorithm -> Int
encodeAlgorithm RSA_2048 = 0
encodeAlgorithm RSA_3072 = 1
encodeAlgorithm RSA_4096 = 2
encodeAlgorithm ECDSA_P256 = 3
encodeAlgorithm ECDSA_P384 = 4
encodeAlgorithm ECDSA_P521 = 5
encodeAlgorithm Ed25519Cert = 6
encodeAlgorithm Ed448Cert = 7

||| Decode Int to CertAlgorithm
decodeAlgorithm : Int -> Maybe CertAlgorithm
decodeAlgorithm 0 = Just RSA_2048
decodeAlgorithm 1 = Just RSA_3072
decodeAlgorithm 2 = Just RSA_4096
decodeAlgorithm 3 = Just ECDSA_P256
decodeAlgorithm 4 = Just ECDSA_P384
decodeAlgorithm 5 = Just ECDSA_P521
decodeAlgorithm 6 = Just Ed25519Cert
decodeAlgorithm 7 = Just Ed448Cert
decodeAlgorithm _ = Nothing

||| Decode Int to HashAlgorithm
decodeHash : Int -> Maybe HashAlgorithm
decodeHash 0 = Just SHA1
decodeHash 1 = Just SHA256
decodeHash 2 = Just SHA384
decodeHash 3 = Just SHA512
decodeHash _ = Nothing

--------------------------------------------------------------------------------
-- Algorithm Checks
--------------------------------------------------------------------------------

export
proven_idris_cert_is_strong_algorithm : Int -> Int
proven_idris_cert_is_strong_algorithm algId = case decodeAlgorithm algId of
  Nothing => 0
  Just alg => encodeBool (isStrongAlgorithm alg)

export
proven_idris_cert_is_weak_hash : Int -> Int
proven_idris_cert_is_weak_hash hashId = case decodeHash hashId of
  Nothing => 1  -- Unknown is weak
  Just h => encodeBool (isWeakHash h)

export
proven_idris_cert_algorithm_name : Int -> String
proven_idris_cert_algorithm_name algId = case decodeAlgorithm algId of
  Nothing => "Unknown"
  Just alg => show alg

--------------------------------------------------------------------------------
-- Certificate Time Checks
--------------------------------------------------------------------------------

export
proven_idris_cert_is_expired : Int -> Int -> Int
proven_idris_cert_is_expired notAfter currentTime =
  encodeBool (currentTime > notAfter)

export
proven_idris_cert_is_valid_at : Int -> Int -> Int -> Int
proven_idris_cert_is_valid_at notBefore notAfter currentTime =
  encodeBool (currentTime >= notBefore && currentTime <= notAfter)

export
proven_idris_cert_days_until_expiry : Int -> Int -> Int
proven_idris_cert_days_until_expiry notAfter currentTime =
  if currentTime >= notAfter
    then 0
    else cast (div (minus (cast notAfter) (cast currentTime)) 86400)

export
proven_idris_cert_expires_within_days : Int -> Int -> Int -> Int
proven_idris_cert_expires_within_days notAfter currentTime days =
  let remaining = if currentTime >= notAfter
                    then 0
                    else div (minus (cast notAfter) (cast currentTime)) 86400
  in encodeBool (remaining <= cast days)

--------------------------------------------------------------------------------
-- Hostname Matching
--------------------------------------------------------------------------------

||| Check if hostname matches any SAN in a comma-separated list
||| Uses the same wildcard matching logic as SafeCert.matchesHostname:
||| - Exact match against any SAN
||| - Wildcard (*.example.com) matches one level only (not sub.sub.example.com)
|||
||| The wildcard constraint prevents *.example.com from matching
||| a.b.example.com (no dots allowed in the matched prefix).
export
proven_idris_cert_matches_hostname : String -> String -> Int
proven_idris_cert_matches_hostname sanList hostname =
  let sans = split (== ',') sanList
  in encodeBool (elem hostname sans || any (\san => matchWildcard san hostname) sans)
  where
    ||| Wildcard matching: *.example.com matches host.example.com
    ||| but NOT sub.host.example.com (no dots in matched prefix)
    matchWildcard : String -> String -> Bool
    matchWildcard pattern host =
      if isPrefixOf "*." pattern
        then let suffix = strSubstr 2 (length pattern) pattern
                 prefixLen = minus (length host) (length suffix)
                 prefix = strSubstr 0 prefixLen host
             in isSuffixOf suffix host && not (isInfixOf "." prefix)
        else pattern == host
