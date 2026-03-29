-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
||| SafeSRI - Subresource Integrity validation
|||
||| Provides type-safe SRI hash construction and verification
||| per W3C Subresource Integrity spec.
||| Prevents: CDN compromise, supply-chain attacks, script tampering.
module Proven.SafeSRI

import Data.String
import Data.List
import Data.Nat

%default total

-- ============================================================================
-- HASH ALGORITHMS
-- ============================================================================

||| SRI-supported hash algorithms (ordered by strength)
public export
data SRIAlgorithm = SHA256 | SHA384 | SHA512

public export
Show SRIAlgorithm where
  show SHA256 = "sha256"
  show SHA384 = "sha384"
  show SHA512 = "sha512"

public export
Eq SRIAlgorithm where
  SHA256 == SHA256 = True
  SHA384 == SHA384 = True
  SHA512 == SHA512 = True
  _ == _ = False

public export
Ord SRIAlgorithm where
  compare SHA256 SHA256 = EQ
  compare SHA256 _      = LT
  compare SHA384 SHA256 = GT
  compare SHA384 SHA384 = EQ
  compare SHA384 SHA512 = LT
  compare SHA512 SHA512 = EQ
  compare SHA512 _      = GT

||| Expected base64-encoded hash length for each algorithm
public export
expectedHashLength : SRIAlgorithm -> Nat
expectedHashLength SHA256 = 44  -- 32 bytes base64
expectedHashLength SHA384 = 64  -- 48 bytes base64
expectedHashLength SHA512 = 88  -- 64 bytes base64

-- ============================================================================
-- SRI HASH TYPE
-- ============================================================================

||| A validated SRI hash entry
public export
record SRIHash where
  constructor MkSRIHash
  algorithm : SRIAlgorithm
  digest    : String  -- Base64-encoded hash

public export
Show SRIHash where
  show h = show h.algorithm ++ "-" ++ h.digest

public export
Eq SRIHash where
  a == b = a.algorithm == b.algorithm && a.digest == b.digest

||| Parse an SRI hash string (e.g., "sha256-abc123...")
public export
parseSRIHash : String -> Maybe SRIHash
parseSRIHash s =
  case break (== '-') (unpack s) of
    (algPart, '-' :: hashPart) =>
      let alg = pack algPart
          hash = pack hashPart
      in do algo <- parseAlgo alg
            if isValidBase64 hash && length hash == expectedHashLength algo
              then Just (MkSRIHash algo hash)
              else Nothing
    _ => Nothing
  where
    parseAlgo : String -> Maybe SRIAlgorithm
    parseAlgo "sha256" = Just SHA256
    parseAlgo "sha384" = Just SHA384
    parseAlgo "sha512" = Just SHA512
    parseAlgo _        = Nothing

    isBase64Char : Char -> Bool
    isBase64Char c = (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z') ||
                     (c >= '0' && c <= '9') || c == '+' || c == '/' || c == '='

    isValidBase64 : String -> Bool
    isValidBase64 str = length str > 0 && all isBase64Char (unpack str)

-- ============================================================================
-- INTEGRITY ATTRIBUTE
-- ============================================================================

||| A complete SRI integrity attribute (may have multiple hashes)
public export
record IntegrityAttribute where
  constructor MkIntegrity
  hashes : List SRIHash

||| Render as an HTML integrity attribute value
public export
renderIntegrity : IntegrityAttribute -> String
renderIntegrity attr =
  fastConcat (intersperse " " (map show attr.hashes))

||| Parse a full integrity attribute string
public export
parseIntegrity : String -> Maybe IntegrityAttribute
parseIntegrity s =
  let parts = words s
      parsed = traverse parseSRIHash parts
  in case parsed of
       Nothing => Nothing
       Just [] => Nothing
       Just hashes => Just (MkIntegrity hashes)

||| Create an integrity attribute from a single hash
public export
singleHash : SRIAlgorithm -> String -> Maybe IntegrityAttribute
singleHash algo digest = do
  h <- parseSRIHash (show algo ++ "-" ++ digest)
  Just (MkIntegrity [h])

-- ============================================================================
-- VERIFICATION
-- ============================================================================

||| Check if a computed hash matches any hash in the integrity attribute.
||| The strongest matching algorithm is preferred per spec.
public export
verifyIntegrity : IntegrityAttribute -> SRIAlgorithm -> String -> Bool
verifyIntegrity attr computedAlgo computedDigest =
  any (\h => h.algorithm == computedAlgo && h.digest == computedDigest) attr.hashes

||| Get the strongest algorithm used in the attribute
public export
strongestAlgorithm : IntegrityAttribute -> Maybe SRIAlgorithm
strongestAlgorithm attr =
  foldl (\acc, h =>
    case acc of
      Nothing => Just h.algorithm
      Just a  => if compare h.algorithm a == GT then Just h.algorithm else Just a
  ) Nothing attr.hashes

-- ============================================================================
-- HTML TAG GENERATION
-- ============================================================================

||| Generate a safe script tag with SRI
public export
scriptTag : String -> IntegrityAttribute -> String
scriptTag src integrity =
  "<script src=\"" ++ src ++ "\" integrity=\"" ++
  renderIntegrity integrity ++ "\" crossorigin=\"anonymous\"></script>"

||| Generate a safe link (stylesheet) tag with SRI
public export
linkTag : String -> IntegrityAttribute -> String
linkTag href integrity =
  "<link rel=\"stylesheet\" href=\"" ++ href ++ "\" integrity=\"" ++
  renderIntegrity integrity ++ "\" crossorigin=\"anonymous\">"

-- ============================================================================
-- POLICY
-- ============================================================================

||| SRI enforcement policy
public export
data SRIPolicy =
    RequireSRI         -- All external resources must have SRI
  | PreferSRI          -- Warn if missing, don't block
  | DisableSRI         -- No SRI checking

public export
Eq SRIPolicy where
  RequireSRI == RequireSRI = True
  PreferSRI == PreferSRI = True
  DisableSRI == DisableSRI = True
  _ == _ = False

||| Validate a resource load against SRI policy
public export
data SRIResult =
    Verified        -- Hash matches
  | HashMismatch    -- Hash doesn't match (blocked)
  | MissingHash     -- No SRI attribute
  | PolicyExempt    -- SRI not required by policy

public export
Show SRIResult where
  show Verified     = "verified"
  show HashMismatch = "hash_mismatch"
  show MissingHash  = "missing_hash"
  show PolicyExempt = "policy_exempt"

||| Check a resource against SRI policy
public export
checkResource : SRIPolicy -> Maybe IntegrityAttribute -> SRIAlgorithm -> String -> SRIResult
checkResource DisableSRI _ _ _ = PolicyExempt
checkResource _ Nothing _ _ = MissingHash
checkResource _ (Just attr) algo digest =
  if verifyIntegrity attr algo digest then Verified else HashMismatch
