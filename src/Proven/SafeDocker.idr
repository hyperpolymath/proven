-- SPDX-License-Identifier: Apache-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
||| SafeDocker - Container operations with security validation
|||
||| Provides type-safe container image reference parsing and validation.
||| Prevents: image tag confusion, registry injection, insecure base images.
||| Note: Uses "container" terminology (Podman-first, per hyperpolymath standards).
module Proven.SafeDocker

import Data.String
import Data.List
import Data.Nat
import Data.Maybe

%default total

-- ----------------------------------------------------------------
-- Image reference types
-- ----------------------------------------------------------------

||| Container registry hostname
public export
data Registry : Type where
  MkRegistry : (host : String) -> Registry

||| Image repository (namespace/name)
public export
data Repository : Type where
  MkRepository : (name : String) -> Repository

||| Image tag
public export
data ImageTag : Type where
  MkImageTag : (tag : String) -> ImageTag

||| Content-addressable digest (algorithm:hex)
public export
data Digest : Type where
  MkDigest : (algorithm : String) -> (hex : String) -> Digest

||| A complete image reference
public export
record ImageRef where
  constructor MkImageRef
  registry   : Maybe Registry
  repository : Repository
  tag        : Maybe ImageTag
  digest     : Maybe Digest

||| Show instances
public export
Show Registry where
  show (MkRegistry h) = h

public export
Show Repository where
  show (MkRepository n) = n

public export
Show ImageTag where
  show (MkImageTag t) = t

public export
Show Digest where
  show (MkDigest alg hex) = alg ++ ":" ++ hex

public export
Show ImageRef where
  show ref =
    let reg = maybe "" (\r => show r ++ "/") (registry ref)
        t = maybe "" (\t => ":" ++ show t) (tag ref)
        d = maybe "" (\d => "@" ++ show d) (digest ref)
    in reg ++ show (repository ref) ++ t ++ d

-- ----------------------------------------------------------------
-- Validation
-- ----------------------------------------------------------------

||| Valid characters for image tags
public export
isValidTagChar : Char -> Bool
isValidTagChar c = isAlphaNum c || elem c (unpack ".-_")

||| Validate an image tag string
public export
isValidTag : String -> Bool
isValidTag s =
  let chars = unpack s
  in length chars > 0 && length chars <= 128 && all isValidTagChar chars

||| Validate a digest string (algorithm:hex format)
public export
isValidDigest : String -> Bool
isValidDigest s =
  case break (== ':') (unpack s) of
    (algChars, ':' :: hexChars) =>
      length algChars > 0 &&
      length hexChars >= 32 &&
      all isAlphaNum algChars &&
      all (\c => isAlphaNum c || c == '+' || c == '-' || c == '.') hexChars
    _ => False

||| Validate repository name characters
public export
isValidRepoChar : Char -> Bool
isValidRepoChar c = isAlphaNum c || elem c (unpack ".-_/")

||| Validate a repository name
public export
isValidRepo : String -> Bool
isValidRepo s =
  let chars = unpack s
  in length chars > 0 &&
     all isValidRepoChar chars &&
     not (isInfixOf ".." s) &&
     not (isPrefixOf "/" s) &&
     not (isSuffixOf "/" s)

||| Parse an image reference string
public export
parseImageRef : String -> Maybe ImageRef
parseImageRef s =
  let (beforeDigest, digestPart) = splitOnDigest s
      (beforeTag, tagPart) = splitOnTag beforeDigest
      (regPart, repoPart) = splitOnRegistry beforeTag
  in if length repoPart == 0
       then Nothing
       else if not (isValidRepo repoPart)
         then Nothing
         else Just (MkImageRef
           (map MkRegistry regPart)
           (MkRepository repoPart)
           (map MkImageTag tagPart)
           (parseDigestStr digestPart))
  where
    splitOnDigest : String -> (String, Maybe String)
    splitOnDigest s =
      case break (== '@') (unpack s) of
        (before, '@' :: after) => (pack before, Just (pack after))
        _ => (s, Nothing)

    splitOnTag : String -> (String, Maybe String)
    splitOnTag s =
      case break (== ':') (unpack s) of
        (before, ':' :: after) =>
          if isValidTag (pack after)
            then (pack before, Just (pack after))
            else (s, Nothing)
        _ => (s, Nothing)

    splitOnRegistry : String -> (Maybe String, String)
    splitOnRegistry s =
      case break (== '/') (unpack s) of
        (before, '/' :: after) =>
          if elem '.' (unpack (pack before)) || elem ':' (unpack (pack before))
            then (Just (pack before), pack after)
            else (Nothing, s)
        _ => (Nothing, s)

    parseDigestStr : Maybe String -> Maybe Digest
    parseDigestStr Nothing = Nothing
    parseDigestStr (Just d) =
      if isValidDigest d
        then case break (== ':') (unpack d) of
               (alg, ':' :: hex) => Just (MkDigest (pack alg) (pack hex))
               _ => Nothing
        else Nothing

-- ----------------------------------------------------------------
-- Security checks
-- ----------------------------------------------------------------

||| Known insecure base images
public export
insecureImages : List String
insecureImages =
  [ "docker.io/library/debian"
  , "docker.io/library/ubuntu"
  , "docker.io/library/centos"
  , "docker.io/library/alpine"  -- Prefer chainguard
  ]

||| Recommended secure base images
public export
secureImages : List String
secureImages =
  [ "cgr.dev/chainguard/wolfi-base"
  , "cgr.dev/chainguard/static"
  , "cgr.dev/chainguard/busybox"
  ]

||| Check if image uses a recommended secure base
public export
isSecureBase : ImageRef -> Bool
isSecureBase ref =
  let fullName = maybe "" (\r => show r ++ "/") (registry ref) ++ show (repository ref)
  in any (\secure => isPrefixOf secure fullName) secureImages

||| Check if image is pinned by digest (immutable reference)
public export
isPinnedByDigest : ImageRef -> Bool
isPinnedByDigest ref = isJust (digest ref)

||| Check if image uses "latest" tag (dangerous in production)
public export
usesLatestTag : ImageRef -> Bool
usesLatestTag ref = case tag ref of
  Nothing => True   -- No tag defaults to "latest"
  Just (MkImageTag t) => t == "latest"

||| Normalize image reference (apply Docker Hub defaults)
public export
normalize : ImageRef -> ImageRef
normalize ref =
  let reg = case registry ref of
              Nothing => Just (MkRegistry "docker.io")
              r => r
      repo = case registry ref of
               Nothing =>
                 if not (isInfixOf "/" (show (repository ref)))
                   then MkRepository ("library/" ++ show (repository ref))
                   else repository ref
               _ => repository ref
      t = case tag ref of
            Nothing => case digest ref of
                         Nothing => Just (MkImageTag "latest")
                         _ => Nothing
            t => t
  in { registry := reg, repository := repo, tag := t } ref

-- ----------------------------------------------------------------
-- Proof types
-- ----------------------------------------------------------------

||| Proof that an image reference is valid
public export
data ValidImageRef : ImageRef -> Type where
  MkValidImageRef : isValidRepo (show (repository ref)) = True -> ValidImageRef ref

||| Proof that an image is digest-pinned (immutable)
public export
data DigestPinned : ImageRef -> Type where
  MkDigestPinned : isPinnedByDigest ref = True -> DigestPinned ref

||| Proof that an image uses a secure base
public export
data SecureBase : ImageRef -> Type where
  MkSecureBase : isSecureBase ref = True -> SecureBase ref

||| Proof that an image does not use "latest" tag
public export
data NotLatest : ImageRef -> Type where
  MkNotLatest : usesLatestTag ref = False -> NotLatest ref
