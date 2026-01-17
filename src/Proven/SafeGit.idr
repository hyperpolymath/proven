-- SPDX-License-Identifier: PMPL-1.0
||| SafeGit - Git operations that cannot crash
|||
||| This module provides:
||| - Safe Git ref validation (branch names, tags)
||| - Commit message format validation
||| - SHA/OID validation
||| - Remote URL parsing and validation
||| - Protection against Git injection attacks
module Proven.SafeGit

import public Proven.Core
import public Proven.SafeUrl

import Data.List
import Data.String

%default total

--------------------------------------------------------------------------------
-- Git Reference Types
--------------------------------------------------------------------------------

||| Git object identifier (SHA-1 or SHA-256)
public export
data ObjectId
  = SHA1 String    -- 40 hex characters
  | SHA256 String  -- 64 hex characters

public export
Eq ObjectId where
  (SHA1 a) == (SHA1 b) = a == b
  (SHA256 a) == (SHA256 b) = a == b
  _ == _ = False

public export
Show ObjectId where
  show (SHA1 s) = s
  show (SHA256 s) = s

||| Abbreviated object identifier
public export
record AbbrevId where
  constructor MkAbbrevId
  value : String
  minLength : Nat

||| Git reference types
public export
data RefType
  = Branch
  | Tag
  | Remote String  -- remote name
  | Head
  | Stash Nat

public export
Show RefType where
  show Branch = "branch"
  show Tag = "tag"
  show (Remote r) = "remote/" ++ r
  show Head = "HEAD"
  show (Stash n) = "stash@{" ++ show n ++ "}"

||| A Git reference (branch, tag, etc.)
public export
record GitRef where
  constructor MkGitRef
  refType : RefType
  name : String
  fullPath : String

public export
Show GitRef where
  show ref = ref.fullPath

||| Remote repository URL types
public export
data RemoteUrlType = HTTPS | SSH | Git | File | Unknown

||| Remote repository URL
public export
record RemoteUrl where
  constructor MkRemoteUrl
  urlType : RemoteUrlType
  host : Maybe String
  port : Maybe Nat
  user : Maybe String
  path : String
  original : String

--------------------------------------------------------------------------------
-- Commit Types
--------------------------------------------------------------------------------

||| Commit author/committer info
public export
record GitIdentity where
  constructor MkGitIdentity
  name : String
  email : String
  timestamp : Integer
  timezone : String

public export
Show GitIdentity where
  show ident = ident.name ++ " <" ++ ident.email ++ ">"

||| Conventional commit type
public export
data CommitType
  = Feat | Fix | Docs | Style | Refactor | Perf
  | Test | Build | CI | Chore | Revert | Custom String

public export
Show CommitType where
  show Feat = "feat"
  show Fix = "fix"
  show Docs = "docs"
  show Style = "style"
  show Refactor = "refactor"
  show Perf = "perf"
  show Test = "test"
  show Build = "build"
  show CI = "ci"
  show Chore = "chore"
  show Revert = "revert"
  show (Custom s) = s

||| Parsed conventional commit message
public export
record ConventionalCommit where
  constructor MkConventionalCommit
  commitType : CommitType
  scope : Maybe String
  breaking : Bool
  description : String
  body : Maybe String
  footers : List (String, String)

||| Commit message (raw or conventional)
public export
data CommitMessage
  = RawMessage String
  | Conventional ConventionalCommit

--------------------------------------------------------------------------------
-- Git Errors
--------------------------------------------------------------------------------

||| Git validation errors
public export
data GitError
  = InvalidRefName String String        -- name, reason
  | InvalidObjectId String              -- the invalid OID
  | ObjectIdTooShort String Nat         -- value, minimum length
  | InvalidRemoteUrl String String      -- url, reason
  | InvalidCommitMessage String         -- reason
  | RefNameContainsForbidden String Char
  | RefNameStartsWithForbidden String String
  | RefNameEndsWithForbidden String String
  | ConsecutiveDotsInRef String
  | RefNameTooLong String Nat
  | EmptyRefName
  | DangerousRefPattern String

public export
Show GitError where
  show (InvalidRefName name reason) = "Invalid ref name '" ++ name ++ "': " ++ reason
  show (InvalidObjectId oid) = "Invalid object ID: " ++ oid
  show (ObjectIdTooShort val min) = "Object ID too short: " ++ val ++ " (minimum " ++ show min ++ " chars)"
  show (InvalidRemoteUrl url reason) = "Invalid remote URL '" ++ url ++ "': " ++ reason
  show (InvalidCommitMessage reason) = "Invalid commit message: " ++ reason
  show (RefNameContainsForbidden name c) = "Ref '" ++ name ++ "' contains forbidden char: " ++ singleton c
  show (RefNameStartsWithForbidden name prefix) = "Ref '" ++ name ++ "' starts with forbidden: " ++ prefix
  show (RefNameEndsWithForbidden name suffix) = "Ref '" ++ name ++ "' ends with forbidden: " ++ suffix
  show (ConsecutiveDotsInRef name) = "Ref '" ++ name ++ "' contains consecutive dots"
  show (RefNameTooLong name len) = "Ref '" ++ name ++ "' too long: " ++ show len ++ " chars"
  show EmptyRefName = "Empty ref name"
  show (DangerousRefPattern pattern) = "Dangerous ref pattern: " ++ pattern

--------------------------------------------------------------------------------
-- Object ID Validation
--------------------------------------------------------------------------------

||| Check if character is valid hex
public export
isHexChar : Char -> Bool
isHexChar c = (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F')

||| Check if string is valid hex
public export
isHexString : String -> Bool
isHexString s = all isHexChar (unpack s)

||| Validate and parse a full SHA-1 (40 chars)
public export
parseSHA1 : String -> Either GitError ObjectId
parseSHA1 s =
  let lower = toLower s
  in if length lower /= 40
       then Left (InvalidObjectId s)
       else if isHexString lower
         then Right (SHA1 lower)
         else Left (InvalidObjectId s)

||| Validate and parse a full SHA-256 (64 chars)
public export
parseSHA256 : String -> Either GitError ObjectId
parseSHA256 s =
  let lower = toLower s
  in if length lower /= 64
       then Left (InvalidObjectId s)
       else if isHexString lower
         then Right (SHA256 lower)
         else Left (InvalidObjectId s)

||| Parse any valid object ID (SHA-1 or SHA-256)
public export
parseObjectId : String -> Either GitError ObjectId
parseObjectId s = case length s of
  40 => parseSHA1 s
  64 => parseSHA256 s
  _ => Left (InvalidObjectId s)

||| Parse abbreviated object ID (minimum 4 chars by default)
public export
parseAbbrevId : String -> Either GitError AbbrevId
parseAbbrevId s =
  let lower = toLower s
      len = length lower
  in if len < 4
       then Left (ObjectIdTooShort s 4)
       else if len > 64
         then Left (InvalidObjectId s)
         else if isHexString lower
           then Right (MkAbbrevId lower 4)
           else Left (InvalidObjectId s)

||| Check if abbreviated ID could match full ID
public export
couldMatch : AbbrevId -> ObjectId -> Bool
couldMatch abbrev full =
  isPrefixOf abbrev.value (show full)

--------------------------------------------------------------------------------
-- Reference Name Validation
--------------------------------------------------------------------------------

||| Characters forbidden anywhere in ref names
public export
forbiddenRefChars : List Char
forbiddenRefChars = [' ', '~', '^', ':', '?', '*', '[', '\\', '\x7F']

||| Check for forbidden characters
public export
containsForbiddenChar : String -> Maybe Char
containsForbiddenChar s =
  find (`elem` forbiddenRefChars) (unpack s)

||| Check for control characters (ASCII 0-31)
public export
containsControlChar : String -> Bool
containsControlChar s = any (\c => ord c < 32) (unpack s)

||| Check for consecutive dots
public export
hasConsecutiveDots : String -> Bool
hasConsecutiveDots s = isInfixOf ".." s

||| Check for @{ sequence (reflog syntax)
public export
hasReflogSyntax : String -> Bool
hasReflogSyntax s = isInfixOf "@{" s

||| Maximum ref name length
public export
maxRefLength : Nat
maxRefLength = 255

||| Validate a Git reference name
public export
validateRefName : String -> Either GitError String
validateRefName "" = Left EmptyRefName
validateRefName name =
  if length name > maxRefLength
    then Left (RefNameTooLong name (length name))
    else case containsForbiddenChar name of
      Just c => Left (RefNameContainsForbidden name c)
      Nothing =>
        if containsControlChar name
          then Left (InvalidRefName name "contains control characters")
          else if hasConsecutiveDots name
            then Left (ConsecutiveDotsInRef name)
            else if hasReflogSyntax name
              then Left (InvalidRefName name "contains @{ sequence")
              else if isPrefixOf "/" name
                then Left (RefNameStartsWithForbidden name "/")
                else if isSuffixOf "/" name
                  then Left (RefNameEndsWithForbidden name "/")
                  else if isSuffixOf ".lock" name
                    then Left (RefNameEndsWithForbidden name ".lock")
                    else if isSuffixOf "." name
                      then Left (RefNameEndsWithForbidden name ".")
                      else Right name

||| Create a validated branch ref
public export
branch : String -> Either GitError GitRef
branch name = do
  validated <- validateRefName name
  Right (MkGitRef Branch validated ("refs/heads/" ++ validated))

||| Create a validated tag ref
public export
tag : String -> Either GitError GitRef
tag name = do
  validated <- validateRefName name
  Right (MkGitRef Tag validated ("refs/tags/" ++ validated))

||| Create a validated remote ref
public export
remoteRef : String -> String -> Either GitError GitRef
remoteRef remote name = do
  validatedRemote <- validateRefName remote
  validatedName <- validateRefName name
  Right (MkGitRef (Remote validatedRemote) validatedName
         ("refs/remotes/" ++ validatedRemote ++ "/" ++ validatedName))

--------------------------------------------------------------------------------
-- Remote URL Parsing
--------------------------------------------------------------------------------

||| Parse SSH URL (git@host:path or ssh://...)
public export
parseSSHUrl : String -> Either GitError RemoteUrl
parseSSHUrl url =
  if isPrefixOf "ssh://" url
    then parseSshProtocol (drop 6 url)
    else if isInfixOf "@" url && isInfixOf ":" url
      then parseScpSyntax url
      else Left (InvalidRemoteUrl url "not a valid SSH URL")
  where
    parseSshProtocol : String -> Either GitError RemoteUrl
    parseSshProtocol rest =
      Right (MkRemoteUrl SSH Nothing Nothing Nothing rest url)

    parseScpSyntax : String -> Either GitError RemoteUrl
    parseScpSyntax s =
      case break (== '@') s of
        (user, rest) =>
          case break (== ':') (drop 1 rest) of
            (host, path) =>
              Right (MkRemoteUrl SSH (Just host) Nothing (Just user) (drop 1 path) url)

||| Parse HTTPS URL
public export
parseHTTPSUrl : String -> Either GitError RemoteUrl
parseHTTPSUrl url =
  if isPrefixOf "https://" url
    then let rest = drop 8 url
         in case break (== '/') rest of
              (host, path) => Right (MkRemoteUrl HTTPS (Just host) Nothing Nothing path url)
    else if isPrefixOf "http://" url
      then let rest = drop 7 url
           in case break (== '/') rest of
                (host, path) => Right (MkRemoteUrl HTTPS (Just host) Nothing Nothing path url)
      else Left (InvalidRemoteUrl url "not a valid HTTP(S) URL")

||| Parse Git protocol URL
public export
parseGitUrl : String -> Either GitError RemoteUrl
parseGitUrl url =
  if isPrefixOf "git://" url
    then let rest = drop 6 url
         in case break (== '/') rest of
              (host, path) => Right (MkRemoteUrl Git (Just host) Nothing Nothing path url)
    else Left (InvalidRemoteUrl url "not a valid Git URL")

||| Parse file URL
public export
parseFileUrl : String -> Either GitError RemoteUrl
parseFileUrl url =
  if isPrefixOf "file://" url
    then Right (MkRemoteUrl File Nothing Nothing Nothing (drop 7 url) url)
    else if isPrefixOf "/" url
      then Right (MkRemoteUrl File Nothing Nothing Nothing url url)
      else Left (InvalidRemoteUrl url "not a valid file URL")

||| Parse any remote URL
public export
parseRemoteUrl : String -> Either GitError RemoteUrl
parseRemoteUrl url =
  if isPrefixOf "https://" url || isPrefixOf "http://" url
    then parseHTTPSUrl url
    else if isPrefixOf "ssh://" url || (isInfixOf "@" url && isInfixOf ":" url)
      then parseSSHUrl url
      else if isPrefixOf "git://" url
        then parseGitUrl url
        else if isPrefixOf "file://" url || isPrefixOf "/" url
          then parseFileUrl url
          else Left (InvalidRemoteUrl url "unknown URL scheme")

||| Extract repository name from remote URL
public export
repoNameFromUrl : RemoteUrl -> Maybe String
repoNameFromUrl remote =
  let path = remote.path
      trimmed = if isSuffixOf ".git" path then dropLast 4 path else path
      segments = split '/' trimmed
  in last' segments

--------------------------------------------------------------------------------
-- Commit Message Validation
--------------------------------------------------------------------------------

||| Maximum subject line length (conventional)
public export
maxSubjectLength : Nat
maxSubjectLength = 72

||| Maximum body line length (conventional)
public export
maxBodyLineLength : Nat
maxBodyLineLength = 100

||| Parse commit type from string
public export
parseCommitType : String -> CommitType
parseCommitType s = case toLower s of
  "feat" => Feat
  "fix" => Fix
  "docs" => Docs
  "style" => Style
  "refactor" => Refactor
  "perf" => Perf
  "test" => Test
  "build" => Build
  "ci" => CI
  "chore" => Chore
  "revert" => Revert
  other => Custom other

||| Validate commit message subject line
public export
validateSubject : String -> Either GitError String
validateSubject "" = Left (InvalidCommitMessage "empty subject line")
validateSubject subject =
  if length subject > maxSubjectLength
    then Left (InvalidCommitMessage ("subject too long: " ++ show (length subject) ++ " chars"))
    else if isSuffixOf "." subject
      then Left (InvalidCommitMessage "subject should not end with period")
      else Right subject

||| Parse conventional commit message
public export
parseConventional : String -> Either GitError ConventionalCommit
parseConventional msg =
  let lines = split '\n' msg
  in case lines of
       [] => Left (InvalidCommitMessage "empty message")
       (subject :: rest) =>
         case break (== ':') subject of
           (_, "") => Left (InvalidCommitMessage "missing colon in conventional format")
           (typeScope, descWithColon) =>
             let desc = trim (drop 1 descWithColon)
                 breaking = isSuffixOf "!" typeScope
                 typeStr = if breaking then dropLast 1 typeScope else typeScope
                 (commitType, scope) = parseTypeScope typeStr
                 body = if null rest then Nothing else Just (unlines rest)
             in Right (MkConventionalCommit commitType scope breaking desc body [])
  where
    parseTypeScope : String -> (CommitType, Maybe String)
    parseTypeScope s =
      case break (== '(') s of
        (t, "") => (parseCommitType t, Nothing)
        (t, scopeWithParens) =>
          let scopeStr = drop 1 (dropLast 1 scopeWithParens)
          in (parseCommitType t, Just scopeStr)

||| Validate any commit message
public export
validateCommitMessage : String -> Either GitError CommitMessage
validateCommitMessage "" = Left (InvalidCommitMessage "empty commit message")
validateCommitMessage msg =
  case parseConventional msg of
    Right conv => Right (Conventional conv)
    Left _ => Right (RawMessage msg)  -- Fall back to raw if not conventional

--------------------------------------------------------------------------------
-- Dangerous Pattern Detection
--------------------------------------------------------------------------------

||| Patterns that could be used in attacks
public export
dangerousPatterns : List String
dangerousPatterns =
  [ "--upload-pack"
  , "--receive-pack"
  , "-c core."
  , "-c protocol."
  , "ext::"
  , "fd::"
  ]

||| Check if string contains dangerous Git patterns
public export
containsDangerousPattern : String -> Maybe String
containsDangerousPattern s =
  find (\p => isInfixOf p s) dangerousPatterns

||| Sanitize string for safe use in Git commands
public export
sanitizeForGit : String -> Either GitError String
sanitizeForGit s =
  case containsDangerousPattern s of
    Just pattern => Left (DangerousRefPattern pattern)
    Nothing => Right s

--------------------------------------------------------------------------------
-- Utility Functions
--------------------------------------------------------------------------------

||| Check if string looks like a SHA
public export
looksLikeSHA : String -> Bool
looksLikeSHA s =
  let len = length s
  in (len >= 4 && len <= 64) && isHexString s

||| Get short form of object ID
public export
shortId : ObjectId -> String
shortId (SHA1 s) = take 7 s
shortId (SHA256 s) = take 8 s

||| Format ref for display
public export
displayRef : GitRef -> String
displayRef ref = case ref.refType of
  Branch => ref.name
  Tag => "v" ++ ref.name
  Remote r => r ++ "/" ++ ref.name
  Head => "HEAD"
  Stash n => "stash@{" ++ show n ++ "}"

||| Check if ref is a branch
public export
isBranch : GitRef -> Bool
isBranch ref = case ref.refType of
  Branch => True
  _ => False

||| Check if ref is a tag
public export
isTag : GitRef -> Bool
isTag ref = case ref.refType of
  Tag => True
  _ => False

||| Common branch names
public export
mainBranch : Either GitError GitRef
mainBranch = branch "main"

public export
masterBranch : Either GitError GitRef
masterBranch = branch "master"

public export
developBranch : Either GitError GitRef
developBranch = branch "develop"
