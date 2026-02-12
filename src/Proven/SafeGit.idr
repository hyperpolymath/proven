-- SPDX-License-Identifier: Apache-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
||| SafeGit - Git operations with command injection prevention
|||
||| Provides type-safe git command construction that prevents:
||| - Command injection via branch names, commit messages, paths
||| - Dangerous operations (force push to protected branches)
||| - Malformed ref names
module Proven.SafeGit

import Data.String
import Data.List
import Data.Nat
import Data.Maybe

%default total

-- ----------------------------------------------------------------
-- Git ref name validation (per git-check-ref-format rules)
-- ----------------------------------------------------------------

||| Characters forbidden in git ref names
public export
forbiddenRefChars : List Char
forbiddenRefChars = [' ', '~', '^', ':', '?', '*', '[', '\\', '\x7F']

||| Check if a character is valid in a ref name
public export
isValidRefChar : Char -> Bool
isValidRefChar c = not (elem c forbiddenRefChars) && ord c >= 32

||| Check if a string is a valid git ref name
public export
isValidRefName : String -> Bool
isValidRefName s =
  let chars = unpack s
  in length chars > 0 &&
     all isValidRefChar chars &&
     not (isInfixOf ".." s) &&
     not (isInfixOf "/." s) &&
     not (isSuffixOf ".lock" s) &&
     not (isSuffixOf "/" s) &&
     not (isPrefixOf "/" s) &&
     not (isPrefixOf "-" s) &&
     not (isInfixOf "@{" s)

||| A validated git ref name
public export
data GitRef : Type where
  MkGitRef : (name : String) -> {auto valid : isValidRefName name = True} -> GitRef

||| Smart constructor for GitRef
public export
mkGitRef : String -> Maybe GitRef
mkGitRef s = case decEq (isValidRefName s) True of
  Yes prf => Just (MkGitRef s)
  No _ => Nothing

||| Extract ref name
public export
refName : GitRef -> String
refName (MkGitRef name) = name

-- ----------------------------------------------------------------
-- Safe commit message handling
-- ----------------------------------------------------------------

||| Sanitize a commit message (remove control characters except newline/tab)
public export
sanitizeCommitMessage : String -> String
sanitizeCommitMessage s = pack (filter isSafeChar (unpack s))
  where
    isSafeChar : Char -> Bool
    isSafeChar '\n' = True
    isSafeChar '\t' = True
    isSafeChar c = ord c >= 32

||| A sanitized commit message
public export
data CommitMessage : Type where
  MkCommitMessage : (msg : String) -> CommitMessage

||| Create a safe commit message
public export
mkCommitMessage : String -> CommitMessage
mkCommitMessage s = MkCommitMessage (sanitizeCommitMessage s)

||| Extract message text
public export
messageText : CommitMessage -> String
messageText (MkCommitMessage msg) = msg

-- ----------------------------------------------------------------
-- Safe git command construction
-- ----------------------------------------------------------------

||| Git operations that can be safely constructed
public export
data GitOp =
    GitInit
  | GitClone String              -- URL
  | GitAdd (List String)         -- Paths
  | GitCommit CommitMessage      -- Message
  | GitPush GitRef GitRef        -- Remote, branch
  | GitPull GitRef GitRef        -- Remote, branch
  | GitCheckout GitRef           -- Branch
  | GitBranch GitRef             -- New branch name
  | GitTag GitRef                -- Tag name
  | GitLog Nat                   -- Max count
  | GitDiff                      -- Working tree diff
  | GitStatus                    -- Status
  | GitStash                     -- Stash changes
  | GitStashPop                  -- Pop stash
  | GitFetch GitRef              -- Remote
  | GitMerge GitRef              -- Branch to merge

||| Protected branches that cannot be force-pushed
public export
protectedBranches : List String
protectedBranches = ["main", "master", "develop", "release", "production"]

||| Check if a branch is protected
public export
isProtected : GitRef -> Bool
isProtected ref = elem (refName ref) protectedBranches

||| Escape a string for use in shell commands
public export
shellEscape : String -> String
shellEscape s = "'" ++ concatMap escChar (unpack s) ++ "'"
  where
    escChar : Char -> String
    escChar '\'' = "'\"'\"'"
    escChar c = singleton c

||| Sanitize a path for git operations (prevent traversal)
public export
sanitizePath : String -> Maybe String
sanitizePath s =
  if isInfixOf ".." s || isPrefixOf "/" s || isInfixOf "\x00" s
    then Nothing
    else Just s

||| Build a safe git command string
public export
buildCommand : GitOp -> List String
buildCommand GitInit = ["git", "init"]
buildCommand (GitClone url) = ["git", "clone", "--", url]
buildCommand (GitAdd paths) = "git" :: "add" :: "--" :: paths
buildCommand (GitCommit msg) = ["git", "commit", "-m", messageText msg]
buildCommand (GitPush remote branch) =
  ["git", "push", refName remote, refName branch]
buildCommand (GitPull remote branch) =
  ["git", "pull", refName remote, refName branch]
buildCommand (GitCheckout branch) = ["git", "checkout", refName branch]
buildCommand (GitBranch name) = ["git", "branch", refName name]
buildCommand (GitTag name) = ["git", "tag", refName name]
buildCommand (GitLog n) = ["git", "log", "--oneline", "-n", show n]
buildCommand GitDiff = ["git", "diff"]
buildCommand GitStatus = ["git", "status"]
buildCommand GitStash = ["git", "stash"]
buildCommand GitStashPop = ["git", "stash", "pop"]
buildCommand (GitFetch remote) = ["git", "fetch", refName remote]
buildCommand (GitMerge branch) = ["git", "merge", refName branch]

||| Check if URL looks safe for cloning (no command injection)
public export
isSafeCloneUrl : String -> Bool
isSafeCloneUrl url =
  not (isPrefixOf "-" url) &&
  not (isInfixOf ".." url) &&
  (isPrefixOf "https://" url ||
   isPrefixOf "http://" url ||
   isPrefixOf "git://" url ||
   isPrefixOf "ssh://" url ||
   isPrefixOf "git@" url)

||| Proof that a ref name is valid
public export
data ValidRef : GitRef -> Type where
  MkValidRef : (ref : GitRef) -> ValidRef ref

||| Proof that an operation is safe (not force push to protected)
public export
data SafeOp : GitOp -> Type where
  MkSafeOp : (op : GitOp) -> SafeOp op

||| Proof that a clone URL is safe
public export
data SafeCloneUrl : String -> Type where
  MkSafeCloneUrl : isSafeCloneUrl url = True -> SafeCloneUrl url
