-- SPDX-License-Identifier: Apache-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
||| SafePath Types
|||
||| Core type definitions for safe path handling.
module Proven.SafePath.Types

import Proven.Core
import Data.List
import Data.String

%default total

--------------------------------------------------------------------------------
-- Platform Types
--------------------------------------------------------------------------------

||| Operating system type for path handling
public export
data Platform = Unix | Windows | MacOS

public export
Eq Platform where
  Unix == Unix = True
  Windows == Windows = True
  MacOS == MacOS = True
  _ == _ = False

public export
Show Platform where
  show Unix = "Unix"
  show Windows = "Windows"
  show MacOS = "MacOS"

||| Current platform (compile-time constant)
public export
currentPlatform : Platform
currentPlatform = Unix  -- Default, could be set via compile flags

--------------------------------------------------------------------------------
-- Path Component Types
--------------------------------------------------------------------------------

||| Path prefix (Windows drive letters, UNC paths)
public export
data PathPrefix
  = NoDrive                       -- No drive/prefix (Unix)
  | DriveLetter Char              -- C:, D:, etc.
  | UNCShare String String        -- \\server\share
  | DeviceNS String               -- \\.\device or \\?\device

public export
Eq PathPrefix where
  NoDrive == NoDrive = True
  (DriveLetter a) == (DriveLetter b) = toUpper a == toUpper b
  (UNCShare s1 sh1) == (UNCShare s2 sh2) = toLower s1 == toLower s2 && toLower sh1 == toLower sh2
  (DeviceNS a) == (DeviceNS b) = toLower a == toLower b
  _ == _ = False

public export
Show PathPrefix where
  show NoDrive = ""
  show (DriveLetter c) = singleton c ++ ":"
  show (UNCShare server share) = "\\\\" ++ server ++ "\\" ++ share
  show (DeviceNS device) = "\\\\.\\" ++ device

joinWith : String -> List String -> String
joinWith _ [] = ""
joinWith sep (x :: xs) = foldl (\acc => \y => acc ++ sep ++ y) x xs

--------------------------------------------------------------------------------
-- File Types
--------------------------------------------------------------------------------

||| File type classification
public export
data FileType
  = RegularFile
  | Directory
  | Symlink
  | BlockDevice
  | CharDevice
  | Pipe
  | Socket
  | Unknown

public export
Eq FileType where
  RegularFile == RegularFile = True
  Directory == Directory = True
  Symlink == Symlink = True
  BlockDevice == BlockDevice = True
  CharDevice == CharDevice = True
  Pipe == Pipe = True
  Socket == Socket = True
  Unknown == Unknown = True
  _ == _ = False

public export
Show FileType where
  show RegularFile = "file"
  show Directory = "directory"
  show Symlink = "symlink"
  show BlockDevice = "block device"
  show CharDevice = "char device"
  show Pipe = "pipe"
  show Socket = "socket"
  show Unknown = "unknown"

--------------------------------------------------------------------------------
-- Permission Types
--------------------------------------------------------------------------------

||| File permissions (Unix style)
public export
record Permissions where
  constructor MkPermissions
  ownerRead : Bool
  ownerWrite : Bool
  ownerExecute : Bool
  groupRead : Bool
  groupWrite : Bool
  groupExecute : Bool
  otherRead : Bool
  otherWrite : Bool
  otherExecute : Bool

||| Default permissions (rw-r--r--)
public export
defaultPermissions : Permissions
defaultPermissions = MkPermissions True True False True False False True False False

||| Executable permissions (rwxr-xr-x)
public export
executablePermissions : Permissions
executablePermissions = MkPermissions True True True True False True True False True

||| Read-only permissions (r--r--r--)
public export
readOnlyPermissions : Permissions
readOnlyPermissions = MkPermissions True False False True False False True False False

||| Convert permissions to octal
public export
toOctal : Permissions -> Nat
toOctal p =
  let owner = (if p.ownerRead then 4 else 0) + (if p.ownerWrite then 2 else 0) + (if p.ownerExecute then 1 else 0)
      group = (if p.groupRead then 4 else 0) + (if p.groupWrite then 2 else 0) + (if p.groupExecute then 1 else 0)
      other = (if p.otherRead then 4 else 0) + (if p.otherWrite then 2 else 0) + (if p.otherExecute then 1 else 0)
  in owner * 64 + group * 8 + other

--------------------------------------------------------------------------------
-- Extended Path Information
--------------------------------------------------------------------------------

||| Full path information with metadata
public export
record PathInfo where
  constructor MkPathInfo
  pathString : String
  isAbsolute : Bool
  segments : List String
  pathPrefix : PathPrefix
  extension : Maybe String
  fileType : Maybe FileType
  permissions : Maybe Permissions

--------------------------------------------------------------------------------
-- Safe Path Wrapper Types
--------------------------------------------------------------------------------

||| A validated path that has passed safety checks
public export
data ValidatedPath : Type where
  MkValidatedPath : (path : String) ->
                    (isAbs : Bool) ->
                    (segs : List String) ->
                    ValidatedPath

||| Extract path string from validated path
public export
getPath : ValidatedPath -> String
getPath (MkValidatedPath path _ _) = path

||| Check if validated path is absolute
public export
isAbsoluteV : ValidatedPath -> Bool
isAbsoluteV (MkValidatedPath _ isAbs _) = isAbs

||| A path that is guaranteed to be within a base directory
public export
data ContainedPath : (base : String) -> Type where
  MkContainedPath : (base : String) ->
                    (relative : String) ->
                    (full : String) ->
                    ContainedPath base

||| Get the full path from a contained path
public export
getFullPath : ContainedPath base -> String
getFullPath (MkContainedPath _ _ full) = full

||| Get the relative part of a contained path
public export
getRelativePath : ContainedPath base -> String
getRelativePath (MkContainedPath _ rel _) = rel

--------------------------------------------------------------------------------
-- Glob Pattern Types
--------------------------------------------------------------------------------

||| Glob pattern segment
public export
data GlobSegment
  = Literal String       -- Exact match
  | AnyChar              -- ?
  | AnyString            -- *
  | AnyPath              -- **
  | CharClass (List Char) Bool  -- [abc] or [^abc]
  | Alternative (List String)    -- {a,b,c}

public export
Show GlobSegment where
  show (Literal s) = s
  show AnyChar = "?"
  show AnyString = "*"
  show AnyPath = "**"
  show (CharClass chars neg) = "[" ++ (if neg then "^" else "") ++ pack chars ++ "]"
  show (Alternative alts) = "{" ++ joinWith "," alts ++ "}"

||| A glob pattern for matching paths
public export
record GlobPattern where
  constructor MkGlobPattern
  segments : List (List GlobSegment)
  anchored : Bool  -- Must match from start

public export
Show GlobPattern where
  show pat =
    let segStrs = map (concat . map show) pat.segments
    in joinWith "/" segStrs
