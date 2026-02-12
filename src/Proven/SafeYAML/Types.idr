-- SPDX-License-Identifier: Apache-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
||| YAML types and security constraints
|||
||| This module defines types for safe YAML processing including:
||| - YAML value representation
||| - Security options to prevent deserialization attacks
||| - Anchor/alias handling
module Proven.SafeYAML.Types

import Proven.Core
import Data.List
import Data.String

%default total

--------------------------------------------------------------------------------
-- YAML Value Types
--------------------------------------------------------------------------------

||| YAML value representation
public export
data YAMLValue : Type where
  ||| Null value
  YNull : YAMLValue

  ||| Boolean value
  YBool : Bool -> YAMLValue

  ||| Integer value
  YInt : Integer -> YAMLValue

  ||| Floating point value
  YFloat : Double -> YAMLValue

  ||| String value
  YString : String -> YAMLValue

  ||| Array/sequence value
  YArray : List YAMLValue -> YAMLValue

  ||| Object/mapping value
  YObject : List (String, YAMLValue) -> YAMLValue

  ||| Binary data (base64 encoded in YAML)
  YBinary : List Bits8 -> YAMLValue

  ||| Timestamp value
  YTimestamp : String -> YAMLValue

public export
Show YAMLValue where
  show YNull = "null"
  show (YBool True) = "true"
  show (YBool False) = "false"
  show (YInt i) = show i
  show (YFloat f) = show f
  show (YString s) = show s
  show (YArray xs) = "[" ++ join ", " (map show xs) ++ "]"
    where
      join : String -> List String -> String
      join _ [] = ""
      join _ [x] = x
      join sep (x :: xs) = x ++ sep ++ join sep xs
  show (YObject kvs) = "{" ++ join ", " (map showKV kvs) ++ "}"
    where
      join : String -> List String -> String
      join _ [] = ""
      join _ [x] = x
      join sep (x :: xs) = x ++ sep ++ join sep xs
      showKV : (String, YAMLValue) -> String
      showKV (k, v) = k ++ ": " ++ show v
  show (YBinary bs) = "!!binary " ++ show (length bs) ++ " bytes"
  show (YTimestamp ts) = "!!timestamp " ++ ts

public export
Eq YAMLValue where
  YNull == YNull = True
  YBool a == YBool b = a == b
  YInt a == YInt b = a == b
  YFloat a == YFloat b = a == b
  YString a == YString b = a == b
  YArray a == YArray b = assert_total (a == b)
  YObject a == YObject b = assert_total (a == b)
  YBinary a == YBinary b = a == b
  YTimestamp a == YTimestamp b = a == b
  _ == _ = False

--------------------------------------------------------------------------------
-- YAML Document
--------------------------------------------------------------------------------

||| A YAML document with optional directives
public export
record YAMLDocument where
  constructor MkYAMLDocument
  version : Maybe String
  tags : List (String, String)  -- Tag handles
  value : YAMLValue

public export
Show YAMLDocument where
  show doc = case doc.version of
               Just v => "%YAML " ++ v ++ "\n---\n" ++ show doc.value
               Nothing => show doc.value

||| Multiple documents in a YAML stream
public export
YAMLStream : Type
YAMLStream = List YAMLDocument

--------------------------------------------------------------------------------
-- YAML Errors
--------------------------------------------------------------------------------

||| YAML processing errors
public export
data YAMLError : Type where
  ||| Invalid YAML syntax
  SyntaxError : (message : String) -> (line : Nat) -> (col : Nat) -> YAMLError

  ||| Anchor not found
  AnchorNotFound : (name : String) -> YAMLError

  ||| Circular reference detected
  CircularReference : (anchor : String) -> YAMLError

  ||| Alias depth exceeded
  AliasDepthExceeded : (depth : Nat) -> (limit : Nat) -> YAMLError

  ||| Document count exceeded
  TooManyDocuments : (count : Nat) -> (limit : Nat) -> YAMLError

  ||| Key too long
  KeyTooLong : (length : Nat) -> (limit : Nat) -> YAMLError

  ||| Value too large
  ValueTooLarge : (size : Nat) -> (limit : Nat) -> YAMLError

  ||| Nesting too deep
  NestingTooDeep : (depth : Nat) -> (limit : Nat) -> YAMLError

  ||| Dangerous tag detected
  DangerousTag : (tag : String) -> YAMLError

  ||| Unsupported YAML version
  UnsupportedVersion : (version : String) -> YAMLError

  ||| Type coercion failed
  TypeMismatch : (expected : String) -> (actual : String) -> YAMLError

public export
Show YAMLError where
  show (SyntaxError msg line col) =
    "SyntaxError at line " ++ show line ++ ", col " ++ show col ++ ": " ++ msg
  show (AnchorNotFound name) = "AnchorNotFound: &" ++ name
  show (CircularReference anchor) = "CircularReference: *" ++ anchor
  show (AliasDepthExceeded depth limit) =
    "AliasDepthExceeded: " ++ show depth ++ " > " ++ show limit
  show (TooManyDocuments count limit) =
    "TooManyDocuments: " ++ show count ++ " > " ++ show limit
  show (KeyTooLong length limit) =
    "KeyTooLong: " ++ show length ++ " > " ++ show limit
  show (ValueTooLarge size limit) =
    "ValueTooLarge: " ++ show size ++ " > " ++ show limit
  show (NestingTooDeep depth limit) =
    "NestingTooDeep: " ++ show depth ++ " > " ++ show limit
  show (DangerousTag tag) = "DangerousTag: " ++ tag
  show (UnsupportedVersion version) = "UnsupportedVersion: " ++ version
  show (TypeMismatch expected actual) =
    "TypeMismatch: expected " ++ expected ++ ", got " ++ actual

public export
Eq YAMLError where
  SyntaxError m1 l1 c1 == SyntaxError m2 l2 c2 = m1 == m2 && l1 == l2 && c1 == c2
  AnchorNotFound n1 == AnchorNotFound n2 = n1 == n2
  CircularReference a1 == CircularReference a2 = a1 == a2
  AliasDepthExceeded d1 l1 == AliasDepthExceeded d2 l2 = d1 == d2 && l1 == l2
  TooManyDocuments c1 l1 == TooManyDocuments c2 l2 = c1 == c2 && l1 == l2
  KeyTooLong l1 m1 == KeyTooLong l2 m2 = l1 == l2 && m1 == m2
  ValueTooLarge s1 l1 == ValueTooLarge s2 l2 = s1 == s2 && l1 == l2
  NestingTooDeep d1 l1 == NestingTooDeep d2 l2 = d1 == d2 && l1 == l2
  DangerousTag t1 == DangerousTag t2 = t1 == t2
  UnsupportedVersion v1 == UnsupportedVersion v2 = v1 == v2
  TypeMismatch e1 a1 == TypeMismatch e2 a2 = e1 == e2 && a1 == a2
  _ == _ = False

||| Result type for YAML operations
public export
YAMLResult : Type -> Type
YAMLResult = Result YAMLError

--------------------------------------------------------------------------------
-- Security Options
--------------------------------------------------------------------------------

||| Security options for YAML parsing
public export
record YAMLSecurityOptions where
  constructor MkYAMLSecurityOptions
  ||| Maximum number of documents in a stream
  maxDocuments : Nat
  ||| Maximum nesting depth
  maxDepth : Nat
  ||| Maximum key length
  maxKeyLength : Nat
  ||| Maximum string value size
  maxValueSize : Nat
  ||| Maximum alias expansion depth
  maxAliasDepth : Nat
  ||| Allow anchors and aliases
  allowAnchors : Bool
  ||| Allow binary tag
  allowBinary : Bool
  ||| Allow custom tags
  allowCustomTags : Bool
  ||| Blocked tags (e.g., !!python/object)
  blockedTags : List String

||| Secure defaults (strictest settings)
public export
secureDefaults : YAMLSecurityOptions
secureDefaults = MkYAMLSecurityOptions
  { maxDocuments = 10
  , maxDepth = 50
  , maxKeyLength = 1024
  , maxValueSize = 1048576  -- 1MB
  , maxAliasDepth = 10
  , allowAnchors = False  -- Prevent alias bombs
  , allowBinary = False
  , allowCustomTags = False
  , blockedTags =
    [ "!!python/object"
    , "!!python/object/apply"
    , "!!python/object/new"
    , "!!python/name"
    , "!!python/module"
    , "!!ruby/object"
    , "!!java/object"
    , "!!php/object"
    , "tag:yaml.org,2002:python/object"
    ]
  }

||| Standard security (allows anchors with limits)
public export
standardSecurity : YAMLSecurityOptions
standardSecurity =
  { allowAnchors := True
  , maxAliasDepth := 20
  , maxDepth := 100
  } secureDefaults

||| Permissive security (for trusted input)
public export
permissiveSecurity : YAMLSecurityOptions
permissiveSecurity =
  { maxDocuments := 100
  , maxDepth := 200
  , maxKeyLength := 10240
  , maxValueSize := 104857600  -- 100MB
  , maxAliasDepth := 50
  , allowAnchors := True
  , allowBinary := True
  , allowCustomTags := True
  , blockedTags :=
    [ "!!python/object/apply"  -- Still block code execution
    , "!!python/object/new"
    ]
  } secureDefaults

--------------------------------------------------------------------------------
-- Dangerous Tags
--------------------------------------------------------------------------------

||| Known dangerous YAML tags that enable code execution
public export
dangerousTags : List String
dangerousTags =
  [ "!!python/object"
  , "!!python/object/apply"
  , "!!python/object/new"
  , "!!python/name"
  , "!!python/module"
  , "!!ruby/object"
  , "!!ruby/object:Gem::Installer"
  , "!!ruby/object:Gem::SpecFetcher"
  , "!!ruby/object:Gem::Requirement"
  , "!!java/object"
  , "!!php/object"
  , "tag:yaml.org,2002:python/object"
  , "tag:yaml.org,2002:ruby/object"
  , "tag:yaml.org,2002:java/object"
  ]

||| Check if tag is dangerous
public export
isDangerousTag : String -> Bool
isDangerousTag tag = tag `elem` dangerousTags

||| Check if tag is blocked by options
public export
isBlockedTag : YAMLSecurityOptions -> String -> Bool
isBlockedTag opts tag = tag `elem` opts.blockedTags

--------------------------------------------------------------------------------
-- YAML Tags
--------------------------------------------------------------------------------

||| Standard YAML tags
public export
data YAMLTag : Type where
  TagNull : YAMLTag
  TagBool : YAMLTag
  TagInt : YAMLTag
  TagFloat : YAMLTag
  TagStr : YAMLTag
  TagSeq : YAMLTag
  TagMap : YAMLTag
  TagBinary : YAMLTag
  TagTimestamp : YAMLTag
  TagCustom : String -> YAMLTag

public export
Show YAMLTag where
  show TagNull = "!!null"
  show TagBool = "!!bool"
  show TagInt = "!!int"
  show TagFloat = "!!float"
  show TagStr = "!!str"
  show TagSeq = "!!seq"
  show TagMap = "!!map"
  show TagBinary = "!!binary"
  show TagTimestamp = "!!timestamp"
  show (TagCustom s) = "!!" ++ s

||| Parse tag string
public export
parseTag : String -> YAMLTag
parseTag "!!null" = TagNull
parseTag "tag:yaml.org,2002:null" = TagNull
parseTag "!!bool" = TagBool
parseTag "tag:yaml.org,2002:bool" = TagBool
parseTag "!!int" = TagInt
parseTag "tag:yaml.org,2002:int" = TagInt
parseTag "!!float" = TagFloat
parseTag "tag:yaml.org,2002:float" = TagFloat
parseTag "!!str" = TagStr
parseTag "tag:yaml.org,2002:str" = TagStr
parseTag "!!seq" = TagSeq
parseTag "tag:yaml.org,2002:seq" = TagSeq
parseTag "!!map" = TagMap
parseTag "tag:yaml.org,2002:map" = TagMap
parseTag "!!binary" = TagBinary
parseTag "tag:yaml.org,2002:binary" = TagBinary
parseTag "!!timestamp" = TagTimestamp
parseTag "tag:yaml.org,2002:timestamp" = TagTimestamp
parseTag s = TagCustom s

--------------------------------------------------------------------------------
-- Type Helpers
--------------------------------------------------------------------------------

||| Get the type name of a YAML value
public export
yamlTypeName : YAMLValue -> String
yamlTypeName YNull = "null"
yamlTypeName (YBool _) = "bool"
yamlTypeName (YInt _) = "int"
yamlTypeName (YFloat _) = "float"
yamlTypeName (YString _) = "string"
yamlTypeName (YArray _) = "array"
yamlTypeName (YObject _) = "object"
yamlTypeName (YBinary _) = "binary"
yamlTypeName (YTimestamp _) = "timestamp"

||| Check if value is null
public export
isNull : YAMLValue -> Bool
isNull YNull = True
isNull _ = False

||| Check if value is scalar
public export
isScalar : YAMLValue -> Bool
isScalar (YArray _) = False
isScalar (YObject _) = False
isScalar _ = True

||| Check if value is collection
public export
isCollection : YAMLValue -> Bool
isCollection = not . isScalar

--------------------------------------------------------------------------------
-- Value Constructors
--------------------------------------------------------------------------------

||| Create null value
public export
null : YAMLValue
null = YNull

||| Create boolean value
public export
bool : Bool -> YAMLValue
bool = YBool

||| Create integer value
public export
int : Integer -> YAMLValue
int = YInt

||| Create float value
public export
float : Double -> YAMLValue
float = YFloat

||| Create string value
public export
str : String -> YAMLValue
str = YString

||| Create array value
public export
array : List YAMLValue -> YAMLValue
array = YArray

||| Create object value
public export
object : List (String, YAMLValue) -> YAMLValue
object = YObject

||| Create binary value
public export
binary : List Bits8 -> YAMLValue
binary = YBinary

||| Create timestamp value
public export
timestamp : String -> YAMLValue
timestamp = YTimestamp
